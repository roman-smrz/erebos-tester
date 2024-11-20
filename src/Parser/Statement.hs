module Parser.Statement (
    testStep,
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Bifunctor
import Data.Kind
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Typeable

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network (Network, Node)
import Parser.Core
import Parser.Expr
import Process (Process)
import Test
import Util

letStatement :: TestParser (Expr TestBlock)
letStatement = do
    line <- getSourceLine
    indent <- L.indentLevel
    wsymbol "let"
    off <- stateOffset <$> getParserState
    name <- varName
    osymbol "="
    SomeExpr e <- someExpr

    localState $ do
        let tname = TypedVarName name
        addVarName off tname
        void $ eol
        body <- testBlock indent
        return $ Pure $ TestBlock [ Let line tname e body ]

forStatement :: TestParser (Expr TestBlock)
forStatement = do
    line <- getSourceLine
    ref <- L.indentLevel
    wsymbol "for"
    voff <- stateOffset <$> getParserState
    name <- varName

    wsymbol "in"
    loff <- stateOffset <$> getParserState
    SomeExpr e <- someExpr
    let err = parseError $ FancyError loff $ S.singleton $ ErrorFail $ T.unpack $
            "expected a list, expression has type '" <> textExprType e <> "'"
    ExprListUnpacker unpack _ <- maybe err return $ exprListUnpacker e

    symbol ":"
    scn
    indent <- L.indentGuard scn GT ref
    localState $ do
        let tname = TypedVarName name
        addVarName voff tname
        body <- testBlock indent
        return $ Pure $ TestBlock [ For line tname (unpack <$> e) body ]

exprStatement :: TestParser (Expr TestBlock)
exprStatement = do
    ref <- L.indentLevel
    off <- stateOffset <$> getParserState
    SomeExpr expr <- someExpr
    choice
        [ continuePartial off ref expr
        , unifyExpr off Proxy expr
        ]
  where
    continuePartial :: ExprType a => Int -> Pos -> Expr a -> TestParser (Expr TestBlock)
    continuePartial off ref expr = do
        symbol ":"
        void eol
        (fun :: Expr (FunctionType TestBlock)) <- unifyExpr off Proxy expr
        scn
        indent <- L.indentGuard scn GT ref
        blockOf indent $ do
            coff <- stateOffset <$> getParserState
            sline <- getSourceLine
            args <- functionArguments (checkFunctionArguments (exprArgs fun)) someExpr literal (\poff -> lookupVarExpr poff sline . VarName)
            let fun' = ArgsApp args fun
            choice
                [ continuePartial coff indent fun'
                , unifyExpr coff Proxy fun'
                ]

class (Typeable a, Typeable (ParamRep a)) => ParamType a where
    type ParamRep a :: Type
    type ParamRep a = a

    parseParam :: proxy a -> TestParser (ParamRep a)
    showParamType :: proxy a -> String

    paramDefault :: proxy a -> TestParser (ParamRep a)
    paramDefault _ = mzero

    paramFromSomeExpr :: proxy a -> SomeExpr -> Maybe (ParamRep a)
    paramFromSomeExpr _ (SomeExpr e) = cast e

    paramExpr :: ParamRep a -> Expr a
    default paramExpr :: ParamRep a ~ a => ParamRep a -> Expr a
    paramExpr = Pure

instance ParamType SourceLine where
    parseParam _ = mzero
    showParamType _ = "<source line>"

instance ExprType a => ParamType (TypedVarName a) where
    parseParam _ = newVarName
    showParamType _ = "<variable>"

instance ExprType a => ParamType (Expr a) where
    parseParam _ = do
        off <- stateOffset <$> getParserState
        SomeExpr e <- literal <|> variable <|> between (symbol "(") (symbol ")") someExpr
        unifyExpr off Proxy e
    showParamType _ = "<" ++ T.unpack (textExprType @a Proxy) ++ ">"

instance ParamType a => ParamType [a] where
    type ParamRep [a] = [ParamRep a]
    parseParam _ = listOf (parseParam @a Proxy)
    showParamType _ = showParamType @a Proxy ++ " [, " ++ showParamType @a Proxy ++ " ...]"
    paramDefault _ = return []
    paramFromSomeExpr _ se@(SomeExpr e) = cast e <|> ((:[]) <$> paramFromSomeExpr @a Proxy se)
    paramExpr = sequenceA . fmap paramExpr

instance ParamType a => ParamType (Maybe a) where
    type ParamRep (Maybe a) = Maybe (ParamRep a)
    parseParam _ = Just <$> parseParam @a Proxy
    showParamType _ = showParamType @a Proxy
    paramDefault _ = return Nothing
    paramFromSomeExpr _ se = Just <$> paramFromSomeExpr @a Proxy se
    paramExpr = sequenceA . fmap paramExpr

instance (ParamType a, ParamType b) => ParamType (Either a b) where
    type ParamRep (Either a b) = Either (ParamRep a) (ParamRep b)
    parseParam _ = try' (Left <$> parseParam @a Proxy) <|> (Right <$> parseParam @b Proxy)
      where
        try' act = try $ region id $ do
            x <- act
            (stateParseErrors <$> getParserState) >>= \case
                [] -> return x
                (_ : _) -> fail ""
    showParamType _ = showParamType @a Proxy ++ " or " ++ showParamType @b Proxy
    paramFromSomeExpr _ se = (Left <$> paramFromSomeExpr @a Proxy se) <|> (Right <$> paramFromSomeExpr @b Proxy se)
    paramExpr = either (fmap Left . paramExpr) (fmap Right . paramExpr)

data SomeParam f = forall a. ParamType a => SomeParam (Proxy a) (f (ParamRep a))

data CommandDef a = CommandDef [(String, SomeParam Proxy)] ([SomeParam Identity] -> Expr a)

instance Functor CommandDef where
    fmap f (CommandDef types ctor) = CommandDef types (fmap f . ctor)

instance Applicative CommandDef where
    pure x = CommandDef [] (\case [] -> Pure x; _ -> error "command arguments mismatch")
    CommandDef types1 ctor1 <*> CommandDef types2 ctor2 =
        CommandDef (types1 ++ types2) $ \params ->
            let (params1, params2) = splitAt (length types1) params
             in ctor1 params1 <*> ctor2 params2

param :: forall a. ParamType a => String -> CommandDef a
param name = CommandDef [(name, SomeParam (Proxy @a) Proxy)] $ \case
    [SomeParam Proxy (Identity x)] -> paramExpr $ fromJust $ cast x
    _ -> error "command arguments mismatch"

newtype ParamOrContext a = ParamOrContext { fromParamOrContext :: a }
    deriving (Functor, Foldable, Traversable)

instance ParamType a => ParamType (ParamOrContext a) where
    type ParamRep (ParamOrContext a) = ParamOrContext (ParamRep a)
    parseParam _ = ParamOrContext <$> parseParam @a Proxy
    showParamType _ = showParamType @a Proxy
    paramDefault _ = gets testContext >>= \case
        se@(SomeExpr ctx)
            | Just e <- paramFromSomeExpr @a Proxy se -> return (ParamOrContext e)
            | otherwise -> fail $ showParamType @a Proxy <> " not available from context type '" <> T.unpack (textExprType ctx) <> "'"
    paramExpr = sequenceA . fmap paramExpr

paramOrContext :: forall a. ParamType a => String -> CommandDef a
paramOrContext name = fromParamOrContext <$> param name

cmdLine :: CommandDef SourceLine
cmdLine = param ""

newtype InnerBlock = InnerBlock { fromInnerBlock :: Expr TestBlock }

instance ParamType InnerBlock where
    parseParam _ = mzero
    showParamType _ = "<code block>"

instance ParamType TestStep where
    parseParam _ = mzero
    showParamType _ = "<code line>"

innerBlock :: CommandDef (Expr TestBlock)
innerBlock = fromInnerBlock <$> param ""

newtype ExprParam a = ExprParam { fromExprParam :: a }
    deriving (Functor, Foldable, Traversable)

instance ExprType a => ParamType (ExprParam a) where
    type ParamRep (ExprParam a) = Expr a
    parseParam _ = do
        off <- stateOffset <$> getParserState
        SomeExpr e <- literal <|> variable <|> between (symbol "(") (symbol ")") someExpr
        unifyExpr off Proxy e
    showParamType _ = "<" ++ T.unpack (textExprType @a Proxy) ++ ">"
    paramExpr = fmap ExprParam

command :: String -> CommandDef TestStep -> TestParser (Expr TestBlock)
command name (CommandDef types ctor) = do
    indent <- L.indentLevel
    line <- getSourceLine
    wsymbol name
    localState $ do
        restOfLine indent [] line $ map (fmap $ \(SomeParam p@(_ :: Proxy p) Proxy) -> SomeParam p $ Nothing @(ParamRep p)) types
  where
    restOfLine :: Pos -> [(Pos, [(String, SomeParam Maybe)])] -> SourceLine -> [(String, SomeParam Maybe)] -> TestParser (Expr TestBlock)
    restOfLine cmdi partials line params = choice
        [do void $ lookAhead eol
            iparams <- forM params $ \case
                (_, SomeParam (p :: Proxy p) Nothing)
                    | Just (Refl :: p :~: SourceLine) <- eqT -> return $ SomeParam p $ Identity line
                    | Just (Refl :: p :~: InnerBlock) <- eqT -> SomeParam p . Identity . InnerBlock <$> restOfParts cmdi partials
                (sym, SomeParam p Nothing) -> choice
                    [ SomeParam p . Identity <$> paramDefault p
                    , fail $ "missing " ++ (if null sym then "" else "'" ++ sym ++ "' ") ++ showParamType p
                    ]
                (_, SomeParam (p :: Proxy p) (Just x)) -> return $ SomeParam p $ Identity x
            return $ (TestBlock . (: [])) <$> ctor iparams

        ,do symbol ":"
            scn
            indent <- L.indentLevel
            restOfParts cmdi ((indent, params) : partials)

        ,do tryParams cmdi partials line [] params
        ]

    restOfParts :: Pos -> [(Pos, [(String, SomeParam Maybe)])] -> TestParser (Expr TestBlock)
    restOfParts cmdi [] = testBlock cmdi
    restOfParts cmdi partials@((partIndent, params) : rest) = do
        scn
        pos <- L.indentLevel
        line <- getSourceLine
        optional eof >>= \case
            Just _ -> return $ Pure mempty
            _ | pos <  partIndent -> restOfParts cmdi rest
              | pos == partIndent -> mappend <$> restOfLine cmdi partials line params <*> restOfParts cmdi partials
              | otherwise         -> L.incorrectIndent EQ partIndent pos

    tryParam sym (SomeParam (p :: Proxy p) cur) = do
        when (not $ null sym) $ wsymbol sym
        when (isJust cur) $ do
            fail $ "multiple " ++ (if null sym then "unnamed" else "'" ++ sym ++ "'") ++ " parameters"
        SomeParam p . Just <$> parseParam @p Proxy

    tryParams cmdi partIndent line prev ((sym, p) : ps) = choice $
        (if null sym then reverse else id) {- try unnamed parameter as last option -} $
        [do p' <- tryParam sym p
            restOfLine cmdi partIndent line $ concat [reverse prev, [(sym, p')], ps]
        ,do tryParams cmdi partIndent line ((sym, p) : prev) ps
        ]
    tryParams _ _ _ _ [] = mzero

testLocal :: TestParser (Expr TestBlock)
testLocal = do
    ref <- L.indentLevel
    wsymbol "local"
    symbol ":"
    void $ eol

    indent <- L.indentGuard scn GT ref
    localState $ testBlock indent

testWith :: TestParser (Expr TestBlock)
testWith = do
    ref <- L.indentLevel
    wsymbol "with"

    off <- stateOffset <$> getParserState
    ctx@(SomeExpr (_ :: Expr ctxe)) <- someExpr
    let expected =
            [ ExprTypePrim @Network Proxy
            , ExprTypePrim @Node Proxy
            , ExprTypePrim @Process Proxy
            ]
    notAllowed <- flip allM expected $ \case
        ExprTypePrim (Proxy :: Proxy a) | Just (Refl :: ctxe :~: a) <- eqT -> return False
        _ -> return True
    when notAllowed $ registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
        "expected " <> T.intercalate ", " (map (("'"<>) . (<>"'") . textSomeExprType) expected) <> ", expression has type '" <> textExprType @ctxe Proxy <> "'"

    symbol ":"
    void $ eol

    indent <- L.indentGuard scn GT ref
    localState $ do
        modify $ \s -> s { testContext = ctx }
        testBlock indent

testSubnet :: TestParser (Expr TestBlock)
testSubnet = command "subnet" $ Subnet
    <$> param ""
    <*> (fromExprParam <$> paramOrContext "of")
    <*> innerBlock

testNode :: TestParser (Expr TestBlock)
testNode = command "node" $ DeclNode
    <$> param ""
    <*> (fromExprParam <$> paramOrContext "on")
    <*> innerBlock

testSpawn :: TestParser (Expr TestBlock)
testSpawn = command "spawn" $ Spawn
    <$> param "as"
    <*> (bimap fromExprParam fromExprParam <$> paramOrContext "on")
    <*> innerBlock

testExpect :: TestParser (Expr TestBlock)
testExpect = command "expect" $ Expect
    <$> cmdLine
    <*> (fromExprParam <$> paramOrContext "from")
    <*> param ""
    <*> param "capture"
    <*> innerBlock

testDisconnectNode :: TestParser (Expr TestBlock)
testDisconnectNode = command "disconnect_node" $ DisconnectNode
    <$> (fromExprParam <$> paramOrContext "")
    <*> innerBlock

testDisconnectNodes :: TestParser (Expr TestBlock)
testDisconnectNodes = command "disconnect_nodes" $ DisconnectNodes
    <$> (fromExprParam <$> paramOrContext "")
    <*> innerBlock

testDisconnectUpstream :: TestParser (Expr TestBlock)
testDisconnectUpstream = command "disconnect_upstream" $ DisconnectUpstream
    <$> (fromExprParam <$> paramOrContext "")
    <*> innerBlock

testPacketLoss :: TestParser (Expr TestBlock)
testPacketLoss = command "packet_loss" $ PacketLoss
    <$> (fromExprParam <$> paramOrContext "")
    <*> (fromExprParam <$> paramOrContext "on")
    <*> innerBlock


testBlock :: Pos -> TestParser (Expr TestBlock)
testBlock indent = blockOf indent testStep

blockOf :: Monoid a => Pos -> TestParser a -> TestParser a
blockOf indent step = go
  where
    go = do
        scn
        pos <- L.indentLevel
        optional eof >>= \case
            Just _ -> return mempty
            _ | pos <  indent -> return mempty
              | pos == indent -> mappend <$> step <*> go
              | otherwise     -> L.incorrectIndent EQ indent pos

testStep :: TestParser (Expr TestBlock)
testStep = choice
    [ letStatement
    , forStatement
    , testLocal
    , testWith
    , testSubnet
    , testNode
    , testSpawn
    , testExpect
    , testDisconnectNode
    , testDisconnectNodes
    , testDisconnectUpstream
    , testPacketLoss
    , exprStatement
    ]
