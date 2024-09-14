module Parser.Statement (
    testStep,
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Kind
import Data.Maybe
import qualified Data.Set as S
import Data.Text qualified as T
import qualified Data.Text.Lazy as TL
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

getSourceLine :: TestParser SourceLine
getSourceLine = do
    pstate <- statePosState <$> getParserState
    return $ SourceLine $ T.concat
        [ T.pack $ sourcePosPretty $ pstateSourcePos pstate
        , T.pack ": "
        , TL.toStrict $ TL.takeWhile (/='\n') $ pstateInput pstate
        ]


letStatement :: TestParser [TestStep]
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
        return [Let line tname e body]

forStatement :: TestParser [TestStep]
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
        return [For line tname (unpack <$> e) body]

exprStatement :: TestParser [ TestStep ]
exprStatement  = do
    expr <- typedExpr
    return [ ExprStatement expr ]

class (Typeable a, Typeable (ParamRep a)) => ParamType a where
    type ParamRep a :: Type
    type ParamRep a = a

    parseParam :: proxy a -> TestParser (ParamRep a)
    showParamType :: proxy a -> String

    paramDefault :: proxy a -> TestParser (ParamRep a)
    paramDefault _ = mzero

    paramFromSomeExpr :: proxy a -> SomeExpr -> Maybe (ParamRep a)
    paramFromSomeExpr _ (SomeExpr e) = cast e

instance ParamType SourceLine where
    parseParam _ = mzero
    showParamType _ = "<source line>"

instance ExprType a => ParamType (TypedVarName a) where
    parseParam _ = newVarName
    showParamType _ = "<variable>"

instance ExprType a => ParamType (Expr a) where
    parseParam _ = typedExpr
    showParamType _ = "<" ++ T.unpack (textExprType @a Proxy) ++ ">"

instance ParamType a => ParamType [a] where
    type ParamRep [a] = [ParamRep a]
    parseParam _ = listOf (parseParam @a Proxy)
    showParamType _ = showParamType @a Proxy ++ " [, " ++ showParamType @a Proxy ++ " ...]"
    paramDefault _ = return []
    paramFromSomeExpr _ se@(SomeExpr e) = cast e <|> ((:[]) <$> paramFromSomeExpr @a Proxy se)

instance ParamType a => ParamType (Maybe a) where
    type ParamRep (Maybe a) = Maybe (ParamRep a)
    parseParam _ = Just <$> parseParam @a Proxy
    showParamType _ = showParamType @a Proxy
    paramDefault _ = return Nothing
    paramFromSomeExpr _ se = Just <$> paramFromSomeExpr @a Proxy se

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

data SomeParam f = forall a. ParamType a => SomeParam (Proxy a) (f (ParamRep a))

data CommandDef a = CommandDef [(String, SomeParam Proxy)] ([SomeParam Identity] -> a)

instance Functor CommandDef where
  fmap f (CommandDef types ctor) = CommandDef types (f . ctor)

instance Applicative CommandDef where
  pure x = CommandDef [] (\case [] -> x; _ -> error "command arguments mismatch")
  CommandDef types1 ctor1 <*> CommandDef types2 ctor2 =
      CommandDef (types1 ++ types2) $ \params ->
          let (params1, params2) = splitAt (length types1) params
           in ctor1 params1 $ ctor2 params2

param :: forall a. ParamType a => String -> CommandDef a
param name = CommandDef [(name, SomeParam (Proxy @a) Proxy)] $ \case
    [SomeParam Proxy (Identity x)] -> fromJust $ cast x
    _ -> error "command arguments mismatch"

data ParamOrContext a

instance ParamType a => ParamType (ParamOrContext a) where
    type ParamRep (ParamOrContext a) = ParamRep a
    parseParam _ = parseParam @a Proxy
    showParamType _ = showParamType @a Proxy
    paramDefault _ = gets testContext >>= \case
        se@(SomeExpr ctx)
            | Just e <- paramFromSomeExpr @a Proxy se -> return e
            | otherwise -> fail $ showParamType @a Proxy <> " not available from context type '" <> T.unpack (textExprType ctx) <> "'"

paramOrContext :: forall a. ParamType a => String -> CommandDef a
paramOrContext name = CommandDef [(name, SomeParam (Proxy @(ParamOrContext a)) Proxy)] $ \case
    [SomeParam Proxy (Identity x)] -> fromJust $ cast x
    _ -> error "command arguments mismatch"

cmdLine :: CommandDef SourceLine
cmdLine = param ""

data InnerBlock

instance ParamType InnerBlock where
    type ParamRep InnerBlock = [TestStep]
    parseParam _ = mzero
    showParamType _ = "<code block>"

instance ParamType TestStep where
    parseParam _ = mzero
    showParamType _ = "<code line>"

innerBlock :: CommandDef [TestStep]
innerBlock = CommandDef [("", SomeParam (Proxy @InnerBlock) Proxy)] $ \case
    [SomeParam Proxy (Identity x)] -> fromJust $ cast x
    _ -> error "command arguments mismatch"

command :: String -> CommandDef TestStep -> TestParser [TestStep]
command name (CommandDef types ctor) = do
    indent <- L.indentLevel
    line <- getSourceLine
    wsymbol name
    localState $ do
        restOfLine indent [] line $ map (fmap $ \(SomeParam p@(_ :: Proxy p) Proxy) -> SomeParam p $ Nothing @(ParamRep p)) types
  where
    restOfLine :: Pos -> [(Pos, [(String, SomeParam Maybe)])] -> SourceLine -> [(String, SomeParam Maybe)] -> TestParser [TestStep]
    restOfLine cmdi partials line params = choice
        [do void $ lookAhead eol
            iparams <- forM params $ \case
                (_, SomeParam (p :: Proxy p) Nothing)
                    | Just (Refl :: p :~: SourceLine) <- eqT -> return $ SomeParam p $ Identity line
                    | Just (Refl :: p :~: InnerBlock) <- eqT -> SomeParam p . Identity <$> restOfParts cmdi partials
                (sym, SomeParam p Nothing) -> choice
                    [ SomeParam p . Identity <$> paramDefault p
                    , fail $ "missing " ++ (if null sym then "" else "'" ++ sym ++ "' ") ++ showParamType p
                    ]
                (_, SomeParam (p :: Proxy p) (Just x)) -> return $ SomeParam p $ Identity x
            return [ctor iparams]

        ,do symbol ":"
            scn
            indent <- L.indentLevel
            restOfParts cmdi ((indent, params) : partials)

        ,do tryParams cmdi partials line [] params
        ]

    restOfParts :: Pos -> [(Pos, [(String, SomeParam Maybe)])] -> TestParser [TestStep]
    restOfParts cmdi [] = testBlock cmdi
    restOfParts cmdi partials@((partIndent, params) : rest) = do
        scn
        pos <- L.indentLevel
        line <- getSourceLine
        optional eof >>= \case
            Just _ -> return []
            _ | pos <  partIndent -> restOfParts cmdi rest
              | pos == partIndent -> (++) <$> restOfLine cmdi partials line params <*> restOfParts cmdi partials
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

testLocal :: TestParser [TestStep]
testLocal = do
    ref <- L.indentLevel
    wsymbol "local"
    symbol ":"
    void $ eol

    indent <- L.indentGuard scn GT ref
    localState $ testBlock indent

testWith :: TestParser [TestStep]
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

testSubnet :: TestParser [TestStep]
testSubnet = command "subnet" $ Subnet
    <$> param ""
    <*> paramOrContext "of"
    <*> innerBlock

testNode :: TestParser [TestStep]
testNode = command "node" $ DeclNode
    <$> param ""
    <*> paramOrContext "on"
    <*> innerBlock

testSpawn :: TestParser [TestStep]
testSpawn = command "spawn" $ Spawn
    <$> param "as"
    <*> paramOrContext "on"
    <*> innerBlock

testSend :: TestParser [TestStep]
testSend = command "send" $ Send
    <$> paramOrContext "to"
    <*> param ""

testExpect :: TestParser [TestStep]
testExpect = command "expect" $ Expect
    <$> cmdLine
    <*> paramOrContext "from"
    <*> param ""
    <*> param "capture"
    <*> innerBlock

testFlush :: TestParser [TestStep]
testFlush = command "flush" $ Flush
    <$> paramOrContext "from"
    <*> param ""

testGuard :: TestParser [TestStep]
testGuard = command "guard" $ Guard
    <$> cmdLine
    <*> param ""

testDisconnectNode :: TestParser [TestStep]
testDisconnectNode = command "disconnect_node" $ DisconnectNode
    <$> paramOrContext ""
    <*> innerBlock

testDisconnectNodes :: TestParser [TestStep]
testDisconnectNodes = command "disconnect_nodes" $ DisconnectNodes
    <$> paramOrContext ""
    <*> innerBlock

testDisconnectUpstream :: TestParser [TestStep]
testDisconnectUpstream = command "disconnect_upstream" $ DisconnectUpstream
    <$> paramOrContext ""
    <*> innerBlock

testPacketLoss :: TestParser [TestStep]
testPacketLoss = command "packet_loss" $ PacketLoss
    <$> param ""
    <*> paramOrContext "on"
    <*> innerBlock


testBlock :: Pos -> TestParser [TestStep]
testBlock indent = concat <$> go
  where
    go = do
        scn
        pos <- L.indentLevel
        optional eof >>= \case
            Just _ -> return []
            _ | pos <  indent -> return []
              | pos == indent -> (:) <$> testStep <*> go
              | otherwise     -> L.incorrectIndent EQ indent pos

testStep :: TestParser [TestStep]
testStep = choice
    [ letStatement
    , forStatement
    , testLocal
    , testWith
    , testSubnet
    , testNode
    , testSpawn
    , testSend
    , testExpect
    , testFlush
    , testGuard
    , testDisconnectNode
    , testDisconnectNodes
    , testDisconnectUpstream
    , testPacketLoss
    , exprStatement
    ]
