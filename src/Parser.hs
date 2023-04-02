{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Applicative (liftA2)
import Control.Monad.Combinators.Expr
import Control.Monad.Identity
import Control.Monad.State

import Data.Char
import Data.Kind
import Data.Maybe
import Data.Scientific
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text qualified as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Typeable
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import System.Exit

import Network (Network, Node)
import Process (Process, ProcName(..))
import Test
import Util

type TestParser = ParsecT Void TestStream (State TestParserState)

type TestStream = TL.Text

data TestParserState = TestParserState
    { testVars :: [(VarName, SomeExprType)]
    , testContext :: SomeExpr
    }

data SomeExprType = forall a. ExprType a => SomeExprType (Proxy a)

someEmptyVar :: SomeExprType -> SomeVarValue
someEmptyVar (SomeExprType (Proxy :: Proxy a)) = SomeVarValue $ emptyVarValue @a

textSomeExprType :: SomeExprType -> Text
textSomeExprType (SomeExprType p) = textExprType p

instance MonadEval TestParser where
    lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") (return . someEmptyVar) =<< gets (lookup name . testVars)
    rootNetwork = return emptyVarValue

skipLineComment :: TestParser ()
skipLineComment = L.skipLineComment $ TL.pack "#"

scn :: TestParser ()
scn = L.space space1 skipLineComment empty

sc :: TestParser ()
sc = L.space hspace1 skipLineComment empty

wordChar :: TestParser (Token TestStream)
wordChar = alphaNumChar <|> char '_'

lexeme :: TestParser a -> TestParser a
lexeme = L.lexeme sc

symbol, osymbol, wsymbol :: String -> TestParser ()
symbol str  = void $       (string (TL.pack str)) <* sc
osymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy operatorChar) <* sc
wsymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy wordChar) <* sc

operatorChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
operatorChar = satisfy $ (`elem` ['.', '+', '-', '*', '/', '='])
{-# INLINE operatorChar #-}

localState :: TestParser a -> TestParser a
localState inner = do
    s <- get
    x <- inner
    put s
    return x

toplevel :: TestParser a -> TestParser a
toplevel = L.nonIndented scn

block :: (a -> [b] -> TestParser c) -> TestParser a -> TestParser b -> TestParser c
block merge header item = L.indentBlock scn $ do
    h <- header
    choice
        [ do symbol ":"
             return $ L.IndentSome Nothing (merge h) item
        , L.IndentNone <$> merge h []
        ]

listOf :: TestParser a -> TestParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ symbol "," >> listOf item, return [] ]

procName :: TestParser ProcName
procName = label "process name" $ lexeme $ do
    c <- lowerChar
    cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
    return $ ProcName $ TL.toStrict (c `TL.cons` cs)

identifier :: TestParser Text
identifier = do
    lexeme $ TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')

varName :: TestParser VarName
varName = VarName <$> identifier

newVarName :: forall a. ExprType a => TestParser (TypedVarName a)
newVarName = do
    off <- stateOffset <$> getParserState
    name <- TypedVarName <$> varName
    addVarName off name
    return name

addVarName :: forall a. ExprType a => Int -> TypedVarName a -> TestParser ()
addVarName off (TypedVarName name) = do
    gets (lookup name . testVars) >>= \case
        Just _ -> parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            T.pack "variable '" <> textVarName name <> T.pack "' already exists"
        Nothing -> return ()
    modify $ \s -> s { testVars = (name, SomeExprType @a Proxy) : testVars s }

someExpansion :: TestParser SomeExpr
someExpansion = do
    void $ char '$'
    choice
        [do name <- VarName . TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')
            SomeVarValue (_ :: a) <- lookupVar name
            return $ SomeExpr $ Variable @a name
        , between (char '{') (char '}') someExpr
        ]

stringExpansion :: ExprType a => Text -> (forall b. ExprType b => Expr b -> [Maybe (Expr a)]) -> TestParser (Expr a)
stringExpansion tname conv = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpansion
    let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
            [ tname, T.pack " expansion not defined for '", textExprType e, T.pack "'" ]

    maybe err return $ listToMaybe $ catMaybes $ conv e

numberLiteral :: TestParser SomeExpr
numberLiteral = label "number" $ lexeme $ do
    x <- L.scientific
    choice
        [ return (SomeExpr $ Pure (x / 100)) <* void (char ('%'))
        , if base10Exponent x == 0
             then return $ SomeExpr $ Pure (coefficient x)
             else return $ SomeExpr $ Pure x
        ]

quotedString :: TestParser (Expr Text)
quotedString = label "string" $ lexeme $ do
    void $ char '"'
    let inner = choice
            [ char '"' >> return []
            , takeWhile1P Nothing (`notElem` ['\"', '\\', '$']) >>= \s -> (Pure (TL.toStrict s):) <$> inner
            ,do void $ char '\\'
                c <- choice
                    [ char '\\' >> return '\\'
                    , char '"' >> return '"'
                    , char '$' >> return '$'
                    , char 'n' >> return '\n'
                    , char 'r' >> return '\r'
                    , char 't' >> return '\t'
                    ]
                (Pure (T.singleton c) :) <$> inner
            ,do e <- stringExpansion (T.pack "string") $ \e ->
                    [ cast e
                    , fmap (T.pack . show @Integer) <$> cast e
                    , fmap (T.pack . show @Scientific) <$> cast e
                    ]
                (e:) <$> inner
            ]
    Concat <$> inner

regex :: TestParser (Expr Regex)
regex = label "regular expression" $ lexeme $ do
    void $ char '/'
    let inner = choice
            [ char '/' >> return []
            , takeWhile1P Nothing (`notElem` ['/', '\\', '$']) >>= \s -> (Pure (RegexPart (TL.toStrict s)) :) <$> inner
            ,do void $ char '\\'
                s <- choice
                    [ char '/' >> return (Pure $ RegexPart $ T.singleton '/')
                    , anySingle >>= \c -> return (Pure $ RegexPart $ T.pack ['\\', c])
                    ]
                (s:) <$> inner
            ,do e <- stringExpansion (T.pack "regex") $ \e ->
                    [ cast e
                    , fmap RegexString <$> cast e
                    , fmap (RegexString . T.pack . show @Integer) <$> cast e
                    , fmap (RegexString . T.pack . show @Scientific) <$> cast e
                    ]
                (e:) <$> inner
            ]
    expr <- Regex <$> inner
    _ <- eval expr -- test regex parsing with empty variables
    return expr

list :: TestParser SomeExpr
list = label "list" $ do
    symbol "["
    SomeExpr x <- someExpr

    let enumErr off = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            "list range enumeration not defined for '" <> textExprType x <> "'"
    choice
        [do symbol "]"
            return $ SomeExpr $ fmap (:[]) x

        ,do off <- stateOffset <$> getParserState
            osymbol ".."
            ExprEnumerator fromTo _ <- maybe (enumErr off) return $ exprEnumerator x
            y <- typedExpr
            symbol "]"
            return $ SomeExpr $ fromTo <$> x <*> y

        ,do symbol ","
            y <- typedExpr

            choice
                [do off <- stateOffset <$> getParserState
                    osymbol ".."
                    ExprEnumerator _ fromThenTo <- maybe (enumErr off) return $ exprEnumerator x
                    z <- typedExpr
                    symbol "]"
                    return $ SomeExpr $ fromThenTo <$> x <*> y <*> z

                ,do symbol ","
                    xs <- listOf typedExpr
                    symbol "]"
                    return $ SomeExpr $ foldr (liftA2 (:)) (Pure []) (x:y:xs)
                ]
        ]

data SomeExpr = forall a. ExprType a => SomeExpr (Expr a)

data SomeUnOp = forall a b. (ExprType a, ExprType b) => SomeUnOp (a -> b)

applyUnOp :: forall a b sa.
    (ExprType a, ExprType b, ExprType sa) =>
    (a -> b) -> Expr sa -> Maybe (Expr b)
applyUnOp op x = do
    Refl :: a :~: sa <- eqT
    return $ op <$> x

data SomeBinOp = forall a b c. (ExprType a, ExprType b, ExprType c) => SomeBinOp (a -> b -> c)

applyBinOp :: forall a b c sa sb.
    (ExprType a, ExprType b, ExprType c, ExprType sa, ExprType sb) =>
    (a -> b -> c) -> Expr sa -> Expr sb -> Maybe (Expr c)
applyBinOp op x y = do
    Refl :: a :~: sa <- eqT
    Refl :: b :~: sb <- eqT
    return $ op <$> x <*> y

someExpr :: TestParser SomeExpr
someExpr = join inner <?> "expression"
  where
    inner = makeExprParser term table

    parens = between (symbol "(") (symbol ")")

    term = parens inner <|> literal <|> variable <?> "term"

    table = [ [ recordSelector
              ]
            , [ prefix "-" $ [ SomeUnOp (negate @Integer)
                             , SomeUnOp (negate @Scientific)
                             ]
              ]
            , [ binary "*" $ [ SomeBinOp ((*) @Integer)
                             , SomeBinOp ((*) @Scientific)
                             ]
              {- TODO: parsing issues with regular expressions
              , binary "/" $ [ SomeBinOp (div @Integer)
                             , SomeBinOp ((/) @Scientific)
                             ]
              -}
              ]
            , [ binary "+" $ [ SomeBinOp ((+) @Integer)
                             , SomeBinOp ((+) @Scientific)
                             ]
              , binary "-" $ [ SomeBinOp ((-) @Integer)
                             , SomeBinOp ((-) @Scientific)
                             ]
              ]
            , [ binary' "==" (\op xs ys -> length xs == length ys && and (zipWith op xs ys)) $
                              [ SomeBinOp ((==) @Integer)
                              , SomeBinOp ((==) @Scientific)
                              , SomeBinOp ((==) @Text)
                              ]
              , binary' "/=" (\op xs ys -> length xs /= length ys || or  (zipWith op xs ys)) $
                              [ SomeBinOp ((/=) @Integer)
                              , SomeBinOp ((/=) @Scientific)
                              , SomeBinOp ((/=) @Text)
                              ]
              ]
            ]

    prefix :: String -> [SomeUnOp] -> Operator TestParser (TestParser SomeExpr)
    prefix name ops = Prefix $ do
        off <- stateOffset <$> getParserState
        void $ osymbol name
        return $ \p -> do
            SomeExpr e <- p
            let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "'"]
            maybe err return $ listToMaybe $ catMaybes $ map (\(SomeUnOp op) -> SomeExpr <$> applyUnOp op e) ops

    binary :: String -> [SomeBinOp] -> Operator TestParser (TestParser SomeExpr)
    binary name = binary' name (undefined :: forall a b. (a -> b -> Void) -> [a] -> [b] -> Integer)
      -- use 'Void' that can never match actually used type to disable recursion

    binary' :: forall c c'. (Typeable c, ExprType c')
            => String
            -> (forall a b. (a -> b -> c) -> [a] -> [b] -> c')
            -> [SomeBinOp]
            -> Operator TestParser (TestParser SomeExpr)
    binary' name listmap ops = InfixL $ do
        off <- stateOffset <$> getParserState
        void $ osymbol name

        return $ \p q -> do
            SomeExpr e <- p
            SomeExpr f <- q

            let eqT' :: forall r s t. (Typeable r, Typeable s, Typeable t) => (r -> s -> t) -> Maybe ((r -> s -> t) :~: (r -> s -> c))
                eqT' _ = eqT

            let proxyOf :: proxy a -> Proxy a
                proxyOf _ = Proxy

            let tryop :: forall a b d sa sb.
                    (ExprType a, ExprType b, ExprType d, ExprType sa, ExprType sb) =>
                    (a -> b -> d) -> Proxy sa -> Proxy sb -> Maybe SomeExpr
                tryop op pe pf = msum
                    [ SomeExpr <$> applyBinOp op e f
                    , do Refl <- eqT' op
                         ExprListUnpacker _ une <- exprListUnpacker pe
                         ExprListUnpacker _ unf <- exprListUnpacker pf
                         tryop (listmap op) (une pe) (unf pf)
                    ]

            let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "' and '", textExprType f, T.pack "'"]
            maybe err return $ listToMaybe $ catMaybes $ map (\(SomeBinOp op) -> tryop op (proxyOf e) (proxyOf f)) ops

    recordSelector :: Operator TestParser (TestParser SomeExpr)
    recordSelector = Postfix $ fmap (foldl1 (flip (.))) $ some $ do
        void $ osymbol "."
        off <- stateOffset <$> getParserState
        m <- identifier
        return $ \p -> do
            SomeExpr e <- p
            let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [ T.pack "value of type ", textExprType e, T.pack " does not have member '", m, T.pack "'" ]
            maybe err return $ applyRecordSelector e <$> lookup m recordMembers

    applyRecordSelector :: ExprType a => Expr a -> RecordSelector a -> SomeExpr
    applyRecordSelector e (RecordSelector f) = SomeExpr $ f <$> e

    literal = label "literal" $ choice
        [ return <$> numberLiteral
        , return . SomeExpr <$> quotedString
        , return . SomeExpr <$> regex
        , return <$> list
        ]

    variable = label "variable" $ do
        name <- varName
        SomeVarValue (_ :: a) <- lookupVar name
        return $ return $ SomeExpr $ Variable @a name

typedExpr :: forall a. ExprType a => TestParser (Expr a)
typedExpr = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpr
    let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
            [ T.pack "expected '", textExprType @a Proxy, T.pack "', expression has type '", textExprType e, T.pack "'" ]
    maybe err return $ cast e


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

instance ParamType ProcName where
    parseParam _ = procName
    showParamType _ = "<proc>"

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

instance (ParamType a, ParamType b) => ParamType (Either a b) where
    type ParamRep (Either a b) = Either (ParamRep a) (ParamRep b)
    parseParam _ = try (Left <$> parseParam @a Proxy) <|> (Right <$> parseParam @b Proxy)
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
            [ SomeExprType @Network Proxy
            , SomeExprType @Node Proxy
            , SomeExprType @Process Proxy
            ]
    notAllowed <- flip allM expected $ \case
        SomeExprType (Proxy :: Proxy a) | Just (Refl :: ctxe :~: a) <- eqT -> return False
        _ -> return True
    when notAllowed $ parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
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


testWait :: TestParser [TestStep]
testWait = do
    wsymbol "wait"
    return [Wait]

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
    , testGuard
    , testDisconnectNode
    , testDisconnectNodes
    , testDisconnectUpstream
    , testPacketLoss
    , testWait
    ]

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header testStep
    where header = do
              wsymbol "test"
              lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseTestDefinitions :: TestParser [Test]
parseTestDefinitions = do
    tests <- many parseTestDefinition
    eof
    return tests

parseTestFile :: FilePath -> IO [Test]
parseTestFile path = do
    content <- TL.readFile path
    let initState = TestParserState
            { testVars = []
            , testContext = SomeExpr RootNetwork
            }
    case evalState (runParserT parseTestDefinitions path content) initState of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right tests -> return tests
