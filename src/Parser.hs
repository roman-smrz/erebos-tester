{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad.Combinators.Expr
import Control.Monad.Identity
import Control.Monad.State

import Data.Char
import Data.Maybe
import Data.Set (Set)
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

import Test

type TestParser = ParsecT Void TestStream (State TestParserState)

type TestStream = TL.Text

data TestParserState = TestParserState
    { testProcs :: Set ProcName
    , testVars :: [(VarName, SomeExprType)]
    }

data SomeExprType = forall a. ExprType a => SomeExprType (Proxy a)

someEmptyVar :: SomeExprType -> SomeVarValue
someEmptyVar (SomeExprType (Proxy :: Proxy a)) = SomeVarValue $ emptyVarValue @a

instance MonadEval TestParser where
    lookupVar (VarName [_, ip]) | ip == T.pack "ip" = return $ SomeVarValue T.empty
    lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") (return . someEmptyVar) =<< gets (lookup name . testVars)

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

symbol :: String -> TestParser ()
symbol = void . L.symbol sc . TL.pack

wsymbol :: String -> TestParser ()
wsymbol str = void $ lexeme $ string (TL.pack str) <* notFollowedBy wordChar

toplevel :: TestParser a -> TestParser a
toplevel = L.nonIndented scn

block :: (a -> [b] -> TestParser c) -> TestParser a -> TestParser b -> TestParser c
block merge header item = L.indentBlock scn $ do
    h <- header
    choice
        [ do try $ void $ lexeme $ char ':'
             return $ L.IndentSome Nothing (merge h) item
        , L.IndentNone <$> merge h []
        ]

listOf :: TestParser a -> TestParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ symbol "," >> listOf item, return [] ]

nodeName :: TestParser NodeName
nodeName = label "network node name" $ lexeme $ do
    c <- lowerChar
    cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
    return $ NodeName $ TL.toStrict (c `TL.cons` cs)

procName :: TestParser ProcName
procName = label "process name" $ lexeme $ do
    c <- lowerChar
    cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '-')
    return $ ProcName $ TL.toStrict (c `TL.cons` cs)

identifier :: TestParser Text
identifier = do
    lexeme $ TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')

varName :: TestParser VarName
varName = lexeme $ do
    VarName . T.splitOn (T.singleton '.') . TL.toStrict <$>
        takeWhile1P Nothing (\x -> isAlphaNum x || x == '_' || x == '.')

newVarName :: forall a proxy. ExprType a => proxy a -> TestParser VarName
newVarName proxy = do
    name <- VarName . (:[]) <$> identifier
    addVarName proxy name
    return name

addVarName :: forall a proxy. ExprType a => proxy a -> VarName -> TestParser ()
addVarName _ name = do
    gets (lookup name . testVars) >>= \case
        Just _ -> fail $ "variable '" ++ unpackVarName name ++ "' already exists"
        Nothing -> return ()
    modify $ \s -> s { testVars = (name, SomeExprType @a Proxy) : testVars s }

someExpansion :: TestParser SomeExpr
someExpansion = do
    void $ char '$'
    choice
        [do name <- VarName . (:[]) . TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')
            SomeVarValue (_ :: a) <- lookupVar name
            return $ SomeExpr $ Variable @a name
        , between (char '{') (char '}') someExpr
        ]

stringExpansion :: Text -> TestParser (Expr Text)
stringExpansion tname = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpansion
    let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
            [ tname, T.pack " expansion not defined for '", textExprType e, T.pack "'" ]

    maybe err return $ listToMaybe $ catMaybes
        [ cast e
        , UnOp (T.pack . show @Integer) <$> cast e
        ]

integerLiteral :: TestParser (Expr Integer)
integerLiteral = Literal . read . TL.unpack <$> takeWhile1P (Just "integer") isDigit

quotedString :: TestParser (Expr Text)
quotedString = label "string" $ lexeme $ do
    void $ char '"'
    let inner = choice
            [ char '"' >> return []
            , takeWhile1P Nothing (`notElem` "\"\\$") >>= \s -> (Literal (TL.toStrict s):) <$> inner
            ,do void $ char '\\'
                c <- choice
                    [ char '\\' >> return '\\'
                    , char '"' >> return '"'
                    , char '$' >> return '$'
                    , char 'n' >> return '\n'
                    , char 'r' >> return '\r'
                    , char 't' >> return '\t'
                    ]
                (Literal (T.singleton c) :) <$> inner
            ,do e <- stringExpansion (T.pack "string")
                (e:) <$> inner
            ]
    Concat <$> inner

regex :: TestParser (Expr Regex)
regex = label "regular expression" $ lexeme $ do
    void $ char '/'
    let inner = choice
            [ char '/' >> return []
            , takeWhile1P Nothing (`notElem` "/\\$") >>= \s -> (Literal (TL.toStrict s) :) <$> inner
            ,do void $ char '\\'
                s <- choice
                    [ char '/' >> return (Literal $ T.singleton '/')
                    , anySingle >>= \c -> return (Literal $ T.pack ['\\', c])
                    ]
                (s:) <$> inner
            ,do e <- stringExpansion (T.pack "regex")
                (e:) <$> inner
            ]
    expr <- Regex <$> inner
    _ <- eval expr -- test regex parsing with empty variables
    return expr

data SomeExpr = forall a. ExprType a => SomeExpr (Expr a)

data SomeUnOp = forall a b. (ExprType a, ExprType b) => SomeUnOp (a -> b)

applyUnOp :: forall a b sa.
    (ExprType a, ExprType b, ExprType sa) =>
    (a -> b) -> Expr sa -> Maybe (Expr b)
applyUnOp op x = do
    Refl :: a :~: sa <- eqT
    return $ UnOp op x

data SomeBinOp = forall a b c. (ExprType a, ExprType b, ExprType c) => SomeBinOp (a -> b -> c)

applyBinOp :: forall a b c sa sb.
    (ExprType a, ExprType b, ExprType c, ExprType sa, ExprType sb) =>
    (a -> b -> c) -> Expr sa -> Expr sb -> Maybe (Expr c)
applyBinOp op x y = do
    Refl :: a :~: sa <- eqT
    Refl :: b :~: sb <- eqT
    return $ BinOp op x y

someExpr :: TestParser SomeExpr
someExpr = join inner <?> "expression"
  where
    inner = makeExprParser term table

    parens = between (symbol "(") (symbol ")")

    term = parens inner <|> literal <|> variable <?> "term"

    table = [ [ prefix "-" $ [ SomeUnOp (negate @Integer) ]
              ]
            , [ binary "*" $ [ SomeBinOp ((*) @Integer) ]
              , binary "/" $ [ SomeBinOp (div @Integer) ]
              ]
            , [ binary "+" $ [ SomeBinOp ((+) @Integer) ]
              , binary "-" $ [ SomeBinOp ((-) @Integer) ]
              ]
            , [ binary "==" $ [ SomeBinOp ((==) @Integer)
                              , SomeBinOp ((==) @Text)
                              ]
              , binary "/=" $ [ SomeBinOp ((/=) @Integer)
                              , SomeBinOp ((/=) @Text)
                              ]
              ]
            ]

    prefix :: String -> [SomeUnOp] -> Operator TestParser (TestParser SomeExpr)
    prefix name ops = Prefix $ do
        off <- stateOffset <$> getParserState
        void $ symbol name
        return $ \p -> do
            SomeExpr e <- p
            let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "'"]
            maybe err return $ listToMaybe $ catMaybes $ map (\(SomeUnOp op) -> SomeExpr <$> applyUnOp op e) ops

    binary :: String -> [SomeBinOp] -> Operator TestParser (TestParser SomeExpr)
    binary name ops = InfixL $ do
        off <- stateOffset <$> getParserState
        void $ symbol name
        return $ \p q -> do
            SomeExpr e <- p
            SomeExpr f <- q
            let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "' and '", textExprType f, T.pack "'"]
            maybe err return $ listToMaybe $ catMaybes $ map (\(SomeBinOp op) -> SomeExpr <$> applyBinOp op e f) ops

    literal = label "literal" $ choice
        [ return . SomeExpr <$> integerLiteral
        , return . SomeExpr <$> quotedString
        , return . SomeExpr <$> regex
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
    wsymbol "let"
    name <- VarName . (:[]) <$> identifier
    symbol "="
    SomeExpr (e :: Expr a) <- someExpr
    void $ eol

    addVarName @a Proxy name
    return [Let line name e]


class Typeable a => ParamType a where
    parseParam :: TestParser a
    showParamType :: proxy a -> String

    paramDefault :: TestParser a
    paramDefault = mzero

instance ParamType SourceLine where
    parseParam = mzero
    showParamType _ = "<source line>"

instance ParamType NodeName where
    parseParam = nodeName
    showParamType _ = "<node>"

instance ParamType ProcName where
    parseParam = procName
    showParamType _ = "<proc>"

instance ParamType VarName where
    parseParam = newVarName @Text Proxy
    showParamType _ = "<variable>"

instance ExprType a => ParamType (Expr a) where
    parseParam = typedExpr
    showParamType _ = "<" ++ T.unpack (textExprType @a Proxy) ++ ">"

instance ParamType a => ParamType [a] where
    parseParam = listOf parseParam
    showParamType _ = showParamType @a Proxy ++ " [, " ++ showParamType @a Proxy ++ " ...]"
    paramDefault = return []

data SomeParam f = forall a. ParamType a => SomeParam (f a)

data CommandDef a = CommandDef [(String, SomeParam Proxy)] ([SomeParam Identity] -> a)

instance Functor CommandDef where
  fmap f (CommandDef types ctor) = CommandDef types (f . ctor)

instance Applicative CommandDef where
  pure x = CommandDef [] (\[] -> x)
  CommandDef types1 ctor1 <*> CommandDef types2 ctor2 =
      CommandDef (types1 ++ types2) $ \params ->
          let (params1, params2) = splitAt (length types1) params
           in ctor1 params1 $ ctor2 params2

param :: forall a. ParamType a => String -> CommandDef a
param name = CommandDef [(name, SomeParam (Proxy @a))] (\[SomeParam (Identity x)] -> fromJust $ cast x)

cmdLine :: CommandDef SourceLine
cmdLine = param ""

command :: String -> CommandDef a -> TestParser [a]
command name (CommandDef types ctor) = do
    line <- getSourceLine
    L.indentBlock scn $ do
        wsymbol name
        helper line $ map (fmap $ \(SomeParam (_ :: Proxy p)) -> SomeParam $ Nothing @p) types
  where
    helper line params = choice
        [do void $ lookAhead eol
            iparams <- forM params $ \case
                (_, SomeParam (Nothing :: Maybe p))
                    | Just (Refl :: p :~: SourceLine) <- eqT -> return $ SomeParam $ Identity line
                (sym, SomeParam (Nothing :: Maybe p)) -> choice
                    [ SomeParam . Identity <$> paramDefault @p
                    , fail $ "missing " ++ (if null sym then "" else "'" ++ sym ++ "' ") ++ showParamType @p Proxy
                    ]
                (_, SomeParam (Just x)) -> return $ SomeParam $ Identity x
            return $ L.IndentNone [ctor iparams]

        ,do symbol ":"
            return $ L.IndentSome Nothing (return . concat) $ do
                line' <- getSourceLine
                L.indentBlock scn $ helper line' params

        ,do tryParams line [] params
        ]

    tryParam sym (SomeParam (cur :: Maybe p)) = do
        when (not $ null sym) $ wsymbol sym
        when (isJust cur) $ do
            fail $ "multiple " ++ (if null sym then "unnamed" else "'" ++ sym ++ "'") ++ " parameters"
        SomeParam . Just <$> parseParam @p

    tryParams line prev ((sym, p) : ps) = choice $
        (if null sym then reverse else id) {- try unnamed parameter as last option -} $
        [do p' <- tryParam sym p
            helper line $ concat [reverse prev, [(sym, p')], ps]
        ,do tryParams line ((sym, p) : prev) ps
        ]
    tryParams _ _ [] = mzero

testSpawn :: TestParser [TestStep]
testSpawn = command "spawn" $ Spawn
    <$> param "as"
    <*> param "on"

testSend :: TestParser [TestStep]
testSend = command "send" $ Send
    <$> param "to"
    <*> param ""

testExpect :: TestParser [TestStep]
testExpect = command "expect" $ Expect
    <$> cmdLine
    <*> param "from"
    <*> param ""
    <*> param "capture"

testGuard :: TestParser [TestStep]
testGuard = command "guard" $ Guard
    <$> cmdLine
    <*> param ""


testWait :: TestParser [TestStep]
testWait = do
    wsymbol "wait"
    return [Wait]

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header $ choice
        [ letStatement
        , testSpawn
        , testSend
        , testExpect
        , testGuard
        , testWait
        ]
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
            { testProcs = S.empty
            , testVars = []
            }
    case evalState (runParserT parseTestDefinitions path content) initState of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right tests -> return tests
