{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Lens (Lens', makeLenses, (^.), (.~))
import Control.Monad.Combinators.Expr
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

import Generics.Deriving.Base (Generic, Rep, U1(..), M1(..), K1(..), (:*:)(..))
import Generics.Deriving.Base qualified as G

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
sc = L.space (void $ takeWhile1P Nothing f) skipLineComment empty
    where f x = x == ' ' || x == '\t'

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


class GInit f where ginit :: f x
instance GInit U1 where ginit = U1
instance GInit (K1 i (Maybe a)) where ginit = K1 Nothing
instance GInit f => GInit (M1 i c f) where ginit = M1 ginit
instance (GInit f, GInit h) => GInit (f :*: h) where ginit = ginit :*: ginit

data Param a = forall b. Param String (Lens' a (Maybe b)) (TestParser b)

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


command :: (Generic b, GInit (Rep b)) => String -> [Param b] -> (SourceLine -> b -> TestParser a) -> TestParser [a]
command name params fin = do
    origline <- getSourceLine
    wsymbol name
    let blockHelper line prev cur = L.indentBlock scn $ helper line prev cur
        helper line prev cur = choice $ concat
            [[ do void $ eol
                  L.IndentNone . (:[]) <$> fin line cur
             ]
            ,[ do void $ lexeme (char ':')
                  return $ L.IndentSome Nothing (return . concat) $ do
                      line' <- getSourceLine
                      blockHelper line' prev cur
             ]
            , flip map params $ \(Param sym l p) -> do
                x <- if null sym
                    then do
                        x <- p
                        when (any null prev) $ do
                            fail $ "multiple unnamed parameters"
                        return x
                    else do
                        wsymbol sym
                        when (any (== sym) prev) $ do
                            fail $ "multiple '" ++ sym ++ "' parameters"
                        p
                helper line (sym:prev) (l .~ Just x $ cur)
            ]

    blockHelper origline [] (G.to ginit)


data SpawnBuilder = SpawnBuilder
    { _spawnBuilderProc :: Maybe ProcName
    , _spawnBuilderNode :: Maybe NodeName
    }
    deriving (Generic)

makeLenses ''SpawnBuilder

testSpawn :: TestParser [TestStep]
testSpawn = command "spawn"
    [ Param "on" spawnBuilderNode nodeName
    , Param "as" spawnBuilderProc procName
    ] $ \_ b -> Spawn
        <$> (maybe (fail "missing 'as' <proc>") return $ b ^. spawnBuilderProc)
        <*> (maybe (fail "missing 'on' <node>") return $ b ^. spawnBuilderNode)


data SendBuilder = SendBuilder
    { _sendBuilderProc :: Maybe ProcName
    , _sendBuilderLine :: Maybe (Expr Text)
    }
    deriving (Generic)

makeLenses ''SendBuilder

testSend :: TestParser [TestStep]
testSend = command "send"
    [ Param "to" sendBuilderProc procName
    , Param "" sendBuilderLine quotedString
    ] $ \_ b -> Send
        <$> (maybe (fail "missing 'to' <proc>") return $ b ^. sendBuilderProc)
        <*> (maybe (fail "missing line to send") return $ b ^. sendBuilderLine)


data ExpectBuilder = ExpectBuilder
    { _expectBuilderProc :: Maybe ProcName
    , _expectBuilderRegex :: Maybe (Expr Regex)
    , _expectBuilderCaptures :: Maybe [VarName]
    }
    deriving (Generic)

makeLenses ''ExpectBuilder

testExpect :: TestParser [TestStep]
testExpect = command "expect"
    [ Param "from" expectBuilderProc procName
    , Param "" expectBuilderRegex regex
    , Param "capture" expectBuilderCaptures (listOf $ newVarName @Text Proxy)
    ] $ \s b -> Expect s
        <$> (maybe (fail "missing 'from' <proc>") return $ b ^. expectBuilderProc)
        <*> (maybe (fail "missing regex to match") return $ b ^. expectBuilderRegex)
        <*> (maybe (return []) return $ b ^. expectBuilderCaptures)


data GuardBuilder = GuardBuilder
    { _guardBuilderExpr :: Maybe (Expr Bool)
    }
    deriving (Generic)

makeLenses ''GuardBuilder

testGuard :: TestParser [TestStep]
testGuard = command "guard"
    [ Param "" guardBuilderExpr typedExpr
    ] $ \s b -> Guard s
        <$> (maybe (fail "missing guard expression") return $ b ^. guardBuilderExpr)


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
