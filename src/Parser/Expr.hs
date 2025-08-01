module Parser.Expr (
    identifier,
    parseModuleName,

    varName,
    newVarName,
    addVarName,

    someExpr,
    typedExpr,
    literal,
    variable,

    stringExpansion,

    checkFunctionArguments,
    functionArguments,
) where

import Control.Applicative (liftA2)
import Control.Monad.Combinators.Expr
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Map qualified as M
import Data.Maybe
import Data.Scientific
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Typeable
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder qualified as Err

import Parser.Core
import Script.Expr
import Script.Expr.Class

reservedWords :: [ Text ]
reservedWords =
    [ "test", "def", "let"
    , "module", "export", "import"
    ]

identifier :: TestParser Text
identifier = label "identifier" $ do
    lexeme $ try $ do
        off <- stateOffset <$> getParserState
        lead <- lowerChar
        rest <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_')
        let ident = TL.toStrict $ TL.fromChunks $ (T.singleton lead :) $ TL.toChunks rest
        when (ident `elem` reservedWords) $ parseError $ Err.err off $ mconcat
            [ Err.utoks $ TL.fromStrict ident
            ]
        return ident

parseModuleName :: TestParser ModuleName
parseModuleName = do
    x <- identifier
    ModuleName . (x :) <$> many (symbol "." >> identifier)

varName :: TestParser VarName
varName = label "variable name" $ VarName <$> identifier

newVarName :: forall a. ExprType a => TestParser (TypedVarName a)
newVarName = do
    off <- stateOffset <$> getParserState
    name <- TypedVarName <$> varName
    addVarName off name
    return name

addVarName :: forall a. ExprType a => Int -> TypedVarName a -> TestParser ()
addVarName off (TypedVarName name) = do
    gets (lookup name . testVars) >>= \case
        Just _ -> registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            T.pack "variable '" <> textVarName name <> T.pack "' already exists"
        Nothing -> return ()
    modify $ \s -> s { testVars = ( name, ( LocalVarName name, ExprTypePrim @a Proxy )) : testVars s }

someExpansion :: TestParser SomeExpr
someExpansion = do
    void $ char '$'
    choice
        [do off <- stateOffset <$> getParserState
            sline <- getSourceLine
            name <- VarName . TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')
            lookupScalarVarExpr off sline name
        , between (char '{') (char '}') someExpr
        ]

expressionExpansion :: forall a. ExprType a => Text -> TestParser (Expr a)
expressionExpansion tname = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpansion
    let err = do
            registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                [ tname, T.pack " expansion not defined for '", textExprType e, T.pack "'" ]
            return $ Undefined "expansion not defined for type"

    maybe err (return . (<$> e)) $ listToMaybe $ catMaybes [ cast (id :: a -> a), exprExpansionConvTo, exprExpansionConvFrom ]

stringExpansion :: TestParser (Expr Text)
stringExpansion = expressionExpansion "string"

numberLiteral :: TestParser SomeExpr
numberLiteral = label "number" $ lexeme $ do
    x <- L.scientific
    choice
        [ return (SomeExpr $ Pure (x / 100)) <* void (char ('%'))
        , if base10Exponent x == 0
             then return $ SomeExpr $ Pure (coefficient x)
             else return $ SomeExpr $ Pure x
        ]

boolLiteral :: TestParser SomeExpr
boolLiteral = label "bool" $ lexeme $ do
    SomeExpr . Pure <$> choice
        [ wsymbol "True"  *> return True
        , wsymbol "False" *> return False
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
            ,do e <- stringExpansion
                (e:) <$> inner
            ]
    Concat <$> inner

regex :: TestParser (Expr Regex)
regex = label "regular expression" $ lexeme $ do
    off <- stateOffset <$> getParserState
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
            ,do e <- expressionExpansion (T.pack "regex")
                (e:) <$> inner
            ]
    parts <- inner
    let testEval = \case
            Pure (RegexPart p) -> p
            _ -> ""
    case regexCompile $ T.concat $ map testEval parts of
        Left err -> registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
            [ "failed to parse regular expression: ", T.pack err ]
        Right _ -> return ()
    return $ Regex parts

list :: TestParser SomeExpr
list = label "list" $ do
    symbol "["
    SomeExpr x <- someExpr

    let enumErr off = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            "list range enumeration not defined for '" <> textExprType x <> "'"
    let exprList = foldr (liftA2 (:)) (Pure [])
    SomeExpr <$> choice
        [do symbol "]"
            return $ exprList [x]

        ,do off <- stateOffset <$> getParserState
            osymbol ".."
            ExprEnumerator fromTo _ <- maybe (enumErr off) return $ exprEnumerator x
            y <- typedExpr
            symbol "]"
            return $ fromTo <$> x <*> y

        ,do symbol ","
            y <- typedExpr

            choice
                [do symbol "]"
                    return $ exprList [x, y]

                ,do off <- stateOffset <$> getParserState
                    osymbol ".."
                    ExprEnumerator _ fromThenTo <- maybe (enumErr off) return $ exprEnumerator x
                    z <- typedExpr
                    symbol "]"
                    return $ fromThenTo <$> x <*> y <*> z

                ,do symbol ","
                    xs <- listOf typedExpr
                    symbol "]"
                    return $ exprList (x:y:xs)
                ]
        ]

data SomeUnOp = forall a b. (ExprType a, ExprType b) => SomeUnOp (a -> b)

applyUnOp :: forall a b sa.
    (ExprType a, ExprType b, ExprType sa) =>
    Int -> (a -> b) -> Expr sa -> TestParser (Expr b)
applyUnOp off op x = do
    x' <- unifyExpr off (Proxy @a) x
    return $ op <$> x'

data SomeBinOp = forall a b c. (ExprType a, ExprType b, ExprType c) => SomeBinOp (a -> b -> c)

applyBinOp :: forall a b c sa sb.
    (ExprType a, ExprType b, ExprType c, ExprType sa, ExprType sb) =>
    Int -> (a -> b -> c) -> Expr sa -> Expr sb -> TestParser (Expr c)
applyBinOp off op x y = do
    x' <- unifyExpr off (Proxy @a) x
    y' <- unifyExpr off (Proxy @b) y
    return $ op <$> x' <*> y'

someExpr :: TestParser SomeExpr
someExpr = join inner <?> "expression"
  where
    inner = makeExprParser term table

    parens = between (symbol "(") (symbol ")")

    term = label "term" $ choice
        [ parens inner
        , return <$> literal
        , return <$> functionCall
        ]

    table = [ [ prefix "-" $ [ SomeUnOp (negate @Integer)
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
                              , SomeBinOp ((==) @Bool)
                              ]
              , binary' "/=" (\op xs ys -> length xs /= length ys || or  (zipWith op xs ys)) $
                              [ SomeBinOp ((/=) @Integer)
                              , SomeBinOp ((/=) @Scientific)
                              , SomeBinOp ((/=) @Text)
                              , SomeBinOp ((/=) @Bool)
                              ]
              , binary ">" $
                  [ SomeBinOp ((>) @Integer)
                  , SomeBinOp ((>) @Scientific)
                  ]
              , binary ">=" $
                  [ SomeBinOp ((>=) @Integer)
                  , SomeBinOp ((>=) @Scientific)
                  ]
              , binary "<=" $
                  [ SomeBinOp ((<=) @Integer)
                  , SomeBinOp ((<=) @Scientific)
                  ]
              , binary "<" $
                  [ SomeBinOp ((<) @Integer)
                  , SomeBinOp ((<) @Scientific)
                  ]
              ]
            ]

    prefix :: String -> [SomeUnOp] -> Operator TestParser (TestParser SomeExpr)
    prefix name ops = Prefix $ do
        off <- stateOffset <$> getParserState
        void $ osymbol name
        return $ \p -> do
            SomeExpr e <- p
            let err = FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "'"]
            region (const err) $
                choice $ map (\(SomeUnOp op) -> SomeExpr <$> applyUnOp off op e) ops


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

            let err = FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                    [T.pack "operator '", T.pack name, T.pack "' not defined for '", textExprType e, T.pack "' and '", textExprType f, T.pack "'"]

            let tryop :: forall a b d sa sb.
                    (ExprType a, ExprType b, ExprType d, ExprType sa, ExprType sb) =>
                    (a -> b -> d) -> Proxy sa -> Proxy sb -> TestParser SomeExpr
                tryop op pe pf = foldl1 (<|>) $
                    [ SomeExpr <$> applyBinOp off op e f
                    , do Refl <- maybe (parseError err) return $ eqT' op
                         ExprListUnpacker _ une <- maybe (parseError err) return $ exprListUnpacker pe
                         ExprListUnpacker _ unf <- maybe (parseError err) return $ exprListUnpacker pf
                         tryop (listmap op) (une pe) (unf pf)
                    ]

            region (const err) $
                foldl1 (<|>) $ map (\(SomeBinOp op) -> tryop op (proxyOf e) (proxyOf f)) ops

typedExpr :: forall a. ExprType a => TestParser (Expr a)
typedExpr = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpr
    unifyExpr off Proxy e

literal :: TestParser SomeExpr
literal = label "literal" $ choice
    [ numberLiteral
    , boolLiteral
    , SomeExpr <$> quotedString
    , SomeExpr <$> regex
    , list
    ]

variable :: TestParser SomeExpr
variable = label "variable" $ do
    off <- stateOffset <$> getParserState
    sline <- getSourceLine
    name <- varName
    e <- lookupVarExpr off sline name
    recordSelector e <|> return e

functionCall :: TestParser SomeExpr
functionCall = do
    sline <- getSourceLine
    variable >>= \case
        SomeExpr e'@(FunVariable argTypes _ _) -> do
            let check = checkFunctionArguments argTypes
            args <- functionArguments check someExpr literal (\poff -> lookupVarExpr poff sline . VarName)
            return $ SomeExpr $ ArgsApp args e'
        e -> return e

recordSelector :: SomeExpr -> TestParser SomeExpr
recordSelector (SomeExpr expr) = do
    void $ osymbol "."
    off <- stateOffset <$> getParserState
    m <- identifier
    let err = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
            [ T.pack "value of type ", textExprType expr, T.pack " does not have member '", m, T.pack "'" ]
    e' <- maybe err return $ applyRecordSelector m expr <$> lookup m recordMembers
    recordSelector e' <|> return e'
  where
    applyRecordSelector :: ExprType a => Text -> Expr a -> RecordSelector a -> SomeExpr
    applyRecordSelector m e (RecordSelector f) = SomeExpr $ App (AnnRecord m) (pure f) e


checkFunctionArguments :: FunctionArguments SomeArgumentType
                       -> Int -> Maybe ArgumentKeyword -> SomeExpr -> TestParser SomeExpr
checkFunctionArguments (FunctionArguments argTypes) poff kw sexpr@(SomeExpr expr) = do
    case M.lookup kw argTypes of
        Just (SomeArgumentType (_ :: ArgumentType expected)) -> do
            withRecovery (\e -> registerParseError e >> return sexpr) $ do
                SomeExpr <$> unifyExpr poff (Proxy @expected) expr
        Nothing -> do
            registerParseError $ FancyError poff $ S.singleton $ ErrorFail $ T.unpack $
                case kw of
                    Just (ArgumentKeyword tkw) -> "unexpected parameter with keyword ‘" <> tkw <> "’"
                    Nothing                    -> "unexpected parameter"
            return sexpr


functionArguments :: (Int -> Maybe ArgumentKeyword -> a -> TestParser b) -> TestParser a -> TestParser a -> (Int -> Text -> TestParser a) -> TestParser (FunctionArguments b)
functionArguments check param lit promote = do
    args <- parseArgs True
    return $ FunctionArguments args
  where
    parseArgs allowUnnamed = choice
        [do off <- stateOffset <$> getParserState
            x <- pparam
            if allowUnnamed
              then do
                  checkAndInsert off Nothing x $ parseArgs False
              else do
                  registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                      [ T.pack "multiple unnamed parameters" ]
                  parseArgs False

        ,do x <- identifier
            off <- stateOffset <$> getParserState
            y <- pparam <|> (promote off =<< identifier)
            checkAndInsert off (Just (ArgumentKeyword x)) y $ parseArgs allowUnnamed

        ,do return M.empty
        ]

    pparam = between (symbol "(") (symbol ")") param <|> lit

    checkAndInsert off kw x cont = M.insert kw <$> check off kw x <*> cont
