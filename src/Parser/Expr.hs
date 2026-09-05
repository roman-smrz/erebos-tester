module Parser.Expr (
    identifier,
    parseModuleName,

    varName,
    newVarName,
    addVarName, addVarNameType,
    constrName,

    TermComplexity(..),
    someExpr,
    typedExpr,
    literal,
    variable,
    constructor,

    someExpansion, expansionTypeCheck,
    expressionExpansion,
    stringExpansion,

    functionArguments,
    applyFunctionArguments,

    typeExpr,
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
addVarName off tname = addVarNameType off tname (ExprTypePrim @a Proxy)

addVarNameType :: forall a. ExprType a => Int -> TypedVarName a -> SomeExprType -> TestParser ()
addVarNameType off (TypedVarName name) stype = do
    gets (lookup name . testVars) >>= \case
        Just _ -> registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            T.pack "variable '" <> textVarName name <> T.pack "' already exists"
        Nothing -> return ()
    modify $ \s -> s { testVars = ( name, ( LocalVarName name, stype )) : testVars s }

constrName :: TestParser VarName
constrName = label "contructor name" $ do
    lexeme $ try $ do
        lead <- upperChar
        rest <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_')
        return $ VarName $ TL.toStrict $ TL.fromChunks $ T.singleton lead : TL.toChunks rest

someExpansion :: TestParser SomeExpr
someExpansion = do
    void $ char '$'
    choice
        [do off <- stateOffset <$> getParserState
            sline <- getSourceLine
            name <- VarName . TL.toStrict <$> takeWhile1P Nothing (\x -> isAlphaNum x || x == '_')
            lookupScalarVarExpr off sline name
        , between (char '{') (char '}') (someExpr FunctionTerm)
        ]

expansionTypeCheck :: forall a. ExprType a => Int -> Text -> SomeExpr -> TestParser (Expr a)
expansionTypeCheck off tname (SomeExpr e) = do
    let err = do
            registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ T.concat
                [ tname, T.pack " expansion not defined for '", textExprType e, T.pack "'" ]
            return $ Undefined "expansion not defined for type"

    maybe err (return . (<$> e)) $ listToMaybe $ catMaybes [ cast (id :: a -> a), exprExpansionConvTo, exprExpansionConvFrom ]

expressionExpansion :: forall a. ExprType a => Text -> TestParser (Expr a)
expressionExpansion tname = do
    off <- stateOffset <$> getParserState
    expansionTypeCheck off tname =<< someExpansion

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
    void $ try $ char '/' <* notFollowedBy (char '=') -- TODO: better parsing rules for regexes
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

    choice
        [do symbol "]"
            tvar <- newTypeVar
            return $ SomeExpr $
                TypeLambda tvar (ExprTypeApp (ExprTypeConstr1 (Proxy :: Proxy [])) [ ExprTypeVar tvar ]) $
                    \case
                        (ExprTypePrim (Proxy :: Proxy a)) -> HidePrimType $ Pure ([] :: [ a ])
                        _ -> Undefined "incomplete type"

        ,do SomeExpr x <- someExpr FunctionTerm
            let enumErr off = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
                    "list range enumeration not defined for ‘" <> textExprType x <> "’"
            let exprList = foldr (liftA2 (:)) (Pure [])

            SomeExpr <$> choice
                [do symbol "]"
                    return $ exprList [ x ]

                ,do off <- stateOffset <$> getParserState
                    osymbol ".."
                    ExprEnumerator fromTo _ <- maybe (enumErr off) return $ exprEnumerator x
                    y <- typedExpr FunctionTerm
                    symbol "]"
                    return $ fromTo <$> x <*> y

                ,do symbol ","
                    y <- typedExpr FunctionTerm

                    choice
                        [do symbol "]"
                            return $ exprList [ x, y ]

                        ,do off <- stateOffset <$> getParserState
                            osymbol ".."
                            ExprEnumerator _ fromThenTo <- maybe (enumErr off) return $ exprEnumerator x
                            z <- typedExpr FunctionTerm
                            symbol "]"
                            return $ fromThenTo <$> x <*> y <*> z

                        ,do symbol ","
                            xs <- listOf (typedExpr FunctionTerm)
                            symbol "]"
                            return $ exprList (x : y : xs)
                        ]
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

data TermComplexity
    = SimpleTerm -- variable name, literal or more complex term in parentheses
    | FunctionTerm -- simple term or function call

someExpr :: TermComplexity -> TestParser SomeExpr
someExpr complexity = label "expression" $ do
    case complexity of
        SimpleTerm -> join termSimple
        FunctionTerm -> join inner
  where
    inner = typeAnnotated $ makeExprParser termFunction table

    parens = between (symbol "(") (symbol ")")

    termSimple = label "term" $ choice
        [ parens inner
        , return <$> literal
        , return <$> variable
        , return <$> constructor
        ]

    termFunction = label "term" $ choice
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
            , [ let tvar = TypeVar "a"
                    targs = FunctionArguments $ M.fromList
                        [ ( Just "$l", ( VarName "$l", SomeArgumentType RequiredArgument $ ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeVar tvar ]) )
                        , ( Just "$r", ( VarName "$r", SomeArgumentType RequiredArgument $ ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeVar tvar ]) )
                        ]
                 in infixrExpr "++" $ SomeExpr $ TypeLambda tvar (ExprTypeFunction (ExprTypeArguments $ fmap snd targs) (ExprTypeApp (ExprTypeConstr1 (Proxy @[])) [ ExprTypeVar tvar ])) $ \case
                        ExprTypePrim (Proxy :: Proxy a) ->
                            HideFunType (fmap snd targs) $ ArgsReq targs $
                                FunctionAbstraction $ ((++) @a)
                                    <$> (Variable SourceLineBuiltin $ LocalVarName $ VarName "$l")
                                    <*> (Variable SourceLineBuiltin $ LocalVarName $ VarName "$r")
                        t -> Undefined ("ambiguous type ‘" <> T.unpack (textSomeExprType t) <> "’ for operator ‘++’") :: Expr DynamicType
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


    infixrExpr :: String -> SomeExpr -> Operator TestParser (TestParser SomeExpr)
    infixrExpr name fun = InfixR $ do
        void $ osymbol name
        return $ \p q -> do
            loff <- stateOffset <$> getParserState
            l <- p
            roff <- stateOffset <$> getParserState
            r <- q
            applyFunctionArguments (FunctionArguments $ M.fromList [ ( Just "$l", ( loff, l ) ), ( Just "$r", ( roff, r ) ) ]) fun


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

    typeAnnotated :: TestParser (TestParser SomeExpr) -> TestParser (TestParser SomeExpr)
    typeAnnotated p = do
        off <- stateOffset <$> getParserState
        p' <- p
        choice
            [ do
                -- colon starts a type annotation, except when at the end of line
                void $ try $ (string ":" <* notFollowedBy operatorChar <* sc <* notFollowedBy eol)
                stype <- typeExpr
                return $ do
                    se <- p'
                    unifySomeExpr off stype se

            , do
                return p'
            ]


typedExpr :: forall a. ExprType a => TermComplexity -> TestParser (Expr a)
typedExpr complexity = do
    off <- stateOffset <$> getParserState
    SomeExpr e <- someExpr complexity
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

constructor :: TestParser SomeExpr
constructor = label "constructor" $ do
    off <- stateOffset <$> getParserState
    sline <- getSourceLine
    name <- constrName
    lookupVarExpr off sline name

functionCall :: TestParser SomeExpr
functionCall = do
    sline <- getSourceLine
    fun <- variable <|> constructor
    args <- functionArguments (\poff _ e -> return ( poff, e )) (someExpr FunctionTerm) literal (\poff -> lookupVarExpr poff sline . VarName)
    applyFunctionArguments args fun

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


applyFunctionArguments :: FunctionArguments ( Int, SomeExpr ) -> SomeExpr -> TestParser SomeExpr
applyFunctionArguments (FunctionArguments margs) sexpr
    | M.null margs = return sexpr
applyFunctionArguments args sexpr@(SomeExpr (expr :: Expr a))
    | Just (Refl :: a :~: DynamicType) <- eqT
    , ExprTypeForall qvar itype <- someExprType sexpr
    = do
        tvar <- newTypeVar
        case renameVarInType qvar tvar itype of
            ExprTypeFunction (ExprTypeArguments args') res' -> do
                ( used, ( _, unexpectedArgs ) ) <- unifyArguments args' args
                unexpectedArguments unexpectedArgs
                t <- fromMaybe (ExprTypeVar tvar) . M.lookup tvar <$> gets testTypeUnif
                resolveKnownTypeVars res' >>= \case
                    ( res''@(ExprTypePrim (Proxy :: Proxy r)), _ ) ->
                        return $ SomeExpr (ArgsApp used (ExposeFunType args' (TypeApp res'' t expr) :: Expr (FunctionType r)))
                    ( r, _ ) ->
                        return $ SomeExpr (ArgsApp used (ExposeFunType args' (TypeApp r t expr) :: Expr (FunctionType DynamicType)))
            _ -> do
                unexpectedArguments args
                return sexpr

    | otherwise
    = case someExprType sexpr of
        ExprTypeFunction (ExprTypeArguments args') res' -> do
            ( used, ( _, unexpectedArgs ) ) <- unifyArguments args' args
            unexpectedArguments unexpectedArgs
            resolveKnownTypeVars res' >>= \case
                ( ExprTypePrim (Proxy :: Proxy r), _ )
                    | Just (Refl :: a :~: FunctionType r) <- eqT
                    -> return $ SomeExpr (ArgsApp used expr)
                _
                    | Just (Refl :: a :~: FunctionType DynamicType) <- eqT
                    -> return $ SomeExpr (ArgsApp used expr)
                _ ->
                    error $ "expecting function type, got: " <> show (typeRep expr)
        _ -> do
            unexpectedArguments args
            return sexpr
  where
    unexpectedArguments (FunctionArguments amap) = do
        forM_ (M.toAscList amap) $ \( kw, ( poff, _ ) ) ->
            registerParseError $ FancyError poff $ S.singleton $ ErrorFail $ T.unpack $
                case kw of
                    Just (ArgumentKeyword tkw) -> "unexpected parameter with keyword ‘" <> tkw <> "’"
                    Nothing                    -> "unexpected parameter"


typeExpr :: TestParser SomeExprType
typeExpr = do
    off <- stateOffset <$> getParserState
    choice
        [ do
            name <- constrName <?> "type constructor name"
            lookupType off name
        , do
            between (symbol "[") (symbol "]") $ do
                inner <- typeExpr
                return $ ExprTypeApp (ExprTypeConstr1 (Proxy :: Proxy [])) [ inner ]
        ]
