module Script.Var (
    VarName(..), textVarName, unpackVarName,
    FqVarName(..), textFqVarName, unpackFqVarName, unqualifyName,
    TypedVarName(..),
    ModuleName(..), textModuleName,
    SourceLine(..), textSourceLine,
) where

import Data.Text (Text)
import Data.Text qualified as T


newtype VarName = VarName Text
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name) = name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName


data FqVarName
    = GlobalVarName ModuleName VarName
    | LocalVarName VarName
    deriving (Eq, Ord)

textFqVarName :: FqVarName -> Text
textFqVarName (GlobalVarName mname vname) = textModuleName mname <> "." <> textVarName vname
textFqVarName (LocalVarName vname) = textVarName vname

unpackFqVarName :: FqVarName -> String
unpackFqVarName = T.unpack . textFqVarName

unqualifyName :: FqVarName -> VarName
unqualifyName (GlobalVarName _ name) = name
unqualifyName (LocalVarName name) = name


newtype TypedVarName a = TypedVarName { fromTypedVarName :: VarName }
    deriving (Eq, Ord)


newtype ModuleName = ModuleName [ Text ]
    deriving (Eq, Ord, Show)

textModuleName :: ModuleName -> Text
textModuleName (ModuleName parts) = T.intercalate "." parts

data SourceLine
    = SourceLine Text
    | SourceLineBuiltin

textSourceLine :: SourceLine -> Text
textSourceLine (SourceLine text) = text
textSourceLine SourceLineBuiltin = "<builtin>"
