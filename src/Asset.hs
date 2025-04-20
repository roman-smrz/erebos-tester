module Asset (
    Asset(..),
    AssetPath(..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable

import Script.Expr.Class

data Asset = Asset
    { assetPath :: AssetPath
    }

newtype AssetPath = AssetPath FilePath

textAssetPath :: AssetPath -> Text
textAssetPath (AssetPath path) = T.pack path

instance ExprType Asset where
    textExprType _ = "asset"
    textExprValue asset = "asset:" <> textAssetPath (assetPath asset)

    recordMembers =
        [ ( "path", RecordSelector $ assetPath )
        ]

instance ExprType AssetPath where
    textExprType _ = "filepath"
    textExprValue = ("filepath:" <>) . textAssetPath

    exprExpansionConvTo = cast textAssetPath
