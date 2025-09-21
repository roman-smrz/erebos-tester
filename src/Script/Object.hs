module Script.Object (
    ObjectId(..),
    ObjectType(..),
    Object(..), SomeObject(..),
    toSomeObject, fromSomeObject,
    destroySomeObject,
) where

import Data.Kind
import Data.Text (Text)
import Data.Typeable

import Script.Expr.Class


newtype ObjectId = ObjectId Int

class Typeable a => ObjectType m a where
    type ConstructorArgs a :: Type
    type ConstructorArgs a = ()

    textObjectType :: proxy (m a) -> proxy a -> Text
    textObjectValue :: proxy (m a) -> a -> Text

    createObject :: ObjectId -> ConstructorArgs a -> m (Object m a)
    destroyObject :: Object m a -> m ()

instance (Typeable m, ObjectType m a) => ExprType (Object m a) where
    textExprType _ = textObjectType (Proxy @(m a)) (Proxy @a)
    textExprValue = textObjectValue (Proxy @(m a)) . objImpl


data Object m a = ObjectType m a => Object
    { objId :: ObjectId
    , objImpl :: a
    }

data SomeObject m = forall a. ObjectType m a => SomeObject
    { sobjId :: ObjectId
    , sobjImpl :: a
    }

toSomeObject :: Object m a -> SomeObject m
toSomeObject Object {..} = SomeObject { sobjId = objId, sobjImpl = objImpl }

fromSomeObject :: ObjectType m a => SomeObject m -> Maybe (Object m a)
fromSomeObject SomeObject {..} = do
    let objId = sobjId
    objImpl <- cast sobjImpl
    return Object {..}

destroySomeObject :: SomeObject m -> m ()
destroySomeObject (SomeObject oid impl) = destroyObject (Object oid impl)
