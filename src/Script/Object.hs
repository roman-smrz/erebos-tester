module Script.Object (
    ObjectId(..),
    ObjectType(..),
    Object(..), SomeObject(..),
    toSomeObject, fromSomeObject,
    destroySomeObject,
) where

import Data.Kind
import Data.Typeable


newtype ObjectId = ObjectId Int

class Typeable a => ObjectType m a where
    type ConstructorArgs a :: Type
    type ConstructorArgs a = ()

    createObject :: ObjectId -> ConstructorArgs a -> m (Object m a)
    destroyObject :: Object m a -> m ()

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
