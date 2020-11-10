{-# LANGUAGE FlexibleInstances #-}
module Foo where
class Foo a where
  doFoo :: a -> IO ()

instance Foo (Maybe Int) where
  doFoo _ = print "mint"

instance Foo (Maybe Bool) where
  doFoo _ = print "mbool"

instance Foo (Maybe Float) where
  doFoo _ = print "mfloat". -- forall a. a ~ blah

main :: IO ()
main = do
  doFoo (undefined :: Maybe Int)
  doFoo (undefined :: Maybe Bool)
  doFoo (undefined :: Maybe Float)
