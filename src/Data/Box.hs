{-# LANGUAGE StaticPointers, GADTs #-}
module Data.Box ( Box
                , intoBox
                , closeBox
                , outBox ) where
import GHC.StaticPtr
import Data.ByteString
import Data.Binary

data Box a where
  Pure :: StaticPtr a -> Box a
  Close :: Box (a -> b) -> Box a -> Box b

intoBox :: StaticPtr a -> Box a
intoBox = Pure

closeBox :: Box (a -> b) -> Box a -> Box b
closeBox = Close

outBox :: Box a -> a
outBox (Pure a) = deRefStaticPtr a
outBox (Close f a) = outBox f (outBox a)
