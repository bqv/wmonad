{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Foreign.Callable (
  module Foreign.Callable,
  module Control.Monad.Trans.Class,
  module Control.Monad.Trans.Writer
) where

import           Prelude
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Foreign.Ptr
import           TextShow
import           TextShow.Data.Char

data ShowableArg = forall a. Show a => Arg a | ArgStr String
data ShowableRes = forall a. Show a => Res a | ResStr String

class Arg a where
  arg :: a -> ShowableArg

instance Show a => Arg a where
  arg = Arg

instance Show ShowableArg where
  showsPrec p (Arg x) = showsPrec p x
  showsPrec _ (ArgStr s) = showString s

instance TextShow ShowableArg where
  showbPrec p (Arg x) = showbPrec p $ FromStringShow x
  showbPrec _ (ArgStr s) = showbLitString s

class Res a where
  res :: a -> ShowableRes

instance {-# OVERLAPS #-} Res (Maybe String) where
  res (Just s) = res s
  res Nothing = ResStr "NULL"

instance {-# OVERLAPS #-} Show (Ptr a) => Res (Maybe (Ptr a)) where
  res (Just x) = res x
  res Nothing = ResStr "0x0"

instance {-# OVERLAPS #-} Res (Maybe ()) where
  res (Just ()) = res True
  res Nothing = res False

instance Show a => Res a where
  res = Res

instance Show ShowableRes where
  showsPrec p (Res x) = showsPrec p x
  showsPrec _ (ResStr s) = showString s

instance TextShow ShowableRes where
  showbPrec p (Res x) = showbPrec p $ FromStringShow x
  showbPrec _ (ResStr s) = showbLitString s

type Call = (String, [ShowableArg], ShowableRes)
type CallT m a = WriterT [Call] m a
trace a = tell [a]

traceC0 :: (Res r) => String -> (IO r) -> (CallT IO r)
traceC0 n f = lift (f) >>= \r -> trace (n, [], res r) >> pure r

traceC1 :: (Arg a, Res r) => String -> (a -> IO r) -> (a -> CallT IO r)
traceC1 n f a = lift (f a) >>= \r -> trace (n, [arg a], res r) >> pure r

traceC2 :: (Arg a, Arg b, Res r) => String -> (a -> b -> IO r) -> (a -> b -> CallT IO r)
traceC2 n f a b = lift (f a b) >>= \r -> trace (n, [arg a, arg b], res r) >> pure r

traceC3 :: (Arg a, Arg b, Arg c, Res r) => String -> (a -> b -> c -> IO r) -> (a -> b -> c -> CallT IO r)
traceC3 n f a b c = lift (f a b c) >>= \r -> trace (n, [arg a, arg b, arg c], res r) >> pure r

traceC4 :: (Arg a, Arg b, Arg c, Arg d, Res r) => String -> (a -> b -> c -> d -> IO r) -> (a -> b -> c -> d -> CallT IO r)
traceC4 n f a b c d = lift (f a b c d) >>= \r -> trace (n, [arg a, arg b, arg c, arg d], res r) >> pure r

traceC5 :: (Arg a, Arg b, Arg c, Arg d, Arg e, Res r) => String -> (a -> b -> c -> d -> e -> IO r) -> (a -> b -> c -> d -> e -> CallT IO r)
traceC5 n f a b c d e = lift (f a b c d e) >>= \r -> trace (n, [arg a, arg b, arg c, arg d, arg e], res r) >> pure r
