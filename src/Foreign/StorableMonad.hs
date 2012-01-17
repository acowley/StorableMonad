-- |Provides applicative and monadic interface for working with
-- Storable values. The idea is that the underlying pointer is
-- threaded through the computation to make reading and writing
-- consecutive values easier.
-- 
-- This is an example module using `StorableMonad` to define a
-- `Storable` instance:
-- 
-- > import Control.Applicative
-- > import Foreign.Ptr
-- > import Foreign.StorableMonad
-- > 
-- > data Foo = Foo Int Char
-- > 
-- > instance Storable Foo where
-- >   alignment _ = alignment (undefined::Ptr ())
-- >   sizeOf _ = sizeOf (undefined::Int) + sizeOf (undefined::Char)
-- >   peek = runStorable (Foo <$> peekS <*> peekS)
-- >   poke ptr (Foo x y) = runStorable (pokeS x >> pokeS y) ptr
module Foreign.StorableMonad (peekS, pokeS, runStorable, StorableM, 
                              module Foreign.Storable) where
import Control.Monad.State.Strict
import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.Storable as S

-- |A state monad that threads a pointer through a computation.
type StorableM a = StateT (Ptr ()) IO a

-- |Action that pokes a value into the current pointer location, then
-- moves the pointer to just after the poked value.
pokeS :: Storable a => a -> StorableM ()
pokeS x = do ptr <- get
             liftIO $ S.poke (castPtr ptr) x
             put (plusPtr ptr (sizeOf x))
{-# INLINE pokeS #-}

-- |Action that peeks a value from the current pointer location, then
-- moves the pointer to just after the peeked value.
peekS :: Storable a => StorableM a
peekS = do ptr <- get
           x <- liftIO $ S.peek (castPtr ptr)
           put (plusPtr ptr (sizeOf x))
           return x
{-# INLINE peekS #-}

-- |Run a StorableM action with the supplied initial pointer location.
runStorable :: StorableM a -> Ptr b -> IO a
runStorable s p = evalStateT s (castPtr p)
{-# INLINE runStorable #-}
