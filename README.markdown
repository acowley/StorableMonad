Small Haskell utility module that simplifies definining `Storable` instances.

Example:

    import Control.Applicative
    import Foreign.Ptr
    import Foreign.StorableMonad
    
    data Foo = Foo Int Char
    
    instance Storable Foo where
      alignment _ = alignment (undefined::Ptr ())
      sizeOf _ = sizeOf (undefined::Int) + sizeOf (undefined::Char)
      peek = runStorable (Foo <$> peekS <*> peekS)
      poke ptr (Foo x y) = runStorable (pokeS x >> pokeS y) ptr

