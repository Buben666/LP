data Singleton a = Singleton a

instance Functor Singleton where
      fmap f (Singleton a) = Singleton (f a)
instance Applicative Singleton  where
      pure a = Singleton a
      (Singleton f) <*> (Singleton a) = Singleton (f a)
instance Monad Singleton where
      (Singleton a) >>= f
       = f a
instance Foldable Singleton where
      foldMap f (Singleton a) = f a
instance Traversable Singleton where
      traverse f (Singleton a) = fmap Singleton (f a)


data Productish a b = Productish a b
instance Functor (Productish x) where
      fmap f (Productish a b) = Productish a (f b)

instance (Monoid a) => Applicative (Productish a) where
      pure = Productish mempty
      Productish c f <*> Productish d x = Productish (c <> d) (f x)

instance (Monoid a) => Monad (Productish a) where
      (Productish a x) >>= f = let Productish b y = f x in Productish (a <> b) y

instance Foldable (Productish a) where
      foldMap f (Productish a b) = f b

instance Traversable (Productish a) where
      traverse f (Productish a b) = fmap (Productish a) (f b)



data Summish a b = First a | Second b

instance Functor (Summish a) where
      fmap f (First x) = First x
      fmap f (Second b) = Second (f b)

instance Applicative (Summish a) where
      pure = Second
      Second f <*> a = fmap f a
      First e <*> _ = First e

instance Monad (Summish a) where
      (Second a) >>= f = f a
      (First a) >>= _ = First a

instance Foldable (Summish a) where
      foldMap f (Second x) = f x
      foldMap _ (First x) = mempty

instance Traversable (Summish a) where
      traverse _ (First a) = pure (First a)
      traverse f (Second a) = fmap Second (f a)



data Optional a = NoValue | HasValue a

instance Functor Optional where
      fmap _ NoValue      = NoValue
      fmap f (HasValue a) = HasValue (f a)
instance Applicative Optional where
      pure = HasValue
      HasValue f <*> x = fmap f x
      NoValue <*> _x = NoValue
instance Monad Optional where
      (HasValue x) >>= k = k x
      NoValue      >>= _ = NoValue
instance Foldable Optional where
      foldMap f (HasValue a) = f a
      foldMap f (NoValue) = mempty
instance Traversable Optional where
      traverse f NoValue = pure NoValue
      traverse f (HasValue x) = HasValue <$> f x



data NotQuiteList a = Value a | Layer (NotQuiteList a)

instance Functor NotQuiteList where
      fmap f (Value x) = Value (f x)
      fmap f (Layer a) = Layer (fmap f a)
instance Applicative NotQuiteList where
      pure x = Value x
      (Layer f1) <*> x = Layer (f1 <*> x)
      (Value f) <*> (Layer x ) = Layer (f <$> x)
      (Value f) <*> (Value x) = Value (f x)
instance Monad NotQuiteList where
      (Value x) >>= f
        = f x
      (Layer l) >>= f
        = Layer (l >>= f)
instance Foldable NotQuiteList where
      foldMap f (Value x) = f x
      foldMap f (Layer l) = foldMap f l
      foldr binop acc (Value x) = binop x acc
      foldr binop acc (Layer l)
        = foldr binop (foldr binop acc l) l
instance Traversable NotQuiteList where
      traverse f (Value a) = fmap Value (f a)
      traverse f (Layer a) = fmap Layer (traverse f a)



data NotEmpty a = LastValue a | MidValue a (NotEmpty a)

instance Functor NotEmpty where
      fmap f (LastValue a) = LastValue (f a)
      fmap f (MidValue a b) = MidValue (f a) (fmap f b)

instance Applicative NotEmpty where
      pure = LastValue
      (LastValue f) <*> LastValue a = LastValue (f a)
      (LastValue f) <*> MidValue a b = pure (f a)
      (MidValue f x) <*> MidValue a b =  MidValue (f a) (x <*> b)
      (MidValue f x) <*> LastValue a =  LastValue (f a)

instance Monad NotEmpty where
      (LastValue a) >>= f = f a
      (MidValue a b) >>= f = f a

instance Foldable NotEmpty where
      foldMap f (LastValue a) = f a
      foldMap f (MidValue a b) = (f a) <> (foldMap f b)

instance Traversable NotEmpty where
      traverse f (LastValue a) = fmap LastValue (f a)
      traverse f (MidValue a b) = MidValue <$> f a <*> traverse f b
