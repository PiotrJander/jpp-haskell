module MaybeTrans(
    MaybeT(..),
    mapMaybeT,
  ) where
import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM, ap)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap
 
instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
