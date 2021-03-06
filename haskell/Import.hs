module Import
    ( module Import
    ) where

import           Prelude                as Import hiding (head, init, last,
                                                   readFile, tail, writeFile)
import           Yesod                  as Import hiding (Route (..))

import           Control.Applicative    as Import (pure, (<$>), (<*>))
import           Data.Text              as Import (Text)

import           Foundation             as Import
import           Settings               as Import
import           Settings.Development   as Import
import           Settings.StaticFiles   as Import
import           ChatData               as Import
import           Control.Concurrent.STM as Import hiding (check)

import qualified Data.Text             as T
import qualified Data.Map              as M

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- utility functions

atm :: MonadIO m => STM a -> m a
atm = liftIO . atomically

roomsToText rs = foldr (\(n, s) t -> T.intercalate "\n" [n, T.pack $ show s, t]) "" info
    where info = M.toAscList (channelInformation rs)
