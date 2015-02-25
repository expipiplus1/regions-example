{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit

data Handle (r :: * -> *) = Handle String

unsafeCreateHandle :: String -> IO (Handle r)
unsafeCreateHandle s = putStrLn ("Creating a handle: " ++ s) >> pure (Handle s)

unsafeDestroyHandle :: Handle r -> IO ()
unsafeDestroyHandle (Handle s) = putStrLn ("Destroying a handle: " ++ s)

safeCreateHandle :: (MonadIO parent, Applicative parent) => String -> RegionT s parent (Handle r)
safeCreateHandle s = do
  h <- liftIO (unsafeCreateHandle s)
  _ <- onExit (unsafeDestroyHandle h)
  pure h

main :: IO ()
main = runRegionT $ do
  _ <- safeCreateHandle "Hello"
  runRegionT (safeCreateHandle "World" >> pure ())
  pure ()

