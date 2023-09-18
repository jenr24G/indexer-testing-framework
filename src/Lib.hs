{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Lib
  ( someFunc,
  )
where
import Data.Kind (Type)
import qualified Control.Concurrent.Async as Control.Concurrent

someFunc :: IO ()
someFunc = putStrLn "someFunc"