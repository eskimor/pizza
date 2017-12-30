{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Switchable where


import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe      (readMay)
import Data.Text (pack, unpack, Text)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T
import Control.Lens hiding (view)
import Control.Monad.Fix
import Control.Monad ((<=<), void)


class Reflex t => Switchable t a where
  doSwitch :: (Reflex t, MonadHold t m) => Event t (a t) -> m (a t)


gDyn :: (MonadHold t m, DomBuilder t m, PostBuild t m, Switchable t a) => Dynamic t (m (a t)) -> m (a t)
gDyn = doSwitch <=< dyn
