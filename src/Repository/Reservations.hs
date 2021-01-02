{-# LANGUAGE FlexibleContexts #-}

module Repository.Reservations
  ( findCount,
  )
where

import ClassyPrelude
import Database.Esqueleto hiding ((<&>))
import Flow
import Model

findCount :: MonadIO m => SqlReadT m (Maybe Int)
findCount =
  (select $ from @(SqlExpr (Entity Reservation)) $ const $ return countRows)
    <&> fmap unValue .> headMay
