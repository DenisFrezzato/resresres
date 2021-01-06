{-# LANGUAGE FlexibleContexts #-}

module Repository.Reservations
  ( findCountByDate,
  )
where

import ClassyPrelude
import Database.Esqueleto hiding ((<&>))
import Flow
import Model

findCountByDate :: MonadIO m => (Maybe UTCTime, Maybe UTCTime) -> SqlReadT m (Maybe Int)
findCountByDate (mAfter, mBefore) =
  ( select $
      from $ \r -> do
        case mAfter of
          Just after -> where_ $ r ^. ReservationDate >=. val after
          Nothing -> return ()
        case mBefore of
          Just before -> where_ $ r ^. ReservationDate >=. val before
          Nothing -> return ()
        return countRows
  )
    <&> fmap unValue .> headMay
