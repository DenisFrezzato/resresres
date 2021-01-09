{-# LANGUAGE FlexibleContexts #-}

module Repository.Reservations
  ( findCountByDate,
    findGuestsByDate,
  )
where

import ClassyPrelude
import Database.Esqueleto hiding ((<&>))
import Flow hiding ((<.))
import Model

findCountByDate :: MonadIO m => (Maybe UTCTime, Maybe UTCTime) -> SqlReadT m Int
findCountByDate (mAfter, mBefore) =
  ( select $
      from $ \r -> do
        case mAfter of
          Just after -> where_ $ r ^. ReservationDate >=. val after
          Nothing -> return ()
        case mBefore of
          Just before -> where_ $ r ^. ReservationDate <. val before
          Nothing -> return ()
        return countRows
  )
    <&> unsafeHead .> unValue

findGuestsByDate :: MonadIO m => (UTCTime, UTCTime) -> SqlReadT m Int
findGuestsByDate (start, end) =
  ( select $
      from $ \r -> do
        where_ $ r ^. ReservationDate >=. val start
        where_ $ r ^. ReservationDate <=. val end
        return $ sum_ @_ @Rational (r ^. ReservationGuests)
  )
    <&> headMay .> (>>= unValue) .> fmap floor .> fromMaybe 0
