module Handler.Reservations where

import Control.Lens hiding ((.>), (<.))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Validation (Validation)
import qualified Data.Validation as V
import Flow hiding ((<.))
import Handler.Pagination
import Import
import qualified Repository.Reservations as R

intToText :: Int -> Text
intToText = show .> T.pack

dayToUTCTime :: Day -> UTCTime
dayToUTCTime d = UTCTime d 0

parseDateParams :: (Maybe Text, Maybe Text) -> Validation (NonEmpty Text) (Maybe Day, Maybe Day)
parseDateParams (mAfter, mBefore) =
  let parsedDays =
        (("after", mAfter), ("before", mBefore))
          & each %~ (\(label, mDay) -> mDay <&> validateDay label)
          & sequenceAOf (each . traversed)
   in case parsedDays of
        V.Success ds@(Just after, Just before) ->
          if after > before
            then V.Failure (pure "after")
            else V.Success ds
        x -> x
  where
    validateDay :: Text -> Text -> Validation (NonEmpty Text) Day
    validateDay name s = case readMay s of
      Just day -> V.Success day
      Nothing -> V.Failure (pure name)

getDates :: MonadHandler m => m (Maybe Day, Maybe Day)
getDates = do
  mAfter <- lookupGetParam "after"
  mBefore <- lookupGetParam "before"
  let parsedDates = parseDateParams (mAfter, mBefore)
  case parsedDates of
    V.Success ds -> return ds
    V.Failure args -> invalidArgs $ NE.toList args

getReservationsR :: Handler Value
getReservationsR = do
  (Pagination page size) <- getPagination
  ds <- getDates
  let (mAfterDay, mBeforeDay) = ds & each %~ fmap dayToUTCTime
      offset = (page - 1) * size
      afterOpt = mAfterDay <&> (ReservationDate >=.)
      beforeOpt = mBeforeDay <&> (ReservationDate <.)
      findReservations =
        selectList
          (catMaybes [afterOpt, beforeOpt])
          [ Asc ReservationCreatedAt,
            LimitTo size,
            OffsetBy offset
          ]
      findCount = R.findCountByDate (mAfterDay, mBeforeDay)
  (reservations, mTotal) <- runDB $ concurrently findReservations findCount
  case mTotal of
    Just total -> returnPaginatedResult page size total reservations
    Nothing -> sendResponseStatus status500 ("This was not supposed to happen" :: Text)

data ReservationPostBody = ReservationPostBody
  { reference :: Text,
    guests :: Int,
    date :: UTCTime,
    notes :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

postReservationsR :: Handler Value
postReservationsR = do
  now <- liftIO getCurrentTime
  body <- (parseCheckJsonBody :: Handler (Result ReservationPostBody))
  case body of
    Error s -> invalidArgs [T.pack s]
    Success ReservationPostBody {..} -> do
      let reservationDto =
            Reservation reference guests date notes now now
      runDB $ insertEntity reservationDto >>= returnJson
