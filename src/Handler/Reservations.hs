module Handler.Reservations where

import Control.Lens hiding ((.>), (<.))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
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
  (reservations, total) <- runDB $ concurrently findReservations findCount
  returnPaginatedResult page size total reservations

data ReservationPostBody = ReservationPostBody
  { reference :: Text,
    guests :: Int,
    date :: UTCTime,
    notes :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

postReservationsR :: Handler Value
postReservationsR = do
  ReservationPostBody {..} <- parseBody
  app <- getYesod
  let diningRoomCapacity = app & appSettings & appDiningRoomCapacity
  now <- liftIO getCurrentTime
  let dateEnd = dateDiff (+) date
      dateStart = dateDiff (-) date
  takenSeats <- runDB $ R.findGuestsByDate (dateStart, dateEnd)
  let isThereEnoughRoom = guests <= diningRoomCapacity - takenSeats
  unless isThereEnoughRoom $ invalidArgs []
  let reservation =
        Reservation reference guests date notes now now
  runDB $ insertEntity reservation >>= returnJson
  where
    parseBody :: HandlerFor App ReservationPostBody
    parseBody =
      parseCheckJsonBody >>= \case
        Error s -> invalidArgs [T.pack s]
        Success x -> return x

    dateDiff :: (DiffTime -> DiffTime -> DiffTime) -> UTCTime -> UTCTime
    dateDiff f date =
      let oneHour = secondsToDiffTime (60 * 60)
       in date
            & \(UTCTime d t) -> UTCTime d (f t oneHour)
