module Handler.Reservations where

import Data.Aeson
import qualified Data.Text as T
import Flow
import Handler.Pagination
import Import
import qualified Repository.Reservations as R

intToText :: Int -> Text
intToText = show .> T.pack

getReservationsR :: Handler Value
getReservationsR = do
  (Pagination page size) <- getPagination
  let offset = (page - 1) * size
      findReservationsQuery =
        selectList
          []
          [ Asc ReservationCreatedAt,
            LimitTo size,
            OffsetBy offset
          ]
  (reservations, mTotal) <- runDB $ concurrently findReservationsQuery R.findCount
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
