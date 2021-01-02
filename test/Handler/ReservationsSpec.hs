module Handler.ReservationsSpec (spec) where

import Data.Time
import Flow
import Handler.Pagination
import TestImport
import Prelude ((!!))

roundTimeToMillis :: UTCTime -> UTCTime
roundTimeToMillis (UTCTime day time) =
  time
    |> diffTimeToPicoseconds
    |> (`div` 1000000)
    |> (* 1000000)
    |> picosecondsToDiffTime
    |> UTCTime day

mkSimplePaginatedResult :: Int -> [a] -> PaginatedResult a
mkSimplePaginatedResult = PaginatedResult defaultPaginationPage defaultPaginationSize

type ReservationPaginatedResult = PaginatedResult (Entity Reservation)

spec :: Spec
spec = withApp $ do
  describe "GET /reservations" $ do
    it "should give a 200" $ do
      get ReservationsR
      statusIs 200

    it "should return a list of reservation" $ do
      now <- liftIO getCurrentTime <&> roundTimeToMillis
      let reservationDto = Reservation "someReference" 3 now Nothing now now

      rKey <- runDB $ insert reservationDto
      get ReservationsR
      (res :: ReservationPaginatedResult) <- requireJSONResponse
      let expected = mkSimplePaginatedResult 1 [Entity rKey reservationDto]
      assertEq "Body response" res expected

    it "should return a paginated result set" $ do
      now <- liftIO getCurrentTime <&> roundTimeToMillis
      let reservationDto1 = Reservation "someReference" 3 now Nothing now now
          reservationDto2 = Reservation "someOtherReference" 2 now (Just "vegan") now now

      rKeys <- runDB $ insertMany [reservationDto1, reservationDto2]
      let key1 = rKeys !! 0
          key2 = rKeys !! 1
          expected1 = PaginatedResult 1 1 2 [Entity key1 reservationDto1]
          expected2 = PaginatedResult 2 1 2 [Entity key2 reservationDto2]

      request $ do
        setMethod "GET"
        setUrl ReservationsR
        addGetParam "page" "1"
        addGetParam "size" "1"
      (res1 :: ReservationPaginatedResult) <- requireJSONResponse
      assertEq "BodyResponse" res1 expected1

      request $ do
        setMethod "GET"
        setUrl ReservationsR
        addGetParam "page" "2"
        addGetParam "size" "1"
      (res2 :: ReservationPaginatedResult) <- requireJSONResponse
      assertEq "BodyResponse" res2 expected2

    describe "should return 400 with invalid pagination parameters" $ do
      it "with page lower than 1" $ do
        request $ do
          setMethod "GET"
          setUrl ReservationsR
          addGetParam "page" "0"
        statusIs 400

      it "with size lower than 1" $ do
        request $ do
          setMethod "GET"
          setUrl ReservationsR
          addGetParam "size" "0"
        statusIs 400

      it "with higher than 1000" $ do
        request $ do
          setMethod "GET"
          setUrl ReservationsR
          addGetParam "size" "1001"
        statusIs 400
