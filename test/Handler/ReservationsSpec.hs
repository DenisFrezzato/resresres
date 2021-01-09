{-# LANGUAGE FlexibleContexts #-}

module Handler.ReservationsSpec (spec) where

import Data.Aeson
import Data.Function
import qualified Data.Text as T
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

mkSimplePaginatedResult :: [a] -> PaginatedResult a
mkSimplePaginatedResult results =
  PaginatedResult defaultPaginationPage defaultPaginationSize (length results) results

type ReservationPaginatedResult = PaginatedResult (Entity Reservation)

spec :: Spec
spec = do
  withApp $ do
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
        let expected = mkSimplePaginatedResult [Entity rKey reservationDto]
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

      it "should filter by reservation date" $ do
        let (Just someDay) = fromGregorianValid 2000 1 1
            someNextDay = addGregorianDurationRollOver calendarDay someDay
            someNextNextDay = addGregorianDurationRollOver calendarDay someNextDay
            someTime = UTCTime someDay 0
            someNextTime = UTCTime someNextDay 0
            someNextNextTime = UTCTime someNextNextDay 0
            mkSimpleReservation date createdAt =
              Reservation (T.pack $ "reference " <> show date) 2 date Nothing createdAt createdAt
            reservationDto1 = mkSimpleReservation someTime someTime
            reservationDto2 = mkSimpleReservation someNextTime someTime
            reservationDto3 = mkSimpleReservation someNextNextTime someTime

        rKeys <- runDB $ insertMany [reservationDto1, reservationDto2, reservationDto3]
        let key2 = rKeys !! 1
            expected = mkSimplePaginatedResult [Entity key2 reservationDto2]

        request $ do
          setMethod "GET"
          setUrl ReservationsR
          addGetParam "after" "2000-01-02"
          addGetParam "before" "2000-01-03"
        (res :: ReservationPaginatedResult) <- requireJSONResponse
        assertEq "BodyResponse" res expected

      describe "should return 400 if data query parameters are invalid" $ do
        it "should fail when `after` is not a valid date" $ do
          request $ do
            setMethod "GET"
            setUrl ReservationsR
            addGetParam "after" "rubbish"
          statusIs 400

        it "should fail when `before` is not a valid date" $ do
          request $ do
            setMethod "GET"
            setUrl ReservationsR
            addGetParam "before" "2020-66-66"
          statusIs 400

        it "should fail when `after` and `before` don't represent a valid time frame" $ do
          request $ do
            setMethod "GET"
            setUrl ReservationsR
            addGetParam "after" "2000-01-02"
            addGetParam "before" "2000-01-01"
          statusIs 400

  withAppWithCapacity 2 $ do
    describe "POST /reservations" $ do
      it "should accept a reservation" $ do
        now <- liftIO getCurrentTime <&> roundTimeToMillis
        let reservationDto = Reservation "someOtherReference" 1 now (Just "vegan") now now

        mkPostRequest reservationDto
        statusIs 200

      describe "should take into account the capacity of the dining room" $ do
        it "should accept a reservation if there is room" $ do
          now <- liftIO getCurrentTime <&> roundTimeToMillis
          let reservationDto1 = Reservation "someReference" 1 now Nothing now now
              reservationDto2 = Reservation "someOtherReference" 1 now (Just "vegan") now now

          _ <- runDB $ insert reservationDto1

          mkPostRequest reservationDto2
          statusIs 200

        it "should not accept a reservation if there is not room" $ do
          now <- liftIO getCurrentTime <&> roundTimeToMillis
          let nowPlusABit = now & \(UTCTime d t) -> UTCTime d $ t + secondsToDiffTime (40 * 60)
              reservationDto1 = Reservation "someReference" 2 now Nothing now now
              reservationDto2 = Reservation "someOtherReference" 1 nowPlusABit (Just "vegan") now now

          _ <- runDB $ insert reservationDto1

          mkPostRequest reservationDto2
          statusIs 400
  where
    mkPostRequest body = request $ do
      setMethod "POST"
      setUrl ReservationsR
      addRequestHeader ("Content-Type", "application/json")
      setRequestBody $ encode body
