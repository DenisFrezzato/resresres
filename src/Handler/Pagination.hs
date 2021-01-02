module Handler.Pagination
  ( Pagination (..),
    PaginatedResult (..),
    getPagination,
    parsePaginationParams,
    returnPaginatedResult,
    defaultPaginationSize,
    defaultPaginationPage,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Data.Validation
import Flow
import Import hiding (singleton)

type Page = Int

type Size = Int

data Pagination = Pagination Page Size deriving (Show, Eq)

defaultPaginationPage :: Page
defaultPaginationPage = 1

defaultPaginationSize :: Size
defaultPaginationSize = 10

integerFromText :: Text -> Maybe Int
integerFromText = T.unpack .> readMay

getPaginationParams :: MonadHandler m => m (Maybe Text, Maybe Text)
getPaginationParams = (,) <$> lookupGetParam "page" <*> lookupGetParam "size"

parsePaginationParams :: (Maybe Text, Maybe Text) -> Validation (NonEmpty Text) Pagination
parsePaginationParams (mPage, mSize) =
  let page = mPage >>= integerFromText |> fromMaybe defaultPaginationPage
      size = mSize >>= integerFromText |> fromMaybe defaultPaginationSize
   in validatePagination page size
  where
    validatePagination :: Page -> Size -> Validation (NonEmpty Text) Pagination
    validatePagination page size = Pagination <$> validatePage page <*> validateSize size

    validatePage page
      | page > 0 = Success page
      | otherwise = Failure $ pure "page"

    validateSize size
      | 0 < size && size <= 1000 = Success size
      | otherwise = Failure $ pure "size"

getPagination :: MonadHandler m => m Pagination
getPagination =
  getPaginationParams
    <&> parsePaginationParams
    >>= \case
      Success p -> return p
      Failure args -> invalidArgs $ toList args

data PaginatedResult a = PaginatedResult
  { page :: Int,
    size :: Int,
    total :: Int,
    results :: [a]
  }
  deriving (Show, Eq, Generic, FromJSON)

returnPaginatedResult :: (Monad m, ToJSON a) => Page -> Size -> Int -> a -> m Value
returnPaginatedResult page size total results =
  returnJson $
    object
      [ "page" .= page,
        "size" .= size,
        "total" .= total,
        "results" .= results
      ]
