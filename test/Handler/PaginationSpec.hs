module Handler.PaginationSpec (spec) where

import Data.List.NonEmpty
import Data.Validation
import Handler.Pagination
import TestImport

spec :: Spec
spec = do
  describe "parsePaginationParams" $ do
    it "should return the pagination parameters" $ do
      let p = parsePaginationParams (Just "7", Just "3")
          expected = Success (Pagination 7 3)
      p `shouldBe` expected

    describe "should return default pagination when parameters are not provided" $ do
      it "should return default page" $ do
        let p = parsePaginationParams (Nothing, Just "2")
            expected = Success (Pagination defaultPaginationPage 2)
        p `shouldBe` expected

      it "should return default size" $ do
        let p = parsePaginationParams (Just "3", Nothing)
            expected = Success (Pagination 3 defaultPaginationSize)
        p `shouldBe` expected

    describe "should return failure with the invalid name of the invalid parameters" $ do
      it "should return the `page`" $ do
        let p = parsePaginationParams (Just "0", Nothing)
            expected = Failure (pure "page")
        p `shouldBe` expected

      describe "should return the `size`" $ do
        it "when is lower than 1" $ do
          let p = parsePaginationParams (Nothing, Just "0")
              expected = Failure (pure "size")
          p `shouldBe` expected

        it "when is higher than 1000" $ do
          let p = parsePaginationParams (Nothing, Just "1001")
              expected = Failure (pure "size")
          p `shouldBe` expected

      it "should return a list of invalid arguments" $ do
        let p = parsePaginationParams (Just "0", Just "1001")
            expected = Failure ("page" :| ["size"])
        p `shouldBe` expected
