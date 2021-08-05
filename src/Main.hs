{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.Logic
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

type Exam = String

data RequestData = RequestData
  { students :: [Student],
    days :: [Day],
    limitRecords :: Int
  }
  deriving (Generic, Show)

instance FromJSON RequestData

instance ToJSON RequestData where
  toEncoding = genericToEncoding defaultOptions

data Student = Student
  { name :: String,
    exams :: [Exam]
  }
  deriving (Generic, Show)

instance FromJSON Student

instance ToJSON Student where
  toEncoding = genericToEncoding defaultOptions

type ExamSchedule = [(Exam, Day)]

choose :: (Foldable f, Alternative m) => f a -> m a
choose = foldr ((<|>) . pure) empty

collectExams :: [Student] -> [Exam]
collectExams students = nub $ students >>= exams

magic :: ([Student], [Day]) -> Logic ExamSchedule
magic (students, days) =
  let allExams = collectExams students
      examDays = [[(exam, day) | day <- days] | exam <- allExams]
      examSchedules :: [ExamSchedule]
      examSchedules = functionSet examDays
   in do
        examSchedule <- choose examSchedules
        guard (testSchedule students examSchedule)
        return examSchedule

testSchedule :: [Student] -> ExamSchedule -> Bool
testSchedule students schedule = and $ do
  student <- students
  let studentsExams = exams student
  let studentDays = map snd . filter (\(exam, day) -> exam `elem` exams student) $ schedule
  return $ nub studentDays == studentDays

functionSet :: [[a]] -> [[a]]
functionSet [] = [[]]
functionSet (xs : xss) = do
  x <- xs
  (x :) <$> functionSet xss

testData :: ([Student], [Day])
testData =
  ( [ Student "StudentA" ["ExamA", "ExamB", "ExamC", "ExamD"],
      Student "StudentB" ["ExamB", "ExamC", "ExamD", "ExamE"],
      Student "StudentC" ["ExamA", "ExamB", "ExamD", "ExamE"]
    ],
    [ fromGregorian 2021 08 05,
      fromGregorian 2021 08 06,
      fromGregorian 2021 08 07,
      fromGregorian 2021 08 08,
      fromGregorian 2021 08 09,
      fromGregorian 2021 08 10,
      fromGregorian 2021 08 11,
      fromGregorian 2021 08 12,
      fromGregorian 2021 08 13
    ]
  )

app :: Application
app request respond = do
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      (handleRequest . queryString $ request)

handleRequest :: Query -> BSL.ByteString
handleRequest qs =
  let decoded :: Either String RequestData
      decoded =
        eitherHead qs
          >>= eitherDecode . BSL.fromStrict . fst
      processed :: Either String [ExamSchedule]
      processed = do
        requestData <- decoded
        return $ (observeMany . limitRecords $ requestData) . magic $ (students requestData, days requestData)
   in encode (if null qs then Left "Query this thing with json in the query string" else processed)

eitherHead :: [a] -> Either String a
eitherHead [] = Left "Empty list."
eitherHead (x : xs) = Right x

{--(BSL.pack . show . observeAll . magic $ testData)--}

main :: IO ()
main = do
  putStrLn "http://localhost:9995/"
  run 9995 app
