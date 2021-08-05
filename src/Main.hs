module Main where

import Control.Applicative
import Control.Monad.Logic
import Data.List
import Data.Time.Calendar

type Exam = String

data Student = Student
  { name :: String,
    exams :: [Exam]
  }

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
      fromGregorian 2021 08 09
    ]
  )

main :: IO ()
main = do
  print . observe $ magic testData