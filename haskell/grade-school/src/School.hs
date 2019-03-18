module School (School, add, empty, grade, sorted) where
import Data.List (sortOn, sort)

type GradeNum = Int
type Name = String
data Student = Student GradeNum Name
type School = [Student]

add :: GradeNum -> Name -> School -> School
add gradeNum name = (:) (Student gradeNum name)

empty :: School
empty = []

grade :: GradeNum -> School -> [String]
grade gradeNum = sort . map getName . filter ((== gradeNum)  . getGradeNum)

sorted :: School -> [(GradeNum, [Name])]
sorted = gradeSort . (foldl insertStudent [])

-- | Sorted Helpers

gradeSort :: [(GradeNum, [Name])] -> [(GradeNum, [Name])]
gradeSort = sortOn fst . map (\(x,y) -> (x, sort y))

insertStudent :: [(GradeNum, [Name])] -> Student -> [(GradeNum, [Name])]
insertStudent acc student
    | any ((isInGrade student) . fst) acc = map (insert student) acc
    | otherwise = (getGradeNum student, [getName student]):acc

insert :: Student -> (GradeNum, [Name]) -> (GradeNum, [Name])
insert (Student gradeNum name) (number, names)
    | gradeNum == number = (number, name:names)
    | otherwise = (number, names)

-- | Helpers

getName :: Student -> Name
getName (Student _ name) = name

getGradeNum :: Student -> GradeNum
getGradeNum (Student gradeNum _) = gradeNum

isInGrade :: Student -> GradeNum -> Bool
isInGrade (Student gradeNum _) compNum = gradeNum == compNum