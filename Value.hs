module Value (Value (..)) where
import Language.ECMAScript3.Syntax
import Data.Int as Int
data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Error String
    | Nil
    | Break
    | FunVal [Id] [Statement]
    | Return Value
    | List [Value]
    | Pointer String
--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show (Error str) = "Error: " ++ str
  show Nil = ""
  show Break = ""
  show (FunVal b c) = "<<FUNCAO>>"
  show (Return a) = "f r " ++ show a
  show (List a) = show a

instance Eq Value where
  (Int a) == (Int b) = a == b
  (List []) == (List []) = True
  (List a) == (List b) = compareLists a b


exists a [] = False
exists a (b:c) = if a == b then True else exists a c

extrct a [] = []
extrct a (b:c) = if a == b then extrct a c else b:(extrct a c)

compareLists [] [] = True
compareLists (a:b) c = if (exists a c) then 
    let 
      newC = (extrct a c) 
    in compareLists b newC
  else False

-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ " " ++ (showListContents as)
