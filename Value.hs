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
  show (FunVal _ _) = ""

instance Eq Value where
  (Int a) == (Int b) = a == b
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ " " ++ (showListContents as)
