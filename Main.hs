import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e
evalExpr env (UnaryAssignExpr op (LVar var))= do
    v <- stateLookup env var
    case v of
        (Error _) -> return Nil
        _ -> do
            e <- postfixOp env op v
            setVar var e

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt1:stmt2)) = do
    case stmt1 of
        BreakStmt _ -> return Break
        _ -> do 
            evalStmt env stmt1
            evalStmt env (BlockStmt stmt2)
--FOR--
evalStmt env (ForStmt initi condi itera action) = ST $ \s ->
     let
        (ST a) = evalStmt env EmptyStmt
        (ignore, newS) = a s
        (ST g) = do
            evalFor env initi        
            case condi of
                (Just (a)) -> do 
                    Bool tf <- evalExpr env a
                    if tf then (do
                        (r1,nenv) <- evalStmt env action
                        if (r1/=Nil) then (
                            case itera of 
                                (Just (b)) -> evalExpr env b
                                Nothing -> return Nil
                            evalStmt env (ForStmt NoInit condi itera action)
                            )
                        else return Nil)
                    else return Nil
                Nothing -> do 
                    (r1,nenv) <- evalStmt env action
                    if (r1/=Nil) then                    
                        case itera of 
                                (Just (b)) -> evalExpr env b
                                Nothing -> return Nil
                        evalStmt env (ForStmt NoInit condi itera action)
                    else return Nil
        (resp,ign) = g newS
        fEnv = intersection ign s
        in (resp,fEnv)
--IFELSE--
evalStmt env (IfStmt expr ifBlock elseBlock) = ST $ \s ->
    let (ST f) = evalExpr env expr
        (Bool b, newS) = f s
        (ST resF) = evalStmt env (if b then ifBlock else elseBlock)
        (resp,ign) = resF newS
        fEnv = intersection ign s
    in (resp,fEnv)
--IF--
evalStmt env (IfSingleStmt expr block) = do
    b <- evalExpr env expr
    case b of
        (Bool a)-> ST $ \s ->
                let (ST f) = evalExpr env expr
                    (Bool b, newS) = f s
                    (ST resF) = evalStmt env (block)
                    (resp, fSt) = resF newS
                    fEnv = intersection fSt s
                in (resp, fEnv)
        (Error _)-> return Nil
--BREAK--
evalStmt env (BreakStmt _) = return Break

evalFor :: StateT -> ForInit -> StateTransformer Value
evalFor env (VarInit a) = do
    evalStmt env (VarDeclStmt a)
evalFor env NoInit = return Nil
evalFor env (ExprInit b) = evalExpr env b 

monadInter :: StateT -> StateT -> StateT
monadInter env1 env2 = Map.intersection env1 env2

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

postfixOp :: StateT -> UnaryAssignOp -> Value -> StateTransformer Value
postfixOp env PostfixInc (Int v1) = return $ Int $ v1 + 1

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe
        (Error $ "Variable " ++ show var ++ " not defined")
        id
        (Map.lookup var (union s env)),
    s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
