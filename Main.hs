import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Data.Int as Int
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env (-1) id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env (-1) var
    case v of
        -- Variable not defined :(
        (Error _) -> do
            e <- evalExpr env expr
            setGlobalVar var env e
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var env e
evalExpr env (AssignExpr assignWithOp (LVar var) expr) = do
    v <- stateLookup env (-1) var
    case v of
        -- Variable not defined :(
        (Error _) -> do
            e <- evalExpr env expr
            setGlobalVar var env e
        -- Variable defined, let's set its value
        val -> do
            expv <- evalExpr env expr
            e <- evalAssignWithOp env assignWithOp val expv
            setVar var env e
evalExpr env (UnaryAssignExpr op (LVar var))= do
    v <- stateLookup env (-1) var
    case v of
        (Error _) -> return Nil
        _ -> do
            e <- postfixOp env op v
            setVar var env e
-- EXPRESSION CALLING --
evalExpr env (CallExpr nome env1) = ST $ \senv ->
    let 
        callSize = length env1
        (ST findFName) = evalExpr env nome
        (VarRef (Id fName)) = nome
        (storedFunc, ignEnv) = findFName senv
        --(ST findFunction) = stateLookup env (-1) fName
        --(storedFunc, ignoreEnv) = findFunction env
        funSize = case storedFunc of
                    (FunVal a1 b1) -> length a1
                    _ -> -1
        (ST retorno) = if (funSize == callSize) then 
                ST $ \s -> 
                let
                    (ST a2) = newScope env
                    (ignoreme, newEnv) = a2 s
                    (ST storedFunc2) = stateLookup newEnv (-1) fName
                    (FunVal env2 blockStmt, ignoreEnv2) = storedFunc2 newEnv
                    (ST funST) = addParam newEnv env2 env1
                    (resp, funEnv) = funST newEnv
                    (ST final) = ST $ \o -> 
                        let
                            (ST execution) = evalStmt funEnv (BlockStmt blockStmt)
                        in execution o
                    (ignoreResp, finEnv) = final funEnv
                    (ST afterFunc) = killScope env
                    (useless, returnedEnv) = afterFunc finEnv
                in (ignoreResp, returnedEnv)
            else if (funSize == -1) then return $ Error $ (show fName) ++ " function has not been declared"
                else return $ Error $ (show nome) ++ " incorrect number of parameters (Expected: " ++ (show funSize) ++ " - Received: " ++ (show callSize) ++ ")"
    in retorno senv

--AssignWithOperation
evalAssignWithOp :: StateT -> AssignOp -> Value -> Value -> StateTransformer Value  
evalAssignWithOp env OpAssignAdd (Int val) (Int expr) = return $ Int  $ val + expr
evalAssignWithOp env OpAssignSub (Int val) (Int expr) = return $ Int  $ val - expr
evalAssignWithOp env OpAssignMul (Int val) (Int expr) = return $ Int  $ val * expr
evalAssignWithOp env OpAssignDiv (Int val) (Int expr) = return $ Int  $ div val expr

--EvalStmt
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
        ReturnStmt a -> do
            case a of
                (Just expr) -> do
                    v <- (evalExpr env expr)
                    return v
                (Nothing) -> return Nil
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
                    if tf then do
                        r1 <- evalStmt env action
                        case r1 of 
                            Break -> return Nil
                            _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
                    else return Nil
                Nothing -> do 
                    r1 <- evalStmt env action
                    case r1 of
                        Break -> return Nil
                        _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
        (resp,ign) = g newS
    in (resp,ign)
--IFELSE--
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    b <- evalExpr env expr
    case b of 
        (Bool a) -> ST $ \s ->
                let (ST f) = evalExpr env expr
                    (Bool b, newS) = f s
                    (ST resF) = evalStmt env (if b then ifBlock else elseBlock)
                    (resp,ign) = resF newS
                in (resp,ign)
        (Error _) -> return Nil
--IF-- 
evalStmt env (IfSingleStmt expr block) = do
    b <- evalExpr env expr
    case b of
        (Bool a)-> ST $ \s ->
                let (ST f) = evalExpr env expr
                    (Bool b, newS) = f s
                    (ST resF) = evalStmt env (block)
                    (resp, fSt) = resF newS
                in (resp, fSt)
        (Error _)-> return Nil
--BREAK--
evalStmt env (BreakStmt _) = return Break
--FUNCTION--
evalStmt env (FunctionStmt (Id var) param block) = do
    v <- stateLookup env (-1) var
    case v of
        (Error _) -> setVar var env (FunVal param block)
        _ -> return $ Error $ (show var) ++ " has already been declared before"
--evalStmt env (ReturnStmt a) = do
    

addParam :: StateT -> [Id] -> [Expression] -> StateTransformer Value
addParam env [] [] = return Nil
addParam env ((Id a):as) (b:bs) = ST $ \s -> 
    let
        (ST expb) = evalExpr env b
        (bVal, ignoreme) = expb s
        (ST p) = setVar a env bVal
        (resp, newEnv) = p s 
        (ST w) = addParam newEnv as bs
    in w newEnv

evalFor :: StateT -> ForInit -> StateTransformer Value
evalFor env (VarInit a) = do
    evalStmt env (VarDeclStmt a)
evalFor env NoInit = return Nil
evalFor env (ExprInit b) = evalExpr env b 

getId :: Id -> String
getId (Id a) = a

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
    var <- stateLookup env (Map.size env) x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env (Map.size env) x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val

--
-- Environment and auxiliary functions
--

environment :: Map Int (Map String Value)
environment = empty

stateLookup :: StateT -> Int -> String -> StateTransformer Value
stateLookup env (-1) var = ST $ \s -> let 
        (ST a) = stateLookup (union s env) (Map.size (union s env)) var
        in a s
stateLookup env 0 var = ST $ \s -> (Error $ "Variable " ++ show var ++ " not defined",s)
stateLookup env stck var = ST $ \s -> let
    (Just currStack) = (Map.lookup stck (union s env))
    (ST retorno) = case (Map.lookup var currStack) of
        Nothing -> stateLookup env (stck-1) var
        (Just a) -> ST $ \f -> let
                (Just frst) = (Map.lookup var currStack)
            in (frst, f)
    in retorno s

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id env Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id env val
--MAGIC DO NOT TOUCH
setVar :: String -> StateT -> Value -> StateTransformer Value
setVar var env val = ST $ \s -> let
        currStack = Map.size (union s env)
        stackPos = Int (currStack)
        newScp = Map.lookup currStack (union s env)
        (ST retorno) = case newScp of
                (Just currScope) -> ST $ \s1 -> let
                        (ST ign) = evalStmt env EmptyStmt 
                        (ret, ignore) = ign s1
                        modificat = insert var val currScope
                        newEnv = insert currStack modificat (union s1 env)
                    in (ret, newEnv)
                (Nothing) -> ST $ \s2 -> let 
                        (ST a) = newScope s2
                        (ignore, newEnv) = a s2
                        (Just newStack) = Map.lookup 1 (union s2 newEnv)
                        modificat = insert var val newStack                        
                        finEnv = insert 1 modificat newEnv
                    in (ignore, finEnv)
    in retorno (union s env)  

setGlobalVar :: String -> StateT -> Value -> StateTransformer Value
setGlobalVar var env val = ST $ \s -> let
        currStack = 1
        stackPos = Int (currStack)
        newScp = Map.lookup currStack (union s env)
        (ST retorno) = case newScp of
                (Just currScope) -> ST $ \s1 -> 
                    let
                        (ST ign) = evalStmt env EmptyStmt 
                        (ret, ignore) = ign s1
                        modificat = insert var val currScope
                        newEnv = insert 1 modificat (union s env)
                    in (ret, newEnv)
                (Nothing) -> ST $ \s2 -> let 
                        (ST a) = newScope s2
                        (ignore, newEnv) = a s2
                        (Just newStack) = Map.lookup 1 (union s newEnv)
                        modificat = insert var val newStack                        
                        finEnv = insert 1 modificat newEnv
                    in (ignore, finEnv)
    in retorno (union s env)     

newScope :: StateT -> StateTransformer Value
newScope env = ST $ \s -> 
    let
        (ST ign) = evalStmt env EmptyStmt 
        (ret, ignore) = ign s
        newScope = insert ((Map.size (union s env)) + 1) empty (union s env)
    in (ret, newScope)

killScope :: StateT -> StateTransformer Value
killScope env = ST $ \s -> 
    let 
        (ST ign) = evalStmt env EmptyStmt 
        (ret, ignore) = ign s
        newScope = delete (Map.size (union s env)) (union s env)
    in (ret, newScope)


--
-- Types and boilerplate
--

type StateT = Map Int (Map String Value)
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
