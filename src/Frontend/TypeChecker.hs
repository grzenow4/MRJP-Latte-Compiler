module Frontend.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import Latte.AbsLatte
import Utils.Common
import Utils.Frontend

assertArgs :: Arg -> Expr -> TCM ()
assertArgs (Ar _ ty _) e = checkExp e >>= assertSame (takeExprPos e) (takeType ty)

checkPrg :: [TopDef] -> TCM ()
checkPrg tds = writeFunctions tds >> checkForMain >> mapM_ checkTopDef tds

checkTopDef :: TopDef -> TCM ()
checkTopDef (FnDef pos t x args ss) = do
    tcs <- get
    mapM_ (\(Ar p ty y) -> do
        if (takeType ty == TVoid)
        then throwError $ newErr p "Function argument cannot be of type void"
        else writeType p (takeStr y) (takeType ty)) args
    modify (\st -> st { ret = Just (takeType t) })
    checkBlock ss
    res <- gets returned
    if not (res || takeType t == TVoid)
    then throwError $ newErr pos ("Function " ++ (takeStr x) ++ " does not return")
    else put tcs

checkBlock :: Block -> TCM ()
checkBlock (Blck _ ss) = do
    tcs <- get
    modify (\s -> s { set = Map.keysSet (env s) })
    mapM_ checkStmt ss
    res <- gets returned
    put tcs { returned = res }

checkStmt :: Stmt -> TCM ()
checkStmt (Empty _) = return ()
checkStmt (Exp _ e) = checkExp e >> return ()
checkStmt (Ass pos e1 e2) = case e1 of
    EVar _ x -> do
        t <- getType pos (takeStr x)
        checkExp e2 >>= assertSame pos t
    _ -> throwError $ newErr pos ("Cannot assign a value to an expression")
checkStmt (Incr pos e) = case e of
    EVar _ x -> do
        getType pos (takeStr x) >>= assertInt pos
    _ -> throwError $ newErr pos ("Cannot increment an expression")
checkStmt (Decr pos e) = case e of
    EVar _ x -> do
        getType pos (takeStr x) >>= assertInt pos
    _ -> throwError $ newErr pos ("Cannot decrement an expression")
checkStmt (Ret pos e) = do
    ret <- gets ret
    case ret of
        Just t -> do
            expTy <- checkExp e
            if expTy == TVoid
            then throwError $ newErr pos "Cannot return void"
            else assertSame pos t expTy >> modify (\st -> st { returned = True })
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (VRet pos) = do
    ret <- gets ret
    case ret of
        Just t -> assertVoid pos t >> modify (\st -> st { returned = True })
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (If pos e s) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s])
    res <- gets returned
    case e of
        ETrue _ -> put tcs { returned = res }
        _ -> put tcs
checkStmt (IfElse pos e s1 s2) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s1])
    res1 <- gets returned
    put tcs
    checkBlock (Blck pos [s2])
    res2 <- gets returned
    case e of
        ETrue _ -> put tcs { returned = res1 }
        EFalse _ -> put tcs { returned = res2 }
        _ -> put tcs { returned = res1 && res2 }
checkStmt (While pos e s) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s])
    res <- gets returned
    case e of
        ETrue _ -> put tcs { returned = res }
        _ -> put tcs
checkStmt (BStmt _ ss) = checkBlock ss
checkStmt (Decl _ t its) = mapM_ (\it -> do
    let x = itemStr it
    let ty = takeType t
    case it of
        NoInit pos _ -> writeType pos x ty
        Init pos _ e -> checkExp e >>= assertSame pos ty >> writeType pos x ty
    ) its

checkExp :: Expr -> TCM TType
checkExp (EOr pos e1 e2) = do
    checkExp e1 >>= assertBool pos
    checkExp e2 >>= assertBool pos
    return TBool
checkExp (EAnd pos e1 e2) = do
    checkExp e1 >>= assertBool pos
    checkExp e2 >>= assertBool pos
    return TBool
checkExp (ERel pos e1 _ e2) = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    case (t1, t2) of
        (TVoid, TVoid) -> throwError $ newErr pos "Cannot compare void types"
        (TBool, TBool) -> throwError $ newErr pos "Cannot compare boolean types"
        _ -> assertSame pos t1 t2
    return TBool
checkExp (EAdd pos e1 op e2) = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    case (t1, t2, op) of
        (TInt, TInt, _) -> return TInt
        (TStr, TStr, (Plus _)) -> return TStr
        _ -> throwError $ newErr pos ("Could not add variable of type " ++ show t1 ++ " to the variable of type " ++ show t2)
checkExp (EMul pos e1 _ e2) = do
    checkExp e1 >>= assertInt pos
    checkExp e2 >>= assertInt pos
    return TInt
checkExp (Not pos e) = do
    checkExp e >>= assertBool pos
    return TBool
checkExp (Neg pos e) = do
    checkExp e >>= assertInt pos
    return TInt
checkExp (EVar pos x) = getType pos (takeStr x)
checkExp (EInt _ _) = return TInt
checkExp (EString _ _) = return TStr
checkExp (ETrue _) = return TBool
checkExp (EFalse _) = return TBool
checkExp (EApp pos x es) =
    if (takeStr x) == "main"
    then throwError $ newErr pos ("main() cannot be invoked")
    else do
        t <- getType pos (takeStr x)
        case t of
            TFun ty args _ -> if length args == length es
                then mapM_ (\(arg, e) -> assertArgs arg e) (zip args es) >> return ty
                else throwError $ newErr pos "Wrong number of arguments passed to a function"
            _ -> throwError $ newErr pos ((takeStr x) ++ " is not a function")

typeCheck :: Program -> Either ErrSt ()
typeCheck (Prog _ prg) = fst $ runState (runExceptT (checkPrg prg)) initState
