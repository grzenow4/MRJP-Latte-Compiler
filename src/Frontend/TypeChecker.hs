module Frontend.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.AbsLatte
import Utils.Common
import Utils.Frontend

assertArg :: Arg -> Expr -> TCM ()
assertArg (Ar _ ty _) e = checkExp e >>= assertSame (takeExprPos e) (takeType ty)

checkField :: Field -> TCM ()
checkField (AttrDef pos t x) = do
    assertType pos (takeType t)
    if takeType t == TVoid
        then throwError $ newErr pos "Cannot declare an attribute of type void"
        else return ()
checkField (MethodDef pos t x args ss) = do
    assertType pos (takeType t)
    tcs <- get
    modify (\st -> st {ret = Just (takeType t), set = Map.keysSet (env st)})
    mapM_ (\(Ar p ty y) -> writeVar p (takeStr y) (takeType ty)) args
    checkBlock ss
    res <- gets returned
    if not (res || takeType t == TVoid)
        then throwError $ newErr pos ("Method " ++ (takeStr x) ++ " does not return")
        else put tcs

checkPrg :: [TopDef] -> TCM ()
checkPrg tds = writeTopDefs tds >> checkForMain >> mapM_ checkTopDef tds

checkTopDef :: TopDef -> TCM ()
checkTopDef (FnDef pos t x args ss) = do
    tcs <- get
    mapM_ (\(Ar p ty y) -> writeVar p (takeStr y) (takeType ty)) args
    modify (\st -> st {ret = Just (takeType t)})
    checkBlock ss
    res <- gets returned
    if not (res || takeType t == TVoid)
        then throwError $ newErr pos ("Function " ++ (takeStr x) ++ " does not return")
        else put tcs
checkTopDef (ClassDef pos x fields) = do
    tcs <- get
    Clss cenv _ _ <- getClass pos (takeStr x)
    modify (\st -> st {env = cenv, self = Just (takeStr x)})
    mapM_ checkField fields
    put tcs
checkTopDef (ClassExt pos x y fields) = do
    checkExtend pos (takeStr x) (Set.empty)
    tcs <- get
    Clss cenv _ _ <- getClass pos (takeStr x)
    modify (\st -> st {env = cenv, self = Just (takeStr x)})
    prepareEnvExt pos (takeStr y)
    mapM_ (checkMethodExt $ takeStr y) fields
    mapM_ checkField fields
    put tcs

checkBlock :: Block -> TCM ()
checkBlock (Blck _ ss) = do
    tcs <- get
    modify (\st -> st {set = Map.keysSet (env st)})
    mapM_ checkStmt ss
    res <- gets returned
    put tcs {returned = res}

checkStmt :: Stmt -> TCM ()
checkStmt (Empty _) = return ()
checkStmt (Exp _ e) = checkExp e >> return ()
checkStmt (Ass _ (EVar p x) e) = do
    t1 <- getVar p (takeStr x)
    t2 <- checkExp e
    case (t1, t2) of
        (TArr _, TNull) -> return ()
        (TClass _, TNull) -> return ()
        _ -> assertSame (takeExprPos e) t1 t2
checkStmt (Ass pos (EAttr _ e1 x) e2) = do
    t <- checkExp e1
    case t of
        TClass name -> do
            t1 <- takeAttr (takeExprPos e1) (takeStr x) name
            t2 <- checkExp e2
            case (t1, t2) of
                (TArr _, TNull) -> return ()
                (TClass _, TNull) -> return ()
                _ -> assertSame (takeExprPos e2) t1 t2
        _ -> throwError $ newErr (takeExprPos e1) "Expression must be a class"
checkStmt (Ass pos e1@(EElem _ _ _) e2) = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    case (t1, t2) of
        (TClass _, TNull) -> return ()
        _ -> assertSame (takeExprPos e2) t1 t2
checkStmt (Ass pos _ _) = throwError $ newErr pos "Cannot assign to an expression"
checkStmt (Incr pos e) = case e of
    EVar _ x -> getVar pos (takeStr x) >>= assertInt pos
    EAttr _ e1 x -> do
        t <- checkExp e1
        case t of
            TClass name -> takeAttr pos (takeStr x) name >>= assertInt pos
            _ -> throwError $ newErr (takeExprPos e1) "Expression must be a class"
    EElem _ _ _ -> checkExp e >>= assertInt pos
    _ -> throwError $ newErr pos "Cannot increment an expression"
checkStmt (Decr pos e) = case e of
    EVar _ x -> getVar pos (takeStr x) >>= assertInt pos
    EAttr _ e1 x -> do
        t <- checkExp e1
        case t of
            TClass name -> takeAttr pos (takeStr x) name >>= assertInt pos
            _ -> throwError $ newErr (takeExprPos e1) "Expression must be a class"
    EElem _ _ _ -> checkExp e >>= assertInt pos
    _ -> throwError $ newErr pos "Cannot decrement an expression"
checkStmt (Ret pos e) = do
    ret <- gets ret
    case ret of
        Just t -> do
            expTy <- checkExp e
            case (t, expTy) of
                (_, TVoid) -> throwError $ newErr pos "Cannot return void"
                (TClass _, TNull) -> modify (\st -> st {returned = True})
                _ -> assertSame (takeExprPos e) t expTy >> modify (\st -> st {returned = True})
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (VRet pos) = do
    ret <- gets ret
    case ret of
        Just t -> assertVoid pos t >> modify (\st -> st {returned = True})
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (If pos e s) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s])
    res <- gets returned
    case simplifyExpr e of
        Right (SBool True) -> put tcs {returned = res}
        Left err -> throwError err
        _ -> put tcs
checkStmt (IfElse pos e s1 s2) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s1])
    res1 <- gets returned
    put tcs
    checkBlock (Blck pos [s2])
    res2 <- gets returned
    case simplifyExpr e of
        Right (SBool True) -> put tcs {returned = res1}
        Right (SBool False) -> put tcs {returned = res2}
        Left err -> throwError err
        _ -> put tcs {returned = res1 && res2}
checkStmt (While pos e s) = do
    tcs <- get
    checkExp e >>= assertBool (takeExprPos e)
    checkBlock (Blck pos [s])
    res <- gets returned
    case simplifyExpr e of
        Right (SBool True) -> put tcs {returned = res}
        Left err -> throwError err
        _ -> put tcs
checkStmt (For pos t x e s) = do
    tcs <- get
    ty <- checkExp e
    case ty of
        TArr typ -> do
            assertSame pos typ (takeType t)
            modify (\st -> st {set = Map.keysSet (env st)})
            writeVar pos (takeStr x) (takeType t)
            case s of
                BStmt _ (Blck _ ss) -> mapM_ checkStmt ss
                _ -> mapM_ checkStmt [s]
            put tcs
        _ -> throwError $ newErr (takeExprPos e) "Can only iterate over an array"
checkStmt (BStmt _ ss) = checkBlock ss
checkStmt (Decl _ t its) =
    mapM_
        ( \it -> do
            let x = itemStr it
            let t1 = takeType t
            case it of
                NoInit pos _ -> writeVar pos x t1
                Init pos _ e -> do
                    t2 <- checkExp e
                    case (t1, t2) of
                        (TArr _, TNull) -> writeVar pos x t1
                        (TClass _, TNull) -> writeVar pos x t1
                        _ -> assertSame (takeExprPos e) t1 t2 >> writeVar pos x t1
        )
        its

checkExp :: Expr -> TCM TType
checkExp e@(EOr _ e1 e2) = case simplifyExpr e of
    Right (SBool _) -> return TBool
    Left err -> throwError err
    _ -> do
        checkExp e1 >>= assertBool (takeExprPos e1)
        checkExp e2 >>= assertBool (takeExprPos e2)
        return TBool
checkExp e@(EAnd _ e1 e2) = case simplifyExpr e of
    Right (SBool _) -> return TBool
    Left err -> throwError err
    _ -> do
        checkExp e1 >>= assertBool (takeExprPos e1)
        checkExp e2 >>= assertBool (takeExprPos e2)
        return TBool
checkExp e@(ERel pos e1 _ e2) = case simplifyExpr e of
    Right (SBool _) -> return TBool
    Left err -> throwError err
    _ -> do
        t1 <- checkExp e1
        t2 <- checkExp e2
        case (t1, t2) of
            (TClass x, TClass y) -> do
                res1 <- isSuperClass pos x y
                res2 <- isSuperClass pos y x
                if res1 || res2
                    then return TBool
                    else throwError $ newErr pos ("Cannot compare class " ++ x ++ " with class " ++ y)
            _ -> assertSame (takeExprPos e2) t1 t2 >> return TBool
checkExp e@(EAdd pos e1 op e2) = case simplifyExpr e of
    Right (SInt _) -> return TInt
    Right (SStr _) -> return TStr
    Left err -> throwError err
    _ -> do
        t1 <- checkExp e1
        t2 <- checkExp e2
        case (t1, t2, op) of
            (TInt, TInt, _) -> return TInt
            (TStr, TStr, Plus _) -> return TStr
            (TStr, TStr, Minus _) -> throwError $ newErr pos "Cannot substract strings"
            _ -> throwError $ newErr pos ("Could not add variable of type " ++ show t1 ++ " to the variable of type " ++ show t2)
checkExp e@(EMul _ e1 _ e2) = case simplifyExpr e of
    Right (SInt _) -> return TInt
    Left err -> throwError err
    _ -> do
        checkExp e1 >>= assertInt (takeExprPos e1)
        checkExp e2 >>= assertInt (takeExprPos e2)
        return TInt
checkExp e@(Not _ e1) = case simplifyExpr e of
    Right (SBool _) -> return TBool
    Left err -> throwError err
    _ -> checkExp e1 >>= assertBool (takeExprPos e1) >> return TBool
checkExp e@(Neg pos e1) = case simplifyExpr e of
    Right (SInt n) -> return TInt
    Left err -> throwError err
    _ -> checkExp e1 >>= assertInt pos >> return TInt
checkExp (EClass pos x) = do
    let name = takeStr x
    getClass pos name
    return $ TClass name
checkExp (EAttr pos e x) = do
    let attr = takeStr x
    t <- checkExp e
    case t of
        TArr _ ->
            if attr == "length"
                then return TInt
                else throwError $ newErr pos ("An array has no attribute " ++ attr)
        TClass name -> takeAttr pos attr name
        _ -> throwError $ newErr (takeExprPos e) "Expression must be an array or a class"
checkExp (EMethod pos e x es) = do
    t <- checkExp e
    case t of
        TClass name -> do
            Func ty args <- takeMethod pos (takeStr x) name
            if length args == length es
                then mapM_ (\(arg, e) -> assertArg arg e) (zip args es) >> return ty
                else throwError $ newErr pos "Wrong number of arguments passed to a method"
        _ -> throwError $ newErr (takeExprPos e) "Expression must be a class"
checkExp (EArr pos t e) = do
    case simplifyExpr e of
        Right (SInt n) ->
            if n < 0
                then throwError $ newErr pos "An array must be of positive size"
                else return ()
        Left err -> throwError err
        _ -> checkExp e >>= assertInt (takeExprPos e)
    case takeType t of
        TVoid -> throwError $ newErr pos "Cannot declare an array of voids"
        TArr _ -> throwError $ newErr pos "Can declare only one-dimensional arrays"
        TClass x -> getClass pos x >> (return $ TArr (TClass x))
        ty -> return $ TArr ty
checkExp (EElem _ e1 e2) = do
    case simplifyExpr e2 of
        Right (SInt n) ->
            if n < 0
                then throwError $ newErr (takeExprPos e2) "Cannot get a negative index of an array"
                else return ()
        Left err -> throwError err
        _ -> checkExp e2 >>= assertInt (takeExprPos e2)
    t <- checkExp e1
    case t of
        TArr ty -> return ty
        _ -> throwError $ newErr (takeExprPos e1) "Expression must be an array"
checkExp (ESelf pos) = do
    ctx <- gets self
    case ctx of
        Just name -> return $ TClass name
        Nothing -> throwError $ newErr pos "Invalid use of self"
checkExp (ENull pos) = return TNull
checkExp (ENullCast pos t) = case takeType t of
    ty@(TArr _) -> assertType pos ty >> return ty
checkExp (ENullClss pos e) = case e of
    (EVar _ x) -> checkExp (EClass pos x)
    _ -> throwError $ newErr pos "Invalid null cast"
checkExp (EVar pos x) = getVar pos (takeStr x)
checkExp (EInt pos n) = do
    if checkBounds n
        then return TInt
        else throwError $ newErr pos ("Number " ++ show n ++ " is out of bounds")
checkExp (EString _ _) = return TStr
checkExp (ETrue _) = return TBool
checkExp (EFalse _) = return TBool
checkExp (EApp pos x es) =
    if (takeStr x) == "main"
        then throwError $ newErr pos "main() cannot be invoked"
        else do
            Func t args <- getFun pos (takeStr x)
            if length args == length es
                then mapM_ (\(arg, e) -> assertArg arg e) (zip args es) >> return t
                else throwError $ newErr pos "Wrong number of arguments passed to a function"

typeCheck :: Program -> Either ErrSt ()
typeCheck (Prog _ prg) = fst $ runState (runExceptT (checkPrg prg)) initState
