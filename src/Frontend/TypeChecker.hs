module Frontend.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.AbsLatte

data TType = TVoid | TInt | TStr | TBool | TFun TType [Arg] Block deriving Eq

type Var = String
type Env = Map.Map Var TType
data TCSt = TCSt {
    env :: Env,
    ret :: Maybe TType,
    set :: Set.Set Var,
    returned :: Bool
}

type Pos = BNFC'Position
data ErrSt = ErrSt {
    pos :: Pos,
    reason :: String
}

type TCM a = ExceptT ErrSt (State TCSt) a

instance Show TType where
    show TVoid = "void"
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show (TFun _ _ _) = "function"

instance Show ErrSt where
    show err = case (pos err, reason err) of
        (Just (row, col), s) -> "(" ++ show row ++ ":" ++ show col ++ "): " ++ s
        (_, s) -> "(_:_): " ++ s

initEnv :: Env
initEnv = Map.fromList [
    ("printInt", TFun TVoid [Ar Nothing (Int Nothing) (Ident "n")] (Blck Nothing [])),
    ("printString", TFun TVoid [Ar Nothing (Str Nothing) (Ident "s")] (Blck Nothing [])),
    ("error", TFun TVoid [] (Blck Nothing [])),
    ("readInt", TFun TInt [] (Blck Nothing [])),
    ("readString", TFun TStr [] (Blck Nothing []))
    ]

initState :: TCSt
initState = TCSt { env = initEnv, ret = Nothing, set = Set.empty, returned = False }

takeStr :: Ident -> String
takeStr (Ident x) = x

itemStr :: Item -> String
itemStr (NoInit _ x) = takeStr x
itemStr (Init _ x _) = takeStr x

newErr :: Pos -> String -> ErrSt
newErr pos s = ErrSt { pos = pos, reason = s }

takeType :: Type -> TType
takeType (Int _) = TInt
takeType (Str _) = TStr
takeType (Bool _) = TBool
takeType (Void _) = TVoid

writeType :: Pos -> Var -> TType -> TCM ()
writeType pos x t = do
    env_ <- gets env
    set_ <- gets set
    case Map.lookup x env_ of
        Just _ -> if Set.member x set_
            then modify (\s -> s { env = Map.insert x t (env s), set = Set.delete x set_ })
            else throwError $ newErr pos ("Variable " ++ x ++ " already declared")
        Nothing -> modify (\s -> s { env = Map.insert x t (env s) })

writeFunctions :: [TopDef] -> TCM ()
writeFunctions = mapM_ (\(FnDef pos t x args ss) -> writeType pos (takeStr x) (TFun (takeType t) args ss))

checkForMain :: TCM ()
checkForMain = do
    env <- gets env
    case Map.lookup "main" env of
        Just (TFun TInt [] _) -> return ()
        _ -> throwError $ newErr Nothing ("The `int main()` function must be declared")

getType :: Pos -> Var -> TCM TType
getType pos x = do
    env <- gets env
    case Map.lookup x env of
        Just t -> return t
        Nothing -> throwError $ newErr pos ("Variable " ++ x ++ " not in scope")

assertVoid :: Pos -> TType -> TCM ()
assertVoid pos t = case t of
    TVoid -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type void with actual type " ++ show t)

assertInt :: Pos -> TType -> TCM ()
assertInt pos t = case t of
    TInt -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type int with actual type " ++ show t)

assertStr :: Pos -> TType -> TCM ()
assertStr pos t = case t of
    TStr -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type string with actual type " ++ show t)

assertBool :: Pos -> TType -> TCM ()
assertBool pos t = case t of
    TBool -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type bool with actual type " ++ show t)

assertSame :: Pos -> TType -> TType -> TCM ()
assertSame pos t1 t2 = if t1 == t2
    then return ()
    else throwError $ newErr pos ("Could not match expected type " ++ show t1 ++ " with actual type " ++ show t2)

assertArgs :: Arg -> Expr -> TCM ()
assertArgs (Ar pos ty _) e = checkExp e >>= assertSame pos (takeType ty)

checkPrg :: [TopDef] -> TCM ()
checkPrg tds = writeFunctions tds >> checkForMain >> mapM_ checkTopDef tds

checkTopDef :: TopDef -> TCM ()
checkTopDef (FnDef pos t x args ss) = do
    tcs <- get
    mapM_ (\(Ar p ty y) -> writeType p (takeStr y) (takeType ty)) args
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
        Just t -> checkExp e >>= assertSame pos t >> modify (\st -> st { returned = True })
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (VRet pos) = do
    ret <- gets ret
    case ret of
        Just t -> assertVoid pos t >> modify (\st -> st { returned = True })
        Nothing -> throwError $ newErr pos "Invalid use of return"
checkStmt (If pos e s) = do
    tcs <- get
    checkExp e >>= assertBool pos
    checkBlock (Blck pos [s])
    res <- gets returned
    case e of
        ETrue _ -> put tcs { returned = res }
        _ -> put tcs
checkStmt (IfElse pos e s1 s2) = do
    tcs <- get
    checkExp e >>= assertBool pos
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
    checkExp e >>= assertBool pos
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
    checkExp e2 >>= assertSame pos t1
    return TBool
checkExp (EAdd pos e1 _ e2) = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        (TStr, TStr) -> return TStr
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
                then mapM_ (\(arg,e) -> assertArgs arg e) (zip args es) >> return ty
                else throwError $ newErr pos "Wrong number of arguments passed to a function"
            _ -> throwError $ newErr pos ((takeStr x) ++ " is not a function")

typeCheck :: Program -> Either ErrSt ()
typeCheck (Prog _ prg) = fst $ runState (runExceptT (checkPrg prg)) initState
