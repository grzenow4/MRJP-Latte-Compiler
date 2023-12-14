module Utils.Frontend where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.AbsLatte
import Utils.Common

type Var = String
type Env = Map.Map Var TType

data Func = Func TType [Arg]
type FEnv = Map.Map Var Func

data Clss = Clss Env FEnv (Maybe String)
type CEnv = Map.Map Var Clss

data TCSt = TCSt {
    env :: Env,
    fun :: FEnv,
    cls :: CEnv,
    ret :: Maybe TType,
    set :: Set.Set Var,
    self :: Maybe Var,
    returned :: Bool
}

type Pos = BNFC'Position
data ErrSt = ErrSt {
    pos :: Pos,
    reason :: String
}

type TCM a = ExceptT ErrSt (State TCSt) a

instance Show ErrSt where
    show err = case (pos err, reason err) of
        (Just (row, col), s) -> "(" ++ show row ++ ":" ++ show col ++ "): " ++ s
        (_, s) -> "(_:_): " ++ s

initFunc :: FEnv
initFunc = Map.fromList [
    ("printInt", Func TVoid [Ar Nothing (Int Nothing) (Ident "n")]),
    ("printString", Func TVoid [Ar Nothing (Str Nothing) (Ident "s")]),
    ("error", Func TVoid []),
    ("readInt", Func TInt []),
    ("readString", Func TStr [])
    ]

initState :: TCSt
initState = TCSt { env = Map.empty
                 , fun = initFunc
                 , cls = Map.empty
                 , ret = Nothing
                 , set = Set.empty
                 , self = Nothing
                 , returned = False }

newErr :: Pos -> String -> ErrSt
newErr pos s = ErrSt { pos = pos, reason = s }

writeVar :: Pos -> Var -> TType -> TCM ()
writeVar pos x t = do
    assertType pos t
    env_ <- gets env
    set_ <- gets set
    if t == TVoid
    then throwError $ newErr pos "Cannot create a variable of type void"
    else case Map.member x env_ of
        True -> if Set.member x set_
                then modify (\st -> st { env = Map.insert x t (env st), set = Set.delete x set_ })
                else throwError $ newErr pos ("Variable " ++ x ++ " already declared")
        False -> modify (\st -> st { env = Map.insert x t (env st) })

getVar :: Pos -> Var -> TCM TType
getVar pos x = do
    env <- gets env
    case Map.lookup x env of
        Just t -> return t
        Nothing -> throwError $ newErr pos ("Variable " ++ x ++ " not in scope")

writeFun :: Pos -> Var -> Func -> TCM ()
writeFun pos name f@(Func t _) = do
    assertType pos t
    fun_ <- gets fun
    if Map.member name fun_
    then throwError $ newErr pos ("Function " ++ name ++ " already declared")
    else modify (\st -> st { fun = Map.insert name f (fun st) })

getFun :: Pos -> Var -> TCM Func
getFun pos name = do
    fun <- gets fun
    case Map.lookup name fun of
        Just f -> return f
        Nothing -> throwError $ newErr pos ("Function " ++ name ++ " not in scope")

writeClass :: Pos -> Var -> Clss -> TCM ()
writeClass pos name c = do
    cls_ <- gets cls
    if Map.member name cls_
    then throwError $ newErr pos ("Class " ++ name ++ " already declared")
    else modify (\st -> st { cls = Map.insert name c (cls st) })

getClass :: Pos -> Var -> TCM Clss
getClass pos name = do
    cls <- gets cls
    case Map.lookup name cls of
        Just c -> return c
        Nothing -> throwError $ newErr pos ("Class " ++ name ++ " not in scope")

writeFields :: [Field] -> TCM (Env, FEnv)
writeFields = foldM (\(env, fenv) field -> case field of
    AttrDef pos t x -> do
        let name = takeStr x
        let ty = takeType t
        case Map.member name env of
            True -> throwError $ newErr pos ("Attribute " ++ name ++ " already defined")
            False -> return (Map.insert name ty env, fenv)
    MethodDef pos t x args _ -> do
        let fname = takeStr x
        let ty = takeType t
        case Map.member fname fenv of
            True -> throwError $ newErr pos ("Method " ++ fname ++ " already defined")
            False -> return (env, Map.insert fname (Func ty args) fenv)
    ) (Map.empty, Map.empty)

writeTopDefs :: [TopDef] -> TCM ()
writeTopDefs = mapM_ (\td -> case td of
    FnDef pos t x args _ -> writeFun pos (takeStr x) (Func (takeType t) args)
    ClassDef pos x fields -> do
        (env, fenv) <- writeFields fields
        writeClass pos (takeStr x) (Clss env fenv Nothing)
    ClassExt pos x y fields -> do
        (env, fenv) <- writeFields fields
        writeClass pos (takeStr x) (Clss env fenv (Just (takeStr y))))

checkExtend :: Pos -> Var -> Set.Set Var -> TCM ()
checkExtend pos x s =
    if Set.member x s
    then throwError $ newErr pos "Cyclic extend detected"
    else do
        Clss _ _ ext <- getClass pos x
        maybe (return ()) (\y -> checkExtend pos y (Set.insert x s)) ext

prepareEnvExt :: Pos -> Var -> TCM ()
prepareEnvExt pos name = do
    Clss cenv _ ext <- getClass pos name
    mapM_ (\(x, t) -> do
        env_ <- gets env
        case Map.lookup x env_ of
            Just ty -> assertSame pos t ty
            Nothing -> modify (\st -> st { env = Map.insert x t (env st) })
        ) (Map.assocs cenv)
    maybe (return ()) (prepareEnvExt pos) ext

checkMethodExt :: Var -> Field -> TCM ()
checkMethodExt _ (AttrDef _ _ _) = return ()
checkMethodExt name f@(MethodDef pos t x args1 _) = do
    Clss _ fenv ext <- getClass pos name
    let fname = takeStr x
    case Map.lookup fname fenv of
        Just (Func ty args2) -> do
            assertSame pos ty (takeType t)
            if length args1 == length args2
            then mapM_ (\(Ar p t1 _, Ar _ t2 _) -> assertSame p (takeType t2) (takeType t1)) (zip args1 args2)
            else throwError $ newErr pos ("Invalid number of parameters in method " ++ fname)
        Nothing -> maybe (return ()) (\y -> checkMethodExt y f) ext

isSuperClass :: Pos -> Var -> Var -> TCM Bool
isSuperClass pos x y = if x == y then return True else do
    Clss _ _ ext <- getClass pos y
    maybe (return False) (isSuperClass pos x) ext

takeAttr :: Pos -> String -> Var -> TCM TType
takeAttr pos attr x = do
    Clss env _ ext <- getClass pos x
    case Map.lookup attr env of
        Just t -> return t
        Nothing -> maybe (throwError $ newErr pos ("Class has no attribute " ++ attr))
                         (takeAttr pos attr) ext

takeMethod :: Pos -> String -> Var -> TCM Func
takeMethod pos method x = do
    Clss _ fenv ext <- getClass pos x
    case Map.lookup method fenv of
        Just f -> return f
        Nothing -> maybe (throwError $ newErr pos ("Class has no method " ++ method))
                         (takeMethod pos method) ext

checkForMain :: TCM ()
checkForMain = do
    fun <- gets fun
    case Map.lookup "main" fun of
        Just (Func TInt []) -> return ()
        _ -> throwError $ newErr Nothing "The `int main()` function must be declared"

assertType :: Pos -> TType -> TCM ()
assertType pos t@(TArr (TArr _)) = throwError $ newErr pos ("Invalid type " ++ show t)
assertType pos t@(TArr TVoid) = throwError $ newErr pos ("Invalid type " ++ show t)
assertType pos t@(TArr TNull) = throwError $ newErr pos ("Invalid type " ++ show t)
assertType pos (TArr (TClass x)) = getClass pos x >> return ()
assertType pos (TClass x) = getClass pos x >> return ()
assertType pos t@TNull = throwError $ newErr pos ("Invalid type " ++ show t)
assertType _ _ = return ()

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

assertArr :: Pos -> TType -> TCM ()
assertArr pos t = case t of
    TArr _ -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type array with actual type " ++ show t)

assertSame :: Pos -> TType -> TType -> TCM ()
assertSame pos t1 t2 = case (t1, t2) of
    (TClass x, TClass y) -> do
        res <- isSuperClass pos x y
        if res
        then return ()
        else throwError $ newErr pos ("Class " ++ y ++ " is not a subclass of " ++ show x)
    _ -> if t1 == t2
         then return ()
         else throwError $ newErr pos ("Could not match expected type " ++ show t1 ++ " with actual type " ++ show t2)

takeExprPos :: Expr -> Pos
takeExprPos (EOr pos _ _) = pos
takeExprPos (EAnd pos _ _) = pos
takeExprPos (ERel pos _ _ _) = pos
takeExprPos (EAdd pos _ _ _) = pos
takeExprPos (EMul pos _ _ _) = pos
takeExprPos (Not pos _) = pos
takeExprPos (Neg pos _) = pos
takeExprPos (EClass pos _) = pos
takeExprPos (EAttr pos _ _) = pos
takeExprPos (EMethod pos _ _ _) = pos
takeExprPos (EArr pos _ _) = pos
takeExprPos (EElem pos _ _) = pos
takeExprPos (ESelf pos) = pos
takeExprPos (ENull pos) = pos
takeExprPos (ENullCast pos _) = pos
takeExprPos (ENullClss pos _) = pos
takeExprPos (EVar pos _) = pos
takeExprPos (EInt pos _) = pos
takeExprPos (EString pos _) = pos
takeExprPos (ETrue pos) = pos
takeExprPos (EFalse pos) = pos
takeExprPos (EApp pos _ _) = pos
