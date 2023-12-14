module Utils.Frontend where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.AbsLatte
import Utils.Common

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

newErr :: Pos -> String -> ErrSt
newErr pos s = ErrSt { pos = pos, reason = s }

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
takeExprPos (ENull pos _) = pos
takeExprPos (EArr pos _ _) = pos
takeExprPos (EElem pos _ _) = pos
takeExprPos (EVar pos _) = pos
takeExprPos (EInt pos _) = pos
takeExprPos (EString pos _) = pos
takeExprPos (ETrue pos) = pos
takeExprPos (EFalse pos) = pos
takeExprPos (EApp pos _ _) = pos
