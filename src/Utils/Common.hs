module Utils.Common where

import Latte.AbsLatte

data SType = SInt Integer | SStr String | SBool Bool | Undefined
data TType
    = TVoid
    | TInt
    | TStr
    | TBool
    | TNull
    | TArr TType
    | TClass String
    deriving (Eq)

type Pos = BNFC'Position
data ErrSt = ErrSt
    { pos :: Pos
    , reason :: String
    }

instance Show TType where
    show TVoid = "void"
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show TNull = "null"
    show (TArr t) = show t ++ "[]"
    show (TClass x) = "class " ++ x

instance Show ErrSt where
    show err = case (pos err, reason err) of
        (Just (row, col), s) -> "(" ++ show row ++ ":" ++ show col ++ "): " ++ s
        (_, s) -> "(_:_): " ++ s

newErr :: Pos -> String -> ErrSt
newErr pos s = ErrSt {pos = pos, reason = s}

takeStr :: Ident -> String
takeStr (Ident x) = x

itemStr :: Item -> String
itemStr (NoInit _ x) = takeStr x
itemStr (Init _ x _) = takeStr x

takeType :: Type -> TType
takeType (Int _) = TInt
takeType (Str _) = TStr
takeType (Bool _) = TBool
takeType (Void _) = TVoid
takeType (Array _ t) = TArr (takeType t)
takeType (Class _ x) = TClass (takeStr x)

aop :: AddOp -> Integer -> Integer -> Integer
aop (Plus _) = (+)
aop (Minus _) = (-)

mop :: MulOp -> Integer -> Integer -> Integer
mop (Times _) = (*)
mop (Div _) = div
mop (Mod _) = rem

rop :: Ord a => RelOp -> a -> a -> Bool
rop (LTH _) = (<)
rop (LE _) = (<=)
rop (GTH _) = (>)
rop (GE _) = (>=)
rop (EQU _) = (==)
rop (NE _) = (/=)

checkBounds :: Integer -> Bool
checkBounds n = n >= -9223372036854775808 && n <= 9223372036854775807

simplifyExpr :: Expr -> Either ErrSt SType
simplifyExpr (EOr _ e1 e2) =
    case (simplifyExpr e1, simplifyExpr e2) of
        (Right (SBool b1), Right (SBool b2)) -> Right $ SBool (b1 || b2)
        _ -> Right Undefined
simplifyExpr (EAnd _ e1 e2) =
    case (simplifyExpr e1, simplifyExpr e2) of
        (Right (SBool b1), Right (SBool b2)) -> Right $ SBool (b1 && b2)
        _ -> Right Undefined
simplifyExpr (ERel _ e1 op e2) =
    case (simplifyExpr e1, simplifyExpr e2) of
        (Right (SInt n1), Right (SInt n2)) -> Right $ SBool (rop op n1 n2)
        (Right (SStr s1), Right (SStr s2)) -> Right $ SBool (rop op s1 s2)
        (Right (SBool b1), Right (SBool b2)) -> Right $ SBool (rop op b1 b2)
        _ -> Right Undefined
simplifyExpr (EAdd pos e1 op e2) =
    case (simplifyExpr e1, simplifyExpr e2, op) of
        (Right (SInt n1), Right (SInt n2), _) ->
            let res = aop op n1 n2
             in if checkBounds res
                    then Right $ SInt res
                    else Left $ newErr pos ("Number " ++ show res ++ " is out of bounds")
        (Right (SStr s1), Right (SStr s2), Plus _) -> Right $ SStr (s1 ++ s2)
        _ -> Right Undefined
simplifyExpr (EMul pos e1 op e2) =
    case (simplifyExpr e1, simplifyExpr e2, op) of
        (Right (SInt _), Right (SInt 0), Div _) -> Left $ newErr pos "Cannot divide by 0"
        (Right (SInt _), Right (SInt 0), Mod _) -> Left $ newErr pos "Cannot divide by 0"
        (Right (SInt n1), Right (SInt n2), _) ->
            let res = mop op n1 n2
             in if checkBounds res
                    then Right $ SInt res
                    else Left $ newErr pos ("Number " ++ show res ++ " is out of bounds")
        _ -> Right Undefined
simplifyExpr (Not _ e) =
    case simplifyExpr e of
        Right (SBool b) -> Right $ SBool (not b)
        _ -> Right Undefined
simplifyExpr (Neg pos e) =
    case simplifyExpr e of
        Right (SInt n) ->
            let res = -n
             in if checkBounds res
                    then Right (SInt res)
                    else Left $ newErr pos ("Number " ++ show res ++ " is out of bounds")
        _ -> Right Undefined
simplifyExpr (EInt _ n) = Right $ SInt n
simplifyExpr (EString _ s) = Right $ SStr s
simplifyExpr (ETrue _) = Right $ SBool True
simplifyExpr (EFalse _) = Right $ SBool False
simplifyExpr _ = Right Undefined
