module Utils.Common where

import Latte.AbsLatte

data TType = TVoid | TInt | TStr | TBool | TNull
           | TArr TType | TClass String deriving Eq

instance Show TType where
    show TVoid = "void"
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show TNull = "null"
    show (TArr t) = show t ++ "[]"
    show (TClass x) = "class " ++ x

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
