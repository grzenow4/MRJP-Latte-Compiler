module Utils.Backend where

import Control.Monad.RWS
import qualified Data.Map as Map

import Backend.Generator
import Latte.AbsLatte
import Utils.Common

type Attributes = Map.Map String (TType, Int)
type Methods = Map.Map String TType
type Extends = Maybe String
type ClassEnv = (Attributes, Methods, Extends)

data Reader = Rd
    { func :: Map.Map String TType
    , clss :: Map.Map String ClassEnv
    }
type Writer = [AsmInstr]
data State = St
    { nextlab :: Int
    , newloc :: Int
    , env :: Map.Map String (TType, String)
    , rodata :: Map.Map String String
    }

type CM a = RWS Reader Writer State a

predefFunc :: [(String, TType)]
predefFunc =
    [ ("printInt", TVoid)
    , ("printString", TVoid)
    , ("error", TVoid)
    , ("readInt", TInt)
    , ("readString", TStr)
    , ("__concatString", TStr)
    ]

initReader :: Reader
initReader = Rd {func = Map.fromList predefFunc, clss = Map.empty}

methodName :: String -> String -> String
methodName cname mname = "__cls_" ++ cname ++ "_mth_" ++ mname

getClassSize :: String -> CM Int
getClassSize name = do
    clss <- asks clss
    case Map.lookup name clss of
        Just (attrs, _, _) -> return $ Map.size attrs
        Nothing -> error "Typechecker failed"

getAttr :: String -> String -> CM (TType, Int)
getAttr className attrName = do
    clss <- asks clss
    case Map.lookup className clss of
        Just (attrs, _, _) -> case Map.lookup attrName attrs of
            Just res -> return res
            Nothing -> error "Typechecker failed"
        Nothing -> error "Typechecker failed"

getMethod :: String -> String -> CM (TType, String)
getMethod className mthName = do
    clss <- asks clss
    case Map.lookup className clss of
        Just (_, methods, extends) -> case Map.lookup mthName methods of
            Just t -> return (t, methodName className mthName)
            Nothing -> let Just ext = extends in getMethod ext mthName
        Nothing -> error "Typechecker failed"

writeFields :: Extends -> [Field] -> ClassEnv
writeFields ext =
    foldl
        ( \(attrs, methods, ext) field -> case field of
            (AttrDef _ t x) -> (Map.insert (takeStr x) (takeType t, Map.size attrs) attrs, methods, ext)
            (MethodDef _ t x _ _) -> (attrs, Map.insert (takeStr x) (takeType t) methods, ext)
        )
        (Map.empty, Map.empty, ext)

myUnion :: Attributes -> Attributes -> Attributes
myUnion m1 m2 = Map.fromList $ helper (Map.toList m1) (Map.toList $ Map.difference m2 m1) (Map.size m1)
  where
    helper :: [(String, (TType, Int))] -> [(String, (TType, Int))] -> Int -> [(String, (TType, Int))]
    helper xs [] _ = xs
    helper xs ((y, (t, _)) : ys) c = helper (xs ++ [(y, (t, c))]) ys (c + 1)

addExtFields :: Reader -> Map.Map String ClassEnv
addExtFields r = Map.map go (clss r)
  where
    go :: ClassEnv -> ClassEnv
    go env1@(attrs, methods, ext) =
        maybe
            env1
            ( \name -> case Map.lookup name (clss r) of
                Just env2 ->
                    let (atr, _, _) = go env2
                     in (myUnion atr attrs, methods, ext)
                Nothing -> error "impossible case"
            )
            ext

writeTopDefs :: [TopDef] -> Reader
writeTopDefs tds = do
    let res =
            foldl
                ( \acc td -> case td of
                    (FnDef _ t x _ _) -> acc {func = Map.insert (takeStr x) (takeType t) (func acc)}
                    (ClassDef _ x fields) -> acc {clss = Map.insert (takeStr x) (writeFields Nothing fields) (clss acc)}
                    (ClassExt _ x y fields) -> acc {clss = Map.insert (takeStr x) (writeFields (Just $ takeStr y) fields) (clss acc)}
                )
                initReader
                tds
    res {clss = addExtFields res}

initState :: State
initState =
    St
        { nextlab = 0
        , newloc = 0
        , env = Map.empty
        , rodata = Map.empty
        }

defaultVal :: TType -> CM String
defaultVal t = case t of
    TStr -> getLocStr ""
    _ -> return "0"

getFunType :: String -> CM TType
getFunType f = do
    func <- asks func
    case Map.lookup f func of
        Just t -> return t
        Nothing -> error "Typechecker failed"

nextLabel :: CM String
nextLabel = do
    l <- gets nextlab
    modify (\st -> st {nextlab = l + 1})
    return $ ".L" ++ show l

newLoc :: CM String
newLoc = do
    modify (\st -> st {newloc = (newloc st) + 1})
    l <- gets newloc
    return $ "QWORD [rbp - " ++ show (8 * l) ++ "]"

writeLoc :: String -> TType -> String -> CM ()
writeLoc x t l = modify (\st -> st {env = Map.insert x (t, l) (env st)})

getLoc :: String -> CM (Maybe (TType, String))
getLoc x = do
    env <- gets env
    case Map.lookup x env of
        Just res -> return $ Just res
        Nothing -> return Nothing

newLocStr :: CM String
newLocStr = do
    l <- Map.size <$> gets rodata
    return $ "_strMsg" ++ show l

writeLocStr :: String -> String -> CM ()
writeLocStr s l = modify (\st -> st {rodata = Map.insert s l (rodata st)})

getLocStr :: String -> CM String
getLocStr s = do
    rdata <- gets rodata
    case Map.lookup s rdata of
        Just loc -> return loc
        Nothing -> do
            l <- newLocStr
            writeLocStr s l
            return l

calcLocSize :: Block -> Int
calcLocSize (Blck _ ss) = sum $ map calcDeclSize ss

calcDeclSize :: Stmt -> Int
calcDeclSize (If _ _ s) = calcDeclSize s
calcDeclSize (IfElse _ _ s1 s2) = calcDeclSize s1 + calcDeclSize s2
calcDeclSize (While _ _ s) = calcDeclSize s
calcDeclSize (BStmt _ blk) = calcLocSize blk
calcDeclSize (Decl _ _ its) = 8 * length its
calcDeclSize (For _ _ _ _ s) = 8 + calcDeclSize s
calcDeclSize _ = 0

takeRelOp :: RelOp -> String
takeRelOp (LTH _) = "l"
takeRelOp (LE _) = "le"
takeRelOp (GTH _) = "g"
takeRelOp (GE _) = "ge"
takeRelOp (EQU _) = "e"
takeRelOp (NE _) = "ne"

addInstr :: [AsmInstr] -> CM ()
addInstr ins = tell ins

addAddOp :: AddOp -> String -> String -> CM ()
addAddOp (Plus _) s1 s2 = addInstr [AsmAdd s1 s2]
addAddOp (Minus _) s1 s2 = addInstr [AsmSub s1 s2]

addMulOp :: MulOp -> String -> String -> CM ()
addMulOp (Times _) s1 s2 = addInstr [AsmMul s1 s2]
addMulOp (Div _) _ s2 = addInstr [AsmCqo, AsmDiv s2]
addMulOp (Mod _) _ s2 = addInstr [AsmCqo, AsmDiv s2, AsmMov "rax" "rdx"]

addPrologue :: String -> CM ()
addPrologue x =
    addInstr
        [ AsmLabel x
        , AsmPush "rbp"
        , AsmMov "rbp" "rsp"
        ]

addEpilogue :: CM ()
addEpilogue =
    addInstr
        [ AsmLabel ".end"
        , AsmMov "rsp" "rbp"
        , AsmPop "rbp"
        , AsmRet
        ]

addExterns :: CM ()
addExterns =
    addInstr
        [ AsmSection ".text"
        , AsmGlobal "main"
        , AsmExtern "printInt"
        , AsmExtern "printString"
        , AsmExtern "error"
        , AsmExtern "readInt"
        , AsmExtern "readString"
        , AsmExtern "__concatString"
        , AsmExtern "__allocArray"
        , AsmExtern "__allocClass"
        ]

addRodata :: CM ()
addRodata = do
    rodata <- gets rodata
    case Map.toList rodata of
        [] -> return ()
        strs -> do
            addInstr [AsmSection ".rodata"]
            mapM_ (\(str, loc) -> addInstr [AsmData loc str]) strs
