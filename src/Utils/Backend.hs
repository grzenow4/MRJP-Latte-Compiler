module Utils.Backend where

import Control.Monad.RWS
import qualified Data.Map as Map

import Backend.Generator
import Latte.AbsLatte
import Utils.Common

type Reader = Map.Map String TType
type Writer = [AsmInstr]
data State = St {
    nextlab :: Int,
    newloc :: Int,
    env :: Map.Map String (TType, String),
    rodata :: Map.Map String String
}

type CM a = RWS Reader Writer State a

predefFunc :: [(String, TType)]
predefFunc = [ ("printInt", TVoid)
             , ("printString", TVoid)
             , ("error", TVoid)
             , ("readInt", TInt)
             , ("readString", TStr)
             , ("__concatString", TStr)
             ]

initReader :: Reader
initReader = Map.fromList predefFunc

writeFunc :: [TopDef] -> Reader
writeFunc = foldl (\acc (FnDef _ t x _ _) -> Map.insert (takeStr x) (takeType t) acc) initReader

initState :: State
initState = St { nextlab = 0,
                 newloc = 0,
                 env = Map.empty,
                 rodata = Map.empty }

defaultVal :: TType -> CM String
defaultVal t = case t of
    TStr -> getLocStr ""
    _ -> return "0"

getFunType :: String -> CM TType
getFunType f = do
    func <- ask
    case Map.lookup f func of
        Just t -> return t
        Nothing -> error "Typechecker failed"

nextLabel :: CM String
nextLabel = do
    l <- gets nextlab
    modify (\st -> st { nextlab = l + 1 })
    return $ ".L" ++ show l

newLoc :: CM String
newLoc = do
    modify (\st -> st { newloc = (newloc st) + 1 })
    l <- gets newloc
    return $ "QWORD [rbp - " ++ show (8 * l) ++ "]"

writeLoc :: String -> TType -> String -> CM ()
writeLoc x t l = modify (\st -> st { env = Map.insert x (t, l) (env st) })

getLoc :: String -> CM (TType, String)
getLoc x = do
    env <- gets env
    case Map.lookup x env of
        Just res -> return res
        Nothing -> error "Typechecker failed"

newLocStr :: CM String
newLocStr = do
    l <- Map.size <$> gets rodata
    return $ "_strMsg" ++ show l

writeLocStr :: String -> String -> CM ()
writeLocStr s l = modify (\st -> st { rodata = Map.insert s l (rodata st) })

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
addMulOp (Div _) s1 s2 = addInstr [AsmCqo, AsmDiv "QWORD [rsp]"]
addMulOp (Mod _) s1 s2 = addInstr [AsmCqo, AsmDiv "QWORD [rsp]", AsmMov "rax" "rdx"]

addPrologue :: String -> CM ()
addPrologue x = addInstr [AsmLabel x, AsmPush "rbp", AsmMov "rbp" "rsp"]

addEpilogue :: CM ()
addEpilogue = addInstr [AsmLabel ".end", AsmMov "rsp" "rbp", AsmPop "rbp", AsmRet]

addRodata :: CM ()
addRodata = do
    rodata <- gets rodata
    case Map.toList rodata of
        [] -> return ()
        strs -> do
            addInstr [AsmSection ".rodata"]
            mapM_ (\(str, loc) -> addInstr [AsmData loc str]) strs
