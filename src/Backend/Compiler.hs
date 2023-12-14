module Backend.Compiler where

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Text.Lazy.Builder (Builder, fromString)

import Backend.Generator
import Latte.AbsLatte
import Utils.Backend
import Utils.Common

compilePrg :: [TopDef] -> CM ()
compilePrg tds = mapM_ compileTopDef tds >> addRodata

compileArgs :: [Arg] -> CM ()
compileArgs args = do
    let regArgs = take 6 args
    let stackExpr = drop 6 args
    let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
    let offset = reverse $ take (length stackExpr) [16, 24..]
    let mem = map (\off -> "QWORD [rbp + " ++ show off ++ "]") offset

    let pushRegs = zipWith (\reg _ -> AsmPush reg) regs regArgs
    mapM_ (\(Ar _ t x) -> newLoc >>= writeLoc (takeStr x) (takeType t)) regArgs
    mapM_ (\((Ar _ t x), l) -> writeLoc (takeStr x) (takeType t) l) (zip stackExpr mem)

    addInstr pushRegs

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef _ t x args ss) = do
    let locsize = calcLocSize ss

    addPrologue (takeStr x)
    compileArgs args
    addInstr [AsmSub "rsp" (show locsize)]
    compileBlock ss
    addEpilogue

    modify (\st -> st { newloc = 0 })
compileTopDef (ClassDef _ _ _) = error "Todo"
compileTopDef (ClassExt _ _ _ _) = error "Todo"

compileBlock :: Block -> CM Bool
compileBlock (Blck _ stmts) = do
    env <- gets env
    res <- helper stmts
    modify (\st -> st { env = env })
    return res where
        helper :: [Stmt] -> CM Bool
        helper [] = return False
        helper (s:ss) = do
            ret <- compileStmt s
            if ret then return True
            else helper ss

compileItem :: Type -> Item -> CM ()
compileItem ty it = do
    let t = takeType ty
    l <- newLoc
    case it of
        NoInit _ _ -> do
            val <- defaultVal t
            addInstr [AsmMov l val]
        Init _ _ e -> do
            compileExp e
            addInstr [AsmMov l "rax"]
    writeLoc (itemStr it) t l

compileStmt :: Stmt -> CM Bool
compileStmt (Empty _) = return False
compileStmt (Exp _ e) = compileExp e >> return False
compileStmt (Ass _ e1 e2) = case e1 of
    (EVar _ x) -> do
        (_, l) <- getLoc (takeStr x)
        compileExp e2
        addInstr [AsmMov l "rax"]
        return False
    _ -> error "Typechecker failed"
compileStmt (Incr _ e) = case e of
    (EVar _ x) -> do
        (_, l) <- getLoc (takeStr x)
        addInstr [ AsmMov "rax" l
                 , AsmAdd "rax" "1"
                 , AsmMov l "rax"
                 ]
        return False
    _ -> error "Typechecker failed"
compileStmt (Decr _ e) = case e of
    (EVar _ x) -> do
        (_, l) <- getLoc (takeStr x)
        addInstr [ AsmMov "rax" l
                 , AsmSub "rax" "1"
                 , AsmMov l "rax"
                 ]
        return False
    _ -> error "Typechecker failed"
compileStmt (Ret _ e) = do
    compileExp e
    addInstr [AsmJmp ".end"]
    return True
compileStmt (VRet _) = do
    addInstr [AsmMov "rax" "0", AsmJmp ".end"]
    return True
compileStmt (If _ e s) = do
    label <- nextLabel
    compileExp e
    addInstr [ AsmTest "rax" "rax"
             , AsmJe label
             ]
    compileStmt s
    addInstr [AsmLabel label]
    return False
compileStmt (IfElse _ e s1 s2) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compileExp e
    addInstr [ AsmTest "rax" "rax"
             , AsmJe label1
             ]
    ret1 <- compileStmt s1
    addInstr [AsmJmp label2, AsmLabel label1]
    ret2 <- compileStmt s2
    addInstr [AsmLabel label2]
    return $ ret1 && ret2
compileStmt (While _ e s) = do
    label1 <- nextLabel
    label2 <- nextLabel
    addInstr [AsmLabel label1]
    compileExp e
    addInstr [ AsmTest "rax" "rax"
             , AsmJe label2
             ]
    compileStmt s
    addInstr [ AsmJmp label1
             , AsmLabel label2
             ]
    return False
compileStmt (For _ _ _ _ _) = error "Todo"
compileStmt (BStmt _ blk) = compileBlock blk
compileStmt (Decl _ ty its) = do
    mapM_ (compileItem ty) its
    return False

compileExp :: Expr -> CM TType
compileExp (EOr _ e1 e2) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compileExp e1
    addInstr [ AsmTest "rax" "rax"
             , AsmJe label1
             , AsmMov "rax" "1"
             , AsmJmp label2
             , AsmLabel label1
             ]
    compileExp e2
    addInstr [AsmLabel label2]
    return TBool
compileExp (EAnd _ e1 e2) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compileExp e1
    addInstr [ AsmTest "rax" "rax"
             , AsmJe label1
             ]
    compileExp e2
    addInstr [ AsmJmp label2
             , AsmLabel label1
             , AsmMov "rax" "0"
             , AsmLabel label2
             ]
    return TBool
compileExp (ERel _ e1 op e2) = do
    compileExp e1
    addInstr [AsmPush "rax"]
    compileExp e2
    addInstr [ AsmMov "r10" "rax"
             , AsmXor "rax" "rax"
             , AsmCmp "QWORD [rsp]" "r10"
             , AsmSet (takeRelOp op) "al"
             ]
    return TBool
compileExp (EAdd _ e1 op e2) = do
    t1 <- compileExp e2
    addInstr [AsmPush "rax"]
    t2 <- compileExp e1
    case (t1, t2) of
        (TInt, TInt) -> addAddOp op "rax" "QWORD [rsp]"
        (TStr, TStr) -> addInstr [ AsmMov "rdi" "rax"
                                 , AsmMov "rsi" "QWORD [rsp]"
                                 , AsmCall "__concatString"
                                 ]
        _ -> error "Typechecker failed"
    addInstr [AsmPop "r10"]
    return t1
compileExp (EMul _ e1 op e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addMulOp op "rax" "QWORD [rsp]"
    addInstr [AsmPop "r10"]
    return TInt
compileExp (Not _ e) = do
    compileExp e
    addInstr [AsmXor "rax" "1"]
    return TBool
compileExp (Neg _ e) = do
    compileExp e
    addInstr [AsmNeg "rax"]
    return TInt
compileExp (EClass _ _) = error "Todo"
compileExp (EAttr _ _ _) = error "Todo"
compileExp (EMethod _ _ _ _) = error "Todo"
compileExp (ENull _ _) = error "Todo"
compileExp (EArr _ _ _) = error "Todo"
compileExp (EElem _ _ _) = error "Todo"
compileExp (EVar _ x) = do
    (t, loc) <- getLoc (takeStr x)
    addInstr [AsmMov "rax" loc]
    return t
compileExp (EInt _ n) = do
    addInstr [AsmMov "rax" (show n)]
    return TInt
compileExp (EString _ s) = do
    loc <- getLocStr s
    addInstr [AsmMov "rax" loc]
    return TStr
compileExp (ETrue _) = do
    addInstr [AsmMov "rax" "1"]
    return TBool
compileExp (EFalse _) = do
    addInstr [AsmMov "rax" "0"]
    return TBool
compileExp (EApp _ x es) = do
    let regExpr = take 6 es
    let stackExpr = drop 6 es
    let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

    let size = 8 * length stackExpr
    l <- gets newloc
    let align = 8 * ((l + length stackExpr) `mod` 2)

    mapM (\(e, reg) -> do compileExp e >> addInstr [AsmMov reg "rax"]) (zip regExpr regs)
    addInstr [AsmSub "rsp" (show $ size + align)]
    mapM (\e -> do
        l <- newLoc
        compileExp e
        addInstr [AsmMov l "rax"]) stackExpr
    addInstr [AsmCall (takeStr x)]
    addInstr [AsmAdd "rsp" (show $ size + align)]

    retType <- getFunType (takeStr x)
    return retType

compile :: Program -> IO Builder
compile (Prog _ topDefs) = do
    let (_, _, code) = runRWS (compilePrg topDefs) (writeFunc topDefs) initState
    return $ mconcat
        [ fromString "section .text\n"
        , fromString "    global main\n\n"
        , fromString "    extern printInt\n"
        , fromString "    extern printString\n"
        , fromString "    extern error\n"
        , fromString "    extern readInt\n"
        , fromString "    extern readString\n"
        , fromString "    extern __concatString\n\n"
        , mconcat $ map (fromString . show) code
        ]
