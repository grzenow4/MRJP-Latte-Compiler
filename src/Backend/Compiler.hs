module Backend.Compiler where

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Text.Lazy.Builder (Builder, fromString)

import Backend.Generator
import Latte.AbsLatte
import Utils.Backend
import Utils.Common

compilePrg :: [TopDef] -> CM ()
compilePrg tds = addExterns >> mapM_ compileTopDef tds >> addRodata

parseArgs :: [Arg] -> [(String, TType)]
parseArgs = foldl (\acc (Ar _ t x) -> acc ++ [(takeStr x, takeType t)]) []

compileArgs :: [(String, TType)] -> CM ()
compileArgs args = do
    let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        regArgs = take 6 args
        stackExpr = drop 6 args
        offset = reverse $ take (length stackExpr) [16, 24..]
        mem = map (\off -> "QWORD [rbp + " ++ show off ++ "]") offset

    mapM_ (\(x, t) -> newLoc >>= writeLoc x t) regArgs
    mapM_ (\((x, t), l) -> writeLoc x t l) (zip stackExpr mem)

    addInstr $ zipWith (\reg _ -> AsmPush reg) regs regArgs

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef _ t x args ss) = do
    let locsize = calcLocSize ss
    addPrologue (takeStr x)
    compileArgs $ parseArgs args
    addInstr [AsmSub "rsp" (show locsize)]
    compileBlock ss
    addEpilogue
    modify (\st -> st { newloc = 0 })
compileTopDef (ClassDef _ name fields) = do
    mapM_ (\field -> case field of
        (AttrDef _ _ _) -> return ()
        (MethodDef _ _ x args ss) -> do
            let locsize = calcLocSize ss
            addPrologue $ methodName (takeStr name) (takeStr x)
            compileArgs $ [("self", TClass (takeStr name))] ++ parseArgs args
            addInstr [AsmSub "rsp" (show locsize)]
            compileBlock ss
            addEpilogue
            modify (\st -> st { newloc = 0 })) fields
compileTopDef (ClassExt p x y fields) = compileTopDef (ClassDef p x fields)

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
        Init _ _ e -> compileExp e >> addInstr [AsmMov l "rax"]
    writeLoc (itemStr it) t l

compileStmt :: Stmt -> CM Bool
compileStmt (Empty _) = return False
compileStmt (Exp _ e) = compileExp e >> return False
compileStmt (Ass _ (EVar _ x) e) = do
    compileExp e
    res <- getLoc (takeStr x)
    case res of
        Just (_, l) -> addInstr [AsmMov l "rax"]
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (_, offset) <- getAttr name (takeStr x)
            addInstr [ AsmPush "rax"
                     , AsmMov "rax" l
                     , AsmPop "rdx"
                     , AsmMov ("[rax + 8 * " ++ show offset ++ "]") "rdx"
                     ]
    return False
compileStmt (Ass _ (EAttr _ e1 x) e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    t <- compileExp e1
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr [AsmPop "rdx", AsmMov ("[rax + 8 * " ++ show offset ++ "]") "rdx"]
    return False
compileStmt (Ass _ (EElem _ e1 e2) e3) = do
    compileExp e3
    addInstr [AsmPush "rax"]
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr [ AsmPop "rdx"
             , AsmPop "r10"
             , AsmMov "[rax + 8 + 8 * rdx]" "r10"
             ]
    return False
compileStmt (Ass _ _ _) = error "Typechecker failed"
compileStmt (Incr _ (EVar _ x)) = do
    res <- getLoc (takeStr x)
    case res of
        Just (_, l) -> do
            addInstr [ AsmMov "rax" l
                     , AsmAdd "rax" "1"
                     , AsmMov l "rax"
                     ]
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (_, offset) <- getAttr name (takeStr x)
            addInstr [ AsmMov "rax" l
                     , AsmAdd ("QWORD [rax + 8 * " ++ show offset ++ "]") "1"
                     , AsmMov l "rax"
                     ]
    return False
compileStmt (Incr _ (EAttr _ e x)) = do
    t <- compileExp e
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr [AsmAdd ("QWORD [rax + 8 * " ++ show offset ++ "]") "1"]
    return False
compileStmt (Incr _ (EElem _ e1 e2)) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr [ AsmPop "rdx"
             , AsmAdd "QWORD [rax + 8 + 8 * rdx]" "1"
             ]
    return False
compileStmt (Decr _ (EVar _ x)) = do
    res <- getLoc (takeStr x)
    case res of
        Just (_, l) -> do
            addInstr [ AsmMov "rax" l
                    , AsmSub "rax" "1"
                    , AsmMov l "rax"
                    ]
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (_, offset) <- getAttr name (takeStr x)
            addInstr [ AsmMov "rax" l
                     , AsmSub ("QWORD [rax + 8 * " ++ show offset ++ "]") "1"
                     , AsmMov l "rax"
                     ]
    return False
compileStmt (Decr _ (EAttr _ e x)) = do
    t <- compileExp e
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr [AsmSub ("QWORD [rax + 8 * " ++ show offset ++ "]") "1"]
    return False
compileStmt (Decr _ (EElem _ e1 e2)) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr [ AsmPop "rdx"
             , AsmSub "QWORD [rax + 8 + 8 * rdx]" "1"
             ]
    return False
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
    addInstr [AsmTest "rax" "rax", AsmJmpRel "e" label]
    compileStmt s
    addInstr [AsmLabel label]
    return False
compileStmt (IfElse _ e s1 s2) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compileExp e
    addInstr [AsmTest "rax" "rax", AsmJmpRel "e" label1]
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
    addInstr [AsmTest "rax" "rax", AsmJmpRel "e" label2]
    compileStmt s
    addInstr [AsmJmp label1, AsmLabel label2]
    return False
compileStmt (For _ t x e s) = do
    label1 <- nextLabel
    label2 <- nextLabel
    env <- gets env
    l <- newLoc
    writeLoc (takeStr x) (takeType t) l

    addInstr [AsmPush "0", AsmLabel label1]
    compileExp e
    addInstr [ AsmMov "rbx" "[rax]"
             , AsmCmp "rbx" "[rsp]"
             , AsmJmpRel "e" label2
             , AsmAdd "QWORD [rsp]" "1"
             , AsmMov "rbx" "[rsp]"
             , AsmMov "rbx" "[rax + 8 * rbx]"
             , AsmMov l "rbx"
             ]
    compileStmt s
    addInstr [ AsmJmp label1
             , AsmLabel label2
             , AsmPop "rbx"
             ]

    modify (\st -> st { env = env })
    return False
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
             , AsmJmpRel "e" label1
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
    addInstr [AsmTest "rax" "rax", AsmJmpRel "e" label1]
    compileExp e2
    addInstr [ AsmJmp label2
             , AsmLabel label1
             , AsmMov "rax" "0"
             , AsmLabel label2
             ]
    return TBool
compileExp (ERel _ e1 op e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr [ AsmPop "rbx"
             , AsmCmp "rax" "rbx"
             , AsmSet (takeRelOp op) "al"
             , AsmMovzx "rax" "al"
             ]
    return TBool
compileExp (EAdd _ e1 op e2) = do
    t2 <- compileExp e2
    addInstr [AsmPush "rax"]
    t1 <- compileExp e1
    addInstr [AsmPop "rbx"]
    case (t1, t2) of
        (TInt, TInt) -> addAddOp op "rax" "rbx"
        (TStr, TStr) -> addInstr [ AsmMov "rdi" "rax"
                                 , AsmMov "rsi" "rbx"
                                 , AsmCall "__concatString"
                                 ]
        _ -> error "Typechecker failed"
    return t1
compileExp (EMul _ e1 op e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr [AsmPop "rbx"]
    addMulOp op "rax" "rbx"
    return TInt
compileExp (Not _ e) = do
    compileExp e
    addInstr [AsmXor "rax" "1"]
    return TBool
compileExp (Neg _ e) = do
    compileExp e
    addInstr [AsmNeg "rax"]
    return TInt
compileExp (EClass _ x) = do
    let name = takeStr x
    size <- getClassSize name
    addInstr [AsmMov "rdi" (show size), AsmCall "__allocClass"]
    return $ TClass name
compileExp (EAttr _ e x) = do
    let attr = takeStr x
    t <- compileExp e
    case t of
        TArr _ -> do
            addInstr [AsmMov "rax" "[rax]"]
            return TInt
        TClass name -> do
            (ty, offset) <- getAttr name attr
            addInstr [AsmMov "rax" ("[rax + 8 * " ++ show offset ++ "]")]
            return ty
        _ -> error "Typechecker failed"
compileExp (EMethod _ e x es) = do
    let mname = takeStr x
    t <- compileExp e
    addInstr [AsmPush "rax"]
    let TClass name = t
    (ty, toCall) <- getMethod name mname
    callFunction es toCall True
    return ty
compileExp (EArr _ t e) = do
    compileExp e
    addInstr [AsmMov "rdi" "rax", AsmCall "__allocArray"]
    return $ TArr (takeType t)
compileExp (EElem _ e1 e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    t <- compileExp e1
    let TArr ty = t
    addInstr [AsmPop "rdx", AsmMov "rax" "[rax + 8 + 8 * rdx]"]
    return ty
compileExp (ESelf _) = do
    res <- getLoc ("self")
    let Just (t, loc) = res
    addInstr [AsmMov "rax" loc] >> return t
compileExp (ENull _) = addInstr [AsmMov "rax" "0"] >> return TNull
compileExp (ENullCast _ t) = addInstr [AsmMov "rax" "0"] >> return (takeType t)
compileExp (ENullClss _ (EVar _ x)) = do
    addInstr [AsmMov "rax" "0"]
    return $ TClass (takeStr x)
compileExp (EVar _ x) = do
    res <- getLoc (takeStr x)
    case res of
        Just (t, loc) -> do
            addInstr [AsmMov "rax" loc]
            return t
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (t, offset) <- getAttr name (takeStr x)
            addInstr [ AsmMov "rax" l
                     , AsmMov "rax" ("[rax + 8 * " ++ show offset ++ "]")
                     ]
            return t
compileExp (EInt _ n) = addInstr [AsmMov "rax" (show n)] >> return TInt
compileExp (EString _ s) = do
    loc <- getLocStr s
    addInstr [AsmMov "rax" loc]
    return TStr
compileExp (ETrue _) = addInstr [AsmMov "rax" "1"] >> return TBool
compileExp (EFalse _) = addInstr [AsmMov "rax" "0"] >> return TBool
compileExp (EApp _ x es) = do
    let fname = takeStr x
    t <- getFunType fname
    callFunction es fname False
    return t

callFunction :: [Expr] -> String -> Bool -> CM ()
callFunction es toCall isMethod = do
    let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        self = if isMethod then 1 else 0
        regExpr = take (6 - self) es
        stackExpr = drop (6 - self) es
        size = 8 * length stackExpr

    mapM (\e -> compileExp e >> addInstr [AsmPush "rax"]) regExpr
    mapM (\e -> do
        l <- newLoc
        compileExp e
        mapM (\reg -> addInstr [AsmPop reg]) (reverse regs)
        addInstr [AsmPush "rax"]
        mapM (\reg -> addInstr [AsmPush reg]) regs
        ) stackExpr
    mapM (\reg -> addInstr [AsmPop reg]) (reverse $ take (length regExpr + self) regs)
    addInstr [AsmCall toCall]
    addInstr [AsmAdd "rsp" (show size)]

compile :: Program -> IO Builder
compile (Prog _ topDefs) = do
    let (_, _, code) = runRWS (compilePrg topDefs) (writeTopDefs topDefs) initState
    return . mconcat $ map (fromString . show) code
