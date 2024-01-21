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
        offset = reverse $ take (length stackExpr) [16, 24 ..]
        mem = map (\off -> "QWORD [rbp + " ++ show off ++ "]") offset

    mapM_ (\(x, t) -> newLoc >>= writeLoc x t) regArgs
    mapM_ (\((x, t), l) -> writeLoc x t l) (zip stackExpr mem)

    addInstr $ zipWith (\reg _ -> AsmPush reg) regs regArgs

compileFunction :: String -> [(String, TType)] -> Block -> CM ()
compileFunction name args ss = do
    let locsize = calcLocSize ss
    addPrologue name
    modify (\st -> st {rspOff = 0})
    compileArgs args
    addInstr [AsmSub "rsp" (show locsize)]
    compileBlock ss
    addEpilogue
    modify (\st -> st {newloc = 0})

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef _ _ x args ss) = compileFunction (takeStr x) (parseArgs args) ss
compileTopDef (ClassDef _ name fields) =
    mapM_
        ( \field -> case field of
            (AttrDef _ _ _) -> return ()
            (MethodDef _ _ x args ss) ->
                compileFunction
                    (methodName (takeStr name) (takeStr x))
                    ([("self", TClass (takeStr name))] ++ parseArgs args)
                    ss
        )
        fields
compileTopDef (ClassExt p x _ fields) = compileTopDef (ClassDef p x fields)

compileBlock :: Block -> CM Bool
compileBlock (Blck _ stmts) = do
    env <- gets env
    res <- helper stmts
    modify (\st -> st {env = env})
    return res
  where
    helper :: [Stmt] -> CM Bool
    helper [] = return False
    helper (s : ss) = do
        ret <- compileStmt s
        if ret
            then return True
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

compileBexprIf :: Expr -> String -> CM ()
compileBexprIf e label1 = case e of
    (EOr _ e1 e2) -> do
        label2 <- nextLabel
        compileExp e1
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "ne" label2
            ]
        compileExp e2
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            , AsmLabel label2
            ]
    (EAnd _ e1 e2) -> do
        compileExp e1
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]
        compileExp e2
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]
    (ERel _ e1 op e2) -> do
        compileExp e2
        addInstr [AsmPush "rax"]
        compileExp e1
        addInstr
            [ AsmPop "r10"
            , AsmCmp "rax" "r10"
            , AsmJmpRel (negRelOp op) label1
            ]
    _ -> do
        compileExp e
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]

compileBExprIfElse :: Expr -> String -> String -> CM ()
compileBExprIfElse e label1 label2 = case e of
    (EOr _ e1 e2) -> do
        label3 <- nextLabel
        compileExp e1
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "ne" label3
            ]
        compileExp e2
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            , AsmLabel label3
            ]
    (EAnd _ e1 e2) -> do
        compileExp e1
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]
        compileExp e2
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]
    (ERel _ e1 op e2) -> do
        compileExp e2
        addInstr [AsmPush "rax"]
        compileExp e1
        addInstr
            [ AsmPop "r10"
            , AsmCmp "rax" "r10"
            , AsmJmpRel (negRelOp op) label1
            ]
    _ -> do
        compileExp e
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            ]

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
            addInstr
                [ AsmPush "rax"
                , AsmMov "rax" l
                , AsmPop "r10"
                , AsmMov ("[rax + 8 * " ++ show offset ++ "]") "r10"
                ]
    return False
compileStmt (Ass _ (EAttr _ e1 x) e2) = do
    compileExp e2
    addInstr [AsmPush "rax"]
    t <- compileExp e1
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr
        [ AsmPop "r10"
        , AsmMov ("[rax + 8 * " ++ show offset ++ "]") "r10"
        ]
    return False
compileStmt (Ass _ (EElem _ e1 e2) e3) = do
    compileExp e3
    addInstr [AsmPush "rax"]
    compileExp e2
    addInstr [AsmPush "rax"]
    compileExp e1
    addInstr
        [ AsmPop "r10"
        , AsmPop "r11"
        , AsmMov "[rax + 8 + 8 * r10]" "r11"
        ]
    return False
compileStmt (Ass _ _ _) = error "Typechecker failed"
compileStmt (Incr _ (EVar _ x)) = do
    res <- getLoc (takeStr x)
    case res of
        Just (_, l) -> addInstr [AsmInc l]
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (_, offset) <- getAttr name (takeStr x)
            addInstr
                [ AsmMov "rax" l
                , AsmInc ("QWORD [rax + 8 * " ++ show offset ++ "]")
                , AsmMov l "rax"
                ]
    return False
compileStmt (Incr _ (EAttr _ e x)) = do
    t <- compileExp e
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr [AsmInc ("QWORD [rax + 8 * " ++ show offset ++ "]")]
    return False
compileStmt (Incr _ (EElem _ e1 e2)) = do
    case simplifyExpr e2 of
        Right (SInt n) -> do
            compileExp e1
            addInstr [AsmInc ("QWORD [rax + 8 + 8 * " ++ show n ++ "]")]
        _ -> do
            compileExp e2
            addInstr [AsmPush "rax"]
            compileExp e1
            addInstr
                [ AsmPop "r10"
                , AsmInc "QWORD [rax + 8 + 8 * r10]"
                ]
    return False
compileStmt (Decr _ (EVar _ x)) = do
    res <- getLoc (takeStr x)
    case res of
        Just (_, l) -> addInstr [AsmDec l]
        Nothing -> do
            res <- getLoc "self"
            let Just (TClass name, l) = res
            (_, offset) <- getAttr name (takeStr x)
            addInstr
                [ AsmMov "rax" l
                , AsmDec ("QWORD [rax + 8 * " ++ show offset ++ "]")
                , AsmMov l "rax"
                ]
    return False
compileStmt (Decr _ (EAttr _ e x)) = do
    t <- compileExp e
    let TClass name = t
    (_, offset) <- getAttr name (takeStr x)
    addInstr [AsmDec ("QWORD [rax + 8 * " ++ show offset ++ "]")]
    return False
compileStmt (Decr _ (EElem _ e1 e2)) = do
    case simplifyExpr e2 of
        Right (SInt n) -> do
            compileExp e1
            addInstr [AsmDec ("QWORD [rax + 8 + 8 * " ++ show n ++ "]")]
        _ -> do
            compileExp e2
            addInstr [AsmPush "rax"]
            compileExp e1
            addInstr
                [ AsmPop "r10"
                , AsmDec "QWORD [rax + 8 + 8 * r10]"
                ]
    return False
compileStmt (Ret _ e) = do
    compileExp e
    addInstr [AsmJmp ".end"]
    return True
compileStmt (VRet _) = do
    addInstr [AsmMov "rax" "0", AsmJmp ".end"]
    return True
compileStmt (If _ e s) = case simplifyExpr e of
    Right (SBool b) -> if b then compileStmt s else return False
    _ -> do
        label <- nextLabel
        compileBexprIf e label
        compileStmt s
        addInstr [AsmLabel label]
        return False
compileStmt (IfElse _ e s1 s2) = case simplifyExpr e of
    Right (SBool b) -> if b then compileStmt s1 else compileStmt s2
    _ -> do
        label1 <- nextLabel
        label2 <- nextLabel
        compileBExprIfElse e label1 label2
        ret1 <- compileStmt s1
        addInstr
            [ AsmJmp label2
            , AsmLabel label1
            ]
        ret2 <- compileStmt s2
        addInstr [AsmLabel label2]
        return $ ret1 && ret2
compileStmt (While _ e s) = do
    label1 <- nextLabel
    label2 <- nextLabel
    addInstr [AsmLabel label1]
    compileBexprIf e label2
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
    addInstr
        [ AsmMov "r10" "[rax]"
        , AsmCmp "r10" "[rsp]"
        , AsmJmpRel "e" label2
        , AsmInc "QWORD [rsp]"
        , AsmMov "r10" "[rsp]"
        , AsmMov "r10" "[rax + 8 * r10]"
        , AsmMov l "r10"
        ]
    compileStmt s
    addInstr
        [ AsmJmp label1
        , AsmLabel label2
        , AsmPop "r10"
        ]

    modify (\st -> st {env = env})
    return False
compileStmt (BStmt _ blk) = compileBlock blk
compileStmt (Decl _ ty its) = do
    mapM_ (compileItem ty) its
    return False

compileExp :: Expr -> CM TType
compileExp e@(EOr _ e1 e2) = case simplifyExpr e of
    Right (SBool b) -> do
        addInstr [AsmMov "rax" (if b then "1" else "0")]
        return TBool
    _ -> do
        label1 <- nextLabel
        label2 <- nextLabel
        compileExp e1
        addInstr
            [ AsmTest "rax" "rax"
            , AsmJmpRel "e" label1
            , AsmMov "rax" "1"
            , AsmJmp label2
            , AsmLabel label1
            ]
        compileExp e2
        addInstr [AsmLabel label2]
        return TBool
compileExp e@(EAnd _ e1 e2) = case simplifyExpr e of
    Right (SBool b) -> do
        addInstr [AsmMov "rax" (if b then "1" else "0")]
        return TBool
    _ -> do
        label1 <- nextLabel
        label2 <- nextLabel
        compileExp e1
        addInstr [AsmTest "rax" "rax", AsmJmpRel "e" label1]
        compileExp e2
        addInstr
            [ AsmJmp label2
            , AsmLabel label1
            , AsmMov "rax" "0"
            , AsmLabel label2
            ]
        return TBool
compileExp e@(ERel _ e1 op e2) = case simplifyExpr e of
    Right (SBool b) -> do
        addInstr [AsmMov "rax" (if b then "1" else "0")]
        return TBool
    _ -> do
        compileExp e2
        addInstr [AsmPush "rax"]
        compileExp e1
        addInstr
            [ AsmPop "r10"
            , AsmCmp "rax" "r10"
            , AsmSet (takeRelOp op) "al"
            , AsmMovzx "rax" "al"
            ]
        return TBool
compileExp e@(EAdd _ e1 op e2) = case simplifyExpr e of
    Right (SInt n) -> do
        addInstr [AsmMov "rax" (show n)]
        return TInt
    Right (SStr s) -> do
        l <- getLocStr s
        addInstr [AsmMov "rax" l]
        return TStr
    _ -> do
        t2 <- compileExp e2
        addInstr [AsmPush "rax"]
        t1 <- compileExp e1
        addInstr [AsmPop "r10"]
        case (t1, t2) of
            (TInt, TInt) -> addAddOp op "rax" "r10"
            (TStr, TStr) ->
                addInstr
                    [ AsmMov "rdi" "rax"
                    , AsmMov "rsi" "r10"
                    , AsmCall "__concatString"
                    ]
            _ -> error "Typechecker failed"
        return t1
compileExp e@(EMul _ e1 op e2) = case simplifyExpr e of
    Right (SInt n) -> do
        addInstr [AsmMov "rax" (show n)]
        return TInt
    _ -> do
        compileExp e2
        addInstr [AsmPush "rax"]
        compileExp e1
        addInstr [AsmPop "r10"]
        addMulOp op "rax" "r10"
        return TInt
compileExp e@(Not _ e1) = case simplifyExpr e of
    Right (SBool b) -> do
        addInstr [AsmMov "rax" (if b then "1" else "0")]
        return TBool
    _ -> do
        compileExp e1
        addInstr [AsmXor "rax" "1"]
        return TBool
compileExp e@(Neg _ e1) = case simplifyExpr e of
    Right (SInt n) -> do
        addInstr [AsmMov "rax" (show n)]
        return TInt
    _ -> do
        compileExp e1
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
    case simplifyExpr e of
        Right (SInt n) -> addInstr [AsmMov "rdi" (show n)]
        _ -> compileExp e >> addInstr [AsmMov "rdi" "rax"]
    addInstr [AsmCall "__allocArray"]
    return $ TArr (takeType t)
compileExp (EElem _ e1 e2) = do
    case simplifyExpr e2 of
        Right (SInt n) -> addInstr [AsmPush (show n)]
        _ -> compileExp e2 >> addInstr [AsmPush "rax"]
    t <- compileExp e1
    let TArr ty = t
    addInstr [AsmPop "r10", AsmMov "rax" "[rax + 8 + 8 * r10]"]
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
            addInstr
                [ AsmMov "rax" l
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
    mapM
        ( \e -> do
            l <- newLoc
            compileExp e
            mapM (\reg -> addInstr [AsmPop reg]) (reverse regs)
            addInstr [AsmPush "rax"]
            mapM (\reg -> addInstr [AsmPush reg]) regs
        )
        stackExpr
    mapM (\reg -> addInstr [AsmPop reg]) (reverse $ take (length regExpr + self) regs)
    addInstr [AsmCall toCall]
    addInstr [AsmAdd "rsp" (show size)]

compile :: Program -> IO Builder
compile (Prog _ topDefs) = do
    let (_, _, code) = runRWS (compilePrg topDefs) (writeTopDefs topDefs) initState
    return . mconcat $ map (fromString . show) code
