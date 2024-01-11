module Backend.Generator where

data AsmInstr
    = AsmMov String String
    | AsmMovzx String String
    | AsmLea String String
    | AsmAdd String String
    | AsmSub String String
    | AsmMul String String
    | AsmDiv String
    | AsmNeg String
    | AsmXor String String
    | AsmPush String
    | AsmPop String
    | AsmCall String
    | AsmTest String String
    | AsmCmp String String
    | AsmSet String String
    | AsmJmpRel String String
    | AsmJmp String
    | AsmSection String
    | AsmData String String
    | AsmLabel String
    | AsmGlobal String
    | AsmExtern String
    | AsmCqo
    | AsmRet

instance Show AsmInstr where
    show (AsmMov s1 s2) = returnFmt $ "mov " ++ s1 ++ ", " ++ s2
    show (AsmMovzx s1 s2) = returnFmt $ "movzx " ++ s1 ++ ", " ++ s2
    show (AsmLea s1 s2) = returnFmt $ "lea " ++ s1 ++ ", " ++ s2
    show (AsmAdd s1 s2) = if s2 == "0" then "" else returnFmt $ "add " ++ s1 ++ ", " ++ s2
    show (AsmSub s1 s2) = if s2 == "0" then "" else returnFmt $ "sub " ++ s1 ++ ", " ++ s2
    show (AsmMul s1 s2) = returnFmt $ "imul " ++ s1 ++ ", " ++ s2
    show (AsmDiv s) = returnFmt $ "idiv " ++ s
    show (AsmNeg s) = returnFmt $ "neg " ++ s
    show (AsmXor s1 s2) = returnFmt $ "xor " ++ s1 ++ ", " ++ s2
    show (AsmPush s) = returnFmt $ "push " ++ s
    show (AsmPop s) = returnFmt $ "pop " ++ s
    show (AsmCall s) = returnFmt $ "call " ++ s
    show (AsmTest s1 s2) = returnFmt $ "test " ++ s1 ++ ", " ++ s2
    show (AsmCmp s1 s2) = returnFmt $ "cmp " ++ s1 ++ ", " ++ s2
    show (AsmSet op s) = returnFmt $ "set" ++ op ++ " " ++ s
    show (AsmJmpRel op s) = returnFmt $ "j" ++ op ++ " " ++ s
    show (AsmJmp s) = returnFmt $ "jmp " ++ s
    show (AsmSection s) = "section " ++ s ++ "\n"
    show (AsmData s1 s2) = s1 ++ " db \"" ++ s2 ++ "\", 0\n"
    show (AsmLabel s) = s ++ ":\n"
    show (AsmGlobal s) = returnFmt $ "global " ++ s
    show (AsmExtern s) = returnFmt $ "extern " ++ s
    show (AsmCqo) = returnFmt "cqo"
    show (AsmRet) = returnFmt "ret"

indent, nl :: String
indent = "    "
nl = "\n"

returnFmt :: String -> String
returnFmt s = indent ++ s ++ nl
