module Compiler where

import Debug.Trace
import AbsJavalette
import PrintJavalette
import ErrM
import Control.Monad.State
import Data.List

type Variables = [[(Ident, Int)]]
-- vars, stackCount, localCount, labelCount, finalCode, tempCode, classname
type MyState = (Variables, Int, Int, Int, String, String, String)
type MyStateM = State MyState


compile :: Program -> String -> String
compile (Program fs) classname =
  do let code = evalState (compileProgram fs >> getFinalCode) (emptyState classname)
     boilerPlate classname code

-- initializing functions
emptyState :: String -> MyState
emptyState classname = ([[]], 0, 0, "", "", classname)

boilerPlate :: String -> String -> String
boilerPlate classname code =
  ".class public " ++ classname ++ "\n" ++
  ".super java/lang/Object\n" ++
  ".method public <init>()V\n" ++
  "  aload_0\n" ++
  "  invokespecial java/lang/Object/<init>()V\n" ++
  "  return\n" ++
  ".end method\n\n" ++
  ".method public static main([Ljava/lang/String;)V\n" ++
  "  .limit locals 1\n" ++
  "  invokestatic " ++ classname ++ "/main()I\n" ++
  "  pop\n" ++
  "  return\n" ++
  ".end method\n\n" ++
  code


compileProgram :: [TopDef] -> MyStateM ()
compileProgram [] = return ()
compileProgram (f:fs) = 
  do compileFunc f
     compileProgram fs
     return ()
     
compileFunc :: TopDef -> MyStateM ()
compileFunc f@(FnDef t (Ident i) a (Block s)) =
  do compileStmts s
     addFinalCode f
     return ()
  
     
compileStmts :: [Stmt] -> MyStateM ()
compileStmts [] = return ()
compileStmts (s:ss) = do compileStmt s
                         compileStmts ss
                       
compileStmt :: Stmt -> MyStateM ()
compileStmt s = case s of
  SExp e            -> do compileExpr e
                          return ()
  Ret e             -> do compileExpr e
                          return ()
  VRet              -> do addTempCode ("  return\n")
                          return ()
  Cond e stm        -> do compileExpr e
                          addTempCode ("  ")
                          return ()
  _                 -> return ()

compileExpr :: Expr -> MyStateM ()
compileExpr expr = case expr of
  TAnot t (EApp (Ident i) exs) -> do ts <- compileList exs []
                                     let tss = intercalate "," (map (\a -> jType a) ts)
                                     ci <- getClassIdent i
                                     addTempCode ("  invokestatic " ++ ci ++ "(" ++ tss ++ ")" ++ (jType t) ++ "\n")
                                     addStackCount 1
                                     return ()
  EApp (Ident "printString") a -> do let EString s = head a
                                     addTempCode ("  ldc " ++ show s ++ "\n")
                                     addTempCode ("  invokestatic Runtime/printString(Ljava/lang/String;)V\n")
                                     return ()
  TAnot t (ELitInt i)           -> do case and [i >= 0, i <= 5] of
                                        True -> addTempCode ("  iconst_" ++ show i ++ "\n")
                                        False -> case i == -1 of
                                          True -> addTempCode ("  iconst_m1")
                                          False -> case and [i >= -128, i <= 127] of
                                                    True -> addTempCode ("  bipush " ++ show i ++ "\n")
                                                    False -> addTempCode ("  sipush " ++ show i ++ "\n")
                                      addStackCount 1
                                      return ()
  TAnot t (EMul e1 op e2)       -> do compileExpr e1
                                      compileExpr e2
                                      let ts = case t of
                                                Int -> "i"
                                                Doub -> "d"
                                      case op of
                                        Times  -> addTempCode ("  " ++ ts ++ "mul\n")
                                        Div    -> addTempCode ("  " ++ ts ++ "div\n")
                                        Mod    -> addTempCode ("  " ++ ts ++ "rem\n")

  TAnot t (EAdd e1 op e2)       -> do compileExpr e1
                                      compileExpr e2
                                      let ts = case t of
                                                Int -> "i"
                                                Doub -> "d"
                                      case op of
                                        Plus  -> addTempCode ("  " ++ ts ++ "add\n")
                                        Minus -> addTempCode ("  " ++ ts ++ "sub\n")
                                      return ()
  TAnot t (ELitDoub d)          -> do case d of
                                        0.0 -> addTempCode ("  dconst_0\n")
                                        1.0 -> addTempCode ("  dconst_1\n")
                                        _   -> addTempCode ("  ldc2_w " ++ show d ++ "\n")
                                      addStackCount 2
                                      return ()
  TAnot t (Neg e)               -> do compileExpr e
                                      case t of
                                        Int -> addTempCode "  ineg\n"
                                        Doub -> addTempCode "  dneg\n"

  e                             -> do addTempCode ("    ; " ++ (show e) ++ "\n")
                                      return ()
                              
compileList :: [Expr] -> [Type] -> MyStateM ([Type])
compileList [] ts = return (ts)
compileList (e2@(TAnot t e):es) ts = do compileExpr e2
                                        (compileList es (ts ++ [t]))

--
-- Helper functions
getFinalCode :: MyStateM (String)
getFinalCode = do (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
                  return final

addFinalCode :: TopDef -> MyStateM ()
addFinalCode (FnDef t (Ident i) a (Block s)) =
  do let p = intercalate "," (map (\(Arg t i) -> jType t) a)
     let rt = case t of
                Bool      -> "  ireturn\n"
                Int       -> "  ireturn\n"
                Doub      -> "  dreturn\n"
                Void      -> "  return\n"
     (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
     let newFinal = final ++
                    ".method public static " ++ i ++ "(" ++ p ++ ")" ++ jType t ++ "\n" ++
                    "  .limit locals " ++ show localCount ++ "\n" ++
                    "  .limit stack " ++ show stackCount ++ "\n" ++
                    temp ++
                    rt ++
                    ".end method\n\n"
     put ([], 0, 0, labelCount, newFinal, "", classname) -- state
     return ()
                   
jType :: Type -> String
jType t = case t of
  Int   -> "I"
  Doub  -> "D"
  Bool  -> "B"
  Void  -> "V"


addTempCode :: String -> MyStateM ()
addTempCode s = do (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
                   let newTemp = temp ++ s
                   put (vars, stackCount, localCount, labelCount, final, newTemp, classname) -- state
                   return ()
                     
addStackCount :: Int -> MyStateM ()
addStackCount i = 
  do (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
     put (vars, stackCount + i, localCount, labelCount, final, temp, classname) -- state
     return ()
     
getClassName :: MyStateM (String)
getClassName = do (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
                  return classname

incLabelCount :: MaStateM (Int)
incLabelCount = do (vars, stackCount, localCount, labelCount, final, temp, classname) <- get
                   put (vars, stackCount, localCount, labelCount + 1, final, temp, classname)
                   return labelCount
     
getClassIdent :: String -> MyStateM (String)
getClassIdent i =
  case find (== i) ["printString", "printDouble", "printInt", "readDouble", "readInt"] of
    Just _  -> return ("Runtime/" ++ i)
    Nothing -> do classname <- getClassName
                  return (classname ++ "/" ++ i)
                      
                  