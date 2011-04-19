module TypeChecker where

import Debug.Trace
import AbsJavalette
import PrintJavalette
import ErrM
import Control.Monad.State
import Data.List

type AnotTree = [TopDef]
type Errors = [String]
type Env = [[(Ident, Type)]]
type MyState = (Env, AnotTree, Errors)
type MyStateM = State MyState


typecheck :: Program -> Err Program
typecheck (Program fs) = do let (at, err) = evalState (checkFuncs fs >> checkProgram fs >> getAnotTree) emptyState
                            if err == []
                                then return (Program at)
                                else fail $ printTree at ++ (show err)

getAnotTree :: MyStateM (AnotTree, Errors)
getAnotTree = do (_, at, e) <- get
                 return (at, e)

checkFuncs :: [TopDef] -> MyStateM ()
checkFuncs [] = return ()
checkFuncs (f@(FnDef t i a (Block s)):fs) = do (env, at, err) <- get
                                               case find (\(ident, _) -> ident == i) (concat env) of
                                                 -- TODO: check if args already in scope
                                                 Nothing  -> do let args = [t | (Arg t _) <- a]
                                                                let newEnv = [(i, (Fun t args))]:env
                                                                put (newEnv, at, err)
                                                                checkFuncs fs
                                                 Just _   -> do addErr (show i ++ " already in scope")
                                                                checkFuncs fs

                                             
checkProgram :: [TopDef] -> MyStateM ()
checkProgram [] = return ()
checkProgram (f:fs) = do checkFun f
                         checkProgram fs

checkFun :: TopDef -> MyStateM ()
checkFun (FnDef t i a (Block s)) = do pushFunScope a
                                      ns <- checkStmts s [] t
                                      popFunScope
                                      b <- addArgs a []
                                      (env, at, err) <- get
                                      let newAt = at ++ [(FnDef t i b (Block ns))]
                                      put (env, newAt, err)
                                      return ()
                            
checkStmts :: [Stmt] -> [Stmt] -> Type -> MyStateM [Stmt]
checkStmts [] nss rt = return nss
checkStmts (s:ss) nss rt = do ns <- checkStmt s rt
                              checkStmts ss (nss ++ [ns]) rt

checkStmt :: Stmt -> Type -> MyStateM Stmt
checkStmt s rt = case s of

    BStmt (Block s)     -> do pushBlockScope
                              ns <- checkStmts s [] rt
                              popBlockScope
                              return (BStmt (Block ns))    
  
    Decl t vars         -> do nvars <- addVars t vars []
                              return (Decl t nvars)

    Ass i e             -> do vt <- findVarType i
                              (TAnot et ne) <- infer e
                              case vt of
                                Just t  -> case t == et of
                                            True  -> return (Ass i (TAnot et ne))
                                            False -> do addErr (show e ++ " is not of the type " ++ 
                                                                show t ++ " (" ++ show i ++ ")")
                                                        return (Ass i (TAnot et ne))
                                Nothing -> return (Ass i e)
                                

    Incr i              -> do m <- findVarType i
                              case m of
                                Just Int    -> return (Incr i)
                                Just Doub   -> return (Incr i)
                                Just t      -> do addErr ("(" ++ show i ++ 
                                                          ") incrementing is only allowed on Int " ++
                                                          "and Doub, not on " ++ 
                                                          show t)
                                                  return (Incr i)
                                Nothing     -> do addErr ("(" ++ show i ++ ") incrementing is only " ++
                                                          "allowed on Int and Doub")
                                                  return (Incr i)
  
    Decr i              -> do m <- findVarType i
                              case m of
                                Just Int    -> return (Decr i)
                                Just Doub   -> return (Decr i)
                                Just t      -> do addErr ("(" ++ show i ++ 
                                                          ") decrementing is only allowed on Int " ++
                                                          "and Doub, not on " ++ 
                                                          show t)
                                                  return (Decr i)
                                Nothing     -> do addErr ("(" ++ show i ++ ") decrementing is only " ++
                                                          "allowed on Int and Doub")
                                                  return (Decr i)

    Ret e               -> do m <- infer e
                              case m of
                                  (TAnot t ne) -> case t == rt of
                                                    True  -> return (Ret m)
                                                    False -> do addErr (show "wrong return type " ++ show t ++
                                                                        " should be " ++ show rt)
                                                                return (Ret m)
                                  _            -> do addErr (show e ++ " return not annotated")
                                                     return (Ret m)
                              -- TODO check if it returns the right type

    VRet                -> return VRet
    
    Cond e stm          -> do m <- infer e
                              ns <- checkStmt stm rt
                              return (Cond m ns)
                              
    CondElse e s1 s2    -> do m <- infer e
                              ns1 <- checkStmt s1 rt
                              ns2 <- checkStmt s2 rt
                              return (CondElse m ns1 ns2)
                              
    While e stm         -> do m <- infer e
                              ns <- checkStmt stm rt
                              return (While m ns)
                              
    SExp e              -> do m <- infer e
                              return (SExp m)
    s                   -> do addErr "Not an valid statement"
                              return s

addArgs :: [Arg] -> [Arg] -> MyStateM ([Arg])
addArgs [] nass = return (nass)
addArgs ((Arg t i):ass) nass = do ((e:env), at, err) <- get
                                  case (find (\(ident, nt) -> ident == i) e) of
                                    Nothing    -> do let newEnv = ((i, t):e):env
                                                     put(trace (show newEnv) newEnv, at, err)
                                                     addArgs ass ((Arg t i):nass)
                                    Just ident -> do addErr (show ident ++ " already in scope")
                                                     addArgs ass ((Arg t i):nass)

                              
addVars :: Type -> [Item] -> [Item] -> MyStateM ([Item])
addVars t [] nis = return (nis)
addVars t ((NoInit i):is) nis = do ((e:env), at, err) <- get
                                   case (find (\(ident, nt) -> ident == i) e) of
                                     Nothing    -> do let newEnv = ((i, t):e):env
                                                      put (newEnv, at, err)
                                                      addVars t is (nis ++ [(NoInit i)])
                                     Just ident -> do addErr (show ident ++ " already initialized")
                                                      addVars t is (nis ++ [(NoInit i)])
addVars t ((Init i ex):is) nis = do (TAnot nt ne) <- infer ex
                                    case t == nt of
                                      True   -> do ((e:env), at, err) <- get
                                                   case (find (\(ident, nt) -> ident == i) e) of
                                                      Nothing    -> do let newEnv = ((i, t):e):env
                                                                       put (newEnv, at, err)
                                                                       addVars t is (nis ++ [(Init i (TAnot nt ne))])
                                                      Just ident -> do addErr (show ident ++ " already initialized")
                                                                       addVars t is (nis ++ [(Init i (TAnot nt ne))])
                                      False  -> do addErr (show ex ++ " is not of type " ++ show t)
                                                   ((e:env), at, err) <- get
                                                   let newEnv = ((i, t):e):env
                                                   put (newEnv, at, err)
                                                   addVars t is (nis ++ [(Init i ex)])
                                   
          
                              
infer :: Expr -> MyStateM Expr
infer expr = case expr of
    EVar i          -> do m <- findVarType i
                          case m of
                              Just t    -> return (TAnot t (EVar i))
                              Nothing   -> return (EVar i)

    ELitInt e       -> return (TAnot Int (ELitInt e))
    ELitDoub e      -> return (TAnot Doub (ELitDoub e))
    ELitTrue        -> return (TAnot Bool ELitTrue)
    ELitFalse       -> return (TAnot Bool ELitFalse)
    EApp i exs      -> do m <- findVarType i
                          nexs <- inferList exs []
                          case m of
                              Just (Fun t a)    -> case ((length nexs) == (length a)) of
                                                        True    -> case (and [t1 == t2 | ((TAnot t1 _),t2) <- zip nexs a]) of
                                                                        True   -> return (TAnot t (EApp i nexs))
                                                                        False  -> do addErr (show i ++ ":s arguments (" ++
                                                                                             show nexs ++ ") are not equal to " ++
                                                                                             show a)
                                                                                     return (TAnot t (EApp i nexs))
                                                        False   -> case i of
                                                                    Ident "printString" -> case exs of
                                                                                            [EString _] -> return (EApp i exs)
                                                                                            _           -> do addErr ("printString only takes one literal string as a argument")
                                                                                                              return (EApp i exs)
                                                                    _                   -> do addErr ("wrong number of arguments for " ++ show i) 
                                                                                              return (TAnot t (EApp i nexs))
                              
                              Nothing           -> do addErr ("wrong arguments in " ++ show i)
                                                      return (EApp i exs) -- TODO: check for Nothing or other
                              
                          
    EString e       -> return (EString e)
    Neg e           -> do m <- infer e
                          case m of
                              (TAnot Int _)     -> return (TAnot Int (Neg m))
                              (TAnot Doub _)    -> return (TAnot Doub (Neg m))
                              _                 -> do addErr (show e ++ " is not of type Int or Bool")
                                                      return (Neg e)
    Not e           -> do m <- infer e
                          case m of
                              (TAnot Bool _)    -> return (TAnot Bool (Not m))
                              _                 -> do addErr (show e ++ " is not of type Bool")
                                                      return (Not e)
                                                      
    EMul e1 op e2   -> do t <- findType e1 e2 [Int, Doub]
                          case t of
                              Just (Int, en1, en2)   -> return (TAnot Int (EMul en1 op en2))
                              Just (Doub, en1, en2)  -> return (TAnot Doub (EMul en1 op en2))
                              Nothing                -> return (EMul e1 op e2)
                              
    EAdd e1 op e2   -> do t <- findType e1 e2 [Int, Doub]
                          case t of
                              Just (Int, en1, en2)   -> return (TAnot Int (EAdd en1 op en2))
                              Just (Doub, en1, en2)  -> return (TAnot Doub (EAdd en1 op en2))
                              Nothing                -> return (EAdd e1 op e2)
                              
    ERel e1 op e2   -> case find (== op) [LTH,LE,GTH,GE] of
                         Just _   -> do t <- findType e1 e2 [Int, Doub]
                                        case t of
                                            Just (Int, en1, en2)   -> return (TAnot Bool (ERel en1 op en2))
                                            Just (Doub, en1, en2)  -> return (TAnot Bool (ERel en1 op en2))
                                            Nothing                -> return (ERel e1 op e2)

                         Nothing  -> do m <- findType e1 e2 [Int, Doub, Bool]
                                        case m of
                                            Just (_, en1, en2)  -> return (TAnot Bool (ERel en1 op en2))
                                            Nothing             -> return (ERel e1 op e2)
                                            
    EAnd e1 e2      -> do t <- findType e1 e2 [Bool]
                          case t of
                              Just (Bool, en1, en2) -> return (TAnot Bool (EAnd en1 en2))
                              Nothing               -> return (EAnd e1 e2)
                              
    EOr e1 e2       -> do t <- findType e1 e2 [Bool]
                          case t of
                              Just (Bool, en1, en2) -> return (TAnot Bool (EOr en1 en2))
                              Nothing               -> return (EOr e1 e2)
                              
inferList :: [Expr] -> [Expr] -> MyStateM [Expr]
inferList [] nes = return nes
inferList (e:es) nes = do ne <- infer e
                          inferList es (nes ++ [ne])

findType :: Expr -> Expr -> [Type] -> MyStateM (Maybe (Type, Expr, Expr))
findType e1 e2 allowed = do t1 <- infer e1
                            t2 <- infer e2
                            let (TAnot tt1 _) = t1
                            let (TAnot tt2 _) = t2
                            case tt1 == tt2 of
                              True  -> case t1 of
                                          (TAnot t _) -> case (find (== t) allowed) of
                                                            Just _  -> return (Just (t, t1, t2))
                                                            Nothing -> do addErr (show t ++ 
                                                                                  " is not allowed here")
                                                                          return Nothing
                                          _           -> return Nothing
                              False -> do addErr (show e1 ++ " and " ++ show e2 ++ " are not of type " ++ show t1)
                                          return Nothing
   
findVarType :: Ident -> MyStateM (Maybe Type)
findVarType var = do (env, at, err) <- get
                     let m = find (\(i, t) -> i == var) (concat env)
                     case m of
                         Just t     -> return (Just (snd t))
                         Nothing    -> do addErr ((show var) ++ " not in scope")
                                          return Nothing
                

-- initializing functions
emptyState :: MyState
emptyState = (emptyEnv, [], [])

emptyEnv :: Env
emptyEnv = [[
    (Ident "printString", (Fun Void [])),
    (Ident "printDouble", (Fun Void [Doub])),
    (Ident "printInt", (Fun Void [Int])),
    (Ident "readDouble", (Fun Doub [])),
    (Ident "readInt", (Fun Int []))
    ]]

-- helper functions
pushFunScope :: [Arg] -> MyStateM ()
pushFunScope a = do (env, at, err) <- get
                    let args = [(i,t) | (Arg t i) <- a]
                    put (args:env, at, err)
                    return ()

popFunScope :: MyStateM ()
popFunScope = do (env, at, err) <- get
                 put (tail env, at, err)
                 return ()
                 
pushBlockScope :: MyStateM ()
pushBlockScope = pushFunScope []

popBlockScope :: MyStateM ()
popBlockScope = popFunScope

addErr :: String -> MyStateM ()
addErr err = do (env, at, errs) <- get
                put (env, at, err:errs)
                return ()
                
