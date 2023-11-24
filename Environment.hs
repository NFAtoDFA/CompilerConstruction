module Environment where
-- test
import System.Exit (exitFailure)

import AbsKOTLIN
import ErrM
import AbsKOTLIN (Field)
import Data.Bool (Bool(False, True))

data FunDec = FunDec Id [Arg] Type deriving Show
data ClassDec = ClassDec Id [VarDec] [FunDec] deriving Show
data VarDec = VarDec Id Type Bool Bool Bool deriving Show

data TypeType = TypeType AbsKOTLIN.Type Bool Bool

emptyClass :: Id -> ClassDec
emptyClass i = (ClassDec i [] [])

data Env = Env {    table :: [[VarDec]],
                    functions :: [FunDec],
                    classes :: [ClassDec]
} deriving Show

-- Environment Operations {{{

emptyEnv = Env {    table = [[]],
                    functions = [],
                    classes = []} 

envAddBlock :: Env -> Env
envAddBlock (Env table funs cls) = (Env ([]:table) funs cls)

envCloseBlock :: Env -> Env
envCloseBlock (Env (t:ts) funs cls) = (Env ts funs cls)

envAddVar :: Env -> VarDec -> Err Env
envAddVar (Env (scope:global) fs cs) (VarDec i t mut nul ini) = case (envVarExists (Env (scope:global) fs cs) i) of
    False -> Ok (Env (((VarDec i t mut nul ini):scope):global) fs cs)
    True -> Bad ("Variable " ++ show i ++ " already Declared in this Scope")

envVarExists :: Env -> Id -> Bool
envVarExists (Env ([]:_) _ _) _ = False
envVarExists (Env (((VarDec id1 _ _ _ _):ts):global) funs classes) id2  | id1 == id2 = True
                                                                        | otherwise = envVarExists (Env (ts:global) funs classes) id2

-- Add a Function into Environment globally
envAddFunc :: Env -> FunDec -> Err Env
envAddFunc e (FunDec i a t) = case (envFuncExists e (FunDec i a t)) of
    False -> Ok e{functions = ((FunDec i a t) : (functions e))}
    True -> Bad ("Function " ++ show i ++ " DECLARED TWICE")

-- Check if function exists globally
envFuncExists :: Env -> FunDec -> Bool
envFuncExists (Env _ [] _) _ = False
envFuncExists (Env _ [(FunDec i1 _ _)] _) (FunDec i2 _ _)   | i1 == i2 = True
                                                            | otherwise = False
envFuncExists (Env t ((FunDec i1 a1 t1):fs) c) (FunDec i2 a2 t2)    | i1 == i2 = True
                                                                    | otherwise = envFuncExists (Env t fs c) (FunDec i2 a2 t2) 

-- }}}

-- Class Operations {{{ 

-- Create Class
envAddClass :: Env -> ClassDec -> Err Env
envAddClass e (ClassDec i fs funs) = case (envClassExists e (ClassDec i fs funs)) of 
    False -> Ok e{ functions = ((FunDec i [] ((TypeId i))):(functions e)), classes = ((ClassDec i fs funs):(classes e))} 
    True -> Bad ("Class " ++ show i ++ " already Exists")

-- Check if Class exists globally
envClassExists :: Env -> ClassDec -> Bool
envClassExists (Env _ _ []) _ = False
envClassExists (Env t f ((ClassDec i1 _ _):cs)) (ClassDec i2 fields2 funs2) | i1 == i2 = True
                                                                            | otherwise = envClassExists (Env t f cs) (ClassDec i2 fields2 funs2)

-- Add a Variable (mutable and immutable) 
classAddVar :: ClassDec -> VarDec -> Err ClassDec
classAddVar (ClassDec id fs funs) (VarDec varId varType mut nul ini) = case (classVarExists (ClassDec id fs funs) varId) of 
    False -> Ok (ClassDec id ((VarDec varId varType mut nul ini):fs) funs)
    True -> Bad ("Variable " ++ show varId ++ " already declared inside of Class " ++ show id)

-- Check if Variable exists in given Class Declaration
classVarExists :: ClassDec -> Id -> Bool
classVarExists (ClassDec _ [] _) _ = False
classVarExists (ClassDec classId ((VarDec varId _ _ _ _):fields) funs) id  | id == varId = True
                                                                    | otherwise = classVarExists (ClassDec classId fields funs) id

-- Search Environment for Field in Class with certain Id
envClassFun :: Env -> AbsKOTLIN.Id -> AbsKOTLIN.Id -> Err FunDec
envClassFun (Env _ _ []) id1 id2 = Bad ("CLASS WITH ID " ++ show id1 ++ " NOT FOUND")
envClassFun (Env t f ((ClassDec classId fields funs):cs)) id1 id2 | classId == id1 = classFunDec (ClassDec classId fields funs) id2
                                                                    | otherwise = envClassFun (Env t f cs) id1 id2

-- check if Function exists in given Class Decl
classFunDec :: ClassDec -> Id -> Err FunDec
classFunDec (ClassDec _ _ []) id2 = Bad ("FUNCTION WITH ID " ++ show id2 ++ " NOT FOUND")
classFunDec (ClassDec classId fields ((FunDec id arg funtyp):funs)) id2 | id == id2 = Ok (FunDec id arg funtyp)
                                                                        | otherwise = classFunDec (ClassDec classId fields funs) id2

-- Search Environment for Field in Class with certain Id
envClassField :: Env -> AbsKOTLIN.Id -> AbsKOTLIN.Id -> Err AbsKOTLIN.Type
envClassField (Env _ _ []) id1 id2 = Bad ("CLASS WITH ID " ++ show id1 ++ " NOT FOUND")
envClassField (Env t f ((ClassDec classId fields funs):cs)) id1 id2 | classId == id1 = classFieldType (ClassDec classId fields funs) id2
                                                                    | otherwise = envClassField (Env t f cs) id1 id2


-- Check if Variable exists in given Class Declaration
classFieldType :: ClassDec -> Id -> Err AbsKOTLIN.Type
classFieldType (ClassDec _ [] _) id2 = Bad ("FIELD WITH ID " ++ show id2 ++ " NOT FOUND")
classFieldType (ClassDec classId ((VarDec varId t _ _ _):fields) funs) id   | id == varId = Ok t
                                                                            | otherwise = classFieldType (ClassDec classId fields funs) id

-- Add a Function to a Class
classAddFunc :: ClassDec -> FunDec -> Err ClassDec
classAddFunc (ClassDec id fs funs) (FunDec i args t) = case (classFuncExists (ClassDec id fs funs) (FunDec i args t)) of
    False -> Ok (ClassDec id fs ((FunDec i args t):funs))
    True -> Bad ("Function "++ show i ++" already exists inside of Class " ++ show id)

-- Check if Function exists in given Class Declaration
classFuncExists :: ClassDec -> FunDec -> Bool
classFuncExists (ClassDec _ _ []) _ = False
classFuncExists (ClassDec i fields ((FunDec id1 args1 t1):funs)) (FunDec id2 args2 t2)  | id1 == id2 = True
                                                                                        | otherwise = classFuncExists (ClassDec i fields funs) (FunDec id2 args2 t2) 

-- }}}

-- Other Operations {{{

-- lookup the corresponding Type of an Id (whole symbol table)
lookupGlobal :: [[VarDec]] -> Id -> Err Type 
lookupGlobal [] i = Bad ("VARIABLE " ++ show i ++ " NON EXISTENT")
lookupGlobal (table:ts) i = case lookupLocal table i of 
    Ok t -> Ok t 
    Bad err -> lookupGlobal ts i 

-- lookup the corresponding Type of an Id (only local scope)
lookupLocal :: [VarDec] -> Id -> Err Type 
lookupLocal [] i = Bad ("VARIABLE " ++ show i ++ " NOT IN SCOPE")
lookupLocal ((VarDec id t mut nul ini):vs) i    | id == i = Ok t 
                                                | otherwise = lookupLocal vs i                                                                  
-- lookup the function
lookupFunc :: Env -> Id -> Err FunDec
lookupFunc (Env _ [] _) i = Bad ("FUNCTION " ++ show i ++ " NOT DECLARED")
lookupFunc (Env t ((FunDec id a typ):fs) c) i | id == i = Ok (FunDec id a typ)
                                              | otherwise = lookupFunc (Env t fs c) i 

-- }}}

varSetInit :: Env -> Id -> Err Env
varSetInit (Env [] fs cs) i = Bad ("VARIABLE " ++ show i ++ " NOT DECLARED")
varSetInit (Env (t:ts) fs cs) i = case varSetInitLocal (Env (t:ts) fs cs) i of
    Ok e2 -> Ok e2
    Bad err -> case varSetInit (Env ts fs cs) i of
        Ok e2 -> Ok e2{ table = (t:(table e2))}
        Bad err -> Bad err

varSetInitLocal :: Env -> Id -> Err Env
varSetInitLocal (Env ([]:global) fs cs) i2 = Bad ("VARIABLE " ++ show i2 ++ " NOT DECLARED LOCALLY")
varSetInitLocal (Env (((VarDec i1 t mut nul ini):ts):global) fs cs) i2  | i1 == i2 = Ok (Env (((VarDec i1 t mut nul True):ts):global) fs cs)
                                                                        | otherwise = case varSetInitLocal (Env (ts:global) fs cs) i2 of
                                                                            Ok e -> Ok e{table = (((VarDec i1 t mut nul ini):(head (table e))):(tail (table e)))}
                                                                            Bad err -> Bad err

-- check if a variable has been initialized
varCheckInit :: Env -> Id -> Bool
varCheckInit (Env [] fs cs) i = False
varCheckInit (Env (t:ts) fs cs) i = varCheckInitLocal (Env (t:ts) fs cs) i

varCheckInitLocal :: Env -> Id -> Bool
varCheckInitLocal (Env ([]:global) fs cs) i2 = False
varCheckInitLocal (Env (((VarDec i1 t mut nul ini):ts):global) fs cs) i2    | i1 == i2 && ini = True
                                                                            | otherwise = varCheckInitLocal (Env (ts:global) fs cs) i2

-- check if a variable is mutable or immutable
varCheckNullable :: Env -> Id -> Bool
varCheckNullable (Env [] fs cs) i = False
varCheckNullable (Env (t:ts) fs cs) i = varCheckNullableLocal (Env (t:ts) fs cs) i

varCheckNullableLocal :: Env -> Id -> Bool
varCheckNullableLocal (Env ([]:global) fs cs) i2 = False
varCheckNullableLocal (Env (((VarDec i1 t mut nul ini):ts):global) fs cs) i2 | i1 == i2 && nul = True
                                                                            | otherwise = varCheckNullableLocal (Env (ts:global) fs cs) i2

-- check if a variable is mutable or immutable
varCheckMutable :: Env -> Id -> Bool
varCheckMutable (Env [] fs cs) i = False
varCheckMutable (Env (t:ts) fs cs) i = varCheckMutableLocal (Env (t:ts) fs cs) i

varCheckMutableLocal :: Env -> Id -> Bool
varCheckMutableLocal (Env ([]:global) fs cs) i2 = False
varCheckMutableLocal (Env (((VarDec i1 t mut nul ini):ts):global) fs cs) i2 | i1 == i2 && mut = True
                                                                            | otherwise = varCheckMutableLocal (Env (ts:global) fs cs) i2

