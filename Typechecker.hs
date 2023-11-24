module Typechecker where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsKOTLIN
import Environment
import Prelude
import PrintKOTLIN
import ErrM

 -- type Err = Either String
type Result = Err String


failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x


-- Initialization of Typechecker {{{

-- Initialize Environment from list of Definitions
addDefs :: Env -> [AbsKOTLIN.Def] -> Err Env
addDefs e [] = Ok e
addDefs e (d:ds) = case d of
    (AbsKOTLIN.DFun i a t s) -> case envAddFunc e (FunDec i a t) of
        Ok e2 -> addDefs e2 ds
        Bad err -> Bad err
    (AbsKOTLIN.DClass i f) -> case readClass(emptyClass i) (AbsKOTLIN.DClass i f) of
        Ok c1 -> case envAddClass e c1 of
            Ok e2 -> addDefs e2 ds
            Bad err -> Bad err
        Bad err -> Bad err

-- Create Class from DClass (without checking for types)
readClass :: ClassDec -> AbsKOTLIN.Def -> Err ClassDec
readClass c (AbsKOTLIN.DClass _ []) = Ok c
readClass (ClassDec id fields funs) (AbsKOTLIN.DClass id3 (f:fs)) = case f of
    (AbsKOTLIN.FVarDecl i t n e) -> case n of
        AbsKOTLIN.PNull     -> case (classAddVar (ClassDec id (fields) funs) (VarDec i t True True True)) of
            Ok c2 -> readClass c2 (AbsKOTLIN.DClass id3 fs)
            Bad err -> Bad err
        AbsKOTLIN.PNotNull     -> case (classAddVar (ClassDec id (fields) funs) (VarDec i t True False True)) of
            Ok c2 -> readClass c2 (AbsKOTLIN.DClass id3 fs)
            Bad err -> Bad err
    (AbsKOTLIN.FValDecl i t n e) -> case n of
        AbsKOTLIN.PNull     -> case (classAddVar (ClassDec id (fields) funs) (VarDec i t False True True)) of
            Ok c2 -> readClass c2 (AbsKOTLIN.DClass id3 fs)
            Bad err -> Bad err
        AbsKOTLIN.PNotNull     -> case (classAddVar (ClassDec id (fields) funs) (VarDec i t False False True)) of
            Ok c2 -> readClass c2 (AbsKOTLIN.DClass id3 fs)
            Bad err -> Bad err
    (AbsKOTLIN.FFunDecl i args pt stm) -> case (classAddFunc (ClassDec id fields funs) (FunDec i args pt)) of
            Ok c2 -> case classAddVar (ClassDec id fields funs) (VarDec i (AbsKOTLIN.TypeFunType (AbsKOTLIN.FType (argsToTypes args) pt)) False False True) of
                Ok c3 -> readClass c3 (AbsKOTLIN.DClass id3 fs)
                Bad err -> Bad (err ++ " in Class " ++ show id)
            Bad err -> Bad err

-- }}}


-- Begin the Typechecking with the AST (Abstract Syntax Tree) as Input
typecheckProgram :: AbsKOTLIN.Program -> Result
typecheckProgram x = case x of
    AbsKOTLIN.PDefs defs -> case addDefs emptyEnv defs of
        Ok e2 -> case checkDefinitions e2 defs of
            Ok e3 -> Ok "TYPECHECK SUCCESSFUL"
            Bad err -> Bad (err)
        Bad err -> Bad (err)

-- Checkers {{{

-- Check Function
classCheckFunc :: Env -> AbsKOTLIN.Field -> Err Env
classCheckFunc e (AbsKOTLIN.FFunDecl i args pt stms) = case checkArguments (envAddBlock e) args of
    Ok e2 -> case pt of
        AbsKOTLIN.Type_Unit    -> case checkStatements e2 pt i args stms of
            Ok e3 -> Ok (envCloseBlock e3)
            Bad err -> Bad err
        t  -> case checkReturn e2 t of
            Ok e3 -> case checkStatements e3 pt i args stms of
                Ok e4 -> Ok (envCloseBlock e4)
                Bad err -> Bad err
            Bad err -> Bad err
    Bad err -> Bad err

-- check if return type exists
checkReturn :: Env -> AbsKOTLIN.Type -> Err Env
checkReturn e t = case checkType e t of
    Ok t2 -> Ok e
    Bad err -> Bad err

-- Check the Definitions Recursively
checkDefinitions :: Env -> [AbsKOTLIN.Def] -> Err Env
checkDefinitions e [] = Ok e
checkDefinitions e [d] = checkDef e d
checkDefinitions e (d:ds) = case checkDef e d of
    Ok e2 -> checkDefinitions e2 ds
    Bad err -> Bad err

checkDef :: Env -> AbsKOTLIN.Def -> Err Env
checkDef e d = case d of
    (AbsKOTLIN.DClass i fields) -> case (checkFields e i fields) of
        Ok e2 -> Ok e2
        Bad err -> Bad err
    (AbsKOTLIN.DFun (AbsKOTLIN.Id i) a t s) -> case checkArguments (envAddBlock e) a of      -- Open a new Block on the Stack and check the arguments of the function
        Bad err -> Bad (err ++ " IN FUNCTION: " ++ i)
        Ok e2 -> case checkReturn e2 t of                                                 -- afterwards check the return type of the function
            Bad err -> Bad err
            Ok e3 -> case checkStatements e3 t (AbsKOTLIN.Id i) a s of                       -- check Statements and ...
                Bad err -> Bad (err ++ " IN FUNCTION " ++ i)
                Ok e4 -> Ok (envCloseBlock e4)

-- Check Type of Fields of a Class
checkFields :: Env -> AbsKOTLIN.Id -> [AbsKOTLIN.Field] -> Err Env
checkFields e i []     = Ok e
checkFields e i (f:fs) = case checkField e i f of
    Ok newe -> checkFields newe i fs
    Bad err -> Bad err

-- Check Type of a Single Field
checkField :: Env -> AbsKOTLIN.Id -> AbsKOTLIN.Field -> Err Env
checkField e i f = case f of
    AbsKOTLIN.FVarDecl id t n exp -> case checkType e t of
        Bad err -> Bad (err ++ " IN CLASS: " ++ show i)
        Ok t -> case inferExp e exp of
            Bad err -> Bad err
            Ok AbsKOTLIN.Type_Null -> case n of
                AbsKOTLIN.PNull    -> Ok e
                AbsKOTLIN.PNotNull -> Bad ("VARIABLE " ++ show id ++" IN CLASS " ++ show i ++ " CANNOT BE NULL")
            Ok t1 -> case t == t1 of
                True  -> Ok e
                False -> Bad ("TYPE MISMATCH cannot assign " ++ show t1 ++ " to Variable " ++ show id ++ " of type " ++ show t)
    AbsKOTLIN.FValDecl id t n exp -> case checkType e t of
        Bad err -> Bad (err ++ " IN CLASS: " ++ show i)
        Ok t -> case inferExp e exp of
            Bad err -> Bad err
            Ok AbsKOTLIN.Type_Null -> case n of
                AbsKOTLIN.PNull    -> Ok e
                AbsKOTLIN.PNotNull -> Bad ("VARIABLE " ++ show id ++" IN CLASS " ++ show i ++ " CANNOT BE NULL")
            Ok t1 -> case t == t1 of
                True  -> Ok e
                False -> Bad ("TYPE MISMATCH cannot assign " ++ show t1 ++ " to Variable " ++ show id ++ " of type " ++ show t)
    AbsKOTLIN.FFunDecl i args pt stm -> case classCheckFunc e (AbsKOTLIN.FFunDecl i args pt stm) of
        Ok e2 -> Ok e2
        Bad err -> Bad err

-- check arguments
checkArguments :: Env -> [AbsKOTLIN.Arg] -> Err Env
checkArguments e [] = Ok e
checkArguments e (a:as) = case (checkArg e a) of
    Ok e2 -> checkArguments e2 as
    Bad err -> Bad ("Argument " ++ show a ++ err)

checkArg :: Env -> AbsKOTLIN.Arg -> Err Env
checkArg (Env (scope:table) funs classes) (AbsKOTLIN.ADecl i t p)   | p == AbsKOTLIN.PNull = case (envAddVar (Env (scope:table) funs classes) (VarDec i t True True True )) of
    Ok e -> Ok e
    Bad err -> Bad err
                                                                    | otherwise = case (envAddVar (Env (scope:table) funs classes) (VarDec i t True False True)) of
    Ok e -> Ok e
    Bad err -> Bad err

-- check the types
checkType :: Env -> AbsKOTLIN.Type -> Err AbsKOTLIN.Type
checkType e t = case t of
    AbsKOTLIN.Type_Unit -> Ok AbsKOTLIN.Type_Unit
    AbsKOTLIN.Type_Boolean -> Ok AbsKOTLIN.Type_Boolean
    AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
    AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
    AbsKOTLIN.Type_String -> Ok AbsKOTLIN.Type_String
    AbsKOTLIN.TypeId id -> case (envClassExists e (ClassDec id [] [])) of
        True  -> Ok (AbsKOTLIN.TypeId id)
        False -> Bad $"TYPE " ++ show id ++ " IS NOT DECLARED"
    AbsKOTLIN.TypeFunType (AbsKOTLIN.FType [] rt) -> case (checkType e rt) of
        Ok rt -> Ok rt
        Bad err -> Bad err
    AbsKOTLIN.TypeFunType (AbsKOTLIN.FType (t:ts) rt) -> case (checkType e t) of
        Ok t -> Ok t
        Bad err -> Bad err



-- check statements
checkStatements :: Env -> AbsKOTLIN.Type -> AbsKOTLIN.Id -> [AbsKOTLIN.Arg] -> [AbsKOTLIN.Stm] -> Err Env
checkStatements e _ _ _ [] = Ok e
checkStatements e t i a [s] = checkStm e t i a s
checkStatements e t i a (s:ss) = case checkStm e t i a s of
    Ok e2 -> checkStatements e2 t i a ss
    Bad err -> Bad err

-- check single statement
checkStm :: Env -> AbsKOTLIN.Type -> AbsKOTLIN.Id -> [AbsKOTLIN.Arg] -> AbsKOTLIN.Stm -> Err Env
checkStm env t i a s = case s of
    AbsKOTLIN.SExp exp -> do
        inferExp env exp
        return env
    AbsKOTLIN.SDeclVar idin -> case idin of
        AbsKOTLIN.IdNoInit i t pn -> case pn of
            AbsKOTLIN.PNull -> envAddVar env (VarDec i t True True False)
            AbsKOTLIN.PNotNull -> envAddVar env (VarDec i t True False False)
        AbsKOTLIN.IdInit i t pn exp -> case pn of
            AbsKOTLIN.PNull -> case envAddVar env (VarDec i t True True False) of
                Ok e2 -> case inferExp e2 exp of
                    Ok iType -> case iType == t || iType == AbsKOTLIN.Type_Null of
                        True -> varSetInit e2 i
                        False -> Bad ("VARIABLE " ++ show i ++ " INITIALIZED WITH VALUE OF WRONG TYPE " ++ show iType)
                    Bad err -> Bad err
                Bad err -> Bad err
            AbsKOTLIN.PNotNull -> case envAddVar env (VarDec i t True False False) of
                Ok e2 -> case inferExp e2 exp of
                    Ok iType -> case iType == t of
                        True -> varSetInit e2 i
                        False -> Bad ("VARIABLE " ++ show i ++ " INITIALIZED WITH VALUE OF WRONG TYPE" ++ show iType)
                    Bad err -> Bad err
                Bad err -> Bad err
    AbsKOTLIN.SDeclVal idin -> case idin of
        AbsKOTLIN.IdNoInit i t pn -> case pn of
            AbsKOTLIN.PNull -> envAddVar env (VarDec i t False True False)
            AbsKOTLIN.PNotNull -> envAddVar env (VarDec i t False False False)
        AbsKOTLIN.IdInit i t pn exp -> case pn of
            AbsKOTLIN.PNull -> case envAddVar env (VarDec i t False True False) of
                Ok e2 -> case inferExp e2 exp of
                    Ok iType -> case iType == t || iType == AbsKOTLIN.Type_Null of
                        True -> varSetInit e2 i
                        False -> Bad ("VARIABLE " ++ show i ++ " INITIALIZED WITH VALUE OF WRONG TYPE" ++ show iType)
                    Bad err -> Bad err
                Bad err -> Bad err
            AbsKOTLIN.PNotNull -> case envAddVar env (VarDec i t False False False) of
                Ok e2 -> case inferExp e2 exp of
                    Ok iType -> case iType == t of
                        True -> varSetInit e2 i
                        False -> Bad ("VARIABLE " ++ show i ++ " INITIALIZED WITH VALUE OF WRONG TYPE" ++ show iType)
                    Bad err -> Bad err
                Bad err -> Bad err
    AbsKOTLIN.SReturn exp -> case (inferExp env exp) of
        Ok t2 -> if t == t2
            then Ok env
            else Bad "RETURN TYPE DOES NOT MATCH FUNCTION DECLARATION"
        Bad err -> Bad err
    AbsKOTLIN.SReturnV -> case t of
        AbsKOTLIN.Type_Unit -> Ok env
        _ -> Bad "RETURN OF UNIT NOT ALLOWED"
    AbsKOTLIN.SWhile exp stm -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Boolean -> case checkStm env t i a stm of
            Ok e -> Ok e
            Bad err -> Bad err
        Ok _ -> Bad "NON-BOOLEAN EXPRESSION IN LOOP CONDITION"
        Bad err -> Bad err
    AbsKOTLIN.SFor id (AbsKOTLIN.RExp rex) stm -> case inferExp env rex of
        Ok AbsKOTLIN.Type_String -> case envAddVar env (VarDec id AbsKOTLIN.Type_String True False True) of
            Ok e1 -> case checkStm e1 t i a stm of
                Ok e2 -> Ok e2
                Bad err -> Bad err
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_IntList -> case envAddVar env (VarDec id AbsKOTLIN.Type_Int True False True) of
            Ok e1 -> case checkStm e1 t i a stm of
                Ok e2 -> Ok e2
                Bad err -> Bad err
            Bad err -> Bad err
        Ok _ -> Bad "RANGE NOT OF RIGHT TYPE"
        Bad err -> Bad err
    AbsKOTLIN.SBlock stms -> case envAddBlock env of
        e -> case checkStatements e t i a stms of
            Ok e2 -> Ok (envCloseBlock e2)
            Bad err -> Bad err
    AbsKOTLIN.SIf exp stm -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Boolean -> case checkStm env t i a stm of
            Ok e -> Ok e
            Bad err -> Bad err
        Ok _ -> Bad "TYPE OF THE EXPRESSION MUST BE BOOL"
        Bad err -> Bad $"ERROR IN IF EXPRESSION" ++ err
    AbsKOTLIN.SIfElse exp stm1 stm2 -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Boolean -> case checkStm env t i a stm1 of
            Bad err -> Bad err
            Ok e1 -> case checkStm e1 t i a stm2 of
                Bad err -> Bad err
                Ok e2 -> Ok e2
        Ok _ -> Bad "TYPE OF THE EXPRESSION MUST BE BOOL"
        Bad err -> Bad $"ERROR IN IF EXPRESSION" ++ err
    AbsKOTLIN.SWhen exp branches -> case inferExp env exp of
        Ok t3 -> checkWhenExp env t3 t i a branches False
        Bad err -> Bad err

checkWhenExp :: Env -> AbsKOTLIN.Type  -> AbsKOTLIN.Type -> AbsKOTLIN.Id -> [AbsKOTLIN.Arg] -> [AbsKOTLIN.Branch] -> Bool -> Err Env
checkWhenExp e _ _ _ _ [] _ = Ok e
checkWhenExp e t t2 i a (b:bs) set = case b of 
    AbsKOTLIN.Branch1 exp stm -> case (inferExp e exp) of
        Ok t3 -> case t3 == t of
            True -> case checkStm e t2 i a stm of
                Ok e2 -> checkWhenExp e t t2 i a bs set
                Bad err -> Bad err
            False -> Bad "WHEN EXPRESSION MISMATCH OF TYPES"
        Bad err -> Bad err
    AbsKOTLIN.Branch2 stm -> case set of
        False -> case checkStm e t2 i a stm of
            Ok e2 -> checkWhenExp e t t2 i a bs True
            Bad err -> Bad err
        True -> Bad "MULTIPLE ELSE BRANCHES IN WHEN STATEMENT" 

-- compare arguments of a function call to its signature
checkArgumentsExp :: Env -> [AbsKOTLIN.Arg] -> [AbsKOTLIN.Exp] -> Err String
checkArgumentsExp _ [] [] = Ok "Function match up"
checkArgumentsExp _ [] rest = Bad "NOT ENOUGH ARGUMENTS GIVEN"
checkArgumentsExp _ rest [] = Bad "MORE ARGUMENTS GIVEN"
checkArgumentsExp env ((AbsKOTLIN.ADecl id1 typ1 pn1):args) (exp:exps) = case inferExp env exp of
    Bad err -> Bad err
    Ok typ2 -> case (typ2 == typ1) of
        False ->  case ((typ2 == AbsKOTLIN.Type_Null) && (pn1 == AbsKOTLIN.PNull)) of
            True -> checkArgumentsExp env args exps
            False -> Bad ("FUNCTION NOT CALLED WITH CORRECT ARGUMENTS: type: " ++ show typ1 ++ " expected but type: " ++ show typ2 ++ " was given")
        True -> checkArgumentsExp env args exps

-- }}}


-- pattern matching for type checking and inference
inferExp :: Env -> AbsKOTLIN.Exp -> Err AbsKOTLIN.Type
inferExp env exp = case exp of
    AbsKOTLIN.EString string -> return AbsKOTLIN.Type_String 
    AbsKOTLIN.ETrue -> return AbsKOTLIN.Type_Boolean
    AbsKOTLIN.EFalse -> return AbsKOTLIN.Type_Boolean
    AbsKOTLIN.EInt integer -> return AbsKOTLIN.Type_Int
    AbsKOTLIN.EDouble double -> return AbsKOTLIN.Type_Double
    AbsKOTLIN.EId id -> case lookupGlobal (table env) id of
        Ok t -> Ok t
        Bad err -> Bad err
    AbsKOTLIN.ENull -> return AbsKOTLIN.Type_Null

    AbsKOTLIN.ERng exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_IntList
            Ok _ -> Bad ("WRONG TYPE IN RANGE")
            Bad err -> Bad err
        Ok _ -> Bad ("WRONG TYPE IN RANGE")
        Bad err -> Bad err

    AbsKOTLIN.EFCall id exp1 -> case lookupFunc env id of
        Bad err -> Bad err
        Ok (FunDec _ a AbsKOTLIN.Type_Unit) ->  case checkArgumentsExp env a exp1 of
            Ok _ -> Ok AbsKOTLIN.Type_Unit
            Bad err -> Bad err
        Ok (FunDec _ a t) -> case checkArgumentsExp env a exp1 of
            Ok _ -> Ok t
            Bad err -> Bad err

    AbsKOTLIN.EProj exp1 exp2 -> case inferExp env exp1 of
        Ok (AbsKOTLIN.TypeId i) -> case exp2 of
            AbsKOTLIN.EId i2 -> case envClassField env i i2 of
                Ok t -> Ok t
                Bad err -> Bad err
            AbsKOTLIN.EFCall i2 exps-> case envClassFun env i i2  of
                Ok (FunDec _ a AbsKOTLIN.Type_Unit) ->  case checkArgumentsExp env a exps of
                    Ok _ -> Ok AbsKOTLIN.Type_Unit
                    Bad err -> Bad err
                Ok (FunDec _ a t) -> case checkArgumentsExp env a exps of
                    Ok _ -> Ok t
                    Bad err -> Bad err
                Bad err -> Bad err
            _ -> Bad "ONLY FUNCTION OR VARIABLE CAN BE PROJECTED"
        Ok _ -> Bad "CANNOT PROJECT FROM NON-CLASS"
        Bad err -> Bad err

    AbsKOTLIN.EPIncr exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "POST-INCREMENT USED WITH NON NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.EPDecr exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "POST-DECREMENT USED WITH NON NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.EIncr exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "INCREMENT USED WITH NON NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.EDecr exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "DECREMENT USED WITH NON NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.EUPlus exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "UNARY OPERATOR USED WITH NON-NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.EUMinus exp -> case inferExp env exp of
        Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
        Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
        Ok _ -> Bad "UNARY OPERATOR USED WITH NON-NUMERIC"
        Bad err -> Bad err

    AbsKOTLIN.ETimes exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '*' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '*' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '*' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.EDiv exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '/' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '/' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '/' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.EAdd exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '+' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '+' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '+' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.ESubstr exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Int
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '-' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Double
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Double
            Ok _ -> Bad "USAGE OF '-' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '-' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.ELt exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '<' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '<' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '<' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.EGt exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '>' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '>' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '>' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.ELtEq exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '<=' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '<=' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '<=' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.EGtEq exp1 exp2 -> case inferExp env exp1 of
        Ok AbsKOTLIN.Type_Double -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '>=' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok AbsKOTLIN.Type_Int -> case inferExp env exp2 of
            Ok AbsKOTLIN.Type_Int -> Ok AbsKOTLIN.Type_Boolean
            Ok AbsKOTLIN.Type_Double -> Ok AbsKOTLIN.Type_Boolean
            Ok _ -> Bad "USAGE OF '>=' OPERATION WITH DIFFERENT TYPES"
            Bad err -> Bad err
        Ok _ -> Bad "USAGE OF '>=' WITH NON-NUMERIC TYPES"
        Bad err -> Bad err

    AbsKOTLIN.EEq exp1 exp2 -> case inferExp env exp1 of
        Bad err  -> Bad err
        Ok type1 -> case inferExp env exp2 of
            Bad err  -> Bad err
            Ok type2 -> Ok AbsKOTLIN.Type_Boolean

    AbsKOTLIN.ENEq exp1 exp2 -> case inferExp env exp1 of
        Bad err  -> Bad err
        Ok type1 -> case inferExp env exp2 of
            Bad err  -> Bad err
            Ok type2 -> Ok AbsKOTLIN.Type_Boolean

    AbsKOTLIN.EAnd exp1 exp2 -> case inferExp env exp1 of
        Bad err  -> Bad err
        Ok type1 -> case inferExp env exp2 of
            Bad err  -> Bad err
            Ok type2 -> case ((type1 == type2) && (type2 == AbsKOTLIN.Type_Boolean)) of
                True  -> Ok AbsKOTLIN.Type_Boolean
                False -> Bad "THE TYPE OF THE OPERANDS IS NOT THE SAME"

    AbsKOTLIN.EOr exp1 exp2 -> case inferExp env exp1 of
        Bad err  -> Bad err
        Ok type1 -> case inferExp env exp2 of
            Bad err  -> Bad err
            Ok type2 -> case ((type1 == type2) && (type2 == AbsKOTLIN.Type_Boolean)) of
                True  -> Ok AbsKOTLIN.Type_Boolean
                False -> Bad "THE TYPE OF THE OPERANDS IS NOT THE SAME"

    AbsKOTLIN.EAss exp1 exp2 -> case exp1 of
        AbsKOTLIN.EId id -> case varCheckMutable env id of
            True ->  case inferExp env exp1 of
                Bad err -> Bad err
                Ok type1 -> case inferExp env exp2 of
                    Bad err -> Bad err
                    Ok type2 -> case type2 == AbsKOTLIN.Type_Null of
                        True -> case varCheckNullable env id of
                            True -> Ok AbsKOTLIN.Type_Null
                            False -> Bad "VARIABLE NOT NULLABLE"
                        False -> case ((type1 == type2) || (type1 == AbsKOTLIN.Type_Double && type2 == AbsKOTLIN.Type_Int)) of
                            True -> Ok type1
                            False -> Bad "THE TYPES OF THE ASSIGNMENT IS NOT THE SAME"
            False -> case varCheckInit env id of
                True -> Bad "CANNOT REASSIGN TO IMMUTABLE VARIABLE"
                False -> case inferExp env exp1 of
                    Bad err -> Bad err
                    Ok type1 -> case inferExp env exp2 of
                        Bad err -> Bad err
                        Ok type2 -> case type2 == AbsKOTLIN.Type_Null of
                            True -> case varCheckNullable env id of
                                True -> Ok AbsKOTLIN.Type_Null
                                False -> Bad "VARIABLE NOT NULLABLE"
                            False -> case ((type1 == type2) || (type1 == AbsKOTLIN.Type_Double && type2 == AbsKOTLIN.Type_Int)) of
                                True -> Ok type1
                                False -> Bad "THE TYPES OF THE ASSIGNMENT IS NOT THE SAME"
        _ -> Bad "Cannot Assign" 
                    

    AbsKOTLIN.ELambda args exp -> case checkArguments (envAddBlock env) args of
        Ok e1 -> case inferExp e1 exp of
            Ok expTyp -> Ok (AbsKOTLIN.TypeFunType (AbsKOTLIN.FType (argsToTypes args) expTyp))
            Bad err -> Bad (err ++ " in Lambda-Expression" )
        Bad err -> Bad (err ++ " in Lambda-Expression")

argsToTypes :: [AbsKOTLIN.Arg] -> [AbsKOTLIN.Type]
argsToTypes [] = []
argsToTypes ((AbsKOTLIN.ADecl _ t _):as) = (t:(argsToTypes as ))   
