 -- Part A
data Term = Var String -- variable  
          | Lambda String Type Term -- abstraction 
          | Tru  -- constant true 
          | Fls   -- constat false 
          | If Term Term Term -- constant false 
          | Zero -- constant zero
          | Succ Term -- successor
          | Pred Term -- predecessor
          | App Term Term  -- application
          | IsZero Term -- zero test
          | TPair Term Term -- Appendix B Pair Term
          | FstPair Term -- Appendix B First Pair Projection 
          | SndPair Term -- Appendix B Second Pair Projection 
          deriving (Eq, Show) 
data Type =  TyFun Type Type -- Type of Functions
          | BoolType -- type of booleans
          | NatType -- type of natural numbers
          | ProdType Type Type -- Appendix B product types
          deriving (Eq, Show)

-- Part B 

subst :: String -> Term -> Term -> Term 
subst x term (Var y) = if x == y then term else (Var y) -- [x |-> t]x = t 
subst x term (App t1 t2) = App (subst x term t1) (subst x term t2) -- [x |-> t]y = y  
subst x term (Lambda y ty t1) = if x == y then (Lambda y ty t1) else (Lambda y ty (subst x term t1)) -- [x |-> t ](t_1 t_2) = ([ x|-> t]t_1)([x |-> t]t_2)
subst x term Tru = Tru -- [ x|-> t ]true = true
subst x term Fls = Fls -- [ x|-> t ]false = false
subst x term (If t1 t2 t3) = if (subst x term t1) == Tru then (subst x term t2) else (subst x term t3) -- [ x |-> t] if t_1 then t_2 else t_3 = if ([x |-> t]t_1) then ([x |-> t] t_2)else ([ x|-> t]t_3)
subst x term Zero = Zero -- [x |-> t]0 = 0
subst x term ( Succ t1 ) = Succ ( subst x term t1 ) -- [ x|-> t]( succ t') = succ ([ x|-> t]t')
subst x term ( Pred t1 ) = Pred ( subst x term t1 ) -- [ x |-> t](pred t') = pred([ x |-> t]t') 
subst x term ( IsZero t1 ) = IsZero ( subst x term t1 ) --[x |-> t](iszero t') = iszero([ x |-> t] t') 
subst x term ( TPair t1 t2 ) = TPair (subst x term t1) (subst x term t2)   -- appendix B [ x|-> t](t_1,t_2) = ([ x |-> t] t_1, [x |-> t]t_2)
subst x term ( FstPair t1 ) = FstPair (subst x term t1) -- [x |-> t](t'.1) = ([x |-> t]t').1
subst x term ( SndPair t1 ) = SndPair (subst x term t1) -- [x |-> t](t'.2) = ([x |-> t]t').2


-- Part C

isNumericValue :: Term -> Bool
isNumericValue Zero = True
isNumericValue (Succ t) = isNumericValue t
isNumericValue (Pred t) = isNumericValue t
isNumericValue _ = False

isValue :: Term -> Bool
isValue Tru = True
isValue Fls = True
isValue (Lambda _ _ _) = True
isValue (TPair t1 t2) = if isValue t1 && isValue t2 then True else False
isValue t = isNumericValue t  

eval1 :: Term -> Maybe Term
-- (E - APPABS)
eval1 (App (Lambda x ty t1) t2) = if isValue t2 then Just (subst x t2 t1)
                                    else
                                    case (eval1 t2) of Just t12 -> Just (App (Lambda x ty t1) t12)
                                                       Nothing -> Nothing 

-- (E - APP2) && (E - APP1)                                                       
eval1 (App t1 t2) = if isValue t1 then 
                    case (eval1 t2) of Just t12 -> Just ( App t1 t12 )
                                       Nothing -> Nothing
                    else
                    case (eval1 t1) of Just t11 -> Just (App t11 t2) 
                                       Nothing -> Nothing                      

-- ( E - IFTRUE )                                       
eval1 ( If Tru t1 t2 ) = Just t1

-- ( E - IFFALSE )
eval1 ( If Fls t1 t2 ) = Just t2

-- ( E - IF )
eval1 ( If t1 t2 t3 ) = 
      case (eval1 t1) of Just t11 -> Just ( If t11 t2 t3 )
                         Nothing -> Nothing

 -- ( E - SUCC )                         
eval1 ( Succ t1 ) = 
      case (eval1 t1) of Just t11 -> Just ( Succ t11 )
                         Nothing -> Nothing

-- ( E - PREDZERO )                         
eval1 ( Pred Zero ) = Just Zero

-- ( E - PREDSUCC )
eval1 ( Pred ( Succ nv1 )) = if isNumericValue nv1 then Just nv1 else Nothing

-- ( E - PRED )
eval1 ( Pred t1 ) = 
      case (eval1 t1) of Just t11 -> Just ( Pred t11 )
                         Nothing -> Nothing 

-- ( E - ISZEROZERO )                          
eval1 ( IsZero Zero ) = Just Tru 

-- ( E - ISZEROSUCC )
eval1 ( IsZero ( Succ nv1 ) ) = Just Fls

-- ( E - ISZERO ) 
eval1 ( IsZero t1 ) = 
      case (eval1 t1 ) of Just t11 -> Just ( IsZero t11 )
                          Nothing -> Nothing

-- ( E - PAIR1 )                          
eval1 ( TPair v1@(Lambda _ _ _) t2 ) = 
      case (eval1 t2) of Just t11  -> Just ( TPair v1 t11 )  -- appendix B
                         Nothing -> Nothing

-- ( E - PAIR2 )                         
eval1 ( TPair t1 t2 ) =
      case (eval1 t1) of Just t11 -> Just ( TPair t11 t2 )
                         Nothing -> Nothing

-- ( E - PROJ1 )                         
eval1 ( FstPair ( TPair t1 t2 ) ) = Just t1

-- ( E - PROJ2 )
eval1 ( SndPair ( TPair t1 t2 ) ) = Just t2

-- ( E - PAIRBETA1 )
eval1 ( FstPair t1 ) =
      case (eval1 t1) of Just t11 -> Just ( FstPair t11 )
                         Nothing -> Nothing

-- ( E - PAIRBETA2 )                         
eval1 ( SndPair t2 ) = 
      case (eval1 t2) of Just t12 -> Just ( SndPair t12 )
                         Nothing -> Nothing                    
eval1 t = Nothing -- EVERYTHING ELSE 


-- Part D

eval :: Term -> Term 
eval t = case eval1 t of Just t1 -> eval t1
                         Nothing -> t

-- Part E 

type Env =  [(String, Maybe Type)]

extendEnv :: (String,Maybe Type) -> Env -> Env
extendEnv x env = x:env

findVar :: String -> Env -> Maybe Type
findVar x env = 
        case lookup x env of Just ty -> ty
                             Nothing -> error "Variable not in scope"


findType :: Env -> Term -> Maybe Type

-- ( T - TRUE )
findType _ Tru = Just BoolType

-- ( T - FALSE )
findType _ Fls = Just BoolType

-- ( T - ZERO )
findType _ Zero = Just NatType

-- ( T - SUCC )
findType env (Succ t) = 
          case (findType env t) of Just NatType -> Just NatType
                                   _ -> Nothing

-- ( T - PRED )                                   
findType env (Pred t) = 
          case (findType env t) of Just NatType -> Just NatType
                                   _ -> Nothing

-- ( T - ISZERO )                                    
findType env (IsZero t) = 
          case (findType env t) of Just NatType -> Just BoolType
                                   _ -> Nothing

-- ( T - IF )                                   
findType env (If t1 t2 t3) = 
          case (findType env t1, findType env t2, findType env t3)
            of (Just BoolType, Just ty2, Just ty3) -> if ty2 == ty3 
                                                          then Just ty2 
                                                          else Nothing
               _ -> Nothing

-- ( T - VAR )
findType env (Var x) = 
    case (findVar x env) of (Just ty) -> Just ty
                            _ -> Nothing


-- ( T - Abs ) 
findType env (Lambda x ty t2) =  
    let env2 = (extendEnv (x, Just ty) env) 
        ty2 = (findType env2 t2) in 
              case ty2 of (Just ty2) -> (Just (TyFun ty ty2)) 
                          _ -> Nothing
            

-- ( T - APP )                
findType env (App t1 t2) = 
    case (findType env t1 , findType env t2) 
       of (Just (TyFun ty1 ty2), Just ty3) -> if ty1 == ty3 then Just ty2 else Nothing
          _ -> Nothing

-- ( T - PAIR )               
findType env (TPair t1 t2) = 
          case (findType env t1, findType env t2)
            of (Just ty1, Just ty2) -> Just (ProdType ty1 ty2) 
               _ -> Nothing

-- ( T - PROJ1 )               
findType env (FstPair t1) =
          case (findType env t1) of Just (ProdType ty1 ty2) -> Just ty1
                                    _ -> Nothing

-- ( T - PROJ2 )                                    
findType env (SndPair t1) =
          case (findType env t1) of Just (ProdType ty1 ty2) -> Just ty2
                                    _ -> Nothing

                                   







