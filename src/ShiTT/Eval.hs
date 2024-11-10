{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ShiTT.Eval where 

import ShiTT.Syntax
import ShiTT.Context
import qualified Data.Map as M
import ShiTT.Meta
import Common
import Debug.Trace (trace)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import ShiTT.OTT

instance Show Value where
  show = pp True -- . force
    where
      ppSp [] = ""
      ppSp ((v, Expl):rest) = ' ' : pp False v ++ ppSp rest
      ppSp ((v, Impl):rest) = "{" ++ pp True v ++ '}' : ppSp rest
      remove_infix = \case
        -- ('-':n) -> n
        n -> n
      pp is_top = \case
        VLam x Expl b -> inParen $ "lambda " ++ x ++ ". "   ++ pp True (b @ x := VVar x)
        VLam x Impl b -> inParen $ "lambda {" ++ x ++ "}. " ++ pp True (b @ x := VVar x)
        VRig x sp -> if null sp then remove_infix x else inParen $ x ++ ppSp sp
        VCon x sp -> pp is_top (VRig x sp)
        VFlex m sp -> pp is_top (VRig ('?':show m) sp)
        VFunc x sp -> pp is_top (VRig x.funName sp)
        VOTTFunc x sp -> pp is_top (VRig x sp)
        VPatVar x sp -> pp is_top (VRig x sp)
        VPi x Expl t b -> inParen $ "Pi (" ++ x ++ ":" ++ pp True t ++ "). " ++ pp True (b @ x := VVar x)
        VPi x Impl t b -> inParen $ "Pi {" ++ x ++ ":" ++ pp True t ++ "}. " ++ pp True (b @ x := VVar x)
        VU -> "U"
        VOTTEqTerm s -> "#" ++ s
        where paren x = '(' : x ++ ")"
              inParen x = if is_top then x else paren x

-- For debug
-- deriving instance Show Value 
-- instance Show Closure where 
--   show (Closure _ b) = show b 

instance Show Context where 
  show ctx = "\n- env:" ++ show ctx.env ++ "\n- typ:" ++ show ctx.types ++ "\n- sig:" ++ show ctx.decls

instance Show Def where 
  show (x := v) = x ++ " := " ++ show v

instance Show Bind where 
  show (x :~ (t, Source)) = x ++ " : " ++ show t  
  show (x :~ (t, Inserted)) = x ++ " : " ++ show t ++ " (Inserted) "

deriving instance Show ElabBind
deriving instance Show ElabDef
deriving instance Show Clause

infixl 7 @
(@) :: Closure -> Def -> Value
clo @ def = eval (clo.cloctx <: def) clo.cloterm

eval :: Context -> Term -> Value 
eval ctx@(env -> env) = \case
  ---
  Var x               -> case M.lookup x env of 
    Just v -> v 
    Nothing -> VVar x
  ---
  App t u i           -> vApp ctx (eval ctx t) (eval ctx u) i
  ---
  Lam x i t           -> VLam x i $ Closure ctx t
  ---
  Pi x i t b          -> VPi x i (eval ctx t) $ Closure ctx b
  ---
  Let x _ t u         -> eval (ctx <: x := eval ctx t) u
  ---
  Func name           -> case M.lookup name ctx.decls.allFunDecls of 
    Just f  -> appFun ctx f []
    Nothing -> case M.lookup name ottPreDefined of 
      Just f  -> ottFun ctx name []
      Nothing -> error "impossible: unknow function"
  ---
  U                   -> VU 
  ---
  PrintCtx t          -> eval ctx t -- trace (show ctx) $ eval ctx t
  ---
  Meta m              -> vMeta m
  ---
  PatVar x            -> case M.lookup x env of 
    Just (VPatVar y []) 
      | y /= x -> eval ctx (PatVar y) 
    Just v -> v 
    Nothing 
      | head x == '*' -> VPatVar x [] -- This is for dealing generated vars from coverage check
      | otherwise -> VPatVar x [] -- trace ("unknow: " ++ x ++ " in env: " ++ show env) $ error "Impossible"
  ---
  Undefiend -> error "Impossible: evalating undefined"
  ---
  OTTEqTerm s -> VOTTEqTerm s
  ---
  InsertedMeta m bds  ->
    let avail_vars = M.filterWithKey 
                      (\ name _ -> case M.lookup name bds of 
                                     Just v -> v == Bound
                                     Nothing -> False -- error $ show ctx ++ "\n" ++ name ++ "\n" ++ show bds ++ show m 
                      ) 
                      env in 
    vAppSp ctx (vMeta m) (map (,Expl) (snd <$> M.toList avail_vars)) 
    -- I don't care the order, since I'm using HOAS

printContext :: Context -> String 
printContext ctx = intercalate "\n" [  
    if isFree ctx x then 
      x ++ " : " ++ show t
    else 
      x ++ " : " ++ show t ++ "\n" ++ (const ' ' <$> [1..length x]) ++ " = " ++ show (fromJust (M.lookup x ctx.env))
  | (x, (t, _)) <- M.toList ctx.types]

quoteSp :: Context -> Term -> Spine -> Term
quoteSp ctx t = \case 
  []            -> t 
  ((u, i) : xs) -> quoteSp ctx (App t (quote ctx u) i) xs 

quote :: Context -> Value -> Term 
quote ctx (force ctx -> t) =
  case t of
    VRig  x sp  -> quoteSp ctx (Var x)  sp
    VCon  x sp  -> quoteSp ctx (Func x) sp 
    VFlex m sp  -> quoteSp ctx (Meta m) sp 
    VPatVar x sp -> quoteSp ctx (PatVar x) sp
    VFunc x sp  -> quoteSp ctx (Func x.funName) sp 
    VOTTFunc x sp -> quoteSp ctx (Func x) sp 
    VU          -> U 
    VLam x i b  -> binder (\x' b' -> Lam x' i b') (freshName ctx x) b
    VPi x i t b -> binder (\x' b' -> Pi x' i (quote ctx t) b') (freshName ctx x) b
    VOTTEqTerm s -> OTTEqTerm s
  where 
    binder :: (Name -> Term -> Term) -> Name -> Closure ->  Term 
    binder con x b = con x (quote (ctx <: x := VVar x) (b @ x := VVar x)) 

normalize :: Context -> Term ->  Term 
normalize ctx term = quote ctx (eval ctx term) 

freshName :: Context -> Name -> Name
freshName ctx@(env -> env) = \case
  "_" -> "_"
  x -> case M.lookup x env of
    Nothing | x `notElem` ctx.decls.definedNames -> x
            | otherwise -> go x (0 :: Int)
    Just _ -> go x (0 :: Int)
  where
    go x n = let x' = x ++ show n in
      case M.lookup x' env of
        Nothing | x `notElem` ctx.decls.definedNames -> x'
                | otherwise -> go x (n + 1)
        Just _ -> go x (n + 1)

freshName' :: [Name] -> Name -> Name
freshName' ls = \case
  "_" -> "_"
  x -> if x `elem` ls then 
      go x (0 :: Int) 
    else
      x  
  where
    go x n = let x' = x ++ show n in
      case x' `elem` ls of
        False -> x'
        True -> go x (n + 1)

vApp :: Context -> Value -> Value -> Icit -> Value
vApp ctx t u i = case t of 
  VLam x _ f -> f @ x := u 
  VFlex m sp -> VFlex m (sp >>> (u, i))
  VRig  x sp -> VRig  x (sp >>> (u, i))
  VPatVar x sp -> VPatVar x (sp >>> (u, i))
  VFunc x sp -> appFun ctx x (sp >>> (u, i))
  VOTTFunc x sp -> ottFun ctx x (sp >>> (u, i))
  VCon x sp -> VCon x (sp >>> (u, i))
  _ -> error $ show (t, u, i) ++ "Impossible"

vAppSp :: Context -> Value -> Spine -> Value 
vAppSp ctx t sp = revCase sp 
 {- sp == []   -> -} t 
 (\fore (u, i) ->    vApp ctx (vAppSp ctx t fore) u i)

match :: Context -> [Pattern] -> Spine -> Maybe [Def]
match ctx [] [] = Just [] 
match ctx (p:ps) (p':ps') = do 
  ret <- match1 ctx p p' 
  rest <- match ctx ps ps'
  pure $ ret ++ rest
match _ p sp = error $ "impossible, unmatched arity : " ++ show p ++ " | " ++ show sp

match1 :: Context -> Pattern -> (Value, Icit) -> Maybe [Def]
match1 ctx (PVar x i) (force ctx -> v, i') | i == i' = Just $ pure (x := v)
match1 ctx (PCon name ps i) (force ctx -> VCon name' ps', i') 
     | i == i' && name == name' = do 
      (dat, con) <- lookupCon' name ctx 
      match ctx ps (drop (length dat.dataPara) ps')
     | otherwise = Nothing 
match1 ctx _ _ = Nothing

matchClause :: Context -> Clause -> Spine -> Maybe Value 
matchClause ctx (Clause patt _ rhs) sp = do 
  defs <- match ctx patt sp 
  pure $ eval (ctx <: defs) rhs

-- | Evaluation of a function defined by pattern match.
matchFun :: Context -> Fun -> Spine -> Maybe Value
matchFun ctx fun@(funClauses -> cls) sp = case cls of 
    Nothing -> Just $ VCon fun.funName sp
    Just cls -> go cls 
  where
    go []       = Nothing 
    go (cl:cls) = case matchClause ctx cl sp of 
      Just v  -> Just v
      Nothing -> go cls

appFun :: Context -> Fun -> Spine -> Value 
appFun ctx fun (splitAt (arity fun) -> (args, rest_args))
  | length args == arity fun 
      = vAppSp 
          ctx 
          ( case matchFun ctx fun args of
              Nothing -> VFunc fun args
              Just res -> res
          ) 
          rest_args
  | otherwise = VFunc fun args

force :: Context -> Value -> Value
force ctx t@(VFlex m sp) = 
  case lookupMeta m of 
    Unsolved -> t
    Solved v -> force ctx (vAppSp ctx v sp)
force _ t = t 

vMeta :: MetaId ->  Value 
vMeta m = 
  case lookupMeta m of 
    Unsolved -> VMeta m
    Solved v -> v

-- | refresh a value so that we can eliminate metavar 
refresh :: Context -> Value -> Value
refresh ctx = eval ctx . quote ctx 

getDataType :: Context -> Data -> VType 
getDataType ctx dat = eval ctx $ go (dat.dataPara ++ dat.dataIx) where 
  go [] = U
  go ((x,i,t):ts) = Pi x i t $ go ts

getFunType :: Context -> Fun -> VType 
getFunType ctx fun = eval ctx $ go fun.funPara where 
  go [] = fun.funRetType
  go ((x,i,t):ts) = Pi x i t $ go ts


-- OTT

ottFun :: Context -> Name -> Spine -> Value 
ottFun ctx f_name sp 
  | f_name == coeName && length sp == 4 = 
      let [s, t, q, v] = map fst sp in 
        coerce ctx v (s, t) q
  | otherwise = VOTTFunc f_name sp 

a $#$ b = App a b Expl

coeApp :: String -> Term -> Term -> Term -> Term 
coeApp eq s t v = Func coeName $#$ s $#$ t $#$ OTTEqTerm eq $#$ v

judgeEqSp :: Context -> Spine -> Spine -> Bool
judgeEqSp ctx sp sp' = case (sp, sp') of 
  ([],       []        ) -> True
  ((v,_):sp, (v',_):sp') -> judgeEq ctx v v' && judgeEqSp ctx sp sp'
  _                      -> False

judgeEq :: Context -> Value -> Value -> Bool
judgeEq ctx (refresh ctx -> t) (refresh ctx -> u) = 
  case (t, u) of 
  ---
  (VU, VU) -> True
  ---
  (VLam x i t, VLam y i' t') | i == i' -> let x' = freshName ctx x in 
    judgeEq (ctx <: freeVar x') (t @ x := VVar x') (t' @ y := (VVar x'))
  ---
  (t', VLam x i  t ) -> let x' = freshName ctx x in 
    judgeEq (ctx <: freeVar x') (vApp ctx t' (VVar x') i) (t @ x := VVar x')
  ---
  (VLam x i t, t') -> let x' = freshName ctx x in 
    judgeEq (ctx <: freeVar x') (t @ x := VVar x') (vApp ctx t' (VVar x') i)
  ---
  (VPi x i a b, VPi x' i' a' b') | i == i' -> do 
    let fre = freshName ctx x in
      judgeEq ctx a a' 
      && judgeEq (ctx <: freeVar fre) (b @ x := VVar fre) (b' @ x' := VVar fre)
  ---
  (VCon con sp, VCon con' sp') | con == con' -> judgeEqSp ctx sp sp' 
  ---
  (VFunc fun sp, VFunc fun' sp') | fun.funName == fun'.funName -> judgeEqSp ctx sp sp' 
  ---
  (VOTTFunc fun sp, VOTTFunc fun' sp') | fun == fun' -> judgeEqSp ctx sp sp' 
  --- 
  (VOTTEqTerm _, VOTTEqTerm _) -> True
  ---
  (VRig x sp, VRig x' sp') | x == x' -> judgeEqSp ctx sp sp' 
  --- 
  (VPatVar x sp, VRig x' sp') | x == x' -> judgeEqSp ctx sp sp'
  ---
  (VRig x sp, VPatVar x' sp') | x == x' -> judgeEqSp ctx sp sp'
  ---
  (VPatVar x sp, VPatVar x' sp') | x == x' -> judgeEqSp ctx sp sp'
  --- 
  _ -> False
  

-- Note, coe : (S T : U) (Q : eq U U S T) -> S -> T
coerce :: Context -> Value -> (VType, VType) -> Value -> Value
coerce ctx (force ctx -> v) (force ctx -> s, force ctx -> t) eq = case (s,t) of 
  (VPi x i a b, VPi x' i' a' b') | i == i' -> 
    let newX = freshName ctx x  -- `newX : a'
        -- COE a' a #(sym (proj1 eq)) newX : a
        coeX =  coerce (ctx <: (newX, a) :=! VVar newX ) (VVar newX) (a', a) 
                (VOTTEqTerm $ symName ++ " (" ++ piEqProj1Name ++ " (" ++ show eq ++ "))")
        b'nX = b @ (x' := VVar newX)
        bCoeX = b @ (x' := coeX)
        -- \ newX -> COE bCoeX b'nX # (v (COE a' a # x))
        term = Lam newX i 
             $ coeApp "todo" 
                (quote ctx bCoeX) 
                (quote ctx b'nX) 
                (App (quote ctx v) 
                     (quote ctx coeX)
                     Expl)
    in  eval ctx term
  _ | judgeEq ctx s t -> v
  _ -> VOTTFunc "coe" [(s,Expl), (t, Expl), (eq, Expl), (v, Expl)] 
