module ShiTT.OTT ( ottPreDefined
                 , coeName
                 , coePName
                 , eqName
                 , symName
                 , reflName
                 , transName
                 , piEqProj1Name
                 , piEqProj2Name
                 ) 
  where
import qualified Data.Map as M
import ShiTT.Syntax


-- OTT predefined names, they are not identifier, so we'll need special syntax for them.
coeName, coePName, eqName, symName, reflName, transName, piEqProj1Name, piEqProj2Name :: Name 
-- coe  : (S T : U) (Q : eq U U S T) -> S -> T
coeName = "coe"
-- coeP : (S T : U) (Q : eq U U S T) (s : S) -> eq S T s (coe S T Q s)    
coePName = "coeP" 
-- eq : (S T : U) (x : S) (y : T) -> U
eqName = "eq"
-- sym : (S T : U) (x : S) (y : T) -> eq S T x y -> eq T S y x
symName = "sym" 
-- refl : (S : U) (x : S) -> eq S S x x
reflName = "refl"
-- trans : (X Y Z : U) (x : X) (y : Y) (z : Z) -> eq X Y x y -> eq Y Z y z -> eq X Z x z
transName = "trans"
-- piEqProj1 : (S1 S2 : U) (T1 : S1 -> U) (T2 : S2 -> U) 
--           -> eq U U ((x : S1) -> T1 x) ((x : S2) -> T2 x) 
--           -> eq U U       S1                 S2
piEqProj1Name = "piEqProj1"
-- piEqProj2 : (S1 S2 : U) (T1 : S1 -> U) (T2 : S2 -> U) 
--           -> eq U U ((x : S1) -> T1 x) ((x : S2) -> T2 x) 
--           -> (s1 : S1) (s2 : S2) 
--           -> (q : eq S1 S2 s1 s2) 
--           -> eq U U (T1 s1) (T2 s2)
piEqProj2Name = "piEqProj2"

ottPreDefined :: M.Map Name Type
ottPreDefined = M.fromList
  [ (coeName, Pi "S" Expl U (Pi "T" Expl U (Pi "Q" Expl (App (App (App (App (Func "eq") U Expl) U Expl) (Var "S") Expl) (Var "T") Expl) (Pi "_" Expl (Var "S") (Var "T")))))
  , (coePName, Pi "S" Expl U (Pi "T" Expl U (Pi "Q" Expl (App (App (App (App (Func "eq") U Expl) U Expl) (Var "S") Expl) (Var "T") Expl) (Pi "s" Expl (Var "S") (App (App (App (App (Func "eq") (Var "S") Expl) (Var "T") Expl) (Var "s") Expl) (App (App (App (App (Func "coe") (Var "S") Expl) (Var "T") Expl) (Var "Q") Expl) (Var "s") Expl) Expl)))))
  , (eqName, Pi "S" Expl U (Pi "T" Expl U (Pi "x" Expl (Var "S") (Pi "y" Expl (Var "T") U))))
  -- , (symName, Pi "S" Expl U (Pi "T" Expl U (Pi "x" Expl (Var "S") (Pi "y" Expl (Var "T") (Pi "_" Expl (App (App (App (App (Func "eq") (Var "S") Expl) (Var "T") Expl) (Var "x") Expl) (Var "y") Expl) (App (App (App (App (Func "eq") (Var "T") Expl) (Var "S") Expl) (Var "y") Expl) (Var "x") Expl))))))
  -- , (reflName, Pi "S" Expl U (Pi "x" Expl (Var "S") (App (App (App (App (Func "eq") (Var "S") Expl) (Var "S") Expl) (Var "x") Expl) (Var "x") Expl)))
  -- , (transName, Pi "X" Expl U (Pi "Y" Expl U (Pi "Z" Expl U (Pi "x" Expl (Var "X") (Pi "y" Expl (Var "Y") (Pi "z" Expl (Var "Z") (Pi "_" Expl (App (App (App (App (Func "eq") (Var "X") Expl) (Var "Y") Expl) (Var "x") Expl) (Var "y") Expl) (Pi "_" Expl (App (App (App (App (Func "eq") (Var "Y") Expl) (Var "Z") Expl) (Var "y") Expl) (Var "z") Expl) (App (App (App (App (Func "eq") (Var "X") Expl) (Var "Z") Expl) (Var "x") Expl) (Var "z") Expl)))))))))
  -- , (piEqProj1Name, Pi "S1" Expl U (Pi "S2" Expl U (Pi "T1" Expl (Pi "_" Expl (Var "S1") U) (Pi "T2" Expl (Pi "_" Expl (Var "S2") U) (Pi "_" Expl (App (App (App (App (Func "eq") U Expl) U Expl) (Pi "x" Expl (Var "S1") (App (Var "T1") (Var "x") Expl)) Expl) (Pi "x" Expl (Var "S2") (App (Var "T2") (Var "x") Expl)) Expl) (App (App (App (App (Func "eq") U Expl) U Expl) (Var "S1") Expl) (Var "S2") Expl))))))
  -- , (piEqProj2Name, Pi "S1" Expl U (Pi "S2" Expl U (Pi "T1" Expl (Pi "_" Expl (Var "S1") U) (Pi "T2" Expl (Pi "_" Expl (Var "S2") U) (Pi "_" Expl (App (App (App (App (Func "eq") U Expl) U Expl) (Pi "x" Expl (Var "S1") (App (Var "T1") (Var "x") Expl)) Expl) (Pi "x" Expl (Var "S2") (App (Var "T2") (Var "x") Expl)) Expl) (Pi "s1" Expl (Var "S1") (Pi "s2" Expl (Var "S2") (Pi "q" Expl (App (App (App (App (Func "eq") (Var "S1") Expl) (Var "S2") Expl) (Var "s1") Expl) (Var "s2") Expl) (App (App (App (App (Func "eq") U Expl) U Expl) (App (Var "T1") (Var "s1") Expl) Expl) (App (Var "T2") (Var "s2") Expl) Expl)))))))))
  ]


