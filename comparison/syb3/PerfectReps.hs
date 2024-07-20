{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-undecidable-instances #-}
module PerfectReps where

import Data.Typeable
import PerfectDatatype
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
  -- Neither this Derive or the customized derive generate
  -- instances of Data for Perfect that make programs work.
import Data.Generics.SYB.WithClass.Instances
import Language.Haskell.TH()

$(derive [''Perfect])
$(derive [''Fork])

{-
-- The instances below are based on what the Derive stuff generates, that's
-- why it looks a bit ugly.
-- However, my attempt to write an instance failed --Alexey
instance Typeable1 Perfect where
    { typeOf1 _ = mkTyConApp
                                  (mkTyCon "Perfect") [] }
constr_a1qi :: Constr
constr_a1qi = mkConstr
                 dataType_a1qh "Zero" [] Prefix
constr_a1qj :: Constr
constr_a1qj = mkConstr
                 dataType_a1qh "Succ" [] Prefix
dataType_a1qh :: DataType
dataType_a1qh = mkDataType
                   "Perfect" [constr_a1qi, constr_a1qj]
instance (Data ctx a_aAC
         --,Data ctx (Perfect (Fork a_aAC))
         -- Uncommenting the above would cause a loop during context
         -- reduction.
         ,Sat (ctx (Perfect a_aAC))
         ,Sat (ctx (Perfect (Fork a_aAC)))
         -- However, now appear Sat's in the context, due to the
         -- context reduction needed to get |Data ctx (Perfect(Fork
         -- a_aAC))| to |Data ctx a_aAc|.
         ,Sat (ctx (Fork a_aAC))
         ,Sat (ctx (Perfect (Fork (Fork a_aAC))))
         -- But it is hopeless! The requirements grow by one step:
         ,Sat (ctx (Fork (Fork a_aAC)))
         ,Sat (ctx (Perfect (Fork (Fork (Fork a_aAC)))))
         -- and so on ...
         --
         -- So it does not work
         ) =>
         Data ctx (Perfect a_aAC) where
    { gfoldl _ f_a1qk z_a1ql x_a1qm
                                                  = case x_a1qm of
                                                      Zero arg_a1qn
                                                        -> f_a1qk (z_a1ql Zero) arg_a1qn
                                                      Succ arg_a1qo
                                                        -> f_a1qk (z_a1ql Succ) arg_a1qo
    ; gunfold _ k_a1qp z_a1qq c_a1qr
                                                   = case
                                                         constrIndex c_a1qr
                                                     of
                                                       1 -> k_a1qp (z_a1qq Zero)
                                                       2 -> k_a1qp (z_a1qq Succ)
                                                       _ -> error "gunfold: fallthrough"
    ; toConstr _ x_a1qs
                                                    = case x_a1qs of
                                                        Zero _ -> constr_a1qi
                                                        Succ _ -> constr_a1qj
    ; dataTypeOf _ _ = dataType_a1qh }


instance Typeable1 Fork where
    { typeOf1 _ = mkTyConApp
                                  (mkTyCon "Fork") [] }
constr_a2fC :: Constr
constr_a2fC = mkConstr
                 dataType_a2fB "Fork" [] Prefix
dataType_a2fB :: DataType
dataType_a2fB = mkDataType
                   "Fork" [constr_a2fC]
instance (Data ctx a_aAz,
          Sat (ctx (Fork a_aAz))) =>
         Data ctx (Fork a_aAz) where
    { gfoldl _ f_a2fD z_a2fE x_a2fF
                                                  = case x_a2fF of
                                                      Fork arg_a2fG arg_a2fH
                                                        -> f_a2fD
                                                             (f_a2fD (z_a2fE Fork) arg_a2fG)
                                                             arg_a2fH
    ; gunfold _ k_a2fI z_a2fJ c_a2fK
                                                   = case
                                                         constrIndex c_a2fK
                                                     of
                                                       1 -> k_a2fI (k_a2fI (z_a2fJ Fork))
                                                       _ -> error "gunfold: fallthrough"
    ; toConstr _ x_a2fL
                                                    = case x_a2fL of Fork _ _ -> constr_a2fC
    ; dataTypeOf _ _ = dataType_a2fB }

-}
