module Derive.Finite

import public Data.Finite
import public Derive.Prelude
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

||| Top-level function declaration implementing the `values` function for
||| the given data type.
export
valuesClaim : Visibility -> (fun : Name) -> (p : ParamTypeInfo) -> Decl
valuesClaim vis fun p =
  let arg := p.applied
      tpe := piAll `(List ~(arg)) (allImplicits p "Finite")
   in simpleClaim vis fun tpe

||| Top-level declaration implementing the `Finite` interface for
||| the given data type.
export
finiteImplClaim : Visibility -> (impl : Name) -> (p : ParamTypeInfo) -> Decl
finiteImplClaim v impl p = implClaimVis v impl (implType "Finite" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

export
finiteImplDef : (fun, impl : Name) -> Decl
finiteImplDef f i = def i [patClause (var i) (var "MkFinite" `app` var f)]

app : TTImp -> List (BoundArg 0 Explicit) -> TTImp
app s []        = s
app s (_ :: xs) = app `(Prelude.(<*>) ~(s) values) xs

add : SnocList TTImp -> TTImp -> TTImp
add [<]       s = s
add (sx :< x) s = add sx `(Prelude.List.(++) ~(x) ~(s))

cons : SnocList TTImp -> TTImp -> TTImp
cons [<]       s = s
cons (sx :< x) s = cons sx `(Prelude.(::) ~(x) ~(s))

rhs : {0 as : _} -> SnocList TTImp -> SnocList TTImp -> List (Con n as) -> TTImp
rhs ss st []      = cons ss (add st `(Prelude.Nil))
rhs ss st (c::cs) = case boundArgs explicit c.args [] <>> [] of
  [] => rhs (ss :< c.nameVar) st cs
  as => rhs ss (st :< app `(Prelude.pure ~(c.nameVar)) as) cs

||| Definition of a (local or top-level) function implementing
||| the values operation.
export
valuesDef : Name -> ParamTypeInfo -> Decl
valuesDef fun p = def fun [patClause (var fun) (rhs [<] [<] p.info.cons)]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `Finite` for a given data type.
export
FiniteVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
FiniteVis vis nms p =
  let fun  := funName p "values"
      impl := implName p "Finite"
   in Right
        [ TL (valuesClaim vis fun p) (valuesDef fun p)
        , TL (finiteImplClaim vis impl p) (finiteImplDef fun impl)
        ]

||| Alias for `FiniteVis Public`
export %inline
Finite : List Name -> ParamTypeInfo -> Res (List TopLevel)
Finite = FiniteVis Public
