module Data.Finite

import Data.Vect

%default total

||| An interface for listing all values of a type with a
||| finite number of inhabitants.
public export
interface Finite a where
  constructor MkFinite
  values : List a

public export %inline
valuesOf : (0 a : Type) -> Finite a => List a
valuesOf _ = values

public export
Finite () where values = [()]

public export
Finite Void where values = []

public export
Finite Bool where values = [False,True]

public export
Finite Ordering where values = [LT,EQ,GT]

public export
Finite a => Finite (Maybe a) where values = Nothing :: map Just values

public export
Finite a => Finite b => Finite (Either a b) where
  values = map Left values ++ map Right values

public export
Finite a => Finite b => Finite (a,b) where
  values = [| MkPair values values |]

public export
{n : _} -> Finite a => Finite (Vect n a) where
  values {n = 0}   = [[]]
  values {n = S k} = [| values :: values |]

weaken : List (Fin k) -> List (Fin $ S k)
weaken []        = []
weaken (x :: xs) = weaken x :: weaken xs

fins : {n : _} -> List (Fin n)
fins {n = 0}   = []
fins {n = S k} = last :: weaken (fins {n = k})

public export %inline
{n : _} -> Finite (Fin n) where
  values = fins
