module Main

import Data.Finite
import Data.Vect

%default total

main : IO ()
main = printLn (the (List $ Fin 12) values)
