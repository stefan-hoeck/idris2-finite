# idris2-finite : A derivable interface for types with a finite number of values

This is a small library around the concept of a *finite* type
(represented by an implementation of the `Data.Finite` interface):
A type with only a finite number of inhabitants. Examples include
`Void` (no values), `Unit` (one value), `Bool` (two values), or
`Ordering` (three values). In addition, composite types such
as `Maybe Void` (one value), or `(Bool, Ordering)` (six values)
are supported as well.

The only function provided by `Finite` is `values`: It is supposed to yield
a list containing every possible value of the type in question.
Since this is a strict list, `Finite` is mainly targeted at types with
small numbers of inhabitants.

Implementations of `Finite` can be automatically derived for regular
algebraic data types using elaborator reflection. See the
project's [test cases](test/src/Main.idr) for some examples.
