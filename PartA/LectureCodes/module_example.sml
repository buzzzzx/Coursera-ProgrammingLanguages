signature Rational_C =
sig
  type rational
  val Whole : int -> rational
end

structure Rational1 :> Rational_C = 
struct
  datatype rational = Whole of int | Frac of int * int
end

fun f x = 
    case x of
        Rational1.Whole i => i
      | _ => 000

val w = f (Rational1.Whole 12)