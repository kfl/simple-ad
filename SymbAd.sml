datatype Expr = X
              | One
              | Zero
              | Neg  of Expr
              | Plus of Expr * Expr
              | Mult of Expr * Expr
              | Exp  of Expr         (* Exp x means e^x *)

fun eval exp x =
    case exp of
        X            => x
      | One          => 1.0
      | Zero         => 0.0
      | Neg e        => ~(eval e x)
      | Plus(e1, e2) => eval e1 x + eval e2 x
      | Mult(e1, e2) => eval e1 x * eval e2 x
      | Exp e        => Math.exp (eval e x)

fun diff exp =
    case exp of
        X            => One
      | One          => Zero
      | Zero         => Zero
      | Neg e        => Neg (diff e)
      | Plus(e1, e2) => Plus(diff e1, diff e2)
      | Mult(e1, e2) => Plus(Mult(e1, diff e2),
                             Mult(diff e1, e2))
      | Exp e        => Mult(Exp e, diff e)    (* chain rule *)

fun dumbAD exp x = eval (diff exp) x


type dualnum = real * real (* the result and the derrivative *)

(* diffEval : Expr -> real -> dualnum *)

fun diffEval exp x =
    case exp of
        X            => (x, 1.0)
      | One          => (1.0, 0.0)
      | Zero         => (0.0, 0.0)
      | Neg e        => let val (ex, ed) = diffEval e x
                        in (~ex, ~ed) end
      | Plus (e, e') => let val (ex, ed)   = diffEval e x
                            val (ex', ed') = diffEval e' x
                        in (ex + ex', ed + ed') end
      | Mult (e, e') => let val (ex, ed)   = diffEval e x
                            val (ex', ed') = diffEval e' x
                        in (ex * ex', ex * ed' + ed * ex') end
      | Exp e        => let val (ex, ed) = diffEval e x
                        in (Math.exp ex, Math.exp ex * ed) end

fun forward exp x = #2(diffEval exp x)
                            
(* Utility functions *)

fun pp exp =
    case exp of
        X            => "X"
      | One          => "1"
      | Zero         => "0"
      | Neg One      => "-1"
      | Neg X        => "-X"
      | Neg e        => "-("^pp e^")"
      | Plus(e1, Neg e2) => concat["(", pp e1, " - ", pp e2, ")"]
      | Plus(e1, e2) => concat["(", pp e1, " + ", pp e2, ")"]
      | Mult(e1, e2) => concat["(", pp e1, " * ", pp e2, ")"]
      | Exp e        => concat["e^", pp e]

                        
fun makeExpr n =
    let fun minus e1 e2 = Plus(e1, Neg e2)
        fun step e = Exp (minus e One)
        fun loop 0 acc = acc
          | loop i acc = loop (i-1) (step acc)
    in  loop n X end

val small = makeExpr 2

fun bigTime ad n = Mosml.time (ad (makeExpr n)) 1.0
