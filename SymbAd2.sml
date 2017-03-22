datatype Expr = X    of int
              | Con  of real
              | Neg  of Expr
              | Plus of Expr * Expr
              | Mult of Expr * Expr
              | Exp  of Expr         (* e^x *)
              | Sin  of Expr         (* sin x *)
              | Cos  of Expr         (* cos x *)

fun lookup xs i = Vector.sub(xs, i)

fun eval exp xs =
    case exp of
        X i          => lookup xs i
      | Con c        => c
      | Neg e        => ~(eval e xs)
      | Plus(e1, e2) => eval e1 xs + eval e2 xs
      | Mult(e1, e2) => eval e1 xs * eval e2 xs
      | Exp e        => Math.exp (eval e xs)
      | Sin e        => Math.sin (eval e xs)
      | Cos e        => Math.cos (eval e xs)


fun zipWith f xs ys = Vector.mapi (fn (i, x) => f(x, Vector.sub(ys, i))) xs


fun zeroS n = Vector.tabulate(n, fn _ => Con 0.0)

fun directionS n i x = Vector.tabulate(n, fn j => if i = j then Con x else Con 0.0)

fun scalarS x v = Vector.map (fn e => Mult(x, e)) v



fun diff expr n =
    case expr of
        X i          => directionS n i 1.0
      | Con _        => zeroS n
      | Neg e        => Vector.map Neg (diff e n)
      | Plus(e1, e2) => zipWith Plus (diff e1 n) (diff e2 n)
      | Mult(e1, e2) => zipWith Plus
                                (scalarS e1 (diff e2 n))
                                (scalarS e2 (diff e1 n))
      | Exp e        => scalarS expr (diff e n)
      | Sin e        => scalarS (Cos e) (diff e n)
      | Cos e        => scalarS (Neg(Sin e)) (diff e n)


fun dumbAD exp xs = Vector.map (fn e => eval e xs) (diff exp (Vector.length xs))

fun zero n = Vector.tabulate(n, fn _ => 0.0)

fun direction n i x = Vector.tabulate(n, fn j => if i = j then x else 0.0)

fun scalar x v = Vector.map (fn e => x * e) v


type dualnum = real * real vector (* the result and the derivative *)


fun forward xs expr =
  let val n = Vector.length xs
      (* diffEval : Expr -> dualnum *)
      fun diffEval expr =
        case expr of
            X i          => (lookup xs i, direction n i 1.0)
          | Con c        => (c, zero n)
          | Neg e        => let val (ex, ed) = diffEval e
                            in (~ex, Vector.map ~ ed) end
          | Plus(e1, e2) => let val (ex1, ed1) = diffEval e1
                                val (ex2, ed2) = diffEval e2
                            in (ex1 + ex2, zipWith op+ ed1 ed2) end
          | Mult (e, e') => let val (ex, ed)   = diffEval e
                                val (ex', ed') = diffEval e'
                            in (ex * ex', zipWith op+ (scalar ex ed') (scalar ex' ed)) end
          | Exp e        => let val (ex, ed) = diffEval e
                                val exp_ex = Math.exp ex
                            in (exp_ex, scalar exp_ex ed) end
          | Sin e        => let val (ex, ed) = diffEval e
                            in  (Math.sin ex, scalar (Math.cos ex) ed) end
          | Cos e        => let val (ex, ed) = diffEval e
                            in  (Math.cos ex, scalar (~(Math.sin ex)) ed) end
  in #2(diffEval expr) end






(* Utility functions *)

fun pp exp =
    case exp of
        X i            => "X"^Int.toString i
      | Con r          => Real.toString r
      | Neg (Con r)    => pp(Con(~r))
      | Neg (e as X _) => "-"^pp e
      | Neg e          => "-("^pp e^")"
      | Plus(e1, Neg e2) => concat["(", pp e1, " - ", pp e2, ")"]
      | Plus(e1, e2) => concat["(", pp e1, " + ", pp e2, ")"]
      | Mult(e1, e2) => concat["(", pp e1, " * ", pp e2, ")"]
      | Exp e        => concat["e^", pp e]
      | Sin e        => concat["sin(", pp e,")"]
      | Cos e        => concat["cos(", pp e,")"]

fun ppv evec = concat(Vector.foldr op:: [] (Vector.mapi (fn(i, e) => concat["df/dX", Int.toString i, " = ", pp e,"\n"]) evec))

fun makeExpr n =
    let val one = Con 1.0
        fun minus e1 e2 = Plus(e1, Neg e2)
        fun step e = Exp (minus e one)
        fun loop 0 acc = acc
          | loop i acc = loop (i-1) (step acc)
    in  loop n (X 0) end

val small = makeExpr 2

val wpFunc = Plus(Sin (X 0), Mult(X 0, X 1)) (* sin(x0) + (x0 * x1) *)

fun bigTime ad n = Mosml.time (ad (makeExpr n)) 1.0
