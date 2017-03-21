datatype Expr = X    of int
              | Con  of real
              | Neg  of Expr
              | Plus of Expr * Expr
              | Mult of Expr * Expr
              | Exp  of Expr         (* e^x *)
              | Sin  of Expr         (* sin x *)
              | Cos  of Expr         (* cos x *)

fun eval exp xs =
    case exp of
        X i          => Vector.sub(xs, i)
      | Con c        => c
      | Neg e        => ~(eval e xs)
      | Plus(e1, e2) => eval e1 xs + eval e2 xs
      | Mult(e1, e2) => eval e1 xs * eval e2 xs
      | Exp e        => Math.exp (eval e xs)
      | Sin e        => Math.sin (eval e xs)
      | Cos e        => Math.cos (eval e xs)


fun zipWith f xs ys = Vector.mapi (fn (i, x) => f(x, Vector.sub(ys, i))) xs

fun zero n = Vector.tabulate(n, fn _ => Con 0.0)

fun direction n i x = Vector.tabulate(n, fn j => if i = j then Con x else Con 0.0)

fun scalar x v = Vector.map (fn e => Mult(x, e)) v
                                   
fun diff expr n =
    case expr of
        X i          => direction n i 1.0
      | Con _        => zero n
      | Neg e        => Vector.map Neg (diff e n)
      | Plus(e1, e2) => zipWith Plus (diff e1 n) (diff e2 n)
      | Mult(e1, e2) => zipWith Plus
                                (scalar e1 (diff e2 n))
                                (scalar e2 (diff e1 n))
                                (* (Vector.map (fn e2 => Mult (e1, e2)) (diff e2 n)) *)
                                (* (Vector.map (fn e1 => Mult (e1, e2)) (diff e1 n)) *)
                                
      | Exp e        => scalar expr (diff e n)
      | Sin e        => scalar (Cos e) (diff e n)
      | Cos e        => scalar (Neg(Sin e)) (diff e n)
      (* | Exp e        => Vector.map (fn e' => Mult(Exp e, e')) (diff e n) *)
      (* | Sin e        => Vector.map (fn e' => Mult(Cos e, e')) (diff e n) *)
      (* | Cos e        => Vector.map (fn e' => Mult(Neg(Sin e), e')) (diff e n) *)


fun dumbAD exp xs = Vector.map (fn e => eval e xs) (diff exp (Vector.length xs))




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
