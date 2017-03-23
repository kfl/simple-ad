structure Expr =
struct

(* Labled expression with labels of type 'e on sub expressions, and
   labels of type 'v on variables. Constants are unlabled.
*)
datatype ('e, 'v) Labled =
           X    of 'v * int
         | Con  of real
         | Neg  of ('e, 'v) Expr
         | Plus of ('e, 'v) Expr * ('e, 'v) Expr
         | Mult of ('e, 'v) Expr * ('e, 'v) Expr
         | Exp  of ('e, 'v) Expr         (* e^x *)
         | Sin  of ('e, 'v) Expr         (* sin x *)
         | Cos  of ('e, 'v) Expr         (* cos x *)
withtype ('e, 'v) Expr = 'e * ('e, 'v) Labled



fun lookup xs i = Vector.sub(xs, i)

fun eval (_, exp) xs =
    case exp of
        X(_, i)      => lookup xs i
      | Con c        => c
      | Neg e        => ~(eval e xs)
      | Plus(e1, e2) => eval e1 xs + eval e2 xs
      | Mult(e1, e2) => eval e1 xs * eval e2 xs
      | Exp e        => Math.exp (eval e xs)
      | Sin e        => Math.sin (eval e xs)
      | Cos e        => Math.cos (eval e xs)



(* Utility functions *)

fun unit_elab e = ((), e)
val & = unit_elab

val const = & o Con

fun ref_elab e = (ref NONE, e)

fun uvar i = X((), i)
fun rvar i = X(ref 0.0, i)



fun pp (_, exp) =
    case exp of
        X (_, i)       => "X"^Int.toString i
      | Con r          => Real.toString r
      | Neg (l, Con r)    => pp(l, Con(~r))
      | Neg (e as (_, X _)) => "-"^pp e
      | Neg e          => "-("^pp e^")"
      | Plus(e1, (_, Neg e2)) => concat["(", pp e1, " - ", pp e2, ")"]
      | Plus(e1, e2) => concat["(", pp e1, " + ", pp e2, ")"]
      | Mult(e1, e2) => concat["(", pp e1, " * ", pp e2, ")"]
      | Exp e        => concat["e^", pp e]
      | Sin e        => concat["sin(", pp e,")"]
      | Cos e        => concat["cos(", pp e,")"]

fun ppv evec = concat(Vector.foldr op:: [] (Vector.mapi (fn(i, e) => concat["df/dX", Int.toString i, " = ", pp e,"\n"]) evec))



fun makeExpr elab vlab n =
    let val one = elab(Con 1.0)
        fun minus e1 e2 = elab(Plus(e1, elab(Neg e2)))
        fun step e = elab(Exp (minus e one))
        fun loop 0 acc = acc
          | loop i acc = loop (i-1) (step acc)
    in  loop n (elab(vlab 0)) end

val small = makeExpr unit_elab uvar 2

val wpFunc =
    let
        fun x i = &(uvar i)
    in &(Plus(&(Sin (x 0)), &(Mult(x 0, x 1)))) (* sin(x0) + (x0 * x1) *)
    end

fun bigTime ad esize n_vars x0 =
  let val xs = Vector.tabulate(n_vars, fn i => if i = 0 then x0 else 0.0)
  in  lookup (Mosml.time (ad xs) (makeExpr ref_elab uvar esize)) 0
  end









fun makeFib elab vlab n =
  let infix ++
      fun x ++ y = elab(Plus(x, y))
      val fib0 = elab(vlab 0)
      val fib1 = elab(vlab 1)
      fun step (f0, f1) = (f1, f0 ++ f1)
      fun loop 0 acc = acc
        | loop i acc = loop (i-1) (step acc)
  in  #2(loop (n-1) (fib0, fib1))
  end

fun fib n = makeFib unit_elab uvar n

fun fibTime ad n n_vars =
  let val xs = Vector.tabulate(n_vars, fn i => if i < 0 then 1.0 else 0.0)
  in  lookup (Mosml.time (ad xs) (makeFib ref_elab uvar n)) 0
  end


end
