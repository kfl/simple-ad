(*
datatype Expr = X    of int
              | Con  of real
              | Neg  of Expr
              | Plus of Expr * Expr
              | Mult of Expr * Expr
              | Exp  of Expr         (* e^x *)
              | Sin  of Expr         (* sin x *)
              | Cos  of Expr         (* cos x *)
*)

datatype Labled = datatype Expr.Labled
val & = Expr.&
                               
fun zipWith f xs ys = Vector.mapi (fn (i, x) => f(x, Vector.sub(ys, i))) xs


fun zeroS n = Vector.tabulate(n, fn _ => Expr.const 0.0)
fun directionS n i = Vector.tabulate(n, fn j => if i = j then Expr.const 1.0 else Expr.const 0.0)
fun scalarS x v = Vector.map (fn e => &(Mult(x, e))) v

                             
fun diff (labled as (_, expr)) n =
    case expr of
        X(_, i)      => directionS n i
      | Con _        => zeroS n
      | Neg e        => Vector.map (& o Neg) (diff e n)
      | Plus(e1, e2) => zipWith (& o Plus) (diff e1 n) (diff e2 n)
      | Mult(e1, e2) => zipWith (& o Plus)
                                (scalarS e1 (diff e2 n))
                                (scalarS e2 (diff e1 n))
      | Exp e        => scalarS labled (diff e n)
      | Sin e        => scalarS (&(Cos e)) (diff e n)
      | Cos e        => scalarS (&(Neg(&(Sin e)))) (diff e n)


fun dumbAD xs exp = Vector.map (fn e => Expr.eval e xs) (diff exp (Vector.length xs))

fun zero n = Vector.tabulate(n, fn _ => 0.0)

fun direction n i = Vector.tabulate(n, fn j => if i = j then 1.0 else 0.0)

fun scalar x v = Vector.map (fn e => x * e) v


type dualnum = real * real vector (* the result and the derivative *)


fun forward xs expr =
  let val n = Vector.length xs
      (* diffEval : Expr -> dualnum *)
      fun diffEval (_, expr) =
        case expr of
            X(_, i)      => (Expr.lookup xs i, direction n i)
          | Con c        => (c, zero n)
          | Neg e        => let val (ex, ed) = diffEval e
                            in (~ex, Vector.map ~ ed) end
          | Plus(e1, e2) => let val (ex1, ed1) = diffEval e1
                                val (ex2, ed2) = diffEval e2
                            in (ex1 + ex2, zipWith op+ ed1 ed2) end
          | Mult (e, e') => let val (ex, ed)   = diffEval e
                                val (ex', ed') = diffEval e'
                            in (ex * ex', zipWith op+ (scalar ex ed')
                                                      (scalar ex' ed)) end
          | Exp e        => let val (ex, ed) = diffEval e
                                val exp_ex = Math.exp ex
                            in (exp_ex, scalar exp_ex ed) end
          | Sin e        => let val (ex, ed) = diffEval e
                            in  (Math.sin ex, scalar (Math.cos ex) ed) end
          | Cos e        => let val (ex, ed) = diffEval e
                            in  (Math.cos ex, scalar (~(Math.sin ex)) ed) end
  in #2(diffEval expr) end

