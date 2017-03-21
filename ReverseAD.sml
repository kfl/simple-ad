
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


fun zipWith f xs ys = Vector.mapi (fn (i, x) => f(x, Vector.sub(ys, i))) xs

fun zeroS n = Vector.tabulate(n, fn _ => Con 0.0)

fun directionS n i x = Vector.tabulate(n, fn j => if i = j then Con x else Con 0.0)

fun scalarS x v = Vector.map (fn e => Mult(x, e)) v


(* evalDecorate label each sub expression with its evaluation *)
fun evalDecorate xs expr =
  let fun decorate (_, expr) =
        case expr of 
            X(vlab, i)   => (lookup xs i, X(vlab, i))
          | Con c        => (c, Con c)
          | Neg e        => let val d as (ex, _) = decorate e
                            in (~ex, Neg d) end
          | Plus(e1, e2) => let val d1 as (ex1, _) = decorate e1
                                val d2 as (ex2, _) = decorate e2
                            in (ex1 + ex2, Plus(d1, d2)) end
          | Mult(e1, e2) => let val d1 as (ex1, _) = decorate e1
                                val d2 as (ex2, _) = decorate e2
                            in (ex1 * ex2, Mult(d1, d2)) end
          | Exp e        => let val d as (ex, _) = decorate e
                            in (Math.exp ex, Exp d) end
          | Sin e        => let val d as (ex, _) = decorate e
                            in  (Math.sin ex, Sin d) end
          | Cos e        => let val d as (ex, _) = decorate e
                            in  (Math.cos ex, Cos d) end
  in decorate expr end

(* sensibilityDecorate pushes sensibility to variable leafs *)
fun sensibilityDecorate expr =
  let fun decorate sens (elab, expr) =
        (elab, case expr of 
                   X(_, i)      => X(sens, i)
                 | Con c        => Con c
                 | Neg e        => let val d = decorate (~sens) e
                                   in Neg d end
                 | Plus(e1, e2) => let val d1 = decorate sens e1
                                       val d2 = decorate sens e2
                                   in  Plus(d1, d2) end
                 | Mult(e1 as (r1, _),
                        e2 as (r2, _)) =>
                                   let val d1 = decorate (r2 * sens) e1
                                       val d2 = decorate (r1 * sens) e2
                                   in  Mult(d1, d2) end
                 | Exp (e as (r, _)) =>
                                   let val d = decorate (Math.exp r * sens) e
                                   in  Exp d end
                 | Sin (e as (r, _)) =>
                                   let val d = decorate (Math.cos r * sens) e
                                   in  Sin d end
                 | Cos (e as (r, _)) =>
                                   let val d = decorate (~(Math.sin r) * sens) e
                                   in  Cos d end)
  in decorate 1.0 expr end

fun zero n = Vector.tabulate(n, fn _ => 0.0)
fun addAt xs i v = Vector.mapi (fn(j, x) => if i = j then x+v else x) xs

fun reduce n expr =
  let fun fold (_, expr) acc =
        case expr of
            X(sens, i) => addAt acc i sens
          | Con _ => acc
          | Neg e => fold e acc
          | Plus(e1, e2) => fold e2 (fold e1 acc)
          | Mult(e1, e2) => fold e2 (fold e1 acc)
          | Exp e => fold e acc
          | Sin e => fold e acc
          | Cos e => fold e acc
  in  fold expr (zero n)
  end

fun reverse xs expr =
  let val n = Vector.length xs
      val ed = evalDecorate xs expr
      val sd = sensibilityDecorate ed
  in  reduce n sd
  end
