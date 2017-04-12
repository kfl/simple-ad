structure Reverse =
struct
local
    datatype Labelled = datatype Expr.Labelled
in

(* evalDecorate label each sub expression with its evaluation *)
fun evalDecorate xs expr =
  let fun decorate (_, expr) =
        case expr of
            X(vlab, i)   => (Expr.lookup xs i, X(vlab, i))
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

(* derivativeDecorate pushes derivative to variable leaves *)
fun derivativeDecorate expr =
  let fun decorate deri (elab, expr) =
        (elab, case expr of
                   X(_, i)      => X(deri, i)
                 | Con c        => Con c
                 | Neg e        => let val d = decorate (~deri) e
                                   in Neg d end
                 | Plus(e1, e2) => let val d1 = decorate deri e1
                                       val d2 = decorate deri e2
                                   in  Plus(d1, d2) end
                 | Mult(e1 as (r1, _),
                        e2 as (r2, _)) =>
                                   let val d1 = decorate (r2 * deri) e1
                                       val d2 = decorate (r1 * deri) e2
                                   in  Mult(d1, d2) end
                 | Exp (e as (r, _)) =>
                                   let val d = decorate (Math.exp r * deri) e
                                   in  Exp d end
                 | Sin (e as (r, _)) =>
                                   let val d = decorate (Math.cos r * deri) e
                                   in  Sin d end
                 | Cos (e as (r, _)) =>
                                   let val d = decorate (~(Math.sin r) * deri) e
                                   in  Cos d end)
  in decorate 1.0 expr end

fun zero n = Vector.tabulate(n, fn _ => 0.0)
fun addAt xs i v = Vector.mapi (fn(j, x) => if i = j then x+v else x) xs

fun reduce n expr =
  let fun fold (_, expr) acc =
        case expr of
            X(deri, i)   => addAt acc i deri
          | Con _        => acc
          | Neg e        => fold e acc
          | Plus(e1, e2) => fold e2 (fold e1 acc)
          | Mult(e1, e2) => fold e2 (fold e1 acc)
          | Exp e        => fold e acc
          | Sin e        => fold e acc
          | Cos e        => fold e acc
  in  fold expr (zero n)
  end

fun reverse xs expr =
  let val n = Vector.length xs
      val ed = evalDecorate xs expr
      val sd = derivativeDecorate ed
  in  reduce n sd
  end






(* derivativeDecorate and reduce can be fused *)
fun derivativeReduce get_eval n expr =
  let infix +=
      fun (arr, i) += x = Array.update(arr, i, Array.sub(arr, i) + x)

      val result = Array.array(n, 0.0)

      fun trav deri (_, expr) =
        case expr of
            X(_, i)             => (result, i) += deri
          | Con c               => ()
          | Neg e               => trav (~deri) e
          | Plus(e1, e2)        => ( trav deri e1
                                   ; trav deri e2
                                   )
          | Mult(e1 as (lab1, _), e2 as (lab2, _)) =>
            let val r1 = get_eval lab1
                val r2 = get_eval lab2
            in trav (r2 * deri) e1
             ; trav (r1 * deri) e2
            end
          | Exp (e as (lab, _)) => trav (Math.exp (get_eval lab) * deri) e
          | Sin (e as (lab, _)) => trav (Math.cos (get_eval lab) * deri) e
          | Cos (e as (lab, _)) => trav (~(Math.sin (get_eval lab)) * deri) e
  in   trav 1.0 expr
     ; Array.vector result
  end

fun reverse_fused xs expr =
  let val n = Vector.length xs
      val ed = evalDecorate xs expr
  in  derivativeReduce (fn x => x) n ed
  end






(* Problem: evalDecorate destroys sharing *)
(* evalUpdate update each sub expression label with its evaluation *)
fun evalUpdate xs expr =
  let fun eval (ref(SOME v), _) = v
        | eval (lab, expr) =
          let val res =
                  case expr of
                      X(_, i)      => Expr.lookup xs i
                    | Con c        => c
                    | Neg e        => ~(eval e)
                    | Plus(e1, e2) => eval e1 + eval e2
                    | Mult(e1, e2) => eval e1 * eval e2
                    | Exp e        => Math.exp (eval e)
                    | Sin e        => Math.sin (eval e)
                    | Cos e        => Math.cos (eval e)
          in  lab := SOME res
            ; res
          end
  in  eval expr
  end


fun reverse_imp xs expr =
  let val n = Vector.length xs
      val _ = evalUpdate xs expr
      fun get_eval (ref(SOME r)) = r
        | get_eval _ = raise Fail "Unevaluated expression found"
  in  derivativeReduce get_eval n expr
  end

end
end
