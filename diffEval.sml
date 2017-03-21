
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



