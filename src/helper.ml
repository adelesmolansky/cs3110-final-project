(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ (acr, ph) ] -> acc ^ pp_elt acr ^ " = " ^ pp_elt ph
      | (acr1, ph1) :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else
            loop (n + 1)
              (acc ^ pp_elt acr1 ^ " = " ^ pp_elt ph1 ^ "; ")
              t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"