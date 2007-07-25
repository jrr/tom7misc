
signature PATTERN =
sig

    exception Pattern of string

    (* elaborate user elab elabt ctx world loc (ob,arms,def) 
       ob: must be variables

       returns elaborated pattern match and its type
       *)
    val elaborate : bool -> (Context.context -> IL.world -> EL.exp -> IL.exp * IL.typ) ->
        (Context.context -> Pos.pos -> EL.typ -> IL.typ) ->
        Context.context -> IL.world -> Pos.pos ->
                           string list * 
                           (EL.pat list * EL.exp) list * 
                           (unit -> EL.exp) -> 
        IL.exp * IL.typ

end
