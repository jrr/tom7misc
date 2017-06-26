
signature VARIABLE =
sig
    type var
        
    val newvar : unit -> var
    val namedvar : string -> var
    val alphavary : var -> var
        
    val eq : var * var -> bool
    val compare : var * var -> order
        
    val basename : var -> string

    val tostring : var -> string

    structure Map : ORD_MAP where type Key.ord_key = var
end
