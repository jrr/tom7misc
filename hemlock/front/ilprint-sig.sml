
signature ILPRINT =
sig

    val ttol : IL.typ -> Layout.layout
    val etol : IL.exp -> Layout.layout
    val dtol : IL.dec -> Layout.layout

end
