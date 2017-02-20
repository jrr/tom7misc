structure AllocateTmps :> ALLOCATETMPS =
struct

  exception AllocateTmps of string

  fun allocate _ = raise AllocateTmps "unimplemented"

end
