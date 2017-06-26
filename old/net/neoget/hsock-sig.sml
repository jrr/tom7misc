
(* simplified inet interface *)

signature HSOCK =
sig

  type address
  type connection

  exception Error

  (* "128.2.1.2" or "spacebar.org" *)
  val address : string -> address

  (* connect to an address and port *)
  val connect : address -> int -> connection
    
  val send : connection -> string -> unit

  (* read the entire socket until EOF *)
  val recvall : connection -> string

  val close : connection -> unit

end
