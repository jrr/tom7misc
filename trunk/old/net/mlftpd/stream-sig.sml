
signature STREAM =
sig

  type 'a stream

  datatype 'a front = Nil | Cons of 'a * 'a stream

  val force : 'a stream -> 'a front
  val delay : (unit -> 'a front) -> 'a stream

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val app : ('a -> 'b) -> 'a stream -> unit
   
  val filter : ('a -> bool) -> 'a stream -> 'a stream
 
  val tolist : 'a stream -> 'a list

end