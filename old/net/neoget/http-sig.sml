
signature HTTP =
sig

  (* get useragent cookie referrer port host path
     returns header, and body of the entire result. *)
  val get :
    string -> string -> string -> int -> string -> string -> 
       string * string

  (* post useragent cookie referrer port host path querystring
     returns header, and body of entire result. *)
  val post :
    string -> string -> string -> int -> string -> string -> string ->
       string * string


  (* escapes url-unsafe characters in a string *)
  val urlencode : string -> string

  (* pass in an alist of (name, value) pairs, and this
     creates the appropriate HTTP query string,
     suitable for get url?querystring or
     posting *)
  val querystring : (string * string) list -> string

end
