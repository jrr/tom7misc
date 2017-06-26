
signature GROUP =
sig

   exception Group of string

   type entry = {name : string,
                 gid : int,
                 members : string list}

   (* initialize the database from the supplied
      filename, typically /etc/group *)
   val readdb : string -> unit

   (* return info for a group name *)
   val lookup : string -> entry option    

   val lookupgid : int -> entry option

   (* get all the group memberships for a login name *)
   val memberships : string -> entry list

end
