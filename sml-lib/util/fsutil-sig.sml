
(* Utilities for manipulating files, paths,
   etc. Needs stringutil. *)

signature FSUTIL =
sig

  (* imperative streams *)
  type 'a stream = unit -> 'a option

  (* does the file exist? *)
  val exists : string -> bool
  (* Given some base name (file/path prefix), find an extension of that
     path that does not exist. Not thread safe. *)
  val tempfilename : string -> string

  type fileinfo = { dir : bool,
                    lnk : bool,
                    mode : Posix.FileSys.S.mode,
                    nlink : int,
                    mtime : Time.time,
                    size : Position.int,
                    uid : Posix.FileSys.uid,
                    gid : Posix.FileSys.gid,
                    name : string }

  (* applies the function to each entry in a directory *)
  val dirapp : (fileinfo -> unit) -> string -> unit

  (* open a directory as a proper memoized stream. Also
     gets . and .. entries. *)
  val dirstream : string -> fileinfo stream

  (* performs filename globbing, like "echo string" would in bash.
     (though only * is supported). Furthermore, this will only allow
     *s in the filename position (not the dir), so */*.txt won't work.

     Note that names in fileinfo are relative to the glob path, so
     the caller will need to concatenate the directory (if any). *)
  val glob : string -> fileinfo stream

  (* As above, but provide an explicit directory. The caller needs to
     concatenate this with the returned fileinfos anyway. *)
  val glob_in_dir : string -> string -> fileinfo stream

  (* Just get the filenames eagerly. These names are concatenated with
     any directory from the glob string, so that they can be opened
     directly. Only returns actual files (where dir = false). *)
  val globfiles : string -> string list

  (* applies the dirhandler as if the argument was passed
     to ls with globbing (fundamental difference is that
     something like ls "*.txt" would list all the .txt
     files in the current dir, while dirapp would list
     only the contents of a dir called "*.txt"). *)
  val ls : (fileinfo -> unit) -> string -> unit

  (* ie "rwxr-xr-x" *)
  val modestring : Posix.FileSys.S.mode -> string

  (* dirplus "/home" "file" or
     dirplus "/home/" "file"
     both return "/home/file"
     (Uses OS conventions, so \ on Windows.) *)
  val dirplus : string -> string -> string

  exception Seek

  (* this is an unfortunate hack. The hack for
     skipo (outstream) would be even worse, so
     it's not implemented. *)

  (* skipi n s reads and discards n bytes *)
  val skipi : int -> TextIO.instream -> unit

  (* retrieve the size of a file *)
  val filesize : string -> int
  val filesize64 : string -> Position.int

  (* splitext "dir/file.0.txt" = ("dir/file.0", "txt") *)
  val splitext : string -> string * string

  (* chdir_excursion s f
     s is a path to a file, either relative or absolute.
     chdirs to the directory holding s for the purpose
     of executing f, and then restores to the previous
     directory. This works even if f raises an exception.

     f is supplied with the base filename in s, which
     will now be appropriate for calls to "open" *)
  val chdir_excursion : string -> (string -> 'a) -> 'a

end
