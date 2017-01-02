structure ToASM :> TOASM =
struct

  structure C = CIL
  structure A = ASM

  exception ToASM of string

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  datatype sz = S8 | S16 | S32
  fun szbytes S8 = 1
    | szbytes S16 = 2
    | szbytes S32 = 4

  (* Signedness doesn't matter any more -- the representation is
     the same and the operations are explicit. *)
  fun ctypsize (C.Word32 _) = S32
    | ctypsize (C.Word16 _) = S16
    | ctypsize (C.Word8 _) = S8
    (* All pointers are 16-bit offsets into DS. *)
    | ctypsize (C.Pointer _) = S16
    (* Labels in code segment, not addresses but no way for
       them to be bigger than 16 bits! *)
    | ctypsize (C.Code _) = S16
    | ctypsize (C.Struct _) = raise ToASM "unimplemented: structs"

  (* XXX maybe need Pointwise for CIL... *)
  (* Apply f to every local in the statement. *)
  (* XXX How to get local's size? *)
(*
  fun apploc_stmt C.End = ()
    | apploc_stmt (C.Return v) = apploc_val v
    | apploc_stmt (C.Bind (_, e, s)) = (apploc_exp e; apploc_stmt s)
    | apploc_stmt (C.Store (v, _, vv, st
  (* Store(address, width, value, rest) *)
  | Store of value * width * value * stmt
  (* GotoIf(cond, true-label, else-branch). *)
  | GotoIf of value * string * stmt
  | Return of value
  | End
*)

  fun doproc (name, C.Func
              { args : (string * C.typ) list,
                ret : C.typ,
                body : string,
                blocks : (string * C.stmt) list }) : A.proc =
    let
      (* XXX check no overlap between these two... *)
      val argsizes : (string * sz) list = ListUtil.mapsecond ctypsize args
      val localsizes : sz SM.map ref = ref SM.empty

      (* XXX crawl through blocks to find all locals *)

      val localsizes = !localsizes

      (* Arguments and locals are not on the normal stack. They go in the
         data segment and are accessed through some base pointer (EDX?
         ESI?) which probably will be pointing 0x20 bytes before the
         frame. We do this because it's possible to take their
         address, and we represent pointers as 16-bit offsets into the
         data segment. *)
        (* XXX we can just read nextpos between the allocs below *)
      val argbytes =
        List.foldl (fn ((_, s), b) => b + szbytes s) 0 argsizes
      val localbytes =
        SM.foldli (fn (_, s, b) => b + szbytes s) 0 localsizes
      val nextpos = ref 0
      (* We use a totally packed layout, ignoring the fact that bad
         alignment causes lost cycles. We are much more constrained
         by code/data size (64k) than clock speed (4+ GHz).
         PERF: Rearranging locals and args to get better alignment
         would be pretty easy, and free from code/data size perspective. *)
      val offsets : (string * int) list ref = ref nil
      fun alloc (s, sz) =
        let in
          offsets := (s, !nextpos) :: !offsets;
          nextpos := !nextpos + szbytes sz
        end
    in
      (* Args must come first *)
      List.app alloc argsizes;
      SM.appi alloc localsizes;
      print ("Frame for proc " ^ name ^ ":\n" ^
             String.concat (map (fn (s, off) => "  " ^ Int.toString off ^
                                 ": " ^ s ^ "\n") (rev (!offsets))));
      A.Proc { name = name,
               blocks = (* XXX ablocks *) nil,
               offsets = !offsets,
               argbytes = argbytes,
               localbytes = localbytes }
    end

  (* XXX globals ! *)
  fun toasm (C.Program { functions, globals = _ }) =
     A.Program { main = "main",
                 procs = map doproc functions }
end