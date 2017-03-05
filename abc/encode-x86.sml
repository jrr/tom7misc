(* Encoding of X86 instructions into bytes. This is functorized
   internally to make the encoded_size function as fast as possible
   (it just counts bytes instead of actually making a vector); this
   function is used heavily during the search procedure to create the
   literal-loading table. *)

(* XXX hide this functor, I guess using mlb *)

functor EncodeX86Fn(val Exn : string -> exn
                    type vec
                    val vec : Word8.word list -> vec
                    val @@ : vec * vec -> vec) =
struct
  (* Maybe should be functor arg... *)
  infix @@

  val << = Word8.<<
  val >> = Word8.>>
  val || = Word8.orb
  infix << || >>

  type word8 = Word8.word
  structure X = X86

  fun decode_size X.S8 : word8 = 0w0
    (* The prefix byte, decoded separately, determines
       operand size. *)
    | decode_size X.S16 = 0w1
    | decode_size X.S32 = 0w1

  fun immediate_size (X.I8 _) = X.S8
    | immediate_size (X.I16 _) = X.S16
    | immediate_size (X.I32 _) = X.S32

  (* Decode a sized immediate. Little-endian.
     XXX verify that this is the correct encoding.
     *)
  fun decode_immediate (X.I8 w) = vec [w]
    | decode_immediate (X.I16 ww) =
    vec [Word8.fromInt (Word16.toInt (Word16.andb (ww, Word16.fromInt 255))),
         Word8.fromInt (Word16.toInt (Word16.andb (Word16.>> (ww, 0w8),
                                                   Word16.fromInt 255)))]
    | decode_immediate (X.I32 w4) =
    vec
    [Word8.fromInt (Word32.toInt (Word32.andb (w4, 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w8), 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w16), 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w24), 0wxFF)))]

  fun decode_reg r : word8 =
    case r of
      X.A => 0w0
    | X.C => 0w1
    | X.D => 0w2
    | X.B => 0w3
    | X.AH_SP => 0w4
    | X.CH_BP => 0w5
    | X.DH_SI => 0w6
    | X.BH_DI => 0w7

  (* This can only return S16 or S32 *)
  fun decode_multireg mr : X.size * word8 =
    case mr of
      X.AX => (X.S16, 0w0)
    | X.CX => (X.S16, 0w1)
    | X.DX => (X.S16, 0w2)
    | X.BX => (X.S16, 0w3)
    | X.SP => (X.S16, 0w4)
    | X.BP => (X.S16, 0w5)
    | X.SI => (X.S16, 0w6)
    | X.DI => (X.S16, 0w7)
    | X.EAX => (X.S32, 0w0)
    | X.ECX => (X.S32, 0w1)
    | X.EDX => (X.S32, 0w2)
    | X.EBX => (X.S32, 0w3)
    | X.ESP => (X.S32, 0w4)
    | X.EBP => (X.S32, 0w5)
    | X.ESI => (X.S32, 0w6)
    | X.EDI => (X.S32, 0w7)

  (* Return unshifted (prefix bytes, mod, rm, suffix bytes) *)
  fun decode_modrm (X.CTX ctx) modrm : (vec * word8 * word8 * vec) =
    let
      val (pfx_addr16, pfx_addr32) = if #default_32 ctx
                                     then (vec [0wx67], vec [])
                                     else (vec [], vec [0wx67])
    in
      case modrm of
        X.IND_EAX => (pfx_addr32, 0w0, 0w0, vec [])
      | X.IND_ECX => (pfx_addr32, 0w0, 0w1, vec [])
      | X.IND_EDX => (pfx_addr32, 0w0, 0w2, vec [])
      | X.IND_EBX => (pfx_addr32, 0w0, 0w3, vec [])
      | X.IND_SIB sib => raise Exn "unimplemented"
      | X.DISP32 w32 => raise Exn "unimplemented"
      | X.IND_ESI => (pfx_addr32, 0w0, 0w6, vec [])
      | X.IND_EDI => (pfx_addr32, 0w0, 0w7, vec [])
      | X.IND_EAX_DISP8 w8 => (pfx_addr32, 0w1, 0w0, vec [w8])
      | X.IND_ECX_DISP8 w8 => (pfx_addr32, 0w1, 0w1, vec [w8])
      | X.IND_EDX_DISP8 w8 => (pfx_addr32, 0w1, 0w2, vec [w8])
      | X.IND_EBX_DISP8 w8 => (pfx_addr32, 0w1, 0w3, vec [w8])
      | X.IND_SIB_DISP8 (sib, w8) => raise Exn "unimplemented"
      | X.IND_EBP_DISP8 w8 => (pfx_addr32, 0w1, 0w5, vec [w8])
      | X.IND_ESI_DISP8 w8 => (pfx_addr32, 0w1, 0w6, vec [w8])
      | X.IND_EDI_DISP8 w8 => (pfx_addr32, 0w1, 0w7, vec [w8])
      (* These are all always non-printable *)
      | X.IND_EAX_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_ECX_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_EDX_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_EBX_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_SIB_DISP32 (sib, w32) => raise Exn "unimplemented"
      | X.IND_EBP_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_ESI_DISP32 w32 => raise Exn "unimplemented"
      | X.IND_EDI_DISP32 w32 => raise Exn "unimplemented"
      | X.Register r =>
          (* Pretty sure address size prefix does nothing *)
          (vec [], 0w3, decode_reg r, vec[])

      (* XXX implement the other 16-bit ones! *)
      | X.IND_SI => (pfx_addr16, 0w0, 0w4, vec [])
      | X.IND_DI => (pfx_addr16, 0w0, 0w5, vec [])
      | X.IND_BX => (pfx_addr16, 0w0, 0w7, vec [])
    end

  (* Returns (prefix, dir, mod, reg, rm, suffix)
     where dir, mod, reg, r/m are unshifted bits
     and prefix and suffix are (typically empty) vectors of bytes *)
  fun decode_args ctx (X.<-(reg, modrm)) :
    (vec * word8 * word8 * word8 * word8 * vec) =
    let val (prefix, md, rm, suffix) = decode_modrm ctx modrm
    in (prefix, 0w1, md, decode_reg reg, rm, suffix)
    end
    | decode_args ctx (X.<~(modrm, reg)) =
    let val (prefix, md, rm, suffix) = decode_modrm ctx modrm
    in (prefix, 0w0, md, decode_reg reg, rm, suffix)
    end

  (* This is the operand-size prefix. The address-size prefix is
     emitted by decode_modrm. *)
  fun get_operand_pfx _ X.S8 = vec[]
    | get_operand_pfx (X.CTX { default_32 = true, ...}) X.S16 = vec [0wx66]
    | get_operand_pfx (X.CTX { default_32 = false, ...}) X.S16 = vec []
    | get_operand_pfx (X.CTX { default_32 = true, ...}) X.S32 = vec []
    | get_operand_pfx (X.CTX { default_32 = false, ...}) X.S32 = vec [0wx66]

  fun encode ctx (i : X.ins) : vec =
    let
      (* Encode a normal instruction (e.g. AND).
         base_opcode is the actual first opcode in the group; not shifted. *)
      fun encode_normal (base_opcode : word8) (size, args) =
        let
          val prefix = get_operand_pfx ctx size
          val opcode = base_opcode >> 0w2
          val s = decode_size size
          val (prefix2, d, md, reg, rm, suffix) = decode_args ctx args
        in
          prefix @@
          prefix2 @@
          vec [(opcode << 0w2) || (d << 0w1) || s,
               (md << 0w6) || (reg << 0w3) || rm] @@
          suffix
        end

      (* Encode a normal instruction that takes an immediate
         (e.g AND_A_IMM).
         base_opcode is the actual first of the two opcodes; not shifted. *)
      fun encode_normal_imm (base_opcode : word8) imm =
        let
          val size = immediate_size imm
          val prefix = get_operand_pfx ctx size
          val opcode = base_opcode >> 0w2
          val s = decode_size size
        in
          prefix @@
          vec [(opcode << 0w2) || s] @@
          decode_immediate imm
        end

      fun encode_multireg_based (base_opcode : word8) mr =
        let
          val (size, r) = decode_multireg mr
          val _ = size = X.S8 andalso raise Exn "impossible"
          val prefix = get_operand_pfx ctx size
        in
          prefix @@
          vec [Word8.+(base_opcode, r)]
        end
    in
      case i of
        X.AND (size, args) => encode_normal 0wx20 (size, args)
      | X.AND_A_IMM imm => encode_normal_imm 0wx24 imm
      | X.SUB (size, args) => encode_normal 0wx28 (size, args)
      | X.SUB_A_IMM imm => encode_normal_imm 0wx2C imm
      | X.XOR (size, args) => encode_normal 0wx30 (size, args)
      | X.XOR_A_IMM imm => encode_normal_imm 0wx34 imm
      | X.CMP (size, args) => encode_normal 0wx38 (size, args)
      | X.CMP_A_IMM imm => encode_normal_imm 0wx3C imm
      | X.INC mr => encode_multireg_based 0wx40 mr
      | X.DEC mr => encode_multireg_based 0wx48 mr
      | X.PUSH mr => encode_multireg_based 0wx50 mr
      | X.POP mr => encode_multireg_based 0wx58 mr
      | X.PUSH_IMM imm =>
          let val pfx = get_operand_pfx ctx (immediate_size imm)
          in
            case imm of
              X.I8 w => pfx @@ vec [0wx6A, w]
            | _ => pfx @@ vec [0wx68] @@ decode_immediate imm
          end
      | X.JO w => vec [0wx70, w]
      | X.JNO w => vec [0wx71, w]
      | X.JB w => vec [0wx72, w]
      | X.JAE w => vec [0wx73, w]
      | X.JZ w => vec [0wx74, w]
      | X.JNZ w => vec [0wx75, w]
      | X.JBE w => vec [0wx76, w]
      | X.JA w => vec [0wx77, w]
      | X.JS w => vec [0wx78, w]
      | X.JNS w => vec [0wx79, w]
      | X.JP w => vec [0wx7a, w]
      | X.JNP w => vec [0wx7b, w]
      | X.JL w => vec [0wx7c, w]
      | X.JGE w => vec [0wx7d, w]
      | X.JLE w => vec [0wx7e, w]

      (* out of gamut *)
      | X.MOV (size, args) => encode_normal 0wx88 (size, args)
      | X.NOP => vec [0wx90]
      | X.INT w => vec [0wxCD, w]
      | X.DB w => vec [w]
      | X.COMMENT _ => vec []

    (*
      | _ => raise Exn "unimplemented ins"
        *)
    end

end

structure EncodeX86 :> ENCODEX86 =
struct

  exception EncodeX86 of string

  structure SizeEncode =
  EncodeX86Fn(val Exn = EncodeX86
              type vec = int
              val vec = List.length
              val @@ = op+)
  structure VecEncode =
  EncodeX86Fn(val Exn = EncodeX86
              type vec = Word8Vector.vector
              val vec = Word8Vector.fromList
              fun @@(v1, v2) = Word8Vector.concat [v1, v2])


  (* Returns the encoded size of the instruction in bytes. *)
  fun encoded_size ctx (i : X86.ins) : int =
    SizeEncode.encode ctx i

  open VecEncode

  fun encodelist ctx il = Word8Vector.concat (map (encode ctx) il)
end
