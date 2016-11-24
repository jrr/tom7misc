structure X86 =
struct

  exception X86 of string

  type word8 = Word8.word
  infix @@
  val vec : word8 list -> word8 vector = Vector.fromList
  fun (v1 : word8 vector) @@ (v2 : word8 vector) =
    Vector.concat [v1, v2]
  val << = Word8.<<
  val >> = Word8.>>
  val || = Word8.orb
  infix << || >>

  (* Operand size. Determined by opcode size bit and (maybe) default
     default operand / size prefix byte. *)
  datatype size = S8 | S16 | S32

  (* Context for encoding of opcodes. We need to know if the default
     operand size is 32-bit. *)
  datatype ctx = CTX of { default_32 : bool }

  datatype sib_index_reg =
    (* 101 is illegal *)
    INDEX_A | INDEX_C | INDEX_D | INDEX_B | INDEX_BP | INDEX_SI | INDEX_DI
  datatype sib_base_reg =
      BASE_A | BASE_C | BASE_D | BASE_B | BASE_SP | BASE_DISP_OR_EBP
    | BASE_SI | BASE_DI

  type sib =
    { (* must be 1, 2, 4, 8 *)
      scale : int,
      (* Always the "long" versions of the
         registers. Note that AH_SP is illegal here. *)
      index : sib_index_reg,
      base : sib_base_reg }


  (* The reg field in the mod-reg-rm byte. This is not the
     full set of x86 registers. Note that the meaning of the
     last four differs based on whether this is a BYTE or MULTI
     instruction. For BYTE instructions, it's the high byte
     in the registers, for MULTI, it's the alternate registers. *)
  (* XXX maybe should just list them out and then something
     checks for validity? *)
  datatype reg = A | C | D | B | AH_SP | CH_BP | DH_SI | BH_DI

  datatype modrm =
    (* mod=00. Indirect through register,
       or special stuff *)
    IND_EAX
  | IND_ECX
  | IND_EDX
  | IND_EBX
  | IND_SIB of sib
  | DISP32 of Word32.word
  | IND_ESI
  | IND_EDI
  (* mod=01. One-byte displacement.
     Despite being of type word8, this is a signed two's-complement
     displacement. *)
  | IND_EAX_DISP8 of Word8.word
  | IND_ECX_DISP8 of Word8.word
  | IND_EDX_DISP8 of Word8.word
  | IND_EBX_DISP8 of Word8.word
  | IND_SIB_DISP8 of sib * Word8.word
  | IND_EPB_DISP8 of Word8.word
  | IND_ESI_DISP8 of Word8.word
  | IND_EDI_DISP8 of Word8.word
  (* mod=10. Four-byte displacement. As above, this is a signed
     two's-complement number. Should be in system byte order
     (so 0wx01 = 1), but will be encoded little-endian. *)
  | IND_EAX_DISP32 of Word32.word
  | IND_ECX_DISP32 of Word32.word
  | IND_EDX_DISP32 of Word32.word
  | IND_EBX_DISP32 of Word32.word
  | IND_SIB_DISP32 of sib * Word32.word
  | IND_EPB_DISP32 of Word32.word
  | IND_ESI_DISP32 of Word32.word
  | IND_EDI_DISP32 of Word32.word
  (* mod=11. Same encoding as reg. *)
  | Register of reg


  (* XXX this is crazy right? *)
  datatype args =
    <- of reg * modrm
  | <~ of modrm * reg

  infix <- <~

  datatype immediate =
    I8 of Word8.word
  | I16 of Word16.word
  | I32 of Word32.word

  (* Incomplete set of X86 opcodes. *)
  datatype ins =
    AND of size * args
    (* AND al/ax/eax <- imm8 *)
  | AND_A_IMM of immediate
    (* ... *)

  fun decode_size S8 : word8 = 0w0
    (* The prefix byte, decoded separately, determines
       operand size. *)
    | decode_size S16 = 0w1
    | decode_size S32 = 0w1

  fun immediate_size (I8 _) = S8
    | immediate_size (I16 _) = S16
    | immediate_size (I32 _) = S32

  (* Decode a sized immediate. Little-endian.
     XXX verify that this is the correct encoding.
     *)
  fun decode_immediate (I8 w) = vec [w]
    | decode_immediate (I16 ww) =
    vec [Word8.fromInt (Word16.toInt (Word16.andb (ww, Word16.fromInt 255))),
         Word8.fromInt (Word16.toInt (Word16.andb (Word16.>> (ww, 0w8),
                                                   Word16.fromInt 255)))]
    | decode_immediate (I32 w4) =
    vec
    [Word8.fromInt (Word32.toInt (Word32.andb (w4, 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w8), 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w16), 0wxFF))),
     Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w4, 0w24), 0wxFF)))]

  fun decode_reg r : word8 =
    case r of
      A => 0w0
    | C => 0w1
    | D => 0w2
    | B => 0w3
    | AH_SP => 0w4
    | CH_BP => 0w5
    | DH_SI => 0w6
    | BH_DI => 0w7

  fun decode_modrm modrm : (word8 * word8 * word8 vector) =
    case modrm of
      IND_EAX => raise X86 "unimplemented"
    | IND_ECX => raise X86 "unimplemented"
    | IND_EDX => raise X86 "unimplemented"
    | IND_EBX => raise X86 "unimplemented"
    | IND_SIB sib => raise X86 "unimplemented"
    | DISP32 w32 => raise X86 "unimplemented"
    | IND_ESI => raise X86 "unimplemented"
    | IND_EDI => raise X86 "unimplemented"
    | IND_EAX_DISP8 w8 => raise X86 "unimplemented"
    | IND_ECX_DISP8 w8 => raise X86 "unimplemented"
    | IND_EDX_DISP8 w8 => raise X86 "unimplemented"
    | IND_EBX_DISP8 w8 => raise X86 "unimplemented"
    | IND_SIB_DISP8 (sib, w8) => raise X86 "unimplemented"
    | IND_EPB_DISP8 w8 => raise X86 "unimplemented"
    | IND_ESI_DISP8 w8 => raise X86 "unimplemented"
    | IND_EDI_DISP8 w8 => raise X86 "unimplemented"
    | IND_EAX_DISP32 w32 => raise X86 "unimplemented"
    | IND_ECX_DISP32 w32 => raise X86 "unimplemented"
    | IND_EDX_DISP32 w32 => raise X86 "unimplemented"
    | IND_EBX_DISP32 w32 => raise X86 "unimplemented"
    | IND_SIB_DISP32 (sib, w32) => raise X86 "unimplemented"
    | IND_EPB_DISP32 w32 => raise X86 "unimplemented"
    | IND_ESI_DISP32 w32 => raise X86 "unimplemented"
    | IND_EDI_DISP32 w32 => raise X86 "unimplemented"
    | Register r => (0w3, decode_reg r, vec[])

  (* Returns unshifted bits for
     dir, mod, reg, r/m,
     and a (typically empty) vector of suffix bytes. *)
  fun decode_args (reg <- modrm) :
    (word8 * word8 * word8 * word8 * word8 vector) =
    let val (md, rm, suffix) = decode_modrm modrm
    in (0w1, md, decode_reg reg, rm, suffix)
    end
    | decode_args (modrm <~ reg) =
    let val (md, rm, suffix) = decode_modrm modrm
    in (0w0, md, decode_reg reg, rm, suffix)
    end

  (* This is the operand-size prefix. 0x67 is address-size
     prefix, which I may need to use for some opcodes? XXX *)
  fun get_prefix _ S8 = vec[]
    | get_prefix (CTX { default_32 = true, ...}) S16 = vec [0wx66]
    | get_prefix (CTX { default_32 = false, ...}) S16 = vec[]
    | get_prefix (CTX { default_32 = true, ...}) S32 = vec[]
    | get_prefix (CTX { default_32 = false, ...}) S32 = vec [0wx66]

  fun encode ctx (i : ins) : word8 vector =
    case i of
      AND (size, args) =>
      let
        val prefix = get_prefix ctx size
        val opcode = 0w020 >> 0w2
        val s = decode_size size
        val (d, md, reg, rm, suffix) = decode_args args
      in
        prefix @@
        vec [(opcode << 0w2) || (d << 0w1) || s,
             (md << 0w6) || (reg << 0w3) || rm] @@
        suffix
      end
    | AND_A_IMM imm =>
      let
        val size = immediate_size imm
        val prefix = get_prefix ctx size
        val opcode = 0w24 >> 0w2
        val s = decode_size size
      in
        prefix @@
        vec [(opcode << 0w2) || s] @@
        decode_immediate imm
      end
(*
    | _ => raise X86 "unimplemented ins"
      *)

  val XXX = AND (S32, A <- Register C)
  val XXY = AND (S8, IND_EAX <~ C)
  val XXZ = AND_A_IMM (I32 0wxAABBCCDD)
end
