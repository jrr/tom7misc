structure X86 =
struct

  exception X86 of string

  type word8 = Word8.word
  type word8vector = Word8Vector.vector
  infix @@
  val vec : word8 list -> Word8Vector.vector = Word8Vector.fromList
  fun (v1 : word8vector) @@ (v2 : word8vector) =
    Word8Vector.concat [v1, v2]
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


  (* The reg field in the mod-reg-rm byte, which is also used in a reg
     when mod/rm=reg, or in register-based opcode sequences like INC
     and POP. This is not the full set of x86 registers. Note that the
     meaning of the last four differs based on whether this is a BYTE
     or MULTI instruction. For BYTE instructions, it's the high byte
     in the registers, for MULTI, it's the alternate registers. *)
  (* XXX maybe should just list them out and then something
     checks for validity? *)
  datatype reg = A | C | D | B | AH_SP | CH_BP | DH_SI | BH_DI

  datatype multireg =
       AX |  CX |  DX |  BX |  SP |  BP |  SI |  DI
    | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI

  (* For indirect r/m (e.g. IND_EBX), these are the 32-bit address
     versions. The 16 bit modes are not listed here, and are sorta
     weird (e.g. bp + di + disp8), but we could support them.
     (XXX we really should -- [BX+disp8] is there and printable,
     and disp16 could even be quite useful.)
     (In 16-bit mode, these need a prefix byte in order to make them
     use the 32-bit registers...) *)
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
     Despite being of SML type word8, this is a signed two's-complement
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
    (* 20-23 AND variants *)
      AND of size * args
    (* 24,25 AND al/ax/eax <- imm *)
    | AND_A_IMM of immediate
    (* 26 ES segment override prefix *)
    (* 27 DAA Decimal Adjust AL after addition
       this instruction is normally useless, but
       might be a shorter way to load certain
       constants into AL? *)
    (* 28-2B SUB variants *)
    | SUB of size * args
    (* 2C,2D SUB al/ax/eax <- imm *)
    | SUB_A_IMM of immediate
    (* 2E CS segment override prefix *)
    (* 2F DAS Decimal Adjust AL after subtraction *)
    (* 30-33 XOR variants *)
    | XOR of size * args
    (* 34,35 XOR al/ax/eax <- imm *)
    | XOR_A_IMM of immediate
    (* 36 SS segment override prefix *)
    (* 37 AAA ASCII Adjust After Addition *)
    (* 38-3B CMP variants *)
    | CMP of size * args
    (* 3C,3D CMP al,ax,eax , imm *)
    | CMP_A_IMM of immediate
    (* 3F DS segment override prefix *)
    (* AAS ASCII Adjust After Subtraction *)
    (* 40-47 INC multibyte register *)
    | INC of multireg
    (* 48-4F DEC multibyte register *)
    | DEC of multireg
    (* 50-57 PUSH multibyte register *)
    | PUSH of multireg
    (* 58-5F POP multibyte register *)
    | POP of multireg
    (* 60,61 PUSH/POP all. XXX useful, but need to look into
       the encoding. *)
    (* 62 BOUND Check array index against bounds *)
    (* 63 ARPL Adjust RPL field of segment selector *)
  (* 64 FS segment override prefix *)
  (* 65 GS segment override prefix *)
  (* 66 operand size override prefix *)
  (* 67 address size override prefix *)
  (* 68 push immediate 16/32 *)
  (* XXX IMUL here *)
  (* XXX JMP short instructions *)

      (* These instructions are out of gamut *)
    | MOV of size * args
    | NOP
    | INT of word8

      (* XXX hax *)
    | DB of word8

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

  (* This can only return S16 or S32 *)
  fun decode_multireg mr : size * word8 =
    case mr of
      AX => (S16, 0w0)
    | CX => (S16, 0w1)
    | DX => (S16, 0w2)
    | BX => (S16, 0w3)
    | SP => (S16, 0w4)
    | BP => (S16, 0w5)
    | SI => (S16, 0w6)
    | DI => (S16, 0w7)
    | EAX => (S32, 0w0)
    | ECX => (S32, 0w1)
    | EDX => (S32, 0w2)
    | EBX => (S32, 0w3)
    | ESP => (S32, 0w4)
    | EBP => (S32, 0w5)
    | ESI => (S32, 0w6)
    | EDI => (S32, 0w7)

  (* Return unshifted (prefix bytes, mod, rm, suffix bytes) *)
  fun decode_modrm (CTX ctx) modrm : (word8vector * word8 * word8 * word8vector) =
    let
      val pfx_addr32 = if #default_32 ctx
                       then vec []
                       else vec [0wx67]
    in
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
      | IND_EBX_DISP8 w8 => (pfx_addr32, 0w1, 0w3, vec [w8])
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
      | Register r =>
          (* Pretty sure address size prefix does nothing *)
          (vec [], 0w3, decode_reg r, vec[])
    end

  (* Returns (prefix, dir, mod, reg, rm, suffix)
     where dir, mod, reg, r/m are unshifted bits
     and prefix and suffix are (typically empty) vectors of bytes *)
  fun decode_args ctx (reg <- modrm) :
    (word8vector * word8 * word8 * word8 * word8 * word8vector) =
    let val (prefix, md, rm, suffix) = decode_modrm ctx modrm
    in (prefix, 0w1, md, decode_reg reg, rm, suffix)
    end
    | decode_args ctx (modrm <~ reg) =
    let val (prefix, md, rm, suffix) = decode_modrm ctx modrm
    in (prefix, 0w0, md, decode_reg reg, rm, suffix)
    end

  (* This is the operand-size prefix. The address-size prefix is
     emitted by decode_modrm. *)
  fun get_operand_prefix _ S8 = vec[]
    | get_operand_prefix (CTX { default_32 = true, ...}) S16 = vec [0wx66]
    | get_operand_prefix (CTX { default_32 = false, ...}) S16 = vec[]
    | get_operand_prefix (CTX { default_32 = true, ...}) S32 = vec[]
    | get_operand_prefix (CTX { default_32 = false, ...}) S32 = vec [0wx66]

  fun encode ctx (i : ins) : word8vector =
    let
      (* Encode a normal instruction (e.g. AND).
         base_opcode is the actual first opcode in the group; not shifted. *)
      fun encode_normal (base_opcode : word8) (size, args) =
        let
          val prefix = get_operand_prefix ctx size
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
          val prefix = get_operand_prefix ctx size
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
          val _ = size = S8 andalso raise X86 "impossible"
          val prefix = get_operand_prefix ctx size
        in
          prefix @@
          vec [Word8.+(base_opcode, r)]
        end
    in
      case i of
        AND (size, args) => encode_normal 0wx20 (size, args)
      | AND_A_IMM imm => encode_normal_imm 0wx24 imm
      | SUB (size, args) => encode_normal 0wx28 (size, args)
      | SUB_A_IMM imm => encode_normal_imm 0wx2C imm
      | XOR (size, args) => encode_normal 0wx30 (size, args)
      | XOR_A_IMM imm => encode_normal_imm 0wx34 imm
      | CMP (size, args) => encode_normal 0wx38 (size, args)
      | CMP_A_IMM imm => encode_normal_imm 0wx3C imm
      | INC mr => encode_multireg_based 0wx40 mr
      | DEC mr => encode_multireg_based 0wx48 mr
      | PUSH mr => encode_multireg_based 0wx50 mr
      | POP mr => encode_multireg_based 0wx58 mr

      (* out of gamut *)
      | MOV (size, args) => encode_normal 0wx88 (size, args)
      | NOP => vec [0wx90]
      | INT w => vec [0wxCD, w]
      | DB w => vec [w]
    (*
      | _ => raise X86 "unimplemented ins"
        *)
    end

  fun multiregstring mr =
    case mr of
      AX  => "AX"
    | CX  => "CX"
    | DX  => "DX"
    | BX  => "BX"
    | SP  => "SP"
    | BP  => "BP"
    | SI  => "SI"
    | DI  => "DI"
    | EAX => "EAX"
    | ECX => "ECX"
    | EDX => "EDX"
    | EBX => "EBX"
    | ESP => "ESP"
    | EBP => "EBP"
    | ESI => "ESI"
    | EDI => "EDI"

end
