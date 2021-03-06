structure X86 =
struct

  exception X86 of string

  type word8 = Word8.word
  type word8vector = Word8Vector.vector

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

  (* Get the 16-bit multireg version of some register *)
  fun reg_to_multireg16 r =
    case r of
      A => AX
    | C => CX
    | D => DX
    | B => BX
    | AH_SP => SP
    | CH_BP => BP
    | DH_SI => SI
    | BH_DI => DI

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
  | IND_EBP_DISP8 of Word8.word
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
  | IND_EBP_DISP32 of Word32.word
  | IND_ESI_DISP32 of Word32.word
  | IND_EDI_DISP32 of Word32.word
  (* mod=11. Same encoding as reg. *)
  | Register of reg

  (* 16-bit modes. XXX add the comprehensive list. *)
  (*
  | IND_BX_SI
  | IND_BX_DI
  | IND_BP_SI
  | IND_BP_DI
    *)
  | IND_SI
  | IND_DI
  (*
  | DISP16 of Word16.word
    *)
  | IND_BX


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
    (* 3C,3D CMP al/ax/eax, imm *)
    | CMP_A_IMM of immediate
    (* 3E DS segment override prefix *)
    (* 3F AAS ASCII Adjust After Subtraction *)
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
    (* 63 ARPL Adjust RPL field of segment selector

       This instruction seems useless outside OS stuff,
       and appears to be illegal in real mode. *)
    (* 64 FS segment override prefix *)
    (* 65 GS segment override prefix *)
    (* 66 operand size override prefix -- implicit *)
    (* 67 address size override prefix -- implicit *)
    (* 68 push immediate 16/32, 6A push immediate 8 *)
    | PUSH_IMM of immediate
    (* 69 IMUL *)
    (* 6C,6D INS ES:DI, DX.
       Note that ES=DS in ABC EXEs.
       Also note that for INS/OUTS, the address size
       prefix can be used to switch between e.g. ESI and SI.
       For ABC we always use the 16-bit modes. *)
    | INS of size
    (* 6E,6F OUTS DX, DS:SI *)
    | OUTS of size
    (* 70-7E JCC+disp8 variants *)
    | JO of Word8.word
    | JNO of Word8.word
    (* JB = JNAE = JC *)
    | JB of Word8.word
    (* JNB = JAE = JC *)
    | JAE of Word8.word
    (* JZ = JE *)
    | JZ of Word8.word
    (* JNZ = JNE *)
    | JNZ of Word8.word
    (* JBE = JNA *)
    | JBE of Word8.word
    | JA of Word8.word
    | JS of Word8.word
    | JNS of Word8.word
    | JP of Word8.word
    | JNP of Word8.word
    (* JL = JNGE *)
    | JL of Word8.word
    (* JNL = JGE *)
    | JGE of Word8.word
    (* JLE = JNG *)
    | JLE of Word8.word
    (* JNLE/JG is 0x7f, not printable. *)

    (* These instructions are out of gamut; just used
       for debugging and development and should not
       appear in "production" compiled binaries. *)
    | MOV of size * args
    | NOP
    | INT of word8

    (* Insert the byte literally, for manual encoding of
       instructions or data. word8 should be printable,
       of course. XXX remove this? *)
    | DB of word8
    (* Meta-instruction that cannot be assembled, just
       for debugging. *)
    | COMMENT of string

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

  fun immediatestring (I8 w) = "I8 " ^ Word8.toString w
    | immediatestring (I16 w) = "I16 " ^ Word16.toString w
    | immediatestring (I32 w) = "I32 " ^ Word32.toString w

  fun sizestring S8 = "S8"
    | sizestring S16 = "S16"
    | sizestring S32 = "S32"

  local
    fun sizedregstring (S8, reg) =
      (case reg of
         A => "AL"
       | C => "CL"
       | D => "DL"
       | B => "BL"
       | AH_SP => "AH"
       | CH_BP => "CH"
       | DH_SI => "DH"
       | BH_DI => "BH")
      | sizedregstring (s, reg) =
         let val sizeprefix = if s = S32 then "E" else ""
         in
           sizeprefix ^
           (case reg of
              A => "AX"
            | C => "CX"
            | D => "DX"
            | B => "BX"
            | AH_SP => "SP"
            | CH_BP => "BP"
            | DH_SI => "SI"
            | BH_DI => "DI")
         end

    (* Render as +dec or -dec *)
    fun disp8 (w : Word8.word) =
      let val i = Word8.toIntX w
      in
        if i >=0 then "+" ^ Int.toString i
        else "-" ^ Int.toString (~i)
      end
    fun modrmstring size modrm =
      case modrm of
        IND_EAX => "[EAX]"
      | IND_ECX => "[ECX]"
      | IND_EDX => "[EDX]"
      | IND_EBX => "[EBX]"
      | IND_SIB sib => "[SIB???]" (* TODO *)
      | DISP32 w => "[0x" ^ Word32.toString w ^ "]"
      | IND_ESI => "[ESI]"
      | IND_EDI => "[EDI]"
      | IND_EAX_DISP8 w8 => "[EAX" ^ disp8 w8 ^ "]"
      | IND_ECX_DISP8 w8 => "[ECX" ^ disp8 w8 ^ "]"
      | IND_EDX_DISP8 w8 => "[EDX" ^ disp8 w8 ^ "]"
      | IND_EBX_DISP8 w8 => "[EBX" ^ disp8 w8 ^ "]"
      | IND_SIB_DISP8 (sib, w8) => "[SIB???" ^ disp8 w8 ^ "]"
      | IND_EBP_DISP8 w8 => "[EBP" ^ disp8 w8 ^ "]"
      | IND_ESI_DISP8 w8 => "[ESI" ^ disp8 w8 ^ "]"
      | IND_EDI_DISP8 w8 => "[EDI" ^ disp8 w8 ^ "]"
      (* XXX not implemented because they are not useful (printable disp32 is
         too big) *)
      | IND_EAX_DISP32 w32 => "[EAX+disp32]"
      | IND_ECX_DISP32 w32 => "[ECX+disp32]"
      | IND_EDX_DISP32 w32 => "[EDX+disp32]"
      | IND_EBX_DISP32 w32 => "[EBX+disp32]"
      | IND_SIB_DISP32 (sib, w32) => "[SIB???+disp32]"
      | IND_EBP_DISP32 w32 => "[EBP+disp32]"
      | IND_ESI_DISP32 w32 => "[ESI+disp32]"
      | IND_EDI_DISP32 w32 => "[EDI+disp32]"
      | Register reg => sizedregstring (size, reg)

      | IND_SI => "[SI]"
      | IND_DI => "[DI]"
      | IND_BX => "[BX]"

    fun sizeargsstring (size, (reg <- modrm)) =
          sizedregstring (size, reg) ^ " <- " ^ modrmstring size modrm
      | sizeargsstring (size, (modrm <~ reg)) =
          modrmstring size modrm ^ " <~ " ^ sizedregstring (size, reg)

  in
    fun regtos A = "A"
      | regtos C = "C"
      | regtos D = "D"
      | regtos B = "B"
      | regtos AH_SP = "AH/SP"
      | regtos CH_BP = "CH/BP"
      | regtos DH_SI = "DH/SI"
      | regtos BH_DI = "BH/DI"

    fun insstring ins =
      case ins of
        AND sa => "AND " ^ sizeargsstring sa
      | AND_A_IMM imm => "AND A, " ^ immediatestring imm
      | SUB sa => "SUB " ^ sizeargsstring sa
      | SUB_A_IMM imm => "SUB A, " ^ immediatestring imm
      | XOR sa => "XOR " ^ sizeargsstring sa
      | XOR_A_IMM imm => "XOR A, " ^ immediatestring imm
      | CMP sa => "CMP " ^ sizeargsstring sa
      | CMP_A_IMM imm => "CMP A, " ^ immediatestring imm
      | INC mr => "INC " ^ multiregstring mr
      | DEC mr => "DEC " ^ multiregstring mr
      | PUSH mr => "PUSH " ^ multiregstring mr
      | POP mr => "POP " ^ multiregstring mr
      | PUSH_IMM imm => "PUSH " ^ immediatestring imm
      | INS sz => "INS ES:EDI <- port DX (" ^ sizestring sz ^ ")"
      | OUTS sz => "OUTS port DX (" ^ sizestring sz ^ ") <- DS:ESI"
      | JO w => "JO " ^ disp8 w
      | JNO w => "JNO " ^ disp8 w
      | JB w => "JB " ^ disp8 w
      | JAE w => "JAE " ^ disp8 w
      | JZ w => "JZ " ^ disp8 w
      | JNZ w => "JNZ " ^ disp8 w
      | JBE w => "JBE " ^ disp8 w
      | JA w => "JA " ^ disp8 w
      | JS w => "JS " ^ disp8 w
      | JNS w => "JNS " ^ disp8 w
      | JP w => "JP " ^ disp8 w
      | JNP w => "JNP " ^ disp8 w
      | JL w => "JL " ^ disp8 w
      | JGE w => "JGE " ^ disp8 w
      | JLE w => "JLE " ^ disp8 w

      | MOV sa => "MOV " ^ sizeargsstring sa
      | NOP => "NOP"
      | INT w => ("INT " ^ Word8.toString w)
      | DB w => ("DB " ^ Word8.toString w)
      | COMMENT m => (";; " ^ m)
(*      | _ => "OTHER_INS" *)
  end

end
