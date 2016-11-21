structure X86 =
struct

  (* Where MULTI = 16 or 32 bits, depending on the current
     mode (and instruction prefix). *)
  datatype dir = REG_DST | RM_DST
  datatype size = BYTE | MULTI

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

  (* Incomplete set of X86 opcodes. *)
  datatype ins =
    AND of size * args
    (* ... *)

  val XXX = AND (BYTE, A <- Register C)

end
