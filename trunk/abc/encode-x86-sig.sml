signature ENCODEX86 =
sig

  exception EncodeX86 of string

  (* Number of bytes in the encoded instruction. *)
  val encoded_size : X86.ctx -> X86.ins -> int

  val decode_multireg : X86.multireg -> X86.size * Word8.word

  val encode : X86.ctx -> X86.ins -> Word8Vector.vector
  val encodelist : X86.ctx -> X86.ins list -> Word8Vector.vector
end
