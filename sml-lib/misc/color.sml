structure Color : COLOR =
struct

    (* translated from C++ code in Escape. *)
    fun hsvtorgbf (h, s, v) =
        (* Singularity when saturation is 0 *)
        if Real.==(s, 0.0)
        then (v, v, v)
        else
            let
                val hue = h * 6.0
                val fh = real (Real.trunc hue)
                val var_1 = v * (1.0 - s)
                val var_2 = v * (1.0 - s * (hue - fh))
                val var_3 = v * (1.0 - s * (1.0 - (hue - fh)))
            in
                case Real.trunc hue of
                    0 => (v, var_3, var_1)
                  | 1 => (var_2, v, var_1)
                  | 2 => (var_1, v, var_3)
                  | 3 => (var_1, var_2, v)
                  | 4 => (var_3, var_1, v)
                  | _ => (v, var_1, var_2)
            end

    fun wf (w : Word8.word) = real (Word8.toInt w) / 255.0
    fun fw (f : Real.real) = Word8.fromInt (Real.trunc (f * 255.0))
    fun rgbf (r, g, b) = (fw r, fw g, fw b)
    fun hsvtorgb (h, s, v) =
        let val (r, g, b) = hsvtorgbf (wf h, wf s, wf v)
        in rgbf (r, g, b)
        end

    val digits = "0123456789ABCDEF"
    fun hexdig i = implode [CharVector.sub (digits, i div 16),
                            CharVector.sub (digits, i mod 16)]
    fun ws w = hexdig (Word8.toInt w)
    fun tohexstring (r, g, b) = ws r ^ ws g ^ ws b

    (* ASCII trick: (ch | 4400) % 55 *)
    fun hexvalue ch =
        Word8.fromInt (SysWord.toInt (SysWord.orb(SysWord.fromInt(ord ch), SysWord.fromInt 4400)) mod 55)

    fun ishex c = ((ord c >= ord #"0" andalso
                    ord c <= ord #"9") orelse
                   (ord c >= ord #"a" andalso
                    ord c <= ord #"f") orelse
                   (ord c >= ord #"A" andalso
                    ord c <= ord #"F"))

    fun fromhexstring s =
        if size s <> 6 orelse not (CharVector.all ishex s)
        then NONE
        else SOME (hexvalue (String.sub(s, 0)) * 0w16 +
                   hexvalue (String.sub(s, 1)),
                   hexvalue (String.sub(s, 2)) * 0w16 +
                   hexvalue (String.sub(s, 3)),
                   hexvalue (String.sub(s, 4)) * 0w16 +
                   hexvalue (String.sub(s, 5)))

    fun onefromhexstring s =
        if size s <> 2 orelse not (CharVector.all ishex s)
        then NONE
        else SOME (hexvalue (String.sub(s, 0)) * 0w16 +
                   hexvalue (String.sub(s, 1)))


    (* This is a straight port of the corresponding function in cc-lib. *)
    fun rgbtolab (r, g, b) =
      let
        (* First we need to un-compand the RGB triplet.
           Technically there are different choices here, but sRGB is what we
           really mean by RGB in this library.
           http://www.brucelindbloom.com/Eqn_RGB_to_XYZ.html *)
        fun srgb_inv_compand ch =
          let
            val inv1055 : real = 1.0 / 1.055
            val inv1292 : real = 1.0 / 12.92
          in
            if ch > 0.04045
            then Math.pow((ch + 0.055) * inv1055, 2.4)
            else ch * inv1292
          end

        val srgb_r = srgb_inv_compand(r)
        val srgb_g = srgb_inv_compand(g)
        val srgb_b = srgb_inv_compand(b)

          (* Now to XYZ color space, whose components are nominally in [0, 1].
             This is just a matrix multiply using a mysterious matrix. Here using sRGB with
             D65 reference white.
             http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html *)
        val x = srgb_r * 0.4124564 + srgb_g * 0.3575761 + srgb_b * 0.1804375
        val y = srgb_r * 0.2126729 + srgb_g * 0.7151522 + srgb_b * 0.0721750
        val z = srgb_r * 0.0193339 + srgb_g * 0.1191920 + srgb_b * 0.9503041

        fun F ch =
          (* Worth it just for the gif:
             http://www.brucelindbloom.com/LContinuity.html *)
          let
            val epsilon = 216.0 / 24389.0
            val kappa_div_116 = (24389.0 / 27.0) / 116.0
            val one_third = 1.0 / 3.0
            val sixteen_116ths = 16.0 / 116.0;
          in
            if ch > epsilon
            then Math.pow(ch, one_third)
            else kappa_div_116 * ch + sixteen_116ths
          end

        val fx = F x
        val fy = F y
        val fz = F z

      in
        ((116.0 * fy) - 16.0,
         500.0 * (fx - fy),
         200.0 * (fy - fz))
      end

    (* Also a direct port from cc-lib. *)
    fun delta_e ((l1, a1, b1),
                 (l2, a2, b2)) =
      let
        val dl = l1 - l2
        val da = a1 - a2
        val db = b1 - b2
        val c1 = Math.sqrt(a1 * a1 + b1 * b1)
        val c2 = Math.sqrt(a2 * a2 + b2 * b2)
        val dc = c1 - c2
        val dhsq = da * da + db * db - dc * dc
        (* later we have (dh / kh*sh)^2, which is
           dh^2 / (kh * sh)^2,
           so keep around dh^2 as dhsq instead of taking square root. *)
        val sc = 1.0 + 0.045 * c1
        val sh = 1.0 + 0.015 * c1
        val v1 = dl
        val v2 = dc / sc
        val v3sq = dhsq / (sh * sh)

        val de = v1 * v1 + v2 * v2 + v3sq
      in
        if de <= 0.0
        then 0.0
        else Math.sqrt de
      end

end