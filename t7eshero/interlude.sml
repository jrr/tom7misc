(* Pausing screen for interstitials. *)
(* XXXX lots of this is duplicated from Wardrobe. The robot drawing code should be
   factored out, in particular. *)
structure Interlude :> INTERLUDE =
struct

  (* open SDL *)
  structure FontSmall = Sprites.FontSmall
  structure Font = Sprites.Font
  structure FontMax = Sprites.FontMax
  structure FontHuge = Sprites.FontHuge
  structure SmallFont3x = Sprites.SmallFont3x
  structure SmallFont = Sprites.SmallFont

  val screen = Sprites.screen

  (* XXX music is disabled. If you want it, fix it. *)
  val BGMIDI = "wardrobe.mid"
  val PRECURSOR = 180
  val SLOWFACTOR = 5
  val MENUTICKS = 0w60

  structure LM = ListMenuFn(val screen = screen)

  local open Womb
  in
    val womb_pattern =
      Womb.pattern 0w100
      [[A,J,M], [B,K,L], [E,J,M], [F,K,L],
       [H,J,M], [G,K,L], [D,J,M], [C,K,L]]
  end

  exception Done and Abort
  fun loop profile (config as { message1, message2, robot,
                                background, graphic }) =
    let
      val () = Sound.all_off ()

      val (divi, thetracks) = Score.fromfile BGMIDI
      val tracks = Score.label PRECURSOR SLOWFACTOR thetracks
      fun slow l = map (fn (delta, e) => (delta * SLOWFACTOR, e)) l
      val () = Song.init ()
      val cursor = Song.cursor_loop (0 - PRECURSOR) (slow (MIDI.merge tracks))

      val humpframe = ref 0
      val humprev = ref false

      val nexta = ref 0w0
      val start = SDL.getticks()
      fun exit () = raise Done

      val closet = Profile.closet profile
      val outfit = ref (Profile.outfit profile)

      fun loopplay () =
        let
          val nows = Song.nowevents cursor
        in
          List.app
          (fn (label, evt) =>
           (case label of
                Match.Music (inst, _) =>
                (case evt of
                     MIDI.NOTEON(ch, note, 0) => Sound.noteoff (ch, note)
                   (* XXX no midi in this interstitial? *)
                   | MIDI.NOTEON(ch, note, vel) => ()
                   (* Sound.noteon (ch, note,
                      Sound.midivel vel,
                      inst) *)
                   | MIDI.NOTEOFF(ch, note, _) => Sound.noteoff (ch, note)
                   | _ => print ("unknown music event: " ^ MIDI.etos evt ^
                                 "\n"))
                   | _ => ()))
            nows
        end

      and draw () =
        let
          val X_ROBOT = Sprites.width -
            (SDL.surface_width(Vector.sub(Sprites.humps, 0)) + 24)
          val Y_ROBOT =
            (Sprites.height -
             SDL.surface_height(Vector.sub(Sprites.humps, 0))) div 2

          fun drawitem item =
            let val (f, x, y) = Vector.sub(Items.frames item, !humpframe)
            in SDL.blitall(f, screen, X_ROBOT + x, Y_ROBOT + y)
            end

          val closet = Profile.closet profile

          fun drawmsg y NONE = ()
            | drawmsg y (SOME msg) =
            FontMax.draw(screen,
                         (Sprites.width - FontMax.sizex_plain msg) div 2,
                         y, Chars.fancy msg);
        in
          (case background of
             Setlist.BG_SOLID c =>
               SDL.fillrect (screen, 0, 0, Sprites.width, Sprites.height, c)
           (* XXX procedural backgrounds not supported here... *)
           | _ =>
               SDL.fillrect (screen, 0, 0, Sprites.width, Sprites.height,
                             SDL.color (0wx22, 0wx00, 0wx00, 0wxFF)));

          (if robot
           then
             let in
               Items.app_behind (!outfit) drawitem;
               SDL.blitall(Vector.sub(Sprites.humps, !humpframe), screen,
                           X_ROBOT, Y_ROBOT);
               Items.app_infront (!outfit) drawitem
             end
           else ());

          (case graphic of
             NONE => ()
           | SOME g =>
               SDL.blitall (g, screen,
                            (Sprites.width - SDL.surface_width g) div 2,
                            (Sprites.height - SDL.surface_height g) div 2));

          (* and messages. *)
          drawmsg 16 message1;
          drawmsg ((Sprites.height - 16) - FontMax.height) message2;

          ()
        end

      and advance () =
        let in
          (* XXX should pause on first, last frames a bit *)
          (if !humprev
           then humpframe := !humpframe - 1
           else humpframe := !humpframe + 1);
          (if !humpframe < 0
           then (humpframe := 0; humprev := false)
           else ());
          (if !humpframe >= Vector.length Sprites.humps
           then (humpframe := (Vector.length Sprites.humps - 1);
                 humprev := true)
           else ())
        end

      and heartbeat () =
        let
          val () = Song.update ()
          val () = Womb.maybenext womb_pattern
          val () = loopplay ()
          val now = SDL.getticks ()
        in
          if now > !nexta
          then (advance();
                nexta := now + MENUTICKS)
          else ()
        end

      and input () =
        case SDL.pollevent () of
            SOME (SDL.E_KeyDown { sym = SDL.SDLK_ESCAPE }) => exit()
          | SOME SDL.E_Quit => raise Hero.Exit
          | SOME (SDL.E_KeyDown { sym = SDL.SDLK_RETURN }) => exit()
          | SOME e =>
            (case Input.map e of
               SOME (_, Input.ButtonDown b) => exit()
             | SOME (_, Input.ButtonUp b) => ()
             | SOME (_, Input.StrumUp) => ()
             | SOME (_, Input.StrumDown) => ()
             | _ => ())
          | NONE => ()

      val nextd = ref 0w0
      fun go () =
        let
          val () = heartbeat ()
          val () = input ()
          val now = SDL.getticks ()
        in
          (if now > !nextd
           then (draw ();
                 nextd := now + MENUTICKS;
                 SDL.flip screen)
           else ());
           go ()
        end
    in
      go () handle Done => Sound.all_off ()
    end

end
