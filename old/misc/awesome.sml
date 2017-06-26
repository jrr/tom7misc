signature AWESOME =
   sig 

      type robot
      val Hugbot : robot
      val Dalek : robot

      type K
      type S

      (* infixt app *)
      type ('f, 'arg) app
      type 'a exp

      (* definable.. *)
      type I = ((S, K) app, K) app

      (* things that appear in index terms also
         need to have phantom versions *)
      type z_
      type s_

      val K_app :  ((K, 'a) app, 'b) app exp -> 'a exp
      val K_ppa :  'a exp -> ((K, 'a) app, 'b) app exp

      val S_app : (((S, 'a) app, 'b) app, 'c) app exp -> (('a, 'c) app, ('b, 'c) app) app exp
      val S_ppa : (('a, 'c) app, ('b, 'c) app) app exp -> (((S, 'a) app, 'b) app, 'c) app exp

      val eta   : 'a exp -> ((S, (K, 'a) app) app, I) app exp
      val ate   : ((S, (K, 'a) app) app, I) app exp -> 'a exp

      val app_congl : ('a1 exp -> 'a2 exp) -> ('a1, 'b) app exp -> ('a2, 'b) app exp
      val app_congr : ('b1 exp -> 'b2 exp) -> ('a, 'b1) app exp -> ('a, 'b2) app exp

(*     val one1 : ((S,(K,s_) app) app,((S,K) app,K) app exp) app exp zist
 *)


      (* easy *)
      type 'a elist
      val elist_cat : ('a -> 'b) -> 'a elist -> 'b elist
      val enil : z_ elist
      val econs : robot -> 'a exp elist -> (s_, 'a) app exp elist


      type 'f zist
      val zist_zat : ('a -> 'b) -> 'a zist -> 'b zist

      (* hard *)
      val zil : ((S, K) app, K) app exp zist
      (* robot -> zist F2 -> zist ([l] s (F2 l)). *)

      (* \l. s (f l) =
         S (K s)
           (S (K f) I)
           *)
      val zons : robot * 'f exp zist ->
        ((S,
          (K, s_) app) app,
         ((S,
           (K, 'f) app) app,
          I) app
         ) app exp zist

end

structure Awesome :> AWESOME =
struct
  datatype robot = Dalek | Hugbot
  
  type K = {}
  type S = {}
  type 'a exp = {}
  type ('a, 'b) app = {}
  type I = {}
  type z_ = {}
  type s_ = {}

  fun K_app () = ()
  fun K_ppa () = ()
  fun S_app () = ()
  fun S_ppa () = ()

  fun eta () = ()
  fun ate () = ()

  fun app_congl _ {} = {}
  fun app_congr _ {} = {}

  datatype Elist = enil | Econs of robot * Elist
  type 'a elist = Elist
  fun econs x y = Econs(x, y)
  fun elist_cat _ x = x


  datatype Zist = zil | zons_ of robot * Zist
  type 'a zist = Zist
  fun zist_zat _ x = x

  val zons = zons_

end


structure User =
struct
  open Awesome

  val mmm = zil

  (* one : ((S,(K,s_) app) app,
     ((S,
       (K,((S,K) app,K) app) 
         app) app,I) app) app 
       zist 
       *)
  val one = zons(Hugbot, zil)
  val one1 = zist_zat (app_congr ate) one
(*  fun zzaca x = zist_zat (app_congr ate) x
  fun zz x = zist_zat ate x *)
  val zomg = zist_zat ate one1

end