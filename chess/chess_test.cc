
#include "chess.h"
#include "base/logging.h"

using Move = Position::Move;

static void ApplyMove(Position *pos, const char *pgn) {
  Move m;
  CHECK(pos->ParseMove(pgn, &m)) << pgn;
  CHECK(pos->IsLegal(m)) << pgn;
  pos->ApplyMove(m);
}

static void Simple() {
  Position pos;
  CHECK(Position::ParseFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR "
			   "w KQkq - 0 1", &pos));
  
  for (const char *m : {
  "e4", "d5", "exd5", "e5", "dxe6", "Ne7", "g3", "h5",
  "Bg2", "a5", "Nh3", "Rh6", "O-O", "Ra6", "a4", "Nbc6",
  "Ra3", "Ne5", "Re1", "N7g6", "Rae3", "Nh8", "exf7+", "Nxf7",
  "Rxe5+", "Rhe6", "Rxe6+", "Kd7", "d4", "Ba3", "d5", "Bxb2",
  "Rh6", "Ne5", "d6", "Ke8", "dxc7", "Bxh3", "cxd8=R+", "Kf7",
  "Re8", "Nf3+", "Kh1", "Bd7", "R1e6", "Bxa4", "Ref6+", "gxf6",
  "Qxf3", "Bxc2", "g4", "hxg4", "Qxg4", "a4", "Be3", "a3",
  "Bxb7", "a2", "f3", "a1=Q", "Qe6+", "Kg7", "Rhh8", "Qxb1+",
  "Kg2", "Rxe6", "Rxe6", "Kxh8", "Re8+", "Kg7", "Bh6+", "Kxh6",
  "Rh8+", "Bh7", "Rxh7+", "Kxh7", "Be4+", "Kg7", "Bxb1", "f5",
  "Bxf5", "Bd4", "h4", "Kh6" }) {
    ApplyMove(&pos, m);
  }
  
}

int main(int argc, char **argv) {
  Simple();
  return 0;
}

/*
1. e4 d5 2. exd5 e5 3. dxe6 Ne7 4. g3 h5
5. Bg2 a5 6. Nh3 Rh6 7. O-O Ra6 8. a4 Nbc6
9. Ra3 Ne5 10. Re1 N7g6 11. Rae3 Nh8 12. exf7+ Nxf7
13. Rxe5+ Rhe6 14. Rxe6+ Kd7 15. d4 Ba3 16. d5 Bxb2
17. Rh6 Ne5 18. d6 Ke8 19. dxc7 Bxh3 20. cxd8=R+ Kf7
21. Re8 Nf3+ 22. Kh1 Bd7 23. R1e6 Bxa4 24. Ref6+ gxf6
25. Qxf3 Bxc2 26. g4 hxg4 27. Qxg4 a4 28. Be3 a3
29. Bxb7 a2 30. f3 a1=Q 31. Qe6+ Kg7 32. Rhh8 Qxb1+
33. Kg2 Rxe6 34. Rxe6 Kxh8 35. Re8+ Kg7 36. Bh6+ Kxh6
37. Rh8+ Bh7 38. Rxh7+ Kxh7 39. Be4+ Kg7 40. Bxb1 f5
41. Bxf5 Bd4 42. h4 Kh6

  "e4", "d5", "exd5", "e5", "dxe6", "Ne7", "g3", "h5",
  "Bg2", "a5", "Nh3", "Rh6", "O-O", "Ra6", "a4", "Nbc6",
  "Ra3", "Ne5", "Re1", "N7g6", "Rae3", "Nh8", "exf7+", "Nxf7",
  "Rxe5+", "Rhe6", "Rxe6+", "Kd7", "d4", "Ba3", "d5", "Bxb2",
  "Rh6", "Ne5", "d6", "Ke8", "dxc7", "Bxh3", "cxd8=R+", "Kf7",
  "Re8", "Nf3+", "Kh1", "Bd7", "R1e6", "Bxa4", "Ref6+", "gxf6",
  "Qxf3", "Bxc2", "g4", "hxg4", "Qxg4", "a4", "Be3", "a3",
  "Bxb7", "a2", "f3", "a1=Q", "Qe6+", "Kg7", "Rhh8", "Qxb1+",
  "Kg2", "Rxe6", "Rxe6", "Kxh8", "Re8+", "Kg7", "Bh6+", "Kxh6",
  "Rh8+", "Bh7", "Rxh7+", "Kxh7", "Be4+", "Kg7", "Bxb1", "f5",
  "Bxf5", "Bd4", "h4", "Kh6" }) {
*/
