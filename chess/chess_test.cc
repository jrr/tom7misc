
#include "chess.h"

#include <string>

#include "base/logging.h"
#include "pgn.h"
#include "packedgame.h"
#include "fates.h"

using namespace std;

using Move = Position::Move;


static Position::Move ApplyMove(Position *pos, const char *pgn, Fates *fates) {
  Move m;
  const Position old_position = *pos;
  CHECK(pos->ParseMove(pgn, &m)) << pgn;
  CHECK(PositionEq{}(old_position, *pos)) << "ParseMove modified board "
    "state!";
  CHECK(pos->IsLegal(m)) << pgn;
  CHECK(PositionEq{}(old_position, *pos)) << "IsLegal modified board "
    "state!";

  uint16_t pm = PackedGame::PackMove(m);
  CHECK((pm & ~0b111111111111) == 0b0) << pm << " for "
				       << Position::DebugMoveString(m);
  const Move m2 = PackedGame::UnpackMove(pm);
  CHECK(Position::MoveEq(m, m2)) << Position::DebugMoveString(m) << " vs "
				 << Position::DebugMoveString(m2);

  if (fates != nullptr) {
    fates->Update(*pos, m);
  }
  pos->ApplyMove(m);
  return m;
}

static std::initializer_list<const char *> kGame1 = {
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
  "Bxf5", "Bd4", "h4", "Kh6"
};

static std::initializer_list<const char *> kGame2 = {
  "e3", "f5", "e4", "f4", "g4", "fxg3", "f4", "a5", "Nh3", "g2", "Bc4",
  "b5", "Nc3", "c5", "Nd5", "e5", "Ne3", "g5", "Nf5", "h5", "Nh4", "d5",
  "Nf3", "gxh1=R+", "Nhg1", "g4", "b4", "h4", "Ba3", "d4", "Qe2", "g3",
  "O-O-O", "h3", "Nxh3", "d3", "Nfg1", "Ra7", "c3", "Bd6", "fxe5",
  "Nh6", "e6", "O-O", "e7+", "Rf7", "exd8=B", "bxc4", "b5", "a4", "b6",
  "Rae7", "e5", "Re6", "Bb4", "g2", "Ba5", "Bd7", "b7", "Nc6", "Bdc7",
  "Ng4", "b8=B", "Ref6", "e6", "Rf1", "e7", "R7f2", "a3", "Nd8",
  "exd8=B", "Bg3", "Bg5", "Rf3", "Bgf4", "Ne5", "Bcxe5", "Bxh3", "Bac7",
  "Be1", "Qxd3", "R1f2", "Qc2", "Rxg1", "d4", "cxd3", "Qxd3", "Rxd3",
  "Rxd3", "Bxc3+", "Rd1", "Rxd1+", "Kxd1", "Rf1+", "Kc2", "Rf2+",
  "Kxc3", "g1=R", "Kc4", "Rxf4+", "Bxf4", "Rg4", "Kxc5", "Rxf4", "Bxf4",
  "Be6", "Kd6", "Bd7", "Kxd7", "Kf7", "h4", "Kg6", "h5+", "Kxh5",
};

static void CheckInit() {
  Position pos;
  const string init_pos = pos.BoardString();

  static const char *START_FEN =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
  CHECK(Position::ParseFEN(START_FEN, &pos));
  string fen_out = pos.ToFEN(0, 1);
  CHECK(START_FEN == fen_out) << fen_out;
  const string fen_pos = pos.BoardString();
  CHECK(init_pos == fen_pos) << init_pos << "\n\n" << fen_pos;
}

static Position MakePos(const string &s) {
  PGN pgn;
  CHECK(PGN::Parse(s.c_str(), &pgn));
  Position pos;
  for (const PGN::Move &m : pgn.moves) {
    (void)ApplyMove(&pos, m.move.c_str(), nullptr);
  }
  return pos;
}

static void PlayGame(std::initializer_list<const char *> game) {
  Position pos;

  PackedGame pgame;
  Fates fates;
  for (const char *m : game) {
    Position::Move move = ApplyMove(&pos, m, &fates);
    pgame.PushMove(PackedGame::PackMove(move));
    printf("%s\n", pos.BoardString().c_str());
  }

  Position pos2;
  for (int i = 0; i < pgame.NumMoves(); i++) {
    Position::Move move = PackedGame::UnpackMove(pgame.GetMove(i));
    CHECK(pos2.IsLegal(move));
    pos2.ApplyMove(move);
  }
  CHECK(PositionEq{}(pos, pos2)) << pos.BoardString() << "\nvs\n"
				 << pos2.BoardString();
}

static void PromotionRegression() {
  {
    Position pos;
    // White pawn about to promote on column 2.
    CHECK(Position::ParseFEN(
	      "8/2P5/7k/8/7K/8/8/8 w - 0 1", &pos));
    Move m;
    m.src_row = 1;
    m.src_col = 2;
    m.dst_row = 0;
    m.dst_col = 2;
    m.promote_to = 0;
    // This wasn't properly rejecting some promotions.
    CHECK(!pos.IsLegal(m));
    m.promote_to = Position::BLACK | Position::KNIGHT;
    CHECK(!pos.IsLegal(m));
    m.promote_to = Position::WHITE | Position::KNIGHT;
    CHECK(pos.IsLegal(m));
  }

  {
    Position pos;
    // Black pawn about to promote on column 2.
    CHECK(Position::ParseFEN(
	      "8/8/7k/8/7K/8/2p5/8 b - 0 1", &pos));
    Move m;
    m.src_row = 6;
    m.src_col = 2;
    m.dst_row = 7;
    m.dst_col = 2;
    m.promote_to = 0;
    CHECK(!pos.IsLegal(m));
    m.promote_to = Position::BLACK | Position::KNIGHT;
    CHECK(pos.IsLegal(m));
    m.promote_to = Position::WHITE | Position::KNIGHT;
    CHECK(!pos.IsLegal(m));
  }
}

static void ReadPGN() {
  PGN pgn;
  const char *kGame =
      R"_([Event "Rated Blitz tournament https://lichess.org/tournament/cXTYjhyI"]
[Site "https://lichess.org/LNU6h60t"]
[White "chesspibito"]
[Black "Padre_Pio"]
[Result "1-0"]
[UTCDate "2017.03.31"]
[UTCTime "22:00:02"]
[WhiteElo "1582"]
[BlackElo "1580"]
[WhiteRatingDiff "+11"]
[BlackRatingDiff "-14"]
[ECO "B02"]
[Opening "Testing \"quoted\" strings c:\\chess\\chess.exe"]
[TimeControl "300+1"]
[Termination "Time forfeit"]

1. e4 { [%clk 0:05:00] } Nf6 { [%clk 0:05:00] } 2. e5 { [%clk 0:04:55] } Nd5 { [%clk 0:04:58] } 3. c4 { [%clk 0:04:53] } Nb6 { [%clk 0:04:56] } 4. a4 { [%clk 0:04:49] } a5 { [%clk 0:04:53] } 5. Qf3 { [%clk 0:04:42] } d6 { [%clk 0:04:47] } 6. d4 { [%clk 0:04:25] } dxe5 { [%clk 0:04:38] } 7. dxe5 { [%clk 0:04:22] } Nc6 { [%clk 0:04:32] } 8. c5 { [%clk 0:04:17] } Nxe5 { [%clk 0:04:21] } 9. Qe4 { [%clk 0:04:03] } Nbd7 { [%clk 0:04:08] } 10. Bb5 { [%clk 0:03:57] } c6 { [%clk 0:03:56] } 11. Be2 { [%clk 0:03:51] } g6 { [%clk 0:03:39] } 12. Bf4 { [%clk 0:03:46] } Bg7 { [%clk 0:03:37] } 13. Nc3 { [%clk 0:03:32] } O-O { [%clk 0:03:31] } 14. Nf3 { [%clk 0:03:14] } Nxf3+ { [%clk 0:03:28] } 15. Bxf3 { [%clk 0:03:11] } Nf6 { [%clk 0:03:22] } 16. Qe3 { [%clk 0:03:04] } Ng4 { [%clk 0:03:13] } 17. Bxg4 { [%clk 0:03:00] } Bxg4 { [%clk 0:03:10] } 18. Bh6 { [%clk 0:02:58] } Re8 { [%clk 0:02:49] } 19. Bxg7 { [%clk 0:02:53] } Kxg7 { [%clk 0:02:48] } 20. O-O { [%clk 0:02:50] } e5 { [%clk 0:02:41] } 21. Rad1 { [%clk 0:02:41] } Qc7 { [%clk 0:02:32] } 22. Rd6 { [%clk 0:02:36] } Rad8 { [%clk 0:02:25] } 23. Ne4 { [%clk 0:02:12] } Bf5 { [%clk 0:02:11] } 24. Qg5 { [%clk 0:01:59] } Bxe4 { [%clk 0:02:06] } 25. Qf6+ { [%clk 0:01:54] } Kg8 { [%clk 0:02:01] } 26. Rfd1 { [%clk 0:01:49] } Bd5 { [%clk 0:01:48] } 27. Rd3 { [%clk 0:01:16] } e4 { [%clk 0:01:45] } 28. Rd4 { [%clk 0:00:58] } Re6 { [%clk 0:01:34] } 29. Rxd8+ { [%clk 0:00:50] } Qxd8 { [%clk 0:01:09] } 30. Qxd8+ { [%clk 0:00:50] } Kg7 { [%clk 0:01:07] } 31. Qd7 { [%clk 0:00:38] } e3 { [%clk 0:00:57] } 32. fxe3 { [%clk 0:00:36] } Rxe3 { [%clk 0:00:56] } 33. Qxb7 { [%clk 0:00:33] } Rb3 { [%clk 0:00:48] } 34. Qe7 { [%clk 0:00:30] } Rxb2 { [%clk 0:00:43] } 35. Rf4 { [%clk 0:00:27] } Rb1+ { [%clk 0:00:38] } 36. Kf2 { [%clk 0:00:26] } Rb2+ { [%clk 0:00:35] } 37. Ke3 { [%clk 0:00:22] } Rxg2 { [%clk 0:00:29] } 38. Qf6+ { [%clk 0:00:21] } Kg8 { [%clk 0:00:26] } 39. Rh4 { [%clk 0:00:17] } h5 { [%clk 0:00:21] } 40. Re4 { [%clk 0:00:14] } Bxe4 { [%clk 0:00:15] } 41. Kxe4 { [%clk 0:00:13] } Re2+ { [%clk 0:00:12] } 42. Kd3 { [%clk 0:00:12] } Rxh2 { [%clk 0:00:09] } 43. Qxc6 { [%clk 0:00:10] } Kg7 { [%clk 0:00:06] } 44. Qe8 { [%clk 0:00:07] } Rh3+ { [%clk 0:00:04] } 45. Ke2 { [%clk 0:00:04] } Rh2+ { [%clk 0:00:02] } 46. Kf3 { [%clk 0:00:03] } Rh3+ { [%clk 0:00:01] } 47. Kg2 { [%clk 0:00:03] } 1-0
)_";
      
  CHECK(PGN::Parse(kGame, &pgn));
  CHECK(pgn.result == PGN::Result::WHITE_WINS);
  CHECK(pgn.GetTimeControl() == std::make_pair(300, 1));
  CHECK(pgn.GetTermination() == PGN::Termination::TIME_FORFEIT);
  
  Position pos;

  Fates fates;
  for (const PGN::Move &m : pgn.moves) {
    ApplyMove(&pos, m.move.c_str(), &fates);
    printf("%s\n", pos.BoardString().c_str());
  }
}

static void ReadPGNUnterminated() {
  PGN pgn;
  const char *kGame =
      R"_([Event "Rated Correspondence game"]
[Site "https://lichess.org/9R8Q3Kkb"]
[White "delfinacasanova"]
[Black "Adrimm"]
[Result "*"]
[UTCDate "2017.06.30"]
[UTCTime "22:00:05"]
[WhiteElo "1290"]
[BlackElo "1372"]
[ECO "A03"]
[Opening "Bird Opening: Dutch Variation"]
[TimeControl "-"]
[Termination "Unterminated"]

1. f4 d5 2. Nf3 f6 3. Nh4 Nc6 4. b4 Nxb4 5. Ba3 Nc6 6. e3 b5 7. Bxb5 Rb8 8. Bd3 e6 9. Rf1 Nh6 10. Rf3 Ra8 11. Rg3 a5 12. Bb5 Ra6 13. Ba4 Qd7 14. Nc3 Bxa3 15. Rxg7 Qxg7 16. Rc1 *
    )_";
  CHECK(PGN::Parse(kGame, &pgn));
  CHECK(pgn.result == PGN::Result::OTHER);
  CHECK(pgn.GetTermination() == PGN::Termination::OTHER);
  CHECK(pgn.GetTimeControl() == std::make_pair(0, 0));
}

static void Regression2Game() {
  PGN pgn;
  const char *kGame =
      R"_([Event "Test"]

1. e4 e5 2. Nf3 d5 3. d3 Bg4 4. h3 Bh5
5. g4 dxe4 6. dxe4 Qxd1+ 7. Kxd1 Bg6 8. Bd3 Nf6
9. h4 Nxe4 10. Rf1 h5 11. Nxe5 Nc5 12. Re1 Nxd3
13. cxd3 Be7 14. Bg5 f6 15. Bxf6 gxf6 16. Nxg6 Rh7
17. gxh5 Nc6 18. h6 Rxh6 19. Nxe7 Nxe7 20. Nc3 Rxh4
21. Nd5 O-O-O 22. Nxe7+ Kd7 23. Nf5 Rf4 24. Re7+ Kc8
25. Rc1 c6 26. Ne3 Rxf2 27. b4 Rxd3+ 28. Ke1 Rdd2
29. Re8+ Kd7 30. Rd1 Re2+ 31. Kf1 Rxd1+ 32. Kxe2 Ra1
33. Re7+ Kxe7 34. Nf5+ Ke6 35. b5 cxb5 36. a4 Ra2+
)_";
      
  CHECK(PGN::Parse(kGame, &pgn));
  
  Position pos;

  Fates fates;
  for (const PGN::Move &m : pgn.moves) {
    printf("== %s ==\n", m.move.c_str());
    ApplyMove(&pos, m.move.c_str(), &fates);
    printf("%s\n", pos.BoardString().c_str());
  }
}


static void Regression1() {
  Position pos;
  CHECK(Position::ParseFEN(
	    "3K4/5p2/6q1/8/4q3/8/6k1/8 b - - 9 60",
	    &pos));
  printf("---- Regression 1 -----\n");
  printf("Start board:\n%s\n", pos.BoardString().c_str());
  Move move;
  CHECK(pos.ParseMove("Qge6", &move));
  CHECK(pos.IsLegal(move));
  printf("%d %d -> %d %d\n", move.src_row, move.src_col,
	 move.dst_row, move.dst_col);
  pos.ApplyMove(move);
  CHECK(move.src_row == 2 && move.src_col == 6);
  printf("Resulting board:\n%s\n", pos.BoardString().c_str());
}

static void Regression2() {
  Position pos;
  CHECK(Position::ParseFEN(
	    "4R3/pp1k4/2p2p2/8/1P6/4N3/P2r1r2/3RK3 b - - 5 30",
	    &pos));
  printf("---- Regression 2 -----\n");
  printf("Start board:\n%s\n", pos.BoardString().c_str());
  Move move;
  CHECK(pos.ParseMove("Re2+", &move));
  CHECK(pos.IsLegal(move));
  printf("%d %d -> %d %d\n", move.src_row, move.src_col,
	 move.dst_row, move.dst_col);
  pos.ApplyMove(move);
  CHECK(move.src_row == 6 && move.src_col == 5);
  printf("Resulting board:\n%s\n", pos.BoardString().c_str());
}

static void ValidMoves1() {
  const char *kGame = R"_([Event "Test"]
  1. e4 e5 2. f4 f5 3. Bb5 g6 4. Nf3 g5
5. Qe2 g4 6. b3 g3 7. Bb2 h5 8. Na3 c5
9. c4 a6 10. Bxa6 b5 11. cxb5 Nc6 12. b6 Na5
13. b7 h4 14. fxe5 d5
)_";

  PGN pgn;
  CHECK(PGN::Parse(kGame, &pgn));
  
  Position pos;

  Fates fates;
  for (const PGN::Move &m : pgn.moves) {
    CHECK(pos.HasLegalMoves());
    CHECK(!pos.GetLegalMoves().empty());
    ApplyMove(&pos, m.move.c_str(), &fates);
  }
  printf("%s\n", pos.BoardString().c_str());

  CHECK(!pos.IsMated());
  CHECK(pos.HasLegalMoves());
  vector<Position::Move> moves = pos.GetLegalMoves();
  // TODO: Could check for the specific expected moves here.
  CHECK(moves.size() == 51);
  
  /*
  for (Position::Move move : moves) {
    printf("  %c%d -> %c%d = %c\n",
	   'a' + move.src_col, 8 - move.src_row,
	   'a' + move.dst_col, 8 - move.dst_row,
	   Position::HumanPieceChar(move.promote_to));
  }
  */
}


static void ValidMoves2() {
  const char *kGame = R"_([Event "Test"]
1. e4 e5 2. f4 f5 3. Bb5 g6 4. Nf3 g5
5. Qe2 g4 6. b3 g3 7. Bb2 h5 8. Na3 c5
9. c4 a6 10. Bxa6 b5 11. cxb5 Nc6 12. b6 Na5
13. b7 h4 14. fxe5 d5 15. Bb5+ Qd7 16. bxa8=Q Nf6
17. exf6 Rh5 18. Bxd7+ Kd8 19. f7 Bh6 20. f8=R+ Kc7
21. Qc6+ Kb8 22. Qxc8+ Ka7 23. Nb5+ Kb6 24. Rf6+ Nc6
25. Rxc6+ Ka5 26. b4+ Ka4 27. Qa8+ Kxb4 28. Qa3#
)_";

  PGN pgn;
  CHECK(PGN::Parse(kGame, &pgn));
  
  Position pos;

  auto IsInVector =
    [](const std::vector<Move> &vec,
       Move m) {
      for (const Move &mm : vec) {
	if (Position::MoveEq(m, mm)) return true;
      }
      return false;
    };

  auto LegalMoveStr =
    [](const Position &pos, const std::vector<Move> &vec) {
      string ret;
      for (Move m : vec) {
	if (!ret.empty()) ret += ", ";
	ret += pos.LongMoveString(m);
      }
      return ret;
    };

  auto NoDupes =
    [](const std::vector<Move> &vec) {
      for (int i = 0; i < vec.size(); i++) {
	for (int j = 0; j < vec.size(); j++) {
	  if (i != j &&
	      Position::MoveEq(vec[i], vec[j]))
	    return false;
	}
      }
      return true;
    };
  
  for (const PGN::Move &m : pgn.moves) {
    CHECK(pos.HasLegalMoves());
    std::vector<Move> legal_moves = pos.GetLegalMoves();
	
    Move move;
    CHECK(pos.ParseMove(m.move.c_str(), &move));
    CHECK(pos.IsLegal(move));
    CHECK(NoDupes(legal_moves)) << LegalMoveStr(pos, legal_moves);
    CHECK(IsInVector(legal_moves, move))
      << "In this position:\n" << pos.BoardString()
      << "\nExpected '" << m.move << "' to be among legal moves:\n"
      << LegalMoveStr(pos, legal_moves);

    // None of these in the above game.
    CHECK(!pos.IsCastling(move));
    CHECK(!pos.IsEnPassant(move));
    ApplyMove(&pos, m.move.c_str(), nullptr);
  }
  printf("%s\n", pos.BoardString().c_str());
  CHECK(pos.IsMated());
  CHECK(!pos.HasLegalMoves());

  int kingrow, kingcol;
  std::tie(kingrow, kingcol) = pos.GetCurrentKing();
  CHECK(kingrow == 4 && kingcol == 1) << kingrow << ", " << kingcol;
}

static void TestShortMove() {
  // With + for check filtered out.
  vector<string> pgn_moves = {
  "e4", "d5", "exd5", "e5", "dxe6", "Ne7", "g3", "h5",
  "Bg2", "a5", "Nh3", "Rh6", "O-O", "Ra6", "a4", "Nbc6",
  "Ra3", "Ne5", "Re1", "N7g6", "Rae3", "Nh8", "exf7", "Nxf7",
  "Rxe5", "Rhe6", "Rxe6", "Kd7", "d4", "Ba3", "d5", "Bxb2",
  "Rh6", "Ne5", "d6", "Ke8", "dxc7", "Bxh3", "cxd8=R", "Kf7",
  "Re8", "Nf3", "Kh1", "Bd7", "R1e6", "Bxa4", "Ref6", "gxf6",
  "Qxf3", "Bxc2", "g4", "hxg4", "Qxg4", "a4", "Be3", "a3",
  "Bxb7", "a2", "f3", "a1=Q", "Qe6", "Kg7", "Rhh8", "Qxb1",
  "Kg2", "Rxe6", "Rxe6", "Kxh8", "Re8", "Kg7", "Bh6", "Kxh6",
  "Rh8", "Bh7", "Rxh7", "Kxh7", "Be4", "Kg7", "Bxb1", "f5",
  "Bxf5", "Bd4", "h4", "Kh6"
  };

  Position pos;
  for (const string &move_string : pgn_moves) {
    Move m;
    CHECK(pos.ParseMove(move_string.c_str(), &m)) << move_string;
    CHECK(pos.IsLegal(m)) << move_string;
    string sm = pos.ShortMoveString(m);
    CHECK(move_string == sm) << "Wanted " << move_string
			     << " but got " << sm << " in:\n"
			     << pos.BoardString();
    pos.ApplyMove(m);
  }
}

static void RegressionBxa8n() {
  Position pos;
  CHECK(Position::ParseFEN(
	    "1rbqkbnr/pPpppppp/8/P7/8/8/2PPPPPP/RNBQKBNR w KQk - 1 7",
	    &pos));
  CHECK(pos.PieceAt(0, 0) == Position::EMPTY);

  Move m;
  m.src_row = 1;
  m.src_col = 1;
  m.dst_row = 0;
  m.dst_col = 0;
  m.promote_to = Position::KNIGHT | Position::WHITE;
  CHECK(!pos.IsLegal(m)) << pos.BoardString();
  // This would be a bad test because ParseMove is allowed to return a
  // different legal move since the input is illegal.
  // CHECK(!pos.ParseMove("bxa8=N", &m)) << pos.BoardString();
}

static void TestEp() {
  const char *kGame = R"_([Event "Test"]
1. e4 e5 2. Ke2 Ke7 3. f4 exf4 4. g4 
)_";
  Position pos = MakePos(kGame);
  CHECK(!pos.IsMated());
  string fen = pos.ToFEN(0, 4);
  CHECK("rnbq1bnr/ppppkppp/8/8/4PpP1/8/PPPPK2P/RNBQ1BNR b - g3 0 4" ==
	fen) << fen;
  Move ep;
  CHECK(pos.ParseMove("fxg3", &ep));
  CHECK(pos.IsEnPassant(ep));
  CHECK(pos.IsLegal(ep));
  CHECK(pos.HasLegalMoves());
  int kingrow, kingcol;
  std::tie(kingrow, kingcol) = pos.GetCurrentKing();
  CHECK(kingrow == 1 && kingcol == 4) << kingrow << ", " << kingcol;
}

static void TestParseMoves() {
  std::vector<PGN::Move> moves;
  CHECK(PGN::ParseMoves("1. d4 d5 2. Nf3", &moves));
  CHECK_EQ(moves.size(), 3);
  CHECK_EQ(moves[0].move, "d4");
  CHECK_EQ(moves[1].move, "d5");
  CHECK_EQ(moves[2].move, "Nf3");
}

int main(int argc, char **argv) {
  CheckInit();
  TestParseMoves();

  PlayGame(kGame1);
  PlayGame(kGame2);

  ReadPGN();
  ReadPGNUnterminated();
  
  Regression1();
  Regression2();

  Regression2Game();

  PromotionRegression();

  ValidMoves1();
  ValidMoves2();

  TestEp();

  TestShortMove();

  RegressionBxa8n();
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
  "Bxf5", "Bd4", "h4", "Kh6"


1. e3 f5 2. e4 f4 3. g4 fxg3 4. f4 a5
5. Nh3 g2 6. Bc4 b5 7. Nc3 c5 8. Nd5 e5
9. Ne3 g5 10. Nf5 h5 11. Nh4 d5 12. Nf3 gxh1=R+
13. Nhg1 g4 14. b4 h4 15. Ba3 d4 16. Qe2 g3
17. O-O-O h3 18. Nxh3 d3 19. Nfg1 Ra7 20. c3 Bd6
21. fxe5 Nh6 22. e6 O-O 23. e7+ Rf7 24. exd8=B bxc4
25. b5 a4 26. b6 Rae7 27. e5 Re6 28. Bb4 g2
29. Ba5 Bd7 30. b7 Nc6 31. Bdc7 Ng4 32. b8=B Ref6
33. e6 Rf1 34. e7 R7f2 35. a3 Nd8 36. exd8=B Bg3
37. Bg5 Rf3 38. Bgf4 Ne5 39. Bcxe5 Bxh3 40. Bac7 Be1
41. Qxd3 R1f2 42. Qc2 Rxg1 43. d4 cxd3 44. Qxd3 Rxd3
45. Rxd3 Bxc3+ 46. Rd1 Rxd1+ 47. Kxd1 Rf1+ 48. Kc2 Rf2+
49. Kxc3 g1=R 50. Kc4 Rxf4+ 51. Bxf4 Rg4 52. Kxc5 Rxf4
53. Bxf4 Be6 54. Kd6 Bd7 55. Kxd7 Kf7 56. h4 Kg6
57. h5+ Kxh5

"e3", "f5", "e4", "f4", "g4", "fxg3", "f4", "a5", "Nh3", "g2", "Bc4",
"b5", "Nc3", "c5", "Nd5", "e5", "Ne3", "g5", "Nf5", "h5", "Nh4", "d5",
"Nf3", "gxh1=R+", "Nhg1", "g4", "b4", "h4", "Ba3", "d4", "Qe2", "g3",
"O-O-O"," h3", "Nxh3", "d3", "Nfg1", "Ra7", "c3", "Bd6", "fxe5",
"Nh6", "e6", "O-O", "e7",+" Rf7", "exd8=B", "bxc4", "b5", "a4", "b6",
"Rae7", "e5", "Re6", "Bb4", "g2", "Ba5", "Bd7", "b7", "Nc6", "Bdc7",
"Ng4", "b8=B", "Ref6", "e6", "Rf1", "e7", "R7f2", "a3", "Nd8",
"exd8=B", "Bg3", "Bg5", "Rf3", "Bgf4", "Ne5", "Bcxe5", "Bxh3", "Bac7",
"Be1", "Qxd3", "R1f2", "Qc2", "Rxg1", "d4", "cxd3", "Qxd3", "Rxd3",
"Rxd3", "Bxc3+", "Rd1", "Rxd1+", "Kxd1", "Rf1+", "Kc2", "Rf2+",
"Kxc3", "g1=R", "Kc4", "Rxf4+", "Bxf4", "Rg4", "Kxc5", "Rxf4", "Bxf4",
"Be6", "Kd6", "Bd7", "Kxd7", "Kf7", "h4", "Kg6", "h5+", "Kxh5",
*/
