
#include "chessmaster.h"

#include <vector>
#include <cstdint>

#include "../cc-lib/threadutil.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/simplefm7.h"
#include "../fceulib/x6502.h"

using namespace std;
using uint8 = uint8_t;
using Move = Position::Move;

Chessmaster::~Chessmaster() {}

Chessmaster::Chessmaster(int level) : level(level) {}

// Board isn't contiguous:
// 8 bytes of pieces (black's pieces), skip 8 bytes
// 8 bytes of pieces (black's pawns), skip 8 bytes
// etc.
// The skipped bytes appear to mirror the piece data
// somehow, but it's a mystery to me. (Looks like
// maybe scratch space for game tree search?)
//
// If you make modifications to the "main" board
// while in "board setup" mode, then when you return
// to play mode, they will take effect. So this
// appears to be the primary representation.
static constexpr int BOARD_START_LOC = 0x100;
static constexpr int WHOSE_MOVE_LOC = 0x665;

// Appears to be 1 when ready for player input.
// BUT when the opponent checkmates me, this flag doesn't
// go true unless I press select to dismiss the message.
// XXX need a solution here
static constexpr int READY_FOR_INPUT = 0x642;

static constexpr int MAX_WAIT_FRAMES = 60 * 12;

// Convert Position::Piece to Chessmaster representation.
static uint8 PieceMaster(uint8 p) {
  if (p == Position::EMPTY) return 0;
  uint8 m = ((p & Position::COLOR_MASK) == Position::BLACK) ? 0x20 : 0x10;
  switch (p & Position::TYPE_MASK) {
  case Position::PAWN:
    return m | 0x00;
  case Position::C_ROOK:
  case Position::ROOK:
    return m | 0x03;
  case Position::KNIGHT:
    return m | 0x02;
  case Position::BISHOP:
    return m | 0x04;
  case Position::QUEEN:
    return m | 0x05;
  case Position::KING:
    return m | 0x01;
  default:
    LOG(FATAL) << "Bad piece " << p;
    return 0;
  }
}

// Convert from Chessmaster to native.
static uint8 MasterPiece(uint8 p) {
  uint8 color = 0;
  switch (p & 0xF0) {
  case 0x00: return Position::EMPTY;
  case 0x10: color = Position::WHITE; break;
  case 0x20: color = Position::BLACK; break;
  default:
    LOG(FATAL) << "Bad master " << p;
    return 0;
  }

  switch (p & 0x0F) {
  case 0x03: return color | Position::ROOK;
  case 0x02: return color | Position::KNIGHT;
  case 0x04: return color | Position::BISHOP;
  case 0x05: return color | Position::QUEEN;
  case 0x01: return color | Position::KING;
  case 0x00: return color | Position::PAWN;
  default:
    LOG(FATAL) << "Bad master " << p;
    return 0;
  }
}

bool Chessmaster::WaitInputReady() {
  for (int i = 0; i < MAX_WAIT_FRAMES; i++) {
    emu->Step(0, 0);
    const uint8 *ram = emu->GetFC()->fceu->RAM;
    if (ram[READY_FOR_INPUT])
      return true;
  }
  return false;
}

void Chessmaster::InitEngine() {
  if (emu.get() != nullptr)
    return;

  emu.reset(Emulator::Create("chessmaster.nes"));
  CHECK(emu.get() != nullptr);

  // This movie starts the game and puts it in board setup mode.
  // No need to tune this for speed because we only do it during
  // initialization.
  vector<uint8> movie =
    SimpleFM7::ParseString(
	"!32_8t26_8t220_8t43_8t28_6c65_3d7_4d5_3d16_5d24_5d26_4a195_");

	//	"!32_8t26_8t200_8t43_8t28_6c65_3d7_4d5_3d16_5d24_5d26_4a50_");

  for (uint8 c : movie) emu->Step(c, 0);

  // Now in board setup mode with it being white's turn (human).
  // Cursor is at the top of the setup menu and on "Set up the board"
  // (naturally) for the main menu.
  edit_save = emu->SaveUncompressed();

  return_to_game = SimpleFM7::ParseString(
      "!" // player 1
      "1_" // one frame buffer after modifying board
      "8c28_8u28_8u28_8a28_" // select, up, up, a  "setup complete"
      "24_"); // wait
  change_sides = SimpleFM7::ParseString(
      "!" // player 1
      "4c12_4u8_4u8_4a8_" // select, up, up, a  "change sides"
      "24_");  // wait
}
  
Move Chessmaster::GetMove(const Position &pos) {
  // Get a move. The position must be legal and have moves!
  // If something goes wrong, returns a move from 0,0 to 0,0.
  Move move;
  move.src_col = 0;
  move.src_row = 0;
  move.dst_col = 0;
  move.dst_row = 0;
  move.promote_to = 0;

  const bool black = pos.BlackMove();
  const uint8 my_mask = black ? Position::BLACK : Position::WHITE;
  const uint8 your_mask = black ? Position::WHITE : Position::BLACK;
  
  {
    MutexLock ml(&emulator_m);
    InitEngine();
    
    // Place in the board editor.
    // I think it will make sense to have two of these; one for each
    // side. Otherwise, consider the invariant that the emulator is
    // just always positioned here.
    emu->LoadUncompressed(edit_save);

    // Blit the position into NES RAM.
    {
      uint8 *ram = emu->GetFC()->fceu->RAM;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const uint8 p = pos.PieceAt(r, c);
	  ram[BOARD_START_LOC + (r * 16) + c] = PieceMaster(p);
	}
      }
    }
    
    {
      printf("after setting it:\n");
      const uint8 *ram = emu->GetFC()->fceu->RAM;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const uint8 op = pos.PieceAt(r, c);
	  const uint8 np = MasterPiece(ram[BOARD_START_LOC + (r * 16) + c]);
	  printf("%c ", Position::HumanPieceChar(np));
	}
	printf("\n");
      }
    }

    emu->Step(0, 0);
    {
      printf("after one step:\n");
      const uint8 *ram = emu->GetFC()->fceu->RAM;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const uint8 op = pos.PieceAt(r, c);
	  const uint8 np = MasterPiece(ram[BOARD_START_LOC + (r * 16) + c]);
	  printf("%c ", Position::HumanPieceChar(np));
	}
	printf("\n");
      }
    }

    
    // XXX handle black/white's turn.
    // This currently assumes white's turn.

    for (uint8 c : return_to_game)
      emu->Step(c, 0);

    {
      printf("after returning to game:\n");
      const uint8 *ram = emu->GetFC()->fceu->RAM;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const uint8 op = pos.PieceAt(r, c);
	  const uint8 np = MasterPiece(ram[BOARD_START_LOC + (r * 16) + c]);
	  printf("%c ", Position::HumanPieceChar(np));
	}
	printf("\n");
      }
    }

    
    // Wait for input to be ready.
    if (!WaitInputReady())
      return move;
    
    for (uint8 c : change_sides)
      emu->Step(c, 0);

    if (!WaitInputReady())
      return move;

    // Now deduce move from the change in board state.
    {
      printf("after makemove:\n");
      const uint8 *ram = emu->GetFC()->fceu->RAM;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const uint8 op = pos.PieceAt(r, c);
	  const uint8 np = MasterPiece(ram[BOARD_START_LOC + (r * 16) + c]);
	  printf("%c ", Position::HumanPieceChar(np));
	}
	printf("\n");
      }
    }

    fflush(stdout);
    
    // XXX
    return move;
  }
}
