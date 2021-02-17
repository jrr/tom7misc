
// Represents a game in a compact form. Each half-move takes 12 bits.
// If the number is in [0, 1792), then it is a non-promoting move,
// as given in pack.h. Otherwise it is a promoting move; we have (8
// (move straight) + 7 (capture left) + 7 (capture right)) * 2 (sides)
// * 4 (knight, bishop, rook, queen) = 176 of these (but they are not
// densely packed; see below).

#ifndef _PACKEDGAME_H
#define _PACKEDGAME_H

#include <vector>
#include <string>
#include <cstdint>

#include "chess.h"
#include "pack.h"
// XXX maybe make this hermetic after I'm confident in it
#include "base/logging.h"
#include "crypt/sha256.h"

struct PackedGame {
  PackedGame() {}
  static std::vector<std::pair<uint64_t, PackedGame>>
  SplitFile(const std::vector<uint8_t> &contents);

  enum class Result {
    WHITE_WINS,
    BLACK_WINS,
    DRAW,
  };

  static uint16_t PackMove(Position::Move move);
  static Position::Move UnpackMove(uint16_t twelve_bits);

  std::vector<uint8_t> Serialize() const;

  int NumMoves() const { return num_moves; }

  void SetResult(Result r) { result = r; }
  Result GetResult() const { return result; }

  uint16_t GetMove(int i) const;

  void PushMove(uint16_t twelve_bits) {
    if (num_moves & 1) {
      packed_moves[packed_moves.size() - 1] |= (twelve_bits >> 8);
      packed_moves.push_back(twelve_bits & 0xFF);
    } else {
      packed_moves.push_back(twelve_bits >> 4);
      packed_moves.push_back(twelve_bits << 4);
    }
    num_moves++;
  }

  // Hash code for the game itself, which only depends on its exact
  // move sequence and result.
  uint64_t HashCode() const;

private:
  inline uint8_t PackedMovesBound(int idx) const {
    CHECK(idx >= 0);
    if (idx >= packed_moves.size()) return 0;
    else return packed_moves[idx];
  }
  static inline uint8_t ResultByte(Result r) {
    switch (r) {
    case Result::WHITE_WINS: return 0b10;
    case Result::BLACK_WINS: return 0b01;
    default:
    case Result::DRAW: return 0b00;
    }
  }

  int num_moves = 0;
  std::vector<uint8_t> packed_moves;
  Result result = Result::DRAW;
};


inline uint16_t PackedGame::PackMove(Position::Move move) {
  if (move.promote_to != 0) {
    auto WhatBits = [](uint8_t p) {
        switch (p & Position::TYPE_MASK) {
        case Position::QUEEN:  return 0b01100000;
        case Position::ROOK:   return 0b01000000;
        case Position::BISHOP: return 0b00100000;
        case Position::KNIGHT: return 0b00000000;
        default:
          LOG(FATAL) << "Bad promote_to " << (int)p;
          return 0;
        }
      };
    auto DeltaBits = [](uint8_t src_col, uint8_t dst_col) {
        if (dst_col < src_col) return 0b10;
        else if (dst_col > src_col) return 0b01;
        else return 0b00;
      };

    // If promoting, it must be a pawn move to the last
    // rank.
    CHECK(move.src_row == 6 || move.src_row == 1);
    // BPPCCCDD, where b is 1 if black's move,
    // PP is one of the four possible promotion pieces,
    // CCC is the source column,
    // DD is 10 (capture left), 00 (move straight), 01 (capture right)
    const uint8_t black_bit = move.src_row == 6 ? 0b10000000 : 0b00000000;
    const uint8_t what_bits = WhatBits(move.promote_to);
    const uint8_t col_bits = move.src_col << 2;
    const uint8_t delta_bits = DeltaBits(move.src_col, move.dst_col);

    const uint16_t promote_offset =
      (black_bit | what_bits | col_bits | delta_bits);
    return PACK_SIZE + promote_offset;
  } else {
    return Pack(move.src_row, move.src_col,
                move.dst_row, move.dst_col);
  }
}

inline Position::Move PackedGame::UnpackMove(uint16_t bits) {
  if (bits < PACK_SIZE) {
    int sr, sc, dr, dc;
    Unpack(bits, &sr, &sc, &dr, &dc);
    Position::Move move;
    move.promote_to = 0;
    move.src_row = sr;
    move.src_col = sc;
    move.dst_row = dr;
    move.dst_col = dc;
    return move;
  } else {
    static constexpr uint8_t WHAT_TABLE[4] = {
      Position::KNIGHT,
      Position::BISHOP,
      Position::ROOK,
      Position::QUEEN,
    };
    const uint16_t promote_bits = bits - PACK_SIZE;

    const bool black_bit = !!(promote_bits & 0b10000000);
    const uint8_t typ = WHAT_TABLE[(promote_bits >> 5) & 0b11];
    const uint8_t col_bits = (promote_bits >> 2) & 0b111;
    const uint8_t delta_bits = promote_bits & 0b11;
    const uint8_t dst_col =
      delta_bits ? (delta_bits == 0b10 ? col_bits - 1 : col_bits + 1) :
      col_bits;
    Position::Move move;
    move.promote_to = typ | (black_bit ? Position::BLACK : Position::WHITE);
    move.src_row = black_bit ? 6 : 1;
    move.dst_row = black_bit ? 7 : 0;
    move.src_col = col_bits;
    move.dst_col = dst_col;
    return move;
  }
}


inline uint16_t PackedGame::GetMove(int i) const {
  // In the general case the twelve bits could span three bytes, but
  // there are actually only two alignments that we get if we start
  // without offset.
  //
  //
  // [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
  // |----------------------||----------------------|
  //           first byte       second byte
  // and
  //             [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
  // |----------------------||----------------------|
  // So, read three bytes:
  const int triple_idx = (i >> 1) * 3;
  // PERF: Only the third can legally be out of bounds. Compiler may
  // already be able to fold the later two bounds checks into the
  // first?
  const uint8_t c = PackedMovesBound(triple_idx + 2);
  const uint8_t b = PackedMovesBound(triple_idx + 1);
  const uint8_t a = PackedMovesBound(triple_idx);
  if (i & 1) {
    return (uint16_t)((b & 0b1111) << 8) | c;
  } else {
    return (uint16_t)(a << 4) | (b >> 4);
  }
}


#endif
