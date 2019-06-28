
#include "player.h"

#include <string>
#include <mutex>
#include <cstdint>
#include <memory>

#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/util.h"
#include "../cc-lib/bignum/big.h"

#include "player-util.h"
#include "numeric-player.h"


using namespace std;
using int64 = int64_t;
using Move = Position::Move;


static const char kPiDigits[] =
  "141592653589793238462643383279502884197169399375105820974944592307816"
  "406286208998628034825342117067982148086513282306647093844609550582231"
  "725359408128481117450284102701938521105559644622948954930381964428810"
  "975665933446128475648233786783165271201909145648566923460348610454326"
  "648213393607260249141273724587006606315588174881520920962829254091715"
  "364367892590360011330530548820466521384146951941511609433057270365759"
  "591953092186117381932611793105118548074462379962749567351885752724891"
  "227938183011949129833673362440656643086021394946395224737190702179860"
  "943702770539217176293176752384674818467669405132000568127145263560827"
  "785771342757789609173637178721468440901224953430146549585371050792279"
  "689258923542019956112129021960864034418159813629774771309960518707211"
  "349999998372978049951059731732816096318595024459455346908302642522308"
  "253344685035261931188171010003137838752886587533208381420617177669147"
  "303598253490428755468731159562863882353787593751957781857780532171226"
  "806613001927876611195909216420198938095257201065485863278865936153381"
  "827968230301952035301852968995773622599413891249721775283479131515574"
  "857242454150695950829533116861727855889075098381754637464939319255060"
  "400927701671139009848824012858361603563707660104710181942955596198946"
  "767837449448255379774726847104047534646208046684259069491293313677028"
  "989152104752162056966024058038150193511253382430035587640247496473263"
  "914199272604269922796782354781636009341721641219924586315030286182974"
  "555706749838505494588586926995690927210797509302955321165344987202755"
  "960236480665499119881834797753566369807426542527862551818417574672890"
  "977772793800081647060016145249192173217214772350141441973568548161361"
  "157352552133475741849468438523323907394143334547762416862518983569485"
  "562099219222184272550254256887671790494601653466804988627232791786085"
  "784383827967976681454100953883786360950680064225125205117392984896084"
  "128488626945604241965285022210661186306744278622039194945047123713786"
  "96095636437191728746776465757396241389086583264599581339047802759009";

static const char kEDigits[] =
  "718281828459045235360287471352662497757247093699959574966967627724076"
  "630353547594571382178525166427427466391932003059921817413596629043572"
  "900334295260595630738132328627943490763233829880753195251019011573834"
  "187930702154089149934884167509244761460668082264800168477411853742345"
  "442437107539077744992069551702761838606261331384583000752044933826560"
  "297606737113200709328709127443747047230696977209310141692836819025515"
  "108657463772111252389784425056953696770785449969967946864454905987931"
  "636889230098793127736178215424999229576351482208269895193668033182528"
  "869398496465105820939239829488793320362509443117301238197068416140397"
  "019837679320683282376464804295311802328782509819455815301756717361332"
  "069811250996181881593041690351598888519345807273866738589422879228499"
  "892086805825749279610484198444363463244968487560233624827041978623209"
  "002160990235304369941849146314093431738143640546253152096183690888707"
  "016768396424378140592714563549061303107208510383750510115747704171898"
  "610687396965521267154688957035035402123407849819334321068170121005627"
  "880235193033224745015853904730419957777093503660416997329725088687696"
  "640355570716226844716256079882651787134195124665201030592123667719432"
  "527867539855894489697096409754591856956380236370162112047742722836489"
  "613422516445078182442352948636372141740238893441247963574370263755294"
  "448337998016125492278509257782562092622648326277933386566481627725164"
  "019105900491644998289315056604725802778631864155195653244258698294695"
  "930801915298721172556347546396447910145904090586298496791287406870504"
  "895858671747985466775757320568128845920541334053922000113786300945560"
  "688166740016984205580403363795376452030402432256613527836951177883863"
  "874439662532249850654995886234281899707733276171783928034946501434558"
  "897071942586398772754710962953741521115136835062752602326484728703920"
  "764310059584116612054529703023647254929666938115137322753645098889031"
  "360205724817658511806303644281231496550704751025446501172721155519486"
  "68508003685322818315219600373562527944951582841882947876108526398139";

namespace {

struct BinaryDigits {
  explicit BinaryDigits(const string &filename) {
    string contents = Util::ReadFile(filename);
    CHECK(!contents.empty()) << filename;

    // Skip "3." in 3.14159...
    CHECK(contents[1] == '.') << filename;
    bits.reserve((contents.size() - 2) * 4);
    for (int i = 2; i < contents.size(); i++) {
      // Values are actually ASCII hex digits.
      uint8 c = contents[i];
      CHECK(Util::IsHexDigit(c)) << filename;
      const uint8 h = Util::HexDigitValue(c);
      // One "digit" is four bits.
      bits.push_back(!!(h & 0b1000));
      bits.push_back(!!(h & 0b0100));
      bits.push_back(!!(h & 0b0010));
      bits.push_back(!!(h & 0b0001));
    }
  }

  vector<bool> bits;
};

const vector<bool> &GetPi() {
  static BinaryDigits *pi = new BinaryDigits("pi.txt");
  return pi->bits;
}

const vector<bool> &GetE() {
  static BinaryDigits *e = new BinaryDigits("e.txt");
  return e->bits;
}

template<const vector<bool> &(*F)(), const char *(*CONSTANT)()>
struct BinaryPlayer : public Player {
  BinaryPlayer() : bits(F()) {}

  struct BinaryGame : public PlayerGame {
    explicit BinaryGame(const BinaryPlayer &p) : player(p) {}
    
    // Most canonical way to order these is asciibetically by their
    // algebraic move names.
    struct LabeledMove {
      Move m;
      string move_string;
    };

    // PERF: Could cache the previous call so that GetMove/ForceMove
    // pair doesn't need to repeat the work.
    void InternalMove(const Position &orig_pos,
		      Position::Move *m, int *bits_used,
		      Explainer *explainer) {
      Position pos = orig_pos;

      std::vector<LabeledMove> labeled;
      std::vector<Position::Move> moves = pos.GetLegalMoves();
      CHECK(!moves.empty());
      if (moves.size() == 1) {
	*m = moves[0];
	*bits_used = 0;
	if (explainer != nullptr) {
	  explainer->SetMessage(
	      StringPrintf("Forced move: %s",
			   pos.ShortMoveString(moves[0]).c_str()));
	}
	return;
      }
      
      labeled.reserve(moves.size());
      for (const Move &m : moves) {
	LabeledMove lm;
	lm.m = m;
	lm.move_string = pos.ShortMoveString(m);
	labeled.push_back(lm);
      }
      
      std::sort(labeled.begin(),
		labeled.end(),
		[](const LabeledMove &a,
		   const LabeledMove &b) {
		  return a.move_string < b.move_string;
		});

      int num_moves = labeled.size();
      // Mask all ones, and no more than 2047 (2^12 - 1), since there
      // are fewer than this many distinct possible moves in ANY
      // position.
      uint32 mask = num_moves - 1;
      mask |= mask >> 1;
      mask |= mask >> 2;
      mask |= mask >> 4;
      mask |= mask >> 8;
      mask |= mask >> 16;

      auto GetNumBits = [mask]() {
	  for (int i = 0; i < 13; i++) {
	    if (!(mask & (1 << i))) {
	      return i;
	    }
	  }
	  LOG(FATAL) << "Impossible " << mask;
	  return 0;
	};
      
      const int num_bits = GetNumBits();
      CHECK((1 << num_bits) - 1 == mask) << num_bits << " " << mask;

      CHECK(stream_idx < player.bits.size() - 16) << "Ran out of bits!! "
						  << stream_idx;
      int used = 0, rejections = 0;
      for (;;) {
	uint32 r = 0u;
	for (int i = 0; i < num_bits; i++) {
	  r <<= 1;
	  r |= player.bits[stream_idx + used + i];
	}
	used += num_bits;
	if (r < num_moves) {
	  *bits_used = used;
	  *m = labeled[r].m;
	  if (explainer != nullptr) {
	    explainer->SetMessage(
		StringPrintf("@%d Used %d bits (%d rejections) to select "
			     "from %d moves:",
			     stream_idx, used, rejections, num_moves));
	    vector<tuple<Position::Move, int64_t, string>> vec;
	    for (int i = 0; i < labeled.size(); i++) {
	      vec.emplace_back(labeled[i].m, i,
			       i == r ?
			       StringPrintf("%s <-",
					    labeled[i].move_string.c_str()) :
			       labeled[i].move_string);
	    }
	    explainer->SetScoredMoves(vec);
	  }
	  return;
	}
	rejections++;
      }
    }
    
    // Conceptually we just want to use some of the bits to choose a
    // move. This is complicated by the get/force interface for
    // stateful players, alas. In any given state, we will use a
    // deterministic number of bits depending on the number of legal
    // moves and the position in the stream. This can be anything from
    // 0 (single move forced) to arbitrarily many if we get unlucky
    // with the rejection sampling! ForceMove ignores the move but
    // advances the stream this number of bits. GetMove computes the
    // move but does not advance the stream. 
    void ForceMove(const Position &pos, Position::Move m_ignored) override {
      Position::Move move_ignored;
      int bits_used = 0;
      InternalMove(pos, &move_ignored, &bits_used, nullptr);
      stream_idx += bits_used;
    }
    
    Move GetMove(const Position &pos, Explainer *explainer) override {
      Position::Move move;
      int bits_used = 0;
      InternalMove(pos, &move, &bits_used, explainer);
      return move;
    }

    int stream_idx = 0;
    const BinaryPlayer &player;
  };

  PlayerGame *CreateGame() override {
    return new BinaryGame(*this);
  }
  
  string Name() const override {
    return StringPrintf("binary_%s", CONSTANT());
  }
  string Desc() const override {
    return StringPrintf(
	"Choose the nth legal move, using bits from the binary "
	"expansion of %s", CONSTANT());
  }

  const vector<bool> &bits;
};

const char *PiName() { return "pi"; }
const char *EName() { return "e"; }


// Only thing we need for chess playing is this function.
// It takes n (number of moves) and figures out which 1/n region
// the rational r falls within. It then scales that region such
// that it now nominally spans [0,1] and returns the new rational
// r' within that.
static void Forward(const BigRat &r, int n,
		    int *k_chosen, BigRat *next_rat) {
  // PERF!
  // CHECK(1 != BigRat::Compare(BigRat(0, 1), r));
  // CHECK(-1 == BigRat::Compare(r, BigRat(1, 1)));

  // We have r = m/d.
  // First find k such that k/n <= r < (k+1)/n.

  for (int k = 0; k < n; k++) {
    // Invariant: r is known to be >= k/n.
    // PERF!
    // CHECK(1 != BigRat::Compare(BigRat(k, n), r));
    // Next bound to consider is (k + 1)/n.
    BigRat ubound(k + 1, n);
    int cmp = BigRat::Compare(r, ubound);
    if (cmp == BQ_LT) {
      // The first time this happens, we know r is in the
      // interval k/n to k+1/n.
      // So r' = (r - k/n)
      //         ---------
      //         (1 / n)
      // which is the same as n * (r - k/n)
      // which is nr - k.

      if (next_rat != nullptr) {
	BigRat nr = BigRat::Times(BigRat(n, 1), r);
	*next_rat = BigRat::Minus(nr, BigRat(k, 1));
      }
      *k_chosen = k;
      return;
    }
  }

  CHECK(false) << "Should be impossible if r is in [0, 1).";
}


template<const char *(*CONSTANT)()>
struct RationalPlayer : public Player {
  RationalPlayer(const string &digits) {
    BigInt numer(digits);
    int num_digits = digits.size();
    string denom = "1";
    denom.reserve(num_digits + 1);
    for (int i = 0; i < num_digits; i++)
      denom.push_back('0');
    start_lb = BigRat(BigInt(numer), BigInt(denom));
    start_ub = BigRat::Plus(start_lb, BigRat(BigInt("1"), BigInt(denom)));
  }

  struct RationalGame : public PlayerGame {
    explicit RationalGame(const RationalPlayer &p)
      : player(p), rat_lb(p.start_lb), rat_ub(p.start_ub) {}
    
    // Most canonical way to order these is asciibetically by their
    // algebraic move names.
    struct LabeledMove {
      Move m;
      string move_string;
    };

    // PERF: Could cache the previous call so that GetMove/ForceMove
    // pair doesn't need to repeat the work.
    void InternalMove(const Position &orig_pos,
		      Position::Move *m,
		      BigRat *next_lb_out,
		      BigRat *next_ub_out,
		      Explainer *explainer) {
      Position pos = orig_pos;

      std::vector<LabeledMove> labeled;
      std::vector<Position::Move> moves = pos.GetLegalMoves();
      CHECK(!moves.empty());
      if (moves.size() == 1) {
	*m = moves[0];
	if (next_lb_out != nullptr) *next_lb_out = rat_lb;
	if (next_ub_out != nullptr) *next_ub_out = rat_ub;
	if (explainer != nullptr) {
	  explainer->SetMessage(
	      StringPrintf("Forced move: %s",
			   pos.ShortMoveString(moves[0]).c_str()));
	}
	return;
      }
      
      labeled.reserve(moves.size());
      for (const Move &m : moves) {
	LabeledMove lm;
	lm.m = m;
	lm.move_string = pos.ShortMoveString(m);
	labeled.push_back(lm);
      }
      
      std::sort(labeled.begin(),
		labeled.end(),
		[](const LabeledMove &a,
		   const LabeledMove &b) {
		  return a.move_string < b.move_string;
		});

      int num_moves = labeled.size();

      int chosen_lb = 0, chosen_ub = 0;
      Forward(rat_lb, num_moves, &chosen_lb, next_lb_out);
      Forward(rat_ub, num_moves, &chosen_ub, next_ub_out);
      CHECK(chosen_lb == chosen_ub) << "Imprecise!! " << CONSTANT();
      int chosen = chosen_lb;
      
      CHECK(chosen < num_moves);
      *m = labeled[chosen].m;

      if (explainer != nullptr) {
	explainer->SetMessage(
	    StringPrintf("Select from %d moves:", num_moves));
	vector<tuple<Position::Move, int64_t, string>> vec;
	for (int i = 0; i < labeled.size(); i++) {
	  vec.emplace_back(labeled[i].m, i,
			   i == chosen ?
			   StringPrintf("%s <-",
					labeled[i].move_string.c_str()) :
			   labeled[i].move_string);
	}
	explainer->SetScoredMoves(vec);
      }
      return;
    }
    
    // As above.
    void ForceMove(const Position &pos, Position::Move m_ignored) override {
      Position::Move move_ignored;
      InternalMove(pos, &move_ignored, &rat_lb, &rat_ub, nullptr);
    }
    
    Move GetMove(const Position &pos, Explainer *explainer) override {
      Position::Move move;
      InternalMove(pos, &move, nullptr, nullptr, explainer);
      return move;
    }

    const RationalPlayer &player;
    BigRat rat_lb, rat_ub;
  };

  PlayerGame *CreateGame() override {
    return new RationalGame(*this);
  }
  
  string Name() const override {
    return StringPrintf("rational_%s", CONSTANT());
  }
  string Desc() const override {
    return StringPrintf(
	"Choose the nth legal move, as though arithmetically coded "
	"using %s", CONSTANT());
  }

  BigRat start_lb, start_ub;
};


}  // namespace



Player *BinaryPi() {
  return new BinaryPlayer<GetPi, PiName>;
}

Player *BinaryE() {
  return new BinaryPlayer<GetE, EName>;
}

Player *RationalPi() {
  return new RationalPlayer<PiName>(kPiDigits);
}

Player *RationalE() {
  return new RationalPlayer<EName>(kEDigits);
}
