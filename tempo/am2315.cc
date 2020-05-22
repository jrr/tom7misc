#include "am2315.h"

#include <unistd.h>
#include <vector>
#include <cstdint>

#include "pi/bcm2835.h"
#include "base/logging.h"


using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;

static constexpr uint8 CMD_READREG = 0x03;
static constexpr uint8 REG_TEMP = 0x02;
static constexpr uint8 REG_HUM = 0x00;
static constexpr uint8 REG_INFO_START = 0x08;

// This thing seems to be fairly flakey.
// Some experiments reading every 500ms (alternating temp/hum):
// [success/attempts = rate]
// CLOCK_DIVIDER_2500: [524/1420 = 36.9%]
// CLOCK_DIVIDER_626: [378/770 = 49.1%]
// 500: [150/306 = 49.0%] (seemingly only the hum reads are working though)
// 400: [0/50 = 0.0%]
// _150: [0/268 = 0.0%]

// With a divider of 1000 and a pre-wakeup:
// [37626/37626 = 100.0%]

static uint16 CRC16(const uint8 *bytes, int len) {
  uint16 crc = 0xFFFF;
  for (int i = 0; i < len; i++) {
    crc ^= bytes[i];
    for (int j = 0; j < 8; j++) {
      if (crc & 0x0001) {
	crc >>= 1;
	crc ^= 0xA001;
      } else {
	crc >>= 1;
      }
    }
  }
  return crc;
}

void AM2315::Initialize() {
  CHECK(bcm2835_i2c_begin()) << "root? called bcm2835_init?";

  // Can tune this for speed, but for this sensor there's likely
  // very little we gain from a fast connection.
  // Conservative:
  // bcm2835_i2c_setClockDivider(BCM2835_I2C_CLOCK_DIVIDER_2500);
  bcm2835_i2c_setClockDivider(1000);

  // Test..
  bcm2835_i2c_setSlaveAddress(AM2315::ADDRESS);
}

static bool WriteVec(const std::vector<uint8> &msg) {
  return BCM2835_I2C_REASON_OK ==
    bcm2835_i2c_write((const char *)&msg[0], msg.size());
}

static bool ReadReg(uint8 reg, uint8 *ret_buf,
		    uint8 length, const char **err = nullptr) {
  const char *err_unused;
  if (err == nullptr) err = &err_unused;
  
  // "wake up sensor".
  // CircuitPython code does one write and a wait of 10ms here,
  // but I found that the first wakeup often fails. Doing it twice
  // but ignoring the first failure yields a pretty reliable result,
  // even when polling at 10-20hz. (Note that readings are only updated
  // every ~2 seconds, so there's not much point to polling fast!)
  //
  // Note that the docs say "wait at least 800us, the largest 3ms",
  // so we may be waiting too long?
  (void)WriteVec({0x00});
  usleep(10000);
  if (!WriteVec({0x00})) {
    *err = "wake-up write";
    return false;
  }
  usleep(10000); // 10ms

  vector<uint8> buf(2 + length + 2);
  
  // read register
  if (!WriteVec({CMD_READREG, reg, length})) {
    *err = "readreg command write";
    return false;
  }
  // Docs say wait at least 1.5ms.
  usleep(2000); // 2ms

  if (BCM2835_I2C_REASON_OK !=
      bcm2835_i2c_read((char *)buf.data(), buf.size())) {
    *err = "read failed";
    return false;
  }

  if (buf[0] != 0x03 || buf[1] != length) {
    *err = "read header was wrong";
    return false;
  }

  // In python code, unpack of "<H" means little-endian 16 bit (Half-word),
  // and ">H" means big-endian. Note the CRC and temp/hum payloads use
  // different byte orders.
  const uint16 crc = (buf[buf.size() -1] << 8) | buf[buf.size() - 2];

  // Not including CRC itself, of course...
  const uint16 computed_crc = CRC16(buf.data(), length + 2);

  if (crc != computed_crc) {
    *err = "wrong crc";
    return false;
  }

  for (int i = 0; i < length; i++)
    ret_buf[i] = buf[i + 2];
  
  return true;
}
  
bool AM2315::ReadTemp(float *temp, const char **err) {
  uint8 result_buf[2];
  if (!ReadReg(REG_TEMP, &result_buf[0], 2, err))
    return false;
  uint16 result = (result_buf[0] << 8) | result_buf[1];
  
  // XXX literal port from python.
  // it is probably wrong because 0 has two representations?
  // just replace this with int16??
  if (result >= 32768) {
    int t = (32768 - (int)result);
    *temp = t * 0.1f;
  } else {
    *temp = result * 0.1f;
  }
  return true;
}


bool AM2315::ReadRH(float *rh, const char **err) {
  uint8 result_buf[2];
  if (!ReadReg(REG_HUM, &result_buf[0], 2, err))
    return false;
  uint16 result = (result_buf[0] << 8) | result_buf[1];
  
  *rh = ((float)result) * 0.1f;
  return true;
}

bool AM2315::ReadInfo(Info *info, const char **err) {
  uint8 result_buf[7];
  if (!ReadReg(REG_INFO_START, &result_buf[0], 7, err))
    return false;
  info->model = (result_buf[0] << 8) | result_buf[1];
  info->version = result_buf[2];
  info->id =
    (result_buf[3] << 24) |
    (result_buf[4] << 16) |
    (result_buf[5] << 8) |
    result_buf[6];
  return true;
}
