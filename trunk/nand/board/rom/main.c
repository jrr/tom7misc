#include "stm32f30x.h"

// in C99, fmaxf() from math.h is the same as IEEE maxNum.
// See https://en.cppreference.com/w/c/numeric/math/fmax
// This function requires <math.h> and libmath, the latter
// which I can't find in this toolchain. But it is actually
// rather easy to implement it according to the spec right
// here.
static float fmaxf(float a, float b) {
  if (a != a) return b;
  if (b != b) return a;
  if (a < b) return b;
  return a;
}

#if !defined(__FPU_PRESENT) || !defined(__FPU_USED)
# error please define these
#endif

// FPU is enabled (see section 4.6.6 in the ARM M4F manual)
// in system_stm32f30x.c if these are turned on.
#if __FPU_PRESENT != 1 || __FPU_USED != 1
# error this program depends on floating-point instructions
#endif

/* zyp's cycle count delay routines */
volatile uint32_t* demcr = (uint32_t*)0xE000EDFC;
volatile uint32_t* dwt_ctrl = (uint32_t*)0xe0001000;
volatile uint32_t* dwt_cyccnt = (uint32_t*)0xe0001004;

void cyccnt_enable() {
  *demcr |= (1<<24);
  *dwt_ctrl |= 1;
}

void cyclesleep(uint32_t cycles) {
  uint32_t start = *dwt_cyccnt;

  while (*dwt_cyccnt - start < cycles) {}
}

// see _gpio.c for light docs
static void InitPinsOut(GPIO_TypeDef *periph, uint16_t pins) {
  GPIO_InitTypeDef gpio;

  gpio.GPIO_Pin = pins;
  gpio.GPIO_Mode = GPIO_Mode_OUT;
  // For this purpose, extreme speeds are not necessary and
  // just increase noise.
  gpio.GPIO_Speed = GPIO_Speed_2MHz;
  // gpio.GPIO_Speed = GPIO_Speed_50MHz;
  // We want push-pull since we connect these directly to
  // inputs.
  gpio.GPIO_OType = GPIO_OType_PP;
  // I think pull-up/pull-down doesn't make sense when using
  // push/pull.
  gpio.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(periph, &gpio);
}

static void InitPinsIn(GPIO_TypeDef *periph, uint16_t pins) {
  GPIO_InitTypeDef gpio;

  gpio.GPIO_Pin = pins;
  gpio.GPIO_Mode = GPIO_Mode_IN;
  // For this purpose, extreme speeds are not necessary and
  // just increase noise.
  gpio.GPIO_Speed = GPIO_Speed_2MHz;
  // Hopefully ignored for IN, but don't leave it uninitialized...
  gpio.GPIO_OType = GPIO_OType_PP;
  // These should all be connected directly to active outputs, so
  // no pull-up/down.
  gpio.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(periph, &gpio);
}

#define A_INPUT_MASK
#define B_INPUT_MASK
#define C_INPUT_MASK

static const float vinf = 1.0f / 0.0f;
static const float vneginf = -1.0f / 0.0f;
static const float vnan = 0.0f / 0.0f;
static const float vneg0 = 1.0f / (-1.0f / 0.0f);

static inline float Binary3ToFloat(uint8_t v) {
  switch (v & 7) {
  default:
  case 0b000: return 0.0f;
  case 0b001: return 1.0f;
  case 0b010: return vinf;
  case 0b011: return vnan;
  case 0b100: return vneg0;
  case 0b101: return -1.0f;
  case 0b110: return vneginf;
  case 0b111: return vnan;
  }
}

// Round a float to the binary3 gamut, and return it
// in binary3 format. The attempt here is to implement
// round-to-nearest, the default rounding mode, but it
// doesn't really matter that much since the function
// we compute can only return +inf or nan.
static inline uint8_t FloatToBinary3(float f) {
  // nans
  if (f != f) return 0b011;
  // Positive and negative infinity.
  if (f == vinf) return 0b010;
  if (f == vneginf) return 0b110;
  // Now, round to nearest. We avoid rounding
  // to infinity, so we are deciding between
  // +/- 0 and +/- 1.
  if (f > 0.5f) return 0b001;
  if (f < -0.5f) return 0b101;
  // Note: Might not actually work for -0?
  // Not clear what a strict reading of IEEE 754 says,
  // but it seems intuitive to convert 32-bit -0
  // to 3-bit -0.
  if (f < 0.0f) return vneg0;
  return 0.0f;
}

// Should behave the same as this:
#define NAND(x, y) (0 << 2) | (1 << 1) | \
      (((x) == 0b010 && (y) == 0b010) ? 1 : 0)

uint8_t NandFp(uint8_t x, uint8_t y) {
  const float fx = Binary3ToFloat(x);
  const float fy = Binary3ToFloat(y);
  const float fz = vinf - fmaxf(fx + fy, vneginf);
  return FloatToBinary3(fz);
}

int main(void) {
  // float w = fmax(1234.0f, vnan);

  // Here clock literally means the oscillator that drives
  // this part of the chip, in other words, "turn on the GPIO B
  // peripheral so that we can use it."
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOA, ENABLE);
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOB, ENABLE);
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOC, ENABLE);

  // 5 NAN gates:
  //
  //       x               y        ->       z
  // 0 (PA0,PA1,PA2) % (PA3,PA4,PA5) -> (PA6,PA7,PA8)
  // 1 (PA9,PA10,PA11) % (PA12,PA15,PB0) -> (PB1,PB2,PB3)
  // 2 (PB4,PB5,PB6) % (PB7,PB8,PB9) -> (PB10,PB11,PB12)
  // 3 (PC0,PC1,PC2) % (PC3,PC4,PC5) -> (PC6,PC7,PC8)
  // 4 (PC9,PC10,PC11) % (PC12,PC13,PC14) -> (PB13,PB14,PB15)
  //
  // Also configure PC15 as out. This is hooked to the header
  // and can send a signal for "i'm working".

  InitPinsIn(GPIOA,
             GPIO_Pin_0 | GPIO_Pin_1 | GPIO_Pin_2 |
             GPIO_Pin_3 | GPIO_Pin_4 | GPIO_Pin_5 |
             GPIO_Pin_9 | GPIO_Pin_10 | GPIO_Pin_11 |
             GPIO_Pin_12 | GPIO_Pin_15);
  InitPinsIn(GPIOB,
             GPIO_Pin_0 |
             GPIO_Pin_4 | GPIO_Pin_5 | GPIO_Pin_6 |
             GPIO_Pin_7 | GPIO_Pin_8 | GPIO_Pin_9);
  InitPinsIn(GPIOC,
             GPIO_Pin_0 | GPIO_Pin_1 | GPIO_Pin_2 |
             GPIO_Pin_3 | GPIO_Pin_4 | GPIO_Pin_5 |
             GPIO_Pin_9 | GPIO_Pin_10 | GPIO_Pin_11 |
             GPIO_Pin_12 | GPIO_Pin_13 | GPIO_Pin_14);

# define OUTPUT_MASK_A (GPIO_Pin_6 | GPIO_Pin_7 | GPIO_Pin_8)
# define OUTPUT_MASK_B (GPIO_Pin_1 | GPIO_Pin_2 | GPIO_Pin_3 |  \
                        GPIO_Pin_10 | GPIO_Pin_11 | GPIO_Pin_12 | \
                        GPIO_Pin_13 | GPIO_Pin_14 | GPIO_Pin_15)
# define OUTPUT_MASK_C (GPIO_Pin_6 | GPIO_Pin_7 | GPIO_Pin_8 | \
                        GPIO_Pin_15)
  InitPinsOut(GPIOA, OUTPUT_MASK_A);
  InitPinsOut(GPIOB, OUTPUT_MASK_B);
  InitPinsOut(GPIOC, OUTPUT_MASK_C);

  GPIOA->BRR = OUTPUT_MASK_A;
  GPIOB->BRR = OUTPUT_MASK_A;
  GPIOC->BRR = OUTPUT_MASK_A;

  /* start cycle counter */
  cyccnt_enable();

  /* loop forever */
  uint32_t num_loops = 0;
  for (;;) {
    num_loops++;
    // XXX check on the rate here; it's dependent on the
    // length of this loop, alas. Could just use cycle counter?
    const uint8_t beacon = (num_loops >> 16) & 1;

    // Full input from the GPIO ports, not permuted.
    const uint16_t inputa = GPIOA->IDR;
    const uint16_t inputb = GPIOB->IDR;
    const uint16_t inputc = GPIOC->IDR;

    // Get bit v of in, as 0 or 1.
#   define READ1(in, v) ((in >> v) & 1)
    // Get the three bits, and output them as a 3-bit number
    // with in0/v0 as the MSB.
#   define READ3(in0, v0, in1, v1, in2, v2) \
    ((READ1(in0, v0) << 2) | (READ1(in1, v1) << 1) | READ1(in2, v2))

  //       x               y        ->       z
  // 0 (PA0,PA1,PA2) % (PA3,PA4,PA5) -> (PA6,PA7,PA8)
  // 1 (PA9,PA10,PA11) % (PA12,PA15,PB0) -> (PB1,PB2,PB3)
  // 2 (PB4,PB5,PB6) % (PB7,PB8,PB9) -> (PB10,PB11,PB12)
  // 3 (PC0,PC1,PC2) % (PC3,PC4,PC5) -> (PC6,PC7,PC8)
  // 4 (PC9,PC10,PC11) % (PC12,PC13,PC14) -> (PB13,PB14,PB15)

    // pull out bits for xi, yi.
    const uint8_t x0 = READ3(inputa, 0, inputa, 1, inputa, 2);
    const uint8_t y0 = READ3(inputa, 3, inputa, 4, inputa, 5);
    const uint8_t x1 = READ3(inputa, 9, inputa, 10, inputa, 11);
    const uint8_t y1 = READ3(inputa, 12, inputa, 15, inputb, 0);
    const uint8_t x2 = READ3(inputb, 4, inputb, 5, inputb, 6);
    const uint8_t y2 = READ3(inputb, 7, inputb, 8, inputb, 9);
    const uint8_t x3 = READ3(inputc, 0, inputc, 1, inputc, 2);
    const uint8_t y3 = READ3(inputc, 3, inputc, 4, inputc, 5);
    const uint8_t x4 = READ3(inputc, 9, inputc, 10, inputc, 11);
    const uint8_t y4 = READ3(inputc, 12, inputc, 13, inputc, 14);

    uint8_t z0 = NandFp(x0, y0);
    uint8_t z1 = NandFp(x1, y1);
    uint8_t z2 = NandFp(x2, y2);
    uint8_t z3 = NandFp(x3, y3);
    uint8_t z4 = NandFp(x4, y4);

#   define GET(z, b) ((uint16_t)(((z) >> (b)) & 1))
    // distribute the three bits of z to bit positions msb, mb, lsb.
    // This suffices because each of our outputs is only sent to a single
    // GPIO peripheral.
#   define DISTRIBUTE(z, msb, mb, lsb) \
    ((GET(z, 2) << (msb)) | (GET(z, 1) << (mb)) | (GET(z, 0) << (lsb)))

    // Now place z into the correct position for output.
    const uint16_t outputa = DISTRIBUTE(z0, 6, 7, 8);
    const uint16_t outputb =
      DISTRIBUTE(z1, 1, 2, 3) |
      DISTRIBUTE(z2, 10, 11, 12) |
      DISTRIBUTE(z4, 13, 14, 15);
    const uint16_t outputc = DISTRIBUTE(z3, 6, 7, 8) | (beacon << 15);

    // BSRR is a 32 bit register, where a 1 in the low 16 bits means
    // turn that gpio pin on, and a 1 in the high 16 means turn it off.
#   define SET_RESET(MASK, out) \
    (((uint32_t)((~(out)) & MASK)) << 16) | ((out) & MASK);
    // Now do the output.
    GPIOA->BSRR = SET_RESET(OUTPUT_MASK_A, outputa);
    GPIOB->BSRR = SET_RESET(OUTPUT_MASK_B, outputb);
    GPIOC->BSRR = SET_RESET(OUTPUT_MASK_C, outputc);

  }
}

#ifdef  USE_FULL_ASSERT
void assert_failed(uint8_t* file, uint32_t line) {
  // (XXX report the message if possible, maybe over SWD?)
  /* Infinite loop */
  while (1) { }
}
#endif
