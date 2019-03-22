#include "stm32f30x.h"

#include <math.h>

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
static void InitPinOutB(uint16_t pins) {
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
  GPIO_Init(GPIOB, &gpio);
}

#define OUTPUT_MASK ((1<<13) | (1<<14) | (1<<15))

int main(void) {
  float vinf = 1.0f / 0.0f;
  float vnan = 0.0f / 0.0f;
  float w = fmax(1234.0f, vnan);

  // Here clock literally means the oscillator that drives
  // this part of the chip, in other words, "turn on the GPIO B
  // peripheral so that we can use it."
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOB, ENABLE);

  /* Enable PB13,14,15 for output */
  InitPinOutB(GPIO_Pin_13 | GPIO_Pin_14 | GPIO_Pin_15);

  GPIOB->BSRR = OUTPUT_MASK;

  /* start cycle counter */
  cyccnt_enable();



  /* loop forever */
  for (;;) {
    for (int i = 0; i < 1000; i++) {

      float f = (float)i / 1000.0f;

      uint8_t b = (int)(f * 8.0f);
      uint32_t new_value = 0;
      if (b & 1) new_value |= 1 << 13;
      if (b & 2) new_value |= 1 << 14;
      if (b & 4) new_value |= 1 << 15;

      // Clear bits that should not be set.
      GPIOB->BRR = (~new_value) & OUTPUT_MASK;
      GPIOB->BSRR = new_value & OUTPUT_MASK;

      cyclesleep(0x004aa20);

      GPIOB->BRR = 1<<13;
      GPIOB->BSRR = 1<<14;

      // cyclesleep(0x084aa20);
    }
  }
}

#ifdef  USE_FULL_ASSERT
void assert_failed(uint8_t* file, uint32_t line) {
  // (XXX report the message if possible, maybe over SWD?)
  /* Infinite loop */
  while (1) { }
}
#endif
