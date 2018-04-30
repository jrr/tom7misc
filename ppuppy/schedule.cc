
#include "schedule.h"

#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

void ScheduleHighPriority(int cpu) {
  // This magic locks our memory so that it doesn't get
  // swapped out, which improves our scheduling latency.
  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if (0 != sched_setscheduler(0, SCHED_FIFO, &sp)) {
    printf("Failed to set scheduling priority...\n");
    perror("setup: ");
  }
  // Also, set affinity to CPU 3. This was isolated from
  // other scheduling by isolcpus=3 in /boot/cmdline.txt.
  (void)cpu;
#if 1
  cpu_set_t cpu_set;
  CPU_ZERO(&cpu_set);
  CPU_SET(cpu, &cpu_set);
  if (0 != sched_setaffinity(0 /* self */,
			     sizeof (cpu_set_t),
			     &cpu_set)) {
    printf("Couldn't set affinity...\n");
    perror("setup: ");
  }
#endif
  // Also, lock memory so it cannot be swapped.
  mlockall(MCL_CURRENT | MCL_FUTURE);
  printf("Main thread at pid %d\n", getpid());
}
