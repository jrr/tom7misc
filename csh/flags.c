#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Dr. Tom Murphy VII Ph.D.");
MODULE_DESCRIPTION("Test");
MODULE_VERSION("NaN");

static int __init Initialize(void) {
  printk(KERN_INFO "(test)\n");
  return 0;
}

static void __exit Cleanup(void) {
  printk(KERN_INFO "(exit)\n");
}

module_init(Initialize);
module_exit(Cleanup);
