#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/fs.h>
#include <linux/uaccess.h>
#include <linux/proc_fs.h>
#include <linux/bug.h>
#include <linux/seq_file.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Dr. Tom Murphy VII Ph.D.");
MODULE_DESCRIPTION("Test");
MODULE_VERSION("NaN");

#define DECL_OPS(name, mask) \
  static int show_ ## name (struct seq_file *sf, void *v) {             \
    int flags = 0;                                                      \
    __asm__ ("pushf\n"                                                  \
             "xorq %%rax, %%rax\n"                                      \
             "popq %%rax\n"                                             \
             "mov %%eax, %0\n" :                                        \
             "=r"(flags) /* output */ :                                 \
             /* input */ :                                              \
             "%rax" /* clobbered */);                                   \
    if (flags & mask) seq_printf(sf, "1");                              \
    /* else seq_printf(sf, ""); */                                      \
    return 0;                                                           \
  }                                                                     \
  static int open_ ## name (struct inode *inode, struct file *file) {   \
    return single_open(file, show_ ## name , NULL);                     \
  }                                                                     \
  static const struct file_operations fops_ ## name = {                 \
    .owner = THIS_MODULE,                                               \
    .open = open_ ## name,                                              \
    .read = seq_read,                                                   \
    .llseek = seq_lseek,                                                \
    .release = single_release,                                          \
  };

DECL_OPS(cf, 0x0001);
DECL_OPS(pf, 0x0004);
DECL_OPS(af, 0x0010);
DECL_OPS(zf, 0x0040);
DECL_OPS(sf, 0x0080);
DECL_OPS(tf, 0x0100);
DECL_OPS(if, 0x0200);
DECL_OPS(df, 0x0400);
DECL_OPS(of, 0x0800);
DECL_OPS(iopl0, 0x1000);
DECL_OPS(iopl1, 0x2000);
DECL_OPS(nt, 0x4000);

static struct proc_dir_entry* flags_dir = 0;

#define REGISTER(name)                                  \
  do {                                                  \
    proc_create(#name, 0, flags_dir, &fops_ ## name);   \
  } while (0)

static int __init Initialize(void) {
  printk(KERN_INFO "proc/flags initialize\n");

  flags_dir = proc_mkdir("flags", NULL);
  if (!flags_dir) {
    printk("Failed to create directory\n");
    return -ENOMEM;
  }

  REGISTER(cf);
  REGISTER(pf);
  REGISTER(af);
  REGISTER(zf);
  REGISTER(sf);
  REGISTER(tf);
  REGISTER(if);
  REGISTER(df);
  REGISTER(of);
  REGISTER(iopl0);
  REGISTER(iopl1);
  REGISTER(nt);

  return 0;
}

#define UNREGISTER(name) \
  do { remove_proc_entry(#name, flags_dir); } while (0)

static void __exit Cleanup(void) {
  UNREGISTER(cf);
  UNREGISTER(pf);
  UNREGISTER(af);
  UNREGISTER(zf);
  UNREGISTER(sf);
  UNREGISTER(tf);
  UNREGISTER(if);
  UNREGISTER(df);
  UNREGISTER(of);
  UNREGISTER(iopl0);
  UNREGISTER(iopl1);
  UNREGISTER(nt);

  remove_proc_entry("flags", NULL);
  printk(KERN_INFO "(proc/flags mod exit)\n");
}

module_init(Initialize);
module_exit(Cleanup);

