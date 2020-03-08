// Creates /proc/flags with a file for each non-reserved bit in the
// x86 FLAGS register (16-bit version, but requires 64-bit OS).

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
MODULE_DESCRIPTION("Direct access to x86 FLAGS register");
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
    seq_printf(sf, "%c", (flags & mask) ? '1' : '0');                   \
    return 0;                                                           \
  }                                                                     \
  static int open_ ## name (struct inode *inode, struct file *file) {   \
    return single_open(file, show_ ## name , NULL);                     \
  }                                                                     \
  static ssize_t write_ ## name (struct file *file,                     \
                                 const char *user_buf,                  \
                                 size_t len, loff_t *offset) {          \
    int flags = 0;                                                      \
    char c = '0';                                                       \
    if (!len) return 0;                                                 \
    copy_from_user(&c, user_buf, 1);                                    \
    if (c == '1') flags = mask;                                         \
    /* only sets lower 32 bits of RFLAGS */                             \
    __asm__ ("pushf\n"                                                  \
             "popq %%rax\n"                                             \
             "or %0, %%eax\n"                                           \
             "pushq %%rax\n"                                            \
             "popf\n" :                                                 \
             /* output */ :                                             \
             "r"(flags) /* input */  :                                  \
             "%rax" /* clobbered */);                                   \
    __asm__ ("pushf\n"                                                  \
             "xorq %%rax, %%rax\n"                                      \
             "popq %%rax\n"                                             \
             "mov %%eax, %0\n" :                                        \
             "=r"(flags) /* output */ :                                 \
             /* input */ :                                              \
             "%rax" /* clobbered */);                                   \
    printk(KERN_INFO "flags now %x", flags);                            \
    return len;                                                         \
  }                                                                     \
  static const struct file_operations fops_ ## name = {                 \
    .owner = THIS_MODULE,                                               \
    .open = open_ ## name,                                              \
    .read = seq_read,                                                   \
    .write = write_ ## name,                                            \
    .llseek = seq_lseek,                                                \
    .release = single_release,                                          \
  };

DECL_OPS(cf, 0x0001);
DECL_OPS(reserved2, 0x0002);
DECL_OPS(pf, 0x0004);
DECL_OPS(reserved8, 0x0008);
DECL_OPS(af, 0x0010);
DECL_OPS(reserved20, 0x0020);
DECL_OPS(zf, 0x0040);
DECL_OPS(sf, 0x0080);
DECL_OPS(tf, 0x0100);
DECL_OPS(if, 0x0200);
DECL_OPS(df, 0x0400);
DECL_OPS(of, 0x0800);
DECL_OPS(iopl0, 0x1000);
DECL_OPS(iopl1, 0x2000);
DECL_OPS(nt, 0x4000);
DECL_OPS(reserved8000, 0x8000);

static struct proc_dir_entry* flags_dir = 0;

#define REGISTER(name)                                  \
  do {                                                  \
    proc_create(#name, 0666, flags_dir, &fops_ ## name);   \
  } while (0)

static int __init Initialize(void) {
  printk(KERN_INFO "proc/flags initialize\n");

  flags_dir = proc_mkdir("flags", NULL);
  if (!flags_dir) {
    printk("Failed to create directory\n");
    return -ENOMEM;
  }

  REGISTER(cf);
  REGISTER(reserved2);
  REGISTER(pf);
  REGISTER(reserved8);
  REGISTER(af);
  REGISTER(reserved20);
  REGISTER(zf);
  REGISTER(sf);
  REGISTER(tf);
  REGISTER(if);
  REGISTER(df);
  REGISTER(of);
  REGISTER(iopl0);
  REGISTER(iopl1);
  REGISTER(nt);
  REGISTER(reserved8000);

  return 0;
}

#define UNREGISTER(name) \
  do { remove_proc_entry(#name, flags_dir); } while (0)

static void __exit Cleanup(void) {
  UNREGISTER(cf);
  UNREGISTER(reserved2);
  UNREGISTER(pf);
  UNREGISTER(reserved8);
  UNREGISTER(af);
  UNREGISTER(reserved20);
  UNREGISTER(zf);
  UNREGISTER(sf);
  UNREGISTER(tf);
  UNREGISTER(if);
  UNREGISTER(df);
  UNREGISTER(of);
  UNREGISTER(iopl0);
  UNREGISTER(iopl1);
  UNREGISTER(nt);
  UNREGISTER(reserved8000);

  remove_proc_entry("flags", NULL);
  printk(KERN_INFO "(proc/flags mod exit)\n");
}

module_init(Initialize);
module_exit(Cleanup);

