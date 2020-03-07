#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/fs.h>
// #include <asm/uaccess.h>
#include <linux/uaccess.h>
#include <linux/proc_fs.h>
#include <linux/bug.h>
#include <linux/seq_file.h>

typedef unsigned short uint16;

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Dr. Tom Murphy VII Ph.D.");
MODULE_DESCRIPTION("Test");
MODULE_VERSION("NaN");

static void Assertions(void) {
  BUILD_BUG_ON(sizeof (uint16) != 2);
  (void)Assertions;
}

#define DEVICE_NAME "flags"

#define DECL_OPS(name, mask) \
  static int show_ ## name (struct seq_file *sf, void *v) {             \
    seq_printf(sf, #name "\n");                                         \
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

#if 0
typedef struct Flag {
  const char *name;
  const uint16_t mask;
  struct proc_dir_entry *entry;
} Flag;


#define NUM_FLAGS 2
static Flag flags[NUM_FLAGS] = {
{"cf", 0x0001, 0},
// 0002 reserved
{"pf", 0x0004, 0},
};
#endif


static struct proc_dir_entry* flags_dir = 0;

#define REGISTER(name)                                  \
  do {                                                  \
    proc_create(#name, 0, flags_dir, &fops_ ## name);   \
  } while (0)

static int __init Initialize(void) {
  (void)Assertions;
  printk(KERN_INFO "proc/flags initialize\n");

  flags_dir = proc_mkdir("flags", NULL);
  if (!flags_dir) {
    printk("Failed to create directory\n");
    return -ENOMEM;
  }

  REGISTER(cf);
  REGISTER(pf);

#if 0
  {
    int i;
    for (i = 0; i < NUM_FLAGS; i++) {
      #define MODE 0444
      proc_create(flags[i].name, 0, flags_dir, &hello_proc_fops);
      struct proc_file_entry *entry = create_proc_entry(flags[i].name, MODE, flags_dir);
      if (entry == NULL) {
        // XXX should clean up entries allocated so far (remove_proc_entry(name, dir)).
        continue;
      }

      flags[i].entry = entry;

      entry->read_proc = Read;
      entry->write_proc = Write;
      entry->owner = THIS_MODULE;
      entry->mode = MODE;
      entry->uid = 0;
      entry->gid = 0;
      entry->size = 2;

    }
  }
#endif

  return 0;
}

#define UNREGISTER(name) \
  do { remove_proc_entry(#name, flags_dir); } while (0)

static void __exit Cleanup(void) {
  UNREGISTER(cf);
  UNREGISTER(pf);
  remove_proc_entry("flags", NULL);
  printk(KERN_INFO "(proc/flags mod exit)\n");
}

module_init(Initialize);
module_exit(Cleanup);

