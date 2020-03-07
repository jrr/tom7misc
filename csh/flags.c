#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/fs.h>
// #include <asm/uaccess.h>
#include <linux/uaccess.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Dr. Tom Murphy VII Ph.D.");
MODULE_DESCRIPTION("Test");
MODULE_VERSION("NaN");

#define DEVICE_NAME "flags"

// 16 bits
#define SIZE 2

static int Open(struct inode *, struct file *);
static int Release(struct inode *, struct file *);
static ssize_t Read(struct file *, char *, size_t, loff_t *);
static ssize_t Write(struct file *, const char *, size_t, loff_t *);
static int major_num = 0;
static int device_open_count = 0;

// XXX how to behave as though the file is 2 bytes long, rather than allowing
// reading forever? I guess we can return at most 2?
static ssize_t Read(struct file *flip, char *buffer, size_t len, loff_t *offset) {
  int bytes_read = 0;
  /* Put data in the buffer */
  while (len) {
    // Must use put_user to write to userspace.
    if (bytes_read & 1) {
      put_user('i', buffer++);
    } else {
      put_user('h', buffer++);
    }
    len--;
    bytes_read++;
  }
  return bytes_read;
}

static ssize_t Write(struct file *flip, const char *buffer, size_t len, loff_t *offset) {
 printk(KERN_ALERT "This operation is not supported.\n");
 return -EINVAL;
}

/* Called when a process opens our device */
static int Open(struct inode *inode, struct file *file) {
  // XXX Is it necessary to prohibit concurrent access?
  if (device_open_count) {
    return -EBUSY;
  }
  device_open_count++;
  // XXX What does this do?
  try_module_get(THIS_MODULE);
  return 0;
}

/* Called when a process closes our device */
static int Release(struct inode *inode, struct file *file) {
 /* Decrement the open counter and usage count. Without this, the module would not unload. */
 device_open_count--;
 module_put(THIS_MODULE);
 return 0;
}

// Function pointers implementing the "file."
static struct file_operations file_ops = {
  .read = &Read,
  .write = &Write,
  .open = &Open,
  .release = &Release,
};

static int __init Initialize(void) {
  major_num = register_chrdev(0, DEVICE_NAME, &file_ops);
  if (major_num < 0) {
    printk(KERN_ALERT "Could not register device: %d\n", major_num);
    return major_num;
  } else {
    printk(KERN_INFO "flags mod device major number %d\n", major_num);
    return 0;
  }
}

static void __exit Cleanup(void) {
  unregister_chrdev(major_num, DEVICE_NAME);
  printk(KERN_INFO "(flags mod exit)\n");
}

module_init(Initialize);
module_exit(Cleanup);
