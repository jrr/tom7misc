#ifndef UNIXSUPPORT_H
#define UNIXSUPPORT_H

#ifdef __linux__
#include <unistd.h>
#endif
#include <sys/types.h>
#include <assert.h>

/* for generic values, always use argument of type void *; should
   move to something more descriptive eventually */

/* Popcorn arrays */
typedef struct arr_internal {int size; void *elts;} *array;

/* Popcorn strings */
typedef struct str_internal {int size; char *chars;} *string;
extern char *hemrunt_convert_pop_string(string);   /* won't copy if
					      null-terminated */
extern char *string_to_Cstring(string);    /* always copies */
extern string Cstring_to_string(char *);   /* always copies */

#if 0

/* for error reporting */
extern void hemrunt_error(char *file, int line, char *msg);

/* utilities */
extern int hemrunt_convert_flags(array flags, int flag_table[]);
extern void *hemrunt_xalloc(int sizeb);
extern void *hemrunt_xalloc_atomic(int sizeb);
extern array hemrunt_copy_poparray(void *(*f)(char *), char ** arr);
extern array hemrunt_copy_popstring_array(char ** arr);

#ifdef __GNUC__
/* Works only in GCC 2.5 and later */
#define INLINE inline 
#define Noreturn __attribute ((noreturn))
#else
#define INLINE
#define Noreturn
#endif

/* only use raise_pop_exception (and thus unix_error) from within the
   function immediately called from Popcorn; otherwise the restoring
   of the base pointer (which is where the exception handler is
   stored) will be incorrect. */

struct pop_exn {
  int exn_tag;
  string loc_str;
  void *arg;
};

extern void raise_pop_exception (struct pop_exn *exn) Noreturn;
extern struct pop_exn *hemrunt_make_hemrunt_error(char *file, int line, 
						int code, char *msg);
extern struct pop_exn *hemrunt_get_hemrunt_error(char *file, int line, char *msg);
#define hemrunt_error(file, line, msg)			\
  raise_pop_exception(hemrunt_get_hemrunt_error(file,line,msg));

extern struct pop_exn *hemrunt_get_nullpointer_exn(char *file, int line);
#define nullpointer_exn(file, line)				\
  raise_pop_exception(hemrunt_get_nullpointer_exn(file, line))

#endif

#endif
