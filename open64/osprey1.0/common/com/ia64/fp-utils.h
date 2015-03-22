#ifndef _INCLUDE_FP_UTILS_H_
#define _INCLUDE_FP_UTILS_H_

#ifdef __cplusplus
  #include <cstdlib>
#else
  #include <stdlib.h>
#endif
#undef strtod
#define strtod ti_strtod /* avoid a symbol conflict with C99 strtod */

#ifdef __cplusplus
extern "C" {
#endif

double strtod(const char *, char **);
char *g_fmt(register char *, double);

#ifdef __cplusplus
}
#endif

#endif
