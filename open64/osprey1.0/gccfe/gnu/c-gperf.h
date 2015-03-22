/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -L C -F ', 0, 0' -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k'1,3,4,5$' c-parse.gperf  */
struct resword { const char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 95
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 20
#define MIN_HASH_VALUE 6
#define MAX_HASH_VALUE 323
/* maximum key range = 318, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned short asso_values[] =
    {
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324,  70, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324,   1, 324,  24,  41,  16,
      104,   1,  80,  38,   2,   1, 324,   8,  12,  41,
       41,   7,  50, 324,   1,   7,   1,  67,  59,   6,
       17,  16,   1, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

static struct resword wordlist[] =
  {
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"int", TYPESPEC, RID_INT},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0},
    {"out", TYPE_QUAL, RID_OUT},
    {"", 0, 0},
    {"else", ELSE, NORID},
    {"__iterator", SCSPEC, RID_ITERATOR},
    {"", 0, 0},
    {"__iterator__", SCSPEC, RID_ITERATOR},
    {"", 0, 0},
    {"restrict", TYPE_QUAL, RID_RESTRICT},
    {"", 0, 0},
    {"__restrict", TYPE_QUAL, RID_RESTRICT},
    {"short", TYPESPEC, RID_SHORT},
    {"__restrict__", TYPE_QUAL, RID_RESTRICT},
    {"", 0, 0}, {"", 0, 0},
    {"while", WHILE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"case", CASE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"strict", TYPE_QUAL, RID_STRICT},
    {"switch", SWITCH, NORID},
    {"__extension__", EXTENSION, NORID},
    {"", 0, 0},
    {"__real__", REALPART, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__attribute", ATTRIBUTE, NORID},
    {"", 0, 0},
    {"__attribute__", ATTRIBUTE, NORID},
    {"", 0, 0},
    {"auto", SCSPEC, RID_AUTO},
    {"in", TYPE_QUAL, RID_IN},
    {"__real", REALPART, NORID},
    {"char", TYPESPEC, RID_CHAR},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"__alignof__", ALIGNOF, NORID},
    {"extern", SCSPEC, RID_EXTERN},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"static", SCSPEC, RID_STATIC},
    {"register", SCSPEC, RID_REGISTER},
    {"goto", GOTO, NORID},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"", 0, 0},
    {"oneway", TYPE_QUAL, RID_ONEWAY},
    {"", 0, 0},
    {"inline", SCSPEC, RID_INLINE},
    {"", 0, 0},
    {"__inline", SCSPEC, RID_INLINE},
    {"", 0, 0},
    {"__inline__", SCSPEC, RID_INLINE},
    {"", 0, 0},
    {"continue", CONTINUE, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"const", TYPE_QUAL, RID_CONST},
    {"", 0, 0},
    {"__const", TYPE_QUAL, RID_CONST},
    {"", 0, 0},
    {"__const__", TYPE_QUAL, RID_CONST},
    {"__imag__", IMAGPART, NORID},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"", 0, 0},
    {"__typeof__", TYPEOF, NORID},
    {"", 0, 0},
    {"__asm__", ASM_KEYWORD, NORID},
    {"inout", TYPE_QUAL, RID_INOUT},
    {"if", IF, NORID},
    {"", 0, 0},
    {"for", FOR, NORID},
    {"", 0, 0},
    {"break", BREAK, NORID},
    {"__label__", LABEL, NORID},
    {"", 0, 0},
    {"__volatile", TYPE_QUAL, RID_VOLATILE},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__volatile__", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"@selector", SELECTOR, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"struct", STRUCT, NORID},
    {"upc_wait", UPC_WAIT, NORID},
    {"@protocol", PROTOCOL, NORID},
    {"", 0, 0},
    {"sizeof", SIZEOF, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"volatile", TYPE_QUAL, RID_VOLATILE},
    {"", 0, 0},
    {"id", OBJECTNAME, RID_ID},
    {"", 0, 0},
    {"asm", ASM_KEYWORD, NORID},
    {"", 0, 0},
    {"__imag", IMAGPART, NORID},
    {"", 0, 0},
    {"do", DO, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"return", RETURN, NORID},
    {"float", TYPESPEC, RID_FLOAT},
    {"__asm", ASM_KEYWORD, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@interface", INTERFACE, NORID},
    {"bupc_assert_type", BUPC_ASSERT_TYPE, NORID},
    {"@class", CLASS, NORID},
    {"__alignof", ALIGNOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0},
    {"long", TYPESPEC, RID_LONG},
    {"", 0, 0}, {"", 0, 0},
    {"bycopy", TYPE_QUAL, RID_BYCOPY},
    {"upc_barrier", UPC_BARRIER, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"@private", PRIVATE, NORID},
    {"", 0, 0},
    {"@encode", ENCODE, NORID},
    {"shared", TYPE_QUAL_S, RID_SHARED},
    {"", 0, 0},
    {"typeof", TYPEOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0},
    {"upc_notify", UPC_NOTIFY, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"enum", ENUM, NORID},
    {"", 0, 0},
    {"__typeof", TYPEOF, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0},
    {"union", UNION, NORID},
    {"", 0, 0}, {"", 0, 0},
    {"relaxed", TYPE_QUAL, RID_RELAXED},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@defs", DEFS, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"upc_fence", UPC_FENCE, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"upc_elemsizeof", UPC_ELEMSIZEOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"upc_forall", FORALL, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"upc_localsizeof", UPC_LOCALSIZEOF, NORID},
    {"", 0, 0},
    {"@protected", PROTECTED, NORID},
    {"", 0, 0},
    {"@compatibility_alias", ALIAS, NORID},
    {"", 0, 0},
    {"signed", TYPESPEC, RID_SIGNED},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"byref", TYPE_QUAL, RID_BYREF},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@public", PUBLIC, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"upc_blocksizeof", UPC_BLOCKSIZEOF, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@implementation", IMPLEMENTATION, NORID},
    {"", 0, 0},
    {"double", TYPESPEC, RID_DOUBLE},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"void", TYPESPEC, RID_VOID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0},
    {"default", DEFAULT, NORID},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"", 0, 0}, {"", 0, 0}, {"", 0, 0},
    {"@end", END, NORID}
  };

#ifdef __GNUC__
__inline
#endif
struct resword *
is_reserved_word (str, len)
     register const char *str;
     register unsigned int len;
{
  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
