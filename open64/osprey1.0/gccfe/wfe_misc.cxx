/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/
#if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#else
#include <values.h>
#endif
#include <sys/types.h>
#include <elf.h>
#include "defs.h"
#include "config.h"
#include "config_debug.h"
#include "config_list.h"
#include "config_targ_aux.h"
#include "controls.h"
#include "erglob.h"
#include "erlib.h"
#include "file_util.h"
#include "flags.h"
#include "glob.h"
#include "mempool.h"
#include "tracing.h"
#include "util.h"
#include "errors.h"
#include "err_host.tab"
#include <stdarg.h>
#include "gnu_config.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
#include "gnu/upc-act.h"
}
#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "symtab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "wfe_decl.h"
#include "wfe_expr.h"
#include "wfe_dst.h"
#include "wfe_misc.h"
#include "wfe_stmt.h"
#include "c_int_model.h"
#include "tree_symtab.h"
#if defined(_AIX) && __GNUC__ == 3 && __GNUC_MINOR__ < 4
extern int      snprintf(char *__restrict__, size_t, const char *__restrict__, ...);
#endif

int WFE_Keep_Zero_Length_Structs = FALSE;

extern int optimize;

//Stores a list of upc system header directories
//Functions from these directories will not have their prototypes emitted in whirl2c.
char * upc_header_dirs[16];
unsigned num_header_dirs = 0;
char *system_header_dirs = 0;
PU_Info *PU_Tree_Root = NULL;
int      wfe_invoke_inliner = FALSE;

extern void Initialize_IRB (void);	/* In lieu of irbutil.h */
extern char *asm_file_name;		/* from toplev.c */

int trace_verbose = FALSE;
// an_error_severity error_threshold = es_warning;

static BOOL Prepare_Source (void);
static void WFE_Stmt_Stack_Init (void);
static void WFE_Stmt_Stack_Free (void);

// The following taken from gnu/flags.h
// our #include of flags.h gets common/util/flags.h instead
enum debug_info_level
{
  DINFO_LEVEL_NONE,     /* Write no debugging info.  */
  DINFO_LEVEL_TERSE,    /* Write minimal info to support tracebacks only.  */
  DINFO_LEVEL_NORMAL,   /* Write info for all declarations (and line table). */
  DINFO_LEVEL_VERBOSE   /* Write normal info plus #define/#undef info.  */
};

/* Specify how much debugging info to generate.  */
extern enum debug_info_level debug_info_level;
// End gnu/flags.h data decl

extern void process_shared(ST_IDX st, int dim, string init);
//extern void process_nonshared(ST_IDX st, tld_pair_p info);
extern void process_nonshared(ST_IDX st, string init);
extern void output_file();

//defined in shared-alloc.cxx
extern string strip(string name, bool id = false);


/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

/*       MAX_DEBUG_LEVEL	2  :: Defined in flags.h */
# define DEF_DEBUG_LEVEL	0
INT8 Debug_Level = DEF_DEBUG_LEVEL;	/* -gn:	debug level */
# define MAX_MSG_LEVEL 2
# define DEF_MSG_LEVEL 2

#ifdef MONGOOSE_CIF
mUINT32 Cif_Level = 0;       	/* CIF level */
#define MAX_CIF_LEVEL 3 
#define DEF_CIF_LEVEL 2 
#endif /* MONGOOSE_CIF */

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION ".B"	/* ACIR file */
#define	IRD_FILE_EXTENSION ".D"	/* Intermediate data file */
#define	ERR_FILE_EXTENSION ".e"	/* Error file */
#define	LST_FILE_EXTENSION ".l"	/* Listing file */
#define	TRC_FILE_EXTENSION ".t"	/* Trace file */
#define DSTDUMP_FILE_EXTENSION ".fe.dst" /* DST dump-file extension */

/* Static data:	command	line information: */
static INT32 Argc;		/* Copy of argc */
static char **Argv;		/* Copy of argv */
static INT32 Source_Arg;	/* Number of current source arg */
static INT32 Src_Count;		/* Number of source files seen */
static char Dash [] = "-";

/* Internal flags: */
static BOOL Echo_Flag =	FALSE;	/* Echo command	lines */
static BOOL Delete_IR_File = FALSE;	/* Delete SGIR file when done */
extern BOOL double_align_exception;
extern BOOL double_inner_exception;
extern BOOL longlong_inner_exception;
extern BOOL sptr_inner_exception;
extern BOOL psptr_inner_exception;
extern BOOL longlong_align_exception;
extern BOOL sptr_align_exception;
extern BOOL psptr_align_exception;
extern BOOL struct_align_promote;
/* ====================================================================
 *
 * Cleanup_Files
 *
 * Close all per-source	files involved in a compilation	and prepare the
 * global variables for	the next source.  This routine is externalized
 * for signal cleanup; the report parameter allows suppressing of error
 * reporting during such cleanup.
 *
 * ====================================================================
 */

void
Cleanup_Files (	BOOL report, BOOL delete_dotofile )
{
  /* No	current	line number for	errors:	*/
  Set_Error_Line (ERROR_LINE_UNKNOWN);

  /* Close source file:	*/
  if ( Src_File	!= NULL	&& Src_File != stdin &&	fclose (Src_File) ) {
    if ( report	) ErrMsg ( EC_Src_Close, Src_File_Name,	errno );
  }
  Src_File = NULL;

  /* Close and delete SGIR file: */
  if ( IR_File != NULL && fclose (IR_File) ) {
    if ( report	) ErrMsg ( EC_IR_Close,	IR_File_Name, errno );
  }
  IR_File = NULL;
  if ( Delete_IR_File && unlink	(IR_File_Name) ) {
    if ( report	) ErrMsg ( EC_IR_Delete, IR_File_Name, errno );
  }

  /* Close listing file: */
  if ( Lst_File	!= NULL	&& Lst_File != stdout && fclose	(Lst_File) ) {
    if ( report	) ErrMsg ( EC_Lst_Close, Lst_File_Name,	errno );
  }
  Lst_File = NULL;

  /* Close trace file: */
  Set_Trace_File ( NULL	);

  /* Disable timing file: */
  Tim_File = NULL;

  /* Finally close error file: */
  Set_Error_File ( NULL	);
  Set_Error_Source ( NULL );
}

/* ====================================================================
 *
 * Terminate
 *
 * Do any necessary cleanup and	terminate the program with the given
 * status.
 *
 * ====================================================================
 */

void
Terminate ( INT status )
{
  /* Close and delete files as necessary: */
  Cleanup_Files	( FALSE, FALSE);

  exit (status);
}

/* ====================================================================
 *
 * Prepare_Source
 *
 * Process the next source argument and	associated file	control	flags
 * from	the command line.  Pre-process the source file unless
 * suppressed, and initialize output files as required.	 Return	TRUE
 * iff we have a successfully pre-processed source file	left to
 * compile.
 *
 * ====================================================================
 */

static BOOL
Prepare_Source ( void )
{
  INT16	i;
  char *cp;
  char *fname;
  INT16 len;
  BOOL  dashdash_flag = FALSE;

  /* Initialize error handler: */
  Init_Error_Handler ( 100 );
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_File ( NULL );
  Set_Error_Phase ( "Front End Driver" );

  /* Clear file names: */
  Src_File_Name = NULL;	/* Source file */
  IR_File_Name = NULL;	/* SGIR file */
  Irb_File_Name = NULL;	/* ACIR file */
  Err_File_Name = Dash;	/* Error file */
  Lst_File_Name = NULL;	/* Listing file */
  Trc_File_Name = NULL;	/* Trace file */
  DSTdump_File_Name = NULL; /* DST dump */

  Delete_IR_File = FALSE;
  
  /* Check the command line flags for -f? and source file names: */
  while ( ++Source_Arg <= Argc ) {
    i = Source_Arg;

    /* Null argument => end of list: */
    if ( Argv[i] == NULL ) return FALSE;

    if ( !dashdash_flag && (*(Argv[i]) == '-' )) {
      cp = Argv[i]+1;	/* Pointer to next flag character */

      /* -oname or -o name are passed to the linker: */
      if ( *cp == 'o' ) {
	++cp;
	if ( *cp == 0 ) {
	  /* Link file name is next command line argument: */
	  ++Source_Arg;
	}
	continue;
      }

      /* process as command-line option group */
      if (strncmp(cp, "OPT:", 4) == 0) { 
	Process_Command_Line_Group (cp, Common_Option_Groups);
    	continue;
      }
    } 
    else {
      Src_Count++;
      dashdash_flag = FALSE;

      /* Copy the given source name: */
      len = strlen ( Argv[i] );
      Src_File_Name = (char *) malloc (len+5);
      strcpy ( Src_File_Name, Argv[i] );

      /* We've got a source file name -- open other files.
       * We want them to be created in the current directory, so we
       * strip off the filename only from Src_File_Name for use:
       */
      fname = Last_Pathname_Component ( Src_File_Name );

      /* Error file first to get error reports: */
      if ( Err_File_Name == NULL ) {
	/* Replace source file extension to get error file: */
	Err_File_Name = New_Extension
			    ( fname, ERR_FILE_EXTENSION	);
      } else if ( *Err_File_Name == '-' ) {
	/* Disable separate error file: */
	Err_File_Name = NULL;
      }
      Set_Error_File ( Err_File_Name );

      /* Trace file next: */
      if ( Trc_File_Name == NULL ) {
	if ( Tracing_Enabled ) {
	  /* Replace source file extension to get trace file: */
	  Trc_File_Name = New_Extension
			    ( fname, TRC_FILE_EXTENSION	);
	}
      } else if ( *Trc_File_Name == '-' ) {
	/* Leave trace file on stdout: */
	Trc_File_Name = NULL;
      }
      Set_Trace_File ( Trc_File_Name );
      if ( Get_Trace (TKIND_INFO, TINFO_TIME) ) Tim_File = TFile;

      /* We're ready to pre-process: */
      IR_File_Name = Src_File_Name;

      /* Open the IR file for compilation: */
      if ( Irb_File_Name == NULL ) {
	if (asm_file_name == NULL) {
		/* Replace source file extension to get listing file: */
		Irb_File_Name = New_Extension (	fname, IRB_FILE_EXTENSION );
	}
	else {
		Irb_File_Name = asm_file_name;
	}
      }

	if ( (Irb_File = fopen ( Irb_File_Name, "w" )) == NULL ) {
	  ErrMsg ( EC_IR_Open, IR_File_Name, errno );
	  Cleanup_Files ( TRUE, FALSE );	/* close opened files */
	  return Prepare_Source ();
	} else {
	  if ( Get_Trace ( TP_MISC, 1) ) {
	    fprintf ( TFile, 
	      "\n%sControl Values: Open_Dot_B_File\n%s\n", DBar, DBar );
	    Print_Controls ( TFile, "", TRUE );
	  }
	}

      /* Configure internal options for this source file */
      Configure_Source ( Src_File_Name );

      return TRUE;
    }
  }

  return FALSE;
}

static const char* base_type_name[] = {"char", "short", "int", "long", "long long", "float", "double", "long double", "size_t", "ptrdiff_t"};

#ifndef MAX_LINE_LEN_UPC_CONFIG
#define MAX_LINE_LEN_UPC_CONFIG 1024
#endif

static unsigned r_size = 0;
static unsigned m_size = 0;
static unsigned shared_align = 0;
static unsigned pshared_align = 0;
static unsigned hreg_align = 0;
static unsigned hmem_align = 0;

static void Read_Config_File(int argc, char** argv) {

  memset(base_ty_size, 0, sizeof (base_ty_size));
  for (int i = 0; i < argc; i++) {
    char* arg = argv[i];
    if (strncmp(arg, "-fconfig-", 9) == 0) {
      char* Config_File_Name = arg + 9;
      FILE* config_file = fopen(Config_File_Name, "r");
      if (config_file == NULL) {
	FmtAssert(false, ("CANNOT OPEN CONFIGURATION FILE: %s\n", Config_File_Name));
      }
      char line[MAX_LINE_LEN_UPC_CONFIG];
      unsigned int size;
      unsigned runtime_major, runtime_minor;
      char buf[MAX_LINE_LEN_UPC_CONFIG];
      char * param = (char *) buf;
      while (fgets(line, MAX_LINE_LEN_UPC_CONFIG, config_file) != NULL) {
        if (strspn(line," \t\n") == strlen(line) || // skip blank lines
            line[strspn(line," \t")] == '#') continue; // and comments
        if (strstr(line,"GASNetConfig") == line ||
            strstr(line,"UPCRConfig") == line) continue; // ignore these
	if (strstr(line, "upc_header_dir") == line) {
	  param = strpbrk(line, "\t ");
	  if (param == NULL) {
	    DevWarn ("Malformed line in upcc config file: %s\n", line);
	    continue;
	  }
	  //get rid of leading space
	  while (isspace(*param)) {
	    param++;
	  }
	  //get rid of trailing whitespace
	  char * end = strpbrk(param, "\t\n ");
	  *end = '\0';
	  upc_header_dirs[num_header_dirs] = (char *) malloc (strlen(param) + 1);
	  strcpy(upc_header_dirs[num_header_dirs++], param); 
	  //fprintf(stderr, "dir: %s###\n", upc_header_dirs[num_header_dirs-1]);
	  if (num_header_dirs > sizeof (upc_header_dirs) / sizeof(char *)) {
	    DevWarn("Overflow on the number of UPC header directories in the upcc config file\n");
	    num_header_dirs = 0;
	  }
	  continue;
	}

	if (strstr(line, "system_header_dirs") == line) {
	  param = strpbrk(line, "\t ");
	  if (param == NULL) {
	    DevWarn ("Malformed line in upcc config file: %s\n", line);
	    continue;
	  }
	  //get rid of leading space
	  while (isspace(*param)) {
	    param++;
	  }
	  //get rid of trailing whitespace
	  char * end = strpbrk(param, "\t\n ");
	  *end = '\0';
	  system_header_dirs = (char *) malloc (strlen(line) + 1);
	  strcpy(system_header_dirs, param); 
	  continue;
	}

	if (sscanf(line, "%s\t%u.%u", param, &runtime_major, &runtime_minor) == 3) {
	  /* Do version check here */
	  if (UPCR_SPEC_MAJOR < runtime_major) {
	    Fail_FmtAssertion(
		    "UPC translator too old (uses interface %d.%d: runtime provides %d.%d)."
		    "\n\nPlease try 'upcc -nightly' or 'upcc -stable' to point at a newer translator,\n"
		    "or download a newer version of the translator from http://upc.lbl.gov/\n",
		    UPCR_SPEC_MAJOR, UPCR_SPEC_MINOR, runtime_major, runtime_minor); 
	  } else if (UPCR_SPEC_MAJOR > runtime_major || UPCR_SPEC_MINOR > runtime_minor) {
	    Fail_FmtAssertion(
		    "UPC runtime too old (provides interface %d.%d: translator needs %d.%d)."
		    "\n\nPlease download a newer version of the UPC runtime from http://upc.lbl.gov/\n",
		    runtime_major, runtime_minor, UPCR_SPEC_MAJOR, UPCR_SPEC_MINOR);
	  }
	  continue;
	}
	if (sscanf(line, "%s\t%u", param, &size) != 2) {
	  DevWarn("Malformed line in upcc config file: %s\n", line);
	  continue;
	}
	if (strcmp(param, "shared_ptr") == 0) {
	  shared_size = size;
	  continue;
	} else if (strcmp(param, "pshared_ptr") == 0) {
	  pshared_size = size;
	  continue;
	} else if (strcmp(param, "maxblocksz") == 0) {
	  max_bsize = size;
	  continue;
	} else if (strcmp(param, "reg_handle") == 0) {
	  r_size = size;
	} else if (strcmp(param, "mem_handle") == 0) {
	  m_size = size;
	}

	int align = strncmp(param, "alignof_", 8) == 0;
	if (align) {
	  param = param + 8;
	}
	
	/* now check for size/align of base types */
	if (strcmp(param, "char") == 0) {
	  base_ty_size[_CHAR][align] = size;  
	} else if (strcmp(param, "short") == 0) {
	  base_ty_size[_SHORT][align] = size;
	} else if (strcmp(param, "int") == 0) {
	  base_ty_size[_INT][align] = size;
	} else if (strcmp(param, "long") == 0) {
	  base_ty_size[_LONG][align] = size;
	} else if (strcmp(param, "longlong") == 0) {
	  base_ty_size[_LONG_LONG][align] = size;
	} else if (strcmp(param, "float") == 0) {
	  base_ty_size[_FLOAT][align] = size;
	} else if (strcmp(param, "double") == 0) {
	  base_ty_size[_DOUBLE][align] = size;
	} else if (strcmp(param, "longdouble") == 0) {
	  base_ty_size[_LONGDOUBLE][align] = size;
	} else if (strcmp(param, "size_t") == 0) {
	  base_ty_size[_SIZE_TY][align] = size;
	} else if (strcmp(param, "ptrdiff_t") == 0) {
	  base_ty_size[_PTRDIFF][align] = size;
	} else if (strcmp(param, "shared_ptr") == 0 && align) {
	  shared_align = size;
	} else if (strcmp(param, "pshared_ptr") == 0 && align) {
	  pshared_align = size;
	} else if (strcmp(param, "mem_handle") == 0 && align) {
	  hmem_align = size;
	} else if (strcmp(param, "reg_handle") == 0 && align) {
	  hreg_align = size;
	} else if(strcmp(param, "dbl_1st") == 0 && align) {
	    double_align_exception = size;
	}  else if(strcmp(param, "dbl_innerstruct") == 0 && align) {
	  if (size == 0)
	    double_inner_exception = TRUE;
	} else if(strcmp(param, "int64_1st") == 0 && align) {
	  longlong_align_exception = size;
	} else if(strcmp(param, "int64_innerstruct") == 0 && align) {
	  if (size == 0)
	    longlong_inner_exception = TRUE;
	} else if(strcmp(param, "sharedptr_1st") == 0 && align) {
	  sptr_align_exception = size;
	}  else if(strcmp(param, "sharedptr_innerstruct") == 0 && align) {
	  if (size != 0)
	    sptr_inner_exception = TRUE;
	} else if(strcmp(param, "psharedptr_1st") == 0 && align) {
	  psptr_align_exception = size;
	} else if(strcmp(param, "psharedptr_innerstruct") == 0 && align) {
	  if (size != 0)
	    psptr_align_exception = TRUE;
	}  else if(strcmp(param,"struct_promote") == 0 && align) {
	  struct_align_promote = size;
	}
      }
      if (shared_size == 0 || pshared_size == 0 || max_bsize == 0) {
	FmtAssert(0, ("Missing pointer-to-shared size in configuration file"));
      }

      /* check if the sizes for base types are set */
      for (int j = _INT; j < _LAST; j++) {
	FmtAssert(base_ty_size[j][0] != 0, ("Missing the size for the base type %s", base_type_name[j]));
	/* for compatiblity with old runtime, use default align if not available in the size file */
      }

      MTYPE_size_reg(MTYPE_FQ) = base_ty_size[_LONGDOUBLE][0] * 8; //in bits
      return;
    }
  }
}
  
extern int is_ia32; /* from toplev.h */

/**
 *  hack to get the front end to target ia32 
 *  All changes that are necessary to get 32-bit backend to work needs to be placed here
 */
static void Set_Upc_Flags(int argc, char** argv) {

  for (int i = 0; i < argc; i++) {
    char* arg = argv[i];
    if (strcmp(arg, "-ia32") == 0) {
      ABI_Name = "ia32";
      is_ia32 = 1;
    } else if (strcmp(arg, "-std=upc") == 0) {
      compiling_upc = 1;
      Compile_Upc = 1; //some common files use this flag
    } else if(strstr(arg, "-g") == arg) {
       Debug_Level = 2;
    }
  }
}

static void Set_Base_Type_Align() {

  /* Use the alignment values from the config file */
  /* 2 byte int */
  if (base_ty_size[_SHORT][0] == 2) {
    /* LP64, ILP32 */
    if (base_ty_size[_SHORT][1] != 0) {
      MTYPE_align_req(MTYPE_I2) = base_ty_size[_SHORT][1];
      MTYPE_align_req(MTYPE_U2) = base_ty_size[_SHORT][1];
    }
  }
  /* 4 byte int */
  if (base_ty_size[_INT][0] == 4) {
    /* LP64, ILP32 */
    if (base_ty_size[_INT][1] != 0) {
      MTYPE_align_req(MTYPE_I4) = base_ty_size[_INT][1];
      MTYPE_align_req(MTYPE_U4) = base_ty_size[_INT][1];
    }
  } else if (base_ty_size[_SHORT][0] == 4) {
    /* ILP64 machines */
    if (base_ty_size[_SHORT][1] != 0) {
      MTYPE_align_req(MTYPE_I4) = base_ty_size[_SHORT][1];
      MTYPE_align_req(MTYPE_U4) = base_ty_size[_SHORT][1];
    }
  }
  /* 8 byte int */
  if (base_ty_size[_LONG][0] == 8) {
    /* LP64, ILP64 */
    if (base_ty_size[_LONG][1] != 0) {
      MTYPE_align_req(MTYPE_I8) = base_ty_size[_LONG][1];
      MTYPE_align_req(MTYPE_U8) = base_ty_size[_LONG][1];
    }
  } else if (base_ty_size[_LONG_LONG][0] == 8) {
    /* ILP32 */
    if (base_ty_size[_LONG_LONG][1] != 0) {
      MTYPE_align_req(MTYPE_I8) = base_ty_size[_LONG_LONG][1];
      MTYPE_align_req(MTYPE_U8) = base_ty_size[_LONG_LONG][1];
    }
  }
  /* 4 byte float */
  if (base_ty_size[_FLOAT][0] == 4) {
    if (base_ty_size[_FLOAT][1] != 0) {
      MTYPE_align_req(MTYPE_F4) = base_ty_size[_FLOAT][1];
    }
  }
  /* 8 byte double */
  if (base_ty_size[_DOUBLE][0] == 8) {
    if (base_ty_size[_DOUBLE][1] != 0) {
      MTYPE_align_req(MTYPE_F8) = base_ty_size[_DOUBLE][1];
    }
  }
}

void
WFE_Init (INT argc, char **argv, char **envp )
{
  Set_Error_Tables ( Phases, host_errlist );
  MEM_Initialize();
  Handle_Signals();

  /* Perform preliminary command line processing: */
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_Phase ( "Front End Driver" );
  Preconfigure ();

#ifdef TARG_MIPS
  ABI_Name = (char *) mips_abi_string;
#endif
#ifdef TARG_IA64
  ABI_Name = "i64";
#endif
#ifdef TARG_IA32
  ABI_Name = "ia32";
#endif
  Init_Controls_Tbl();
  Argc = argc;
  Argv = argv;

  //WEI: code for supporting ia32 backend
  Set_Upc_Flags(argc, argv);
  if (compiling_upc) { /* set by Set_Upc_Flags */
    Read_Config_File(argc, argv);
  }
  
  Configure ();
  Initialize_C_Int_Model();
  IR_reader_init();

  Set_Base_Type_Align();
  Initialize_Symbol_Tables (TRUE);
  if (compiling_upc) {
     Create_Special_Shared_Global_Symbols() ;
     /* Need two shared_ptr symbols because the front end needs to compute the size for them */
     Initialize_Upc_Types ("shared_ptr_struct", shared_size, shared_align, 
			   "pshared_ptr_struct", pshared_size, pshared_align,
			   "reg_handle_t", r_size, hreg_align,
			   "mem_handle_t", m_size, hmem_align);
     Set_TY_align(shared_ptr_idx, shared_align);
     Set_TY_align(pshared_ptr_idx, pshared_align);
  }

  WFE_Stmt_Stack_Init ();
  WFE_Stmt_Init ();
  WFE_Expr_Init ();
  WHIRL_Mldid_Mstid_On = TRUE;
  WN_Simp_Fold_LDA = TRUE;  // fold (LDA offset) + const to LDA (offset+const)
			    // since the static initialization code relies on it
  WHIRL_Keep_Cvt_On = TRUE; // so simplifier won't I8I4CVT
  Opt_Level = optimize;

  // This is not right: we should match what gnu does
  // and this is only an approximation.
  if(!Debug_Level)
     Debug_Level = (debug_info_level >= DINFO_LEVEL_NORMAL)? 2:0;
} /* WFE_Init */

void
WFE_File_Init (INT argc, char **argv)
{
  /* Process each source file: */
  Prepare_Source();
  MEM_POOL_Push (&MEM_src_pool);
  Restore_Cmd_Line_Ctrls();

  /* If the user forgot to specify sources, complain: */
  if ( Src_Count == 0 ) {
    ErrMsg ( EC_No_Sources );
  }

  Open_Output_Info ( Irb_File_Name );
  DST_build(argc, argv);	// do initial setup of dst

}

extern char* get_type(int mtype) {
  
  switch (mtype) {
  case MTYPE_I1:
    return "int8_t";
  case MTYPE_I2:
    return "int16_t";
  case MTYPE_I4:
    return "int32_t";
  case MTYPE_U1:
    return "uint8_t";
  case MTYPE_U2:
    return "uint16_t";
  case MTYPE_U4:
    return "uint32_t";
  case MTYPE_I8:
    return "int64_t";
  case MTYPE_U8:
    return "uint64_t";
  case MTYPE_F4:
    return "float";
  case MTYPE_F8:
    return "double";
  case MTYPE_F10:
  case MTYPE_F16:
    return "long double";
  case MTYPE_FQ:
    return "long double";
  default:
    fprintf(stderr, "COULDN'T FIND CORRECT TYPE IN get_type() for mtype: %d\n", mtype);
    return "NONE";
  }
}

/* helper for output_Init_Val() */
static char * output_Init_String_Val(char *s, TCON tc)
{
      //bug1382:  Check for long string literals (>128 chars)
      int slen = Targ_String_Length(tc) - 1; // subtract 1 for the extra \0 
      if (slen > ((256/4)-3)) { 
	//allocate 4X the string length, since in the worst case every char may be 
	//an escaped non-printing character (e.g.'\255').  Also leave space for quotes and \0 
	char *tmp = (char*) malloc(4 * slen + 3); 
	free(s);
	s = tmp;
      }
      char* str = s, *orig = Targ_String_Address(tc);
      *str++ = '\"';
      for (int i = 0; i < slen; i++) {
	str = append_char(str, orig[i]);
      }
      while (str[-1] == '\0') str--;
      *(str++) = '\"';
      *(str++) = '\0';
      return s;
}

/* Borrow the logic from whirl2c 
 * This should eventually go away when we move the init code into whirl2c 
 * caller of this function is responsible for freeing the character array malloced in
 * each call
 */

static char * output_Init_Val(INITV_IDX idx, TY_IDX ty = 0) {

  TCON tc; //used for INITVKIND_VAL
  ST_IDX rhs_st; //used for INITVKIND_SYMOFF
  int mtype;
  char *str = NULL;
  char *s = (char*) malloc(256); //big enough to hold any C identifiers  

  switch (INITV_kind(idx)) {
  case INITVKIND_ZERO:
    sprintf(s, "0");
    break;
  case INITVKIND_ONE:
    sprintf(s, "1");
    break;
  case INITVKIND_VAL:
    tc = INITV_tc_val(idx);
    mtype = TCON_ty(tc);
    if (ty != 0 && MTYPE_is_integral(TY_mtype(ty))) {
      if (MTYPE_is_signed(mtype) && MTYPE_is_unsigned(TY_mtype(ty))) {
	//bug1451:  use an unsigned value to initialize unsigned variables
	mtype = MTYPE_complement(mtype);
      }
    }
    switch (mtype) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
      str = Targ_Print("%d", tc);
      break;
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      str = Targ_Print("%uU", tc);
      break;
    case MTYPE_I8:
      str = Targ_Print("%1lldLL", tc);
      break;
    case MTYPE_U8:
      str = Targ_Print("%lluULL", tc);
      break;
    case MTYPE_F4:
      str = Targ_Print("%.7g", tc);
      if (strstr(str, ".") == NULL) {
	//no trailing zeros are printed, need to add it before printing the F suffix
	strcat(str, ".0F");
      } else {
	strcat(str, "F");
      }
      break;
    case MTYPE_F8:
      str = Targ_Print("%.15g", tc);
      break;
    case MTYPE_FQ:
      str = Targ_Print("%Lf", tc);
      break;
    case MTYPE_STR: {
      s = output_Init_String_Val(s, tc);
      str = NULL;
      break;
    }
    default:
      FmtAssert(false, ("Unexpected type for init value"));
    } //inner switch

    if (str != NULL) {
      strcpy(s, str);
    }
    break;

  case INITVKIND_SYMOFF:
    rhs_st = INITV_st(idx);
    // part of bug1275 originates because we ignore INITV_ofst(idx)
    if (ty != 0 && Type_Is_Shared_Ptr(ty)) {
      TY_IDX rhs_ty = ST_type(rhs_st);
      if (!TY_is_shared(rhs_ty)) {
	//should be caught elsewhere, just in case...
	FmtAssert(0, ("ASSIGNING a non-shared value to a pointer to shared data: "));
      }
      bool lhs_pshared = Type_Is_Shared_Ptr(TY_pointed(ty))
	      			? TY_is_pshared(TY_pointed(ty))
	      			: TY_is_pshared(ty);
      if (lhs_pshared && !TY_is_pshared(rhs_ty)) {
	sprintf(s, "upcr_shared_to_pshared(%s)", ST_name(rhs_st));
      } else if (!lhs_pshared && TY_is_pshared(rhs_ty)) {
	sprintf(s, "upcr_pshared_to_shared(%s)", ST_name(rhs_st));
      } else {
	sprintf(s, "%s", ST_name(rhs_st));
      }
    } else {
      if (ST_class(rhs_st) == CLASS_CONST) {
	//case for string constants
	tc = STC_val(ST_ptr(rhs_st));
	Is_True(TCON_ty(tc) == MTYPE_STR, ("initial expression is not a string constant"));
#if 0
	sprintf(s, "\"%s\"", Targ_String_Address(tc));
#else
        s = output_Init_String_Val(s, tc);
        str = NULL;
#endif
      } else if (TY_kind(ST_type(rhs_st)) == KIND_ARRAY) {
	//arrays DO NOT need a "&"
	sprintf(s, "%s", ST_name(rhs_st));
      } else {
	//initialization for regular pointers, need a "&"
	sprintf(s, "&%s", ST_name(rhs_st));
      }
    }
    break;
  default:
    sprintf(s, "NONE");
  }
  return s;
}

static unsigned int get_dim(TY_IDX idx) {

  switch (TY_kind(idx)) {
  case KIND_STRUCT:
    return 1;
  case KIND_ARRAY: {
    int dim = 0;
    for (;TY_kind(idx) == KIND_ARRAY; idx = TY_etype(idx), dim++);
    return dim;
  }
  default:
    return 0;
  }
}

static string get_init_exp(ST_IDX st, 
			   INITV_IDX* idx = NULL ,
			   TY_IDX ty_idx = 0, 
			   string init_exp = "");

/**
 *
 * Get the correct init exp for the given INITV_IDX idx
 * dim is the number of dimensions for arrays
 */
static string get_init_exp(ST_IDX st, INITV_IDX* idx, TY_IDX ty_idx, string init_exp ) {

  char* buf = NULL;
  INITV_IDX tmp;
  string current = "";
  UINT size;
  UINT init = 0;
  UINT esize = 0;
  UINT tsize = 0;
  UINT last_pad = 0;
  TY_IDX ety = 0;

  if(st) {
    if (ST_init_value_zero(ST_ptr(st))) {
      return "0";
    }
    if(idx == NULL) {
      idx = &tmp;
      *idx = INITV_index(ST_IDX_level(st), st);
      init_exp = "";
    }
    ty_idx = ST_type(st);
    //check for pointer initialized to zero
  }
  
  int dim = get_dim(ty_idx);
  int kind = TY_kind(ty_idx);
  size = TY_size(ty_idx);

  if (*idx == INITV_Table_Size() + 1) {
    return "NONE";
  }  

  switch (kind) {
  case KIND_ARRAY:
    {
      INITV_IDX next_idx;
      BOOL prev_pad = FALSE;
      esize = TY_size(Get_Inner_Array_Type(ty_idx));
      ety = Get_Inner_Array_Type(ty_idx);
      int first = 1;
      //  init_exp = "{";
      for(next_idx = *idx; next_idx != 0 && tsize < size ; next_idx = INITV_next(next_idx)) {
	

	string tmps = "";
	if(INITV_kind(next_idx) == INITVKIND_PAD) {
	  if(INITV_pad(next_idx) != 0 || 
	     (INITV_pad(next_idx) == 0 && INITV_next(next_idx) != 0))
	    current = "{" + current + "}";
	  last_pad = INITV_pad(next_idx);
	  tsize += last_pad;
	  prev_pad = TRUE;
	  
	} else { /* proper value */
	  
	  if(TY_kind(ety) == KIND_STRUCT) {
	   tmps =  get_init_exp(0,&next_idx, Get_Inner_Array_Type(ty_idx), tmps);
	  } else {
	    buf = output_Init_Val(next_idx, 0);
	    if(INITV_kind(next_idx) == INITVKIND_VAL) { // For bug 2702
	      TCON tc = INITV_tc_val(next_idx);
	      if(TCON_ty(tc) == MTYPE_STR)
		esize = Targ_String_Length(tc);
	    }
	  }
	  if (prev_pad) {
	    if (init_exp == "") 
	      init_exp = current;
	    else 
	      init_exp = init_exp + "," + current;
	    current = "";
	  }
	  tsize += esize;
	  if(TY_kind(ety) != KIND_STRUCT)
	    current  += buf;
	  else 
	    current += tmps;
	  current  += ",";
	  prev_pad = FALSE;
	}
      }

      if(init_exp == "") {
	if(st) { 
	  if(last_pad)
	    init_exp = current;
	  else 
	    init_exp = "{" + current + "}";
	} else {
	  //for 1 dim arrays that are fully initialized we get here without 
	  //the proper paranthetisation
	  if(prev_pad)
	    init_exp = current;
	  else
	    init_exp = "{" + current + "}";

	}
      } else {
	if (st)
	  init_exp = "{" + init_exp + ", {" + current + "} }";
	else 
	  init_exp = current;
      }	
      *idx = next_idx - 1;
    
    break;
    }   
    
  case KIND_STRUCT:
    {
      init_exp += "{";
      TY_IDX fty, prev_fty;
      UINT nelems = 0;
      FLD_HANDLE fld = FLD_get_to_field (ty_idx, 1, nelems);
      FLD_ITER  fiter  = Make_fld_iter(TY_fld(ty_idx));
      FLD_ITER last = Fld_Table.end ();
      FLD_HANDLE fh;
      FLD_HANDLE prev_fld;

      INITV_IDX  next_idx = *idx;
      
      do {
	fh = FLD_HANDLE(fiter);
	fty = FLD_type(fh);
	
	if(INITV_kind(next_idx) == INITVKIND_PAD ) {
	  //check to make sure that the padding is not due to
	  //the alignment of the next field
	  prev_fty = FLD_type(prev_fld);
	  if(INITV_pad(next_idx) == 0) 
	    next_idx = INITV_next(next_idx);
	  else
	    if( TY_size(prev_fty) + INITV_pad(next_idx) == 
		FLD_adjusted_ofst(fh) - FLD_adjusted_ofst(prev_fld)) {
	      
	      //This code generates init values for the padding fields
	      //w2c adds to aggregates. For the time being we disable
	      //explicit padding in w2c.
	     //  init_exp += "{";      
// 	      for(int i = 0; i < INITV_pad(next_idx)-1; i++) {
// 		init_exp += "0";
// 		init_exp += ",";
// 	      }
// 	      init_exp += "0},";

	      next_idx = INITV_next(next_idx);
	    }	  
	}

	switch (TY_kind(fty)) {
	case KIND_SCALAR:
	case KIND_POINTER:
	  if (INITV_kind(next_idx) != INITVKIND_PAD) {
	    buf = output_Init_Val(next_idx, 0);
	    init_exp += buf;
	    init_exp += ",";
	  }
	  break;
	case KIND_ARRAY:
	  init_exp += get_init_exp(0, &next_idx, fty, "") ;
	  init_exp += ",";
	  break;
	case KIND_STRUCT:
	  init_exp += get_init_exp(0, &next_idx, fty, "") + ",";
	  break;
	default:
	  FmtAssert(0,("",""));
	}
	next_idx = INITV_next(next_idx);
	prev_fld = fh;
     } while (!FLD_last_field(fiter) && ++fiter != last && next_idx);
     
      *idx = next_idx;
      init_exp += "}";
    }
    break;
  case KIND_SCALAR:
  case KIND_POINTER:
    buf = output_Init_Val(*idx, ty_idx);
    init_exp += buf;
    break;
  }

  free (buf);
  return init_exp;
}

/* Print out a description of the given type (used by bupc_assert_type)*/
static string debug_type (TY_IDX ty) {

  string s;
  char buf[15];
  if (TY_is_shared(ty)) {
    snprintf(buf, 15, "[%d] ", Get_Type_Block_Size(ty));
    s = "shared " + (string) buf;
  } else {
    s = "private ";
  }

  /* print out type qualifiers */
  if (TY_is_const(ty)) {
    s += "const ";
  }
  if (TY_is_volatile(ty)) {
    s += "volatile ";
  }

  switch (TY_kind(ty)) {
  case KIND_VOID:
    s += "void";
    break;
  case KIND_SCALAR:
    s += (string) get_type(TY_mtype(ty));
    break;
  case KIND_STRUCT:
    s += "struct " + (string) TY_name(ty);
    break;
  case KIND_POINTER:
    s += "pointer to ";
    s += debug_type(TY_pointed(ty));
    break;
  case KIND_ARRAY:
    s += "array of ";
    s += debug_type(TY_etype(ty));
    break;
  case KIND_FUNCTION: 
    /* supporting function pointers */
    s += "function: (";
    s += debug_type(TY_ret_type(ty));
    s += ") (";
    for (TYLIST_IDX params = TY_parms(ty);Tylist_Table[params] != 0; params = TYLIST_next(params)) {
      s += debug_type(TYLIST_item(Tylist_Table[params]));
      s += ", ";
    }
    s += ")";
    break;
  default:
    FmtAssert(false, ("Invalid kind of types in debug_type"));    
  }

  return s;
}

void debug_type_wrapper(int ty, char * s) {
  string ty_s = debug_type((TY_IDX) ty);
  strncpy (s, (char *) ty_s.c_str(), 1024);
}

static string file_name = ""; 
static FILE* out_file = NULL;


int get_TY_wrapper(tree type) {
  return Get_TY(type);
}

UINT64 get_real_size(int t_idx) {

  switch (TY_kind(t_idx)) {
  case KIND_SCALAR:
    return TY_size(t_idx);
  case KIND_POINTER: 
    /* for pointer-to-shared */
    if (TY_is_shared(TY_pointed(t_idx))) {
      return TY_is_pshared(TY_pointed(t_idx)) ? pshared_size : shared_size;
    }
    return TY_size(t_idx);
  case KIND_ARRAY: {
    TY_IDX elt = TY_etype(t_idx);
    while (TY_kind(elt) == KIND_ARRAY) {
      elt = TY_etype(elt);
    }
    UINT64 num_elts = TY_size(t_idx) / TY_size(elt);
    return num_elts * get_real_size(elt);
  }
  case KIND_STRUCT: 
    return TY_adjusted_size(t_idx);
  default:
    error_with_file_and_line(input_filename, lineno, "Incomplete or unknown type\n");
    Fatal_Error("Compilation aborted");

  }  
}

void add_shared_symbol(ST_IDX st, int thread_dim) {
  process_shared(st, thread_dim, get_init_exp(st));
}

void add_TLD_symbol(ST_IDX st) {

  //fprintf(stderr, "Adding symbol: %s\n", ST_name(st));
  process_nonshared(st, get_init_exp(st));
}

/* from wfe_decl.cxx */
extern void add_symbols();

void
WFE_File_Finish (void)
{
  Verify_SYMTAB (GLOBAL_SYMTAB);
  ST* st;
  int i;
  bool missing_type = false;
  
 //  if(Debug_Level >= 2)
//     WFE_Patch_File_Statics();

  if (threads_int != 0) {
    //pass the number of (static) threads to the backend via an INITO
    if (upc_threads_st == NULL) {
      upc_threads_st = New_ST(GLOBAL_SYMTAB);
      ST_Init (upc_threads_st, Save_Str("THREADS"), CLASS_VAR, SCLASS_DGLOBAL, 
	       EXPORT_PREEMPTIBLE, MTYPE_To_TY(Integer_type));
    }
    INITO_IDX inito = New_INITO(upc_threads_st);
    INITV_IDX inv = New_INITV();
    INITV_Init_Integer (inv, Integer_type, threads_int);
    Set_INITO_val(inito, inv);
    Set_ST_is_initialized(upc_threads_st);
  }

  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i)
    if (ST_class(st) == CLASS_VAR) {
      TY_IDX ty = ST_type(st);
      if (TY_size(ty) == 0 && (TY_kind(ty) == KIND_STRUCT || TY_kind(ty) == KIND_VOID /* for upc_lock_t*/)) {
	/* The case of a variable declared with incomplete struct.  
	 * We do not catch it when processing the declaration, since C allows a struct to be forward declared
	 * Here if the variable is not extern, we output an error
	 * see bug 244.
	 */
	if (ST_sclass(st) != SCLASS_EXTERN) {
	  /* It seems to be a bug in gcc front end; calling error() does not 
	     lead to a fatal exit code (main() in gccfe/gnu has already returned??).
	     So we have to explicitly exit here. 
	  */
	  error("storage size of %s is not known", ST_name(st));
	  missing_type = true;
	}
      }
	
    }

  if (missing_type) {
    exit(RC_USER_ERROR);
  }

  /* in the unlikely case that the file contains no functions... */
  add_symbols();

  Write_Global_Info (PU_Tree_Root);
  Close_Output_Info ();
  IR_reader_finish ();
  MEM_POOL_Pop (&MEM_src_pool);
  
  output_file();
  
  
}

void
WFE_Finish ()
{
  WFE_Stmt_Stack_Free ();
}

void
WFE_Check_Errors (int *error_count, int *warning_count, BOOL *need_inliner)
{
  
  /* If we've seen errors, note them and terminate: */
  Get_Error_Count ( error_count, warning_count);
  *need_inliner = wfe_invoke_inliner;
}

#define ENLARGE(x) (x + (x >> 1))
#define WN_STMT_STACK_SIZE 32

typedef struct wn_stmt {
  WN            *wn;
  WFE_STMT_KIND  kind;
} WN_STMT;

static WN_STMT *wn_stmt_stack;
static WN_STMT *wn_stmt_sp;
static WN_STMT *wn_stmt_stack_last;
static INT      wn_stmt_stack_size;

char * WFE_Stmt_Kind_Name [wfe_stmk_last+1] = {
  "'unknown'",
  "'function entry'",
  "'function pragma'",
  "'function body'",
  "'region pragmas'",
  "'scope'",
  "'if condition'",
  "'if then clause'",
  "'if else clause'",
  "'while condition'",
  "'while body'",
  "'dowhile condition'",
  "'dowhile body'",
  "'for condition'",
  "'for body'",
  "'switch'",
  "'comma'",
  "'rcomma'",
  "'last'"
};

static void
WFE_Stmt_Stack_Init (void)
{
  wn_stmt_stack_size = WN_STMT_STACK_SIZE;
  wn_stmt_stack      = (WN_STMT *) malloc (sizeof (WN_STMT) *
                                           wn_stmt_stack_size );
  wn_stmt_sp         = wn_stmt_stack - 1;
  wn_stmt_stack_last = wn_stmt_stack + wn_stmt_stack_size - 1;
} /* WFE_Stmt_Stack_Init */

static void
WFE_Stmt_Stack_Free (void)
{
  free (wn_stmt_stack);
  wn_stmt_stack = NULL;
} /* WFE_Stmt_stack_free */

void
WFE_Stmt_Push (WN* wn, WFE_STMT_KIND kind, SRCPOS srcpos)
{
  INT new_stack_size;

  if (wn_stmt_sp == wn_stmt_stack_last) {
    new_stack_size = ENLARGE(wn_stmt_stack_size);
    wn_stmt_stack =
      (WN_STMT *) realloc (wn_stmt_stack, new_stack_size * sizeof (WN_STMT));
    wn_stmt_sp = wn_stmt_stack + wn_stmt_stack_size - 1;
    wn_stmt_stack_size = new_stack_size;
    wn_stmt_stack_last = wn_stmt_stack + wn_stmt_stack_size - 1;
  }
  ++wn_stmt_sp;
  wn_stmt_sp->wn   = wn;
  wn_stmt_sp->kind = kind;

  if (srcpos) {
    WN_Set_Linenum ( wn, srcpos );
  }
} /* WFE_Stmt_Push */

BOOL 
WFE_Stmt_Empty(void) {

  return (wn_stmt_sp < wn_stmt_stack);
}

WN*
WFE_Stmt_Top (void)
{
  FmtAssert (wn_stmt_sp >= wn_stmt_stack,
             ("no more entries on stack in function WFE_Stmt_Top"));

  return (wn_stmt_sp->wn);
} /* WFE_Stmt_Top */

void
WFE_Stmt_Append (WN* wn, SRCPOS srcpos)
{
  WN * body;
  WN * last;

  if (srcpos) {
    WN_Set_Linenum ( wn, srcpos );
    if (WN_operator(wn) == OPR_BLOCK && WN_first(wn) != NULL)
    	WN_Set_Linenum ( WN_first(wn), srcpos );
  }

  body = WFE_Stmt_Top ();

  if (body) {

    last = WN_last(body);
    WN_INSERT_BlockAfter (body, last, wn);
  }
} /* WFE_Stmt_Append */


WN*
WFE_Stmt_Last (void)
{
  WN * body;

  body = WFE_Stmt_Top ();
  return (WN_last(body));
} /* WFE_Stmt_Last */


WN *
WFE_Stmt_Delete ()
{
  WN * body;
  WN * last;
  WN * prev;

  body = WFE_Stmt_Top ();
  last = WN_last(body);
  prev = WN_prev(last);
  if (prev)
    WN_next(prev)  = NULL;
  else
    WN_first(body) = NULL;
  WN_last(body) = prev;
  WN_prev(last) = NULL;

  return last;
} /* WFE_Stmt_Delete */


WN*
WFE_Stmt_Pop (WFE_STMT_KIND kind)
{
  WN * wn;

  FmtAssert (wn_stmt_sp >= wn_stmt_stack,
             ("no more entries on stack in function WFE_Stmt_Pop"));

  FmtAssert (wn_stmt_sp->kind == kind,
             ("mismatch in statements: expected %s, got %s\n",
              WFE_Stmt_Kind_Name [kind],
              WFE_Stmt_Kind_Name [wn_stmt_sp->kind]));

  wn = wn_stmt_sp->wn;
  wn_stmt_sp--;

  return (wn);
} /* WFE_Stmt_Pop */

extern "C" int is_64bit_target() {
  
  return  TY_size(MTYPE_To_TY(Pointer_type)) > 4 ? 1 : 0;
}

extern "C" int int_is_64bit() {
  
  return  TY_size(MTYPE_To_TY(Integer_type)) > 4 ? 1 : 0;
}

extern "C" tree size_ll(unsigned long long sz) {
  if(is_64bit_target() && sizeof(HOST_WIDE_INT) < sizeof(long long)) {
    tree t = build_int_2(sz & 0xFFFFFFFFllu, (sz>>32) & 0xFFFFFFFFllu);
    TREE_TYPE (t) = sizetype;
    TREE_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (t) = force_fit_type (t, 0);
    return t;
  } else
    return size_int(sz);
}
