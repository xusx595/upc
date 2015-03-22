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


/* ====================================================================
 * ====================================================================
 *
 * Module: driver_util.c
 * $Revisionr: 1.34 $
 * $Date: 2012/04/28 01:48:17 $
 * $Author: phargrov $
 * $Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/be/driver_util.cxx $
 *
 * Revision history:
 *  16-Feb-95 - Original Version
 *
 * Description:
 *  Utility functions for the be driver.
 *
 * ====================================================================
 * ====================================================================
 */

#include <ctype.h>
#include <cmplrs/rcodes.h>
#include "defs.h"
#include "glob.h"		    /* for Src_File_Name, etc. */
#include "erglob.h"		    /* for EC_File_Name, etc. */
#include "file_util.h"		    /* for New_Extension() */
#include "phase.h"		    /* for phase_ogroup_table */
#include "flags.h"		    /* for Process_Command_Line_Group() */
#include "config.h"		    /* for Debug_Level */
#include "config_list.h"
#include "timing.h"		    /* for Initialize_Timing() */
#include "tracing.h"		    /* for Set_Trace() */
#include "wn.h"			    /* for WN */
#include "stab.h"
#include "stblock.h"
#include "wn_lower.h"
#include "wn_fio.h"
#include "wn_instrument.h"
#include "driver_util.h"

int compiling_upc_flag = 0;
//defined in upc_symtab_utils.cxx
extern int debug_requested;

/* argc and argv for phase-specific flags */
static UINT phase_argc[PHASE_COUNT];
static STRING *phase_argv[PHASE_COUNT];
static UINT phase_max_argc[PHASE_COUNT];

/* Options based on user flags:	*/
# define MAX_MSG_LEVEL 2

/* Default file	extensions: */
#define	ERR_FILE_EXTENSION ".e"	    /* Error file */
#define	TRC_FILE_EXTENSION ".t"	    /* Trace file */
#define IRB_FILE_EXTENSION ".B"	    /* WHIRL file */
#define LST_FILE_EXTENSION ".l"	    /* Listing file */
#define TLOG_FILE_EXTENSION ".tlog" /* Transformation log file */

static BOOL Tlog_Enabled = FALSE;
extern BOOL Run_Dsm_Cloner;
extern BOOL Run_Dsm_Check;
extern BOOL Run_Dsm_Common_Check;
extern BOOL Run_autopar;
extern BOOL Run_MemCtr;
static BOOL Dsm_Recompile = FALSE;

//from upc_wn_util.cxx
extern BOOL Use_Valget;
extern BOOL Use_Type_Access;

/*
 * Handle_Phase_Specific_Options
 */
static void
add_phase_args (BE_PHASES phase, char *flag)
{
    if (phase_argc[phase] == 0) {
	phase_max_argc[phase] = 4;
	phase_argv[phase] = (STRING *)
	    malloc (phase_max_argc[phase] * sizeof(STRING *));
    } else if (phase_argc[phase] >= phase_max_argc[phase]) {
	phase_max_argc[phase] *= 2;
	phase_argv[phase] = (STRING *)
	    realloc (phase_argv[phase],
		     phase_max_argc[phase] * sizeof(STRING *));
    }
    
    FmtAssert (phase_argv[phase], ("No more memory."));
    phase_argv[phase][(phase_argc[phase])++] = flag;
    
} /* add_phase_args */


void
Get_Phase_Args (BE_PHASES phase, INT *argc, char ***argv)
{
    *argc = phase_argc[phase];
    *argv = phase_argv[phase];
} /* Get_Phase_Args */


/*
 * Recognize phase-specific option groups and put them aside for later
 * processing.
 */
static BOOL
Handle_Phase_Specific_Options (char *flag)
{
    register PHASE_SPECIFIC_OPTION_GROUP *og;
    BE_PHASES phase = PHASE_COMMON;

    for (og = phase_ogroup_table; og->group_name != NULL; og++)
	/* skip the leading '-' before comparing */
	if (flag[1] == og->group_name[0] &&
	    strncmp (flag+1, og->group_name, og->group_name_length) == 0) {
	    phase = og->phase;
	    break;
	}

    if (phase == PHASE_COMMON || phase >= PHASE_COUNT)
	return FALSE;

    add_phase_args (phase, flag);

    return TRUE;

} /* Handle_Phase_Specific_Options */

#ifndef MAX_LINE_LEN_UPC_CONFIG
#define MAX_LINE_LEN_UPC_CONFIG 1024
#endif

static void Process_UPC_Config_File(const char* name) {

  FILE* config_file = fopen(name, "r");
  if (config_file == NULL) {
    FmtAssert(false, ("CANNOT OPEN CONFIGURATION FILE: %s\n", name));
  }
  char line[MAX_LINE_LEN_UPC_CONFIG];
  char buf[MAX_LINE_LEN_UPC_CONFIG];
  int val;
  char * param = (char *) buf;
  while (fgets(line, MAX_LINE_LEN_UPC_CONFIG, config_file) != NULL) {

    if (strspn(line," \t\n") == strlen(line) || // skip blank lines
	line[strspn(line," \t")] == '#') 
      continue; // and comments
    if (strstr(line,"GASNetConfig") == line ||
	strstr(line,"UPCRConfig") == line) 
      continue; // ignore these
    if (strstr(line, "use_type_interface") == line) {
      if (sscanf(line, "%s\t%d", param, &val) != 2) {
	DevWarn("Malformed line in upcc-size config file: %s\n", line);
	break;
      } 
      if (val == 1) {
	Use_Type_Access = TRUE;
      }
      break;
    }
  }
      
}


/* ====================================================================
 *
 * Process_Command_Line
 *
 * Process the command line arguments.	Evaluate all flags and set up
 * global options. 
 *
 * ==================================================================== */

void
Process_Command_Line (INT argc, char **argv)
{
    INT16 i,j;
    char *cp;
    BOOL Echo_Flag = FALSE;
    INT Src_Count = 0;
    char *myname;
    BOOL opt_set = FALSE;
    BOOL dashdash_flag = FALSE;
    
    /* Check the command line flags: */
    for (i = 1; i < argc; i++) {
	if ( argv[i] != NULL && (strcmp(argv[i],"--")==0)) {
	  dashdash_flag = TRUE;
	  continue;
	}
	if ( !dashdash_flag && argv[i] != NULL && *(argv[i]) == '-' ) {
	    cp = argv[i]+1;	    /* Pointer to next flag character */

	      if (strcmp(cp, "run-w2c")==0) {
		Run_w2c = TRUE;
		continue;
	      }

	      if (strcmp(cp, "run-thrille")==0) {
		Run_thrille = TRUE;
		continue;
	      }

	      
	    if (Handle_Phase_Specific_Options (argv[i]))
		continue;
      
	    /* process as command-line option group */
	    if (Process_Command_Line_Group (cp, Common_Option_Groups))
		continue;

	    switch ( *cp++ ) {
              
	    case 'c':
		if (strcmp (cp, "mds") == 0 && Run_ipl) {
		    while (i < argc) {
			add_phase_args (PHASE_IPL, argv[i]);
			i++;
		    }
		} else
		    ErrMsg (EC_Unknown_Flag, *(cp-1), argv[i]);
		break;
	    case 'd':
		if (strcmp (cp, "sm_clone") == 0 )
                    Run_Dsm_Cloner = TRUE;
		else if (strcmp (cp, "sm") == 0 )
                    Run_Dsm_Common_Check = TRUE;
		else if (strcmp (cp, "sm_check") == 0 )
                    Run_Dsm_Check = TRUE;
		else if (strcmp (cp, "sm_recompile") == 0 )
                    Dsm_Recompile = TRUE;
		else if(strcmp(cp,"s2s") == 0) {
			debug_requested = 1;
                } else if(strcmp(cp, "o-msg-vect") == 0) {
		    run_msg_vect = 1;
                    Run_lno = 1;
                    if(!Run_w2c) {
	               opt_set = TRUE;
	               Opt_Level = 3;
                    }      
		} else if(strcmp(cp, "o-ptr-locality") == 0) {
		  run_ptr_locality = 1;
		} else if (strcmp(cp, "o-forall-opt") == 0) {
		    run_forall_opt = 1;
		} else if (strcmp(cp, "o-valget") == 0) {
		    Use_Valget = TRUE;
		} else if (strcmp(cp, "o-autonb") == 0) {
		  run_auto_nb = 1; 
		}  else if (!strcmp(cp, "o-split-phase")) {
		  run_split_phase = 1;
		  run_ptr_coalescing = 1;
		} else if (!strcmp(cp, "o-pre-add")) {
		  run_pre_add = 1;
		} else if (!strcmp(cp, "o-ptr-coalesce")) {
		  run_ptr_coalescing = 1;
		}   else 
		    ErrMsg (EC_Unknown_Flag, *(cp-1), argv[i]);
		break;

	    case '#':		    /* Echo command lines: */
	    case 'v':
		if ( Echo_Flag )
		    break;	    /* Don't do this twice */
		Echo_Flag = TRUE;
		for ( j=0; j<=argc; j++ )
		    if ( argv[j] ) fprintf (stderr," %s",argv[j]);
		fprintf   (stderr, "\n");
		break;

	    case 'f':		    /* file options */
		if (*cp == 0)
		    ErrMsg (EC_File_Name, '?', argv[i]);
		else if (strncmp(cp, "config-", 7) == 0) {
		  char * name = cp + 7;
		  Process_UPC_Config_File(name);
		}
		else if (*(cp+1) != ',' && *(cp+1) != ':')
		    ErrMsg (EC_File_Name, *cp, argv[i]);
		else {
		    switch (*cp) {
		    case 'f':
			Feedback_File_Name = cp + 2;
			break;
		    case 'i':
			Set_Instrumentation_File_Name(cp + 2);
			break;
		    case 'o':
			Obj_File_Name = cp + 2;
			/* fall through */
		    case 's':	    /* CG-specific */
		    case 'a':
			add_phase_args (PHASE_CG, argv[i]);
			break;
			
		    case 'l':	    /* listing file */
			List_Enabled = TRUE;
			Lst_File_Name = cp + 2;
			break;
			
		    case 'q':	    /* Transformation log file: */
			Tlog_File_Name = cp + 2;
			break;

		    case 't':	    /* Error file: */
			Trc_File_Name = cp + 2;
			break;

		    case 'B':	    /* WHIRL file */
			Irb_File_Name = cp + 2;
			break;

		    case 'G':	    /* WHIRL file */
			Global_File_Name = cp + 2;
			break;

		    default:
			ErrMsg ( EC_File_Flag, *cp, argv[i] );
			break;
		    }
		}
		break;
		
	    case 'g':		    /* Debug level: */
		Debug_Level = Get_Numeric_Flag (&cp, 0, MAX_DEBUG_LEVEL, 2,
						argv[i]);
		/* for the time being whirl2c gets confused when Debug_Level is set */
		/* until we fix it just record that debugging was requested so
		   whirl2c can still mangle_names */
		if(Debug_Level == 2) {
		  debug_requested = 1;
		  Debug_Level = 0;
		}
		if (Debug_Level > 0 && !opt_set)
		    Opt_Level = 0;
		break;
              
	    case 'G':		    /* max size of elements in .sdata/.sbss */
		Max_Sdata_Elt_Size =
		    Get_Numeric_Flag (&cp, 0, MAX_SDATA_ELT_SIZE,
				      DEF_SDATA_ELT_SIZE, argv[i] ); 
		break;
              
	    case 'm':		    /* Message reporting: */
		if (!strcmp( cp, "pio" )) {
		  mp_io = TRUE;
		  cp += 3;
		  break;
		} else if (!strcmp(cp, "plist")) {
		  Run_w2fc_early = TRUE; 
	   	  cp += 5; 
		  break;
                } else if (!strcmp(cp, "emctr")) {
                  Run_MemCtr = TRUE;
                  cp += 5;
                  break;
                }
		j = Get_Numeric_Flag (&cp, 0, MAX_MSG_LEVEL, MAX_MSG_LEVEL,
				      argv[i] ); 
		switch (j) {
		case 0: Min_Error_Severity = ES_ERROR;    break;
		case 1: Min_Error_Severity = ES_WARNING;  break;
		case 2: Min_Error_Severity = ES_ADVISORY; break;
		}
		break;

	    case 'n':
		if (!strcmp( cp, "o_exceptions" )) {
		  CXX_Exceptions_On = FALSE;
		} else if (!strcmp(cp, "o-split-phase")) {
		  run_split_phase = 0;
		  run_ptr_coalescing = 0;
		} else if (!strcmp(cp, "o-forall-opt")) {
		  run_forall_opt = 0;
		} else if (!strcmp(cp, "o-msg-vect")) {
		  run_msg_vect = 0;
		} else if (!strcmp(cp, "o-pre-add")) {
		  run_pre_add = 0;
		} else if (!strcmp(cp, "o-ptr-coalesce")) {
		  run_ptr_coalescing = 0;
		} else if(!strcmp(cp,"o-ptr-locality")) {
		  run_ptr_locality = 0;
		} else if (!strcmp(cp, "o-autonb")) {
		  run_auto_nb = 0;
		} else if (!strcmp(cp, "o-valget")) {
		  Use_Valget = FALSE;
		} else {
		  ErrMsg ( EC_Unknown_Flag, *(cp-1), argv[i] );
		}
		break;

	    case 'e':
		if (!strcmp( cp, "xceptions" )) {
		  CXX_Exceptions_On = TRUE;
		}
		else {
		  ErrMsg ( EC_Unknown_Flag, *(cp-1), argv[i] );
		}
		break;
	
 	    case 'O':		    /* Optimization level: */
	      Opt_Level = Get_Numeric_Flag (&cp, 0, MAX_OPT_LEVEL,
					      DEF_O_LEVEL, argv[i] ); 
	      opt_set = TRUE;
	      break;
	   
	    case 's':
		if (strcmp (cp, "how") == 0) {
		    Show_Progress = TRUE;
		    break;
		} else if( strcmp(cp, "td=upc") == 0) {
		  Compile_Upc = TRUE;
		  break;
		}
		/* else fall through */
				    /* CG-specific flags */
	    case 'a':		    /* -align(8,16,32,64) */
	    case 'S':		    /* -S: Produce assembly file: */
		add_phase_args (PHASE_CG, argv[i]);
		break;
              
	    case 't':		    /* Trace specification: */
		/* handle the -tfprev10 option to fix tfp hardware bugs. */
                if ( strncmp ( cp-1, "tfprev10", 8 ) == 0 ) {
		  add_phase_args (PHASE_CG, argv[i]);
		  break;
		} if(strcmp (cp - 1, "trace-msg-vect") == 0) {
                   trace_msg_vect = 1;
                 }else {
		  Process_Trace_Option ( cp-2 );
		}

		break;

	    case 'w':		    /* Suppress warnings */
		if (strncmp(cp, "off", 3) == 0) {
		    Rag_Handle_Woff_Args(cp + 3);
		}
		else {
		    Min_Error_Severity = ES_ERROR;
		}
		break;

	    case 'p': 
		if (strncmp(cp, "fa", 2) == 0) { 
		  Run_autopar = TRUE; 
		  cp += 2;
		} else if (strcmp(cp, "threads") == 0) {
		  use_TLD = TRUE;
		} else {
		  ErrMsg (EC_Unknown_Flag, *(cp-1), argv[i]); 
		}
                break;
           
	    case 'l':
	      if(strncmp(cp,"ang=upc",7) == 0)
		compiling_upc_flag = 1;
	      break;
	      /*
#ifdef COMPILE_UPC
	    case 'u': 
	      if (strncmp(cp, "pcopt=", 6) == 0) {
		  cp += 6;
		  int tmp = Get_Numeric_Flag (&cp, 0, 0xffffffff,
					      0, argv[i] );
		  fprintf(stderr, "%d %d\n", tmp, (upc_opt_mask ^ tmp) & UPC_OPT_SPLIT);
		  upc_opt_mask ^= Get_Numeric_Flag (&cp, 0, 0xffffffff,
						    0, argv[i] );
		  
		  break;
	      }
	      //fall through to error case
#endif
	      */
	    default:		    /* What's this? */
	      ErrMsg ( EC_Unknown_Flag, *(cp-1), argv[i] );
	      break;
	    }
	} else if (argv[i] != NULL) {
	    dashdash_flag = FALSE;
	    Src_Count++;
	    Src_File_Name = argv[i];
	} 
    }

    if (Dsm_Recompile)
      Run_Dsm_Common_Check = FALSE;

    if ( Tracing_Enabled ) {
      Initialize_Timing (TRUE);
    }

    myname = Last_Pathname_Component (argv[0]);

    if (myname[0] == 'i' && strcmp (myname, "ipl") == 0) {
	Run_ipl = TRUE;
	/* We don't support olimit region for ipl (yet).  So if we overflow
	   the olimit, we don't want to run preopt, but still run ipl. */
	Olimit_opt = FALSE;
    } else {
	Run_ipl = FALSE;

	switch (myname[0]) {
	case 'l':
	    if (strcmp (myname, "lnopt") == 0)
		Run_lno = TRUE;
	    break;
	case 'w':
	    if (strcmp (myname, "wopt") == 0)
		Run_wopt = TRUE;
	    else if (strcmp (myname, "whirl2c") == 0)
		Run_w2c = TRUE;
	    else if (strcmp (myname, "whirl2f") == 0)
		Run_w2f = TRUE;
	    break;
	case 'p':
	    if (strcmp (myname, "preopt") == 0)
		Run_preopt = TRUE;
	    else if (strcmp (myname, "purple") == 0)
		Run_purple = TRUE;
	    break;
	case 'c':
	    if (strcmp (myname, "cg") == 0)
		Run_cg = TRUE;
	    break;
	}
    }

    if (Src_Count == 0) {
	ErrMsg ( EC_No_Sources );
	exit (RC_USER_ERROR);
    }
    
    if (Run_lno && Run_preopt)
	Run_preopt = FALSE;

    /* -tt1:1 requests all of the performance trace flags: */
    if ( Get_Trace ( TP_PTRACE1, TP_PTRACE1_ALL ) ) {
      Set_Trace ( TP_PTRACE1, 0xffffffff );
      Set_Trace ( TP_PTRACE2, 0xffffffff );
    }
    /* and any individual performance tracing enables tlogs */
    if ( Get_Trace ( TP_PTRACE1, 0xffffffff ) ||
	 Get_Trace ( TP_PTRACE2, 0xffffffff ) )
    {
      Tlog_Enabled=TRUE;
    }

    /* -ti64 requests a listing of all the -tt flags: */
    if ( Get_Trace ( TKIND_INFO, TINFO_TFLAGS ) ) {
      List_Phase_Numbers ();
    }

    //WEI: disable optimizer for now
    //Run_lno = Run_wopt = Run_ipl = Run_preopt = Run_autopar = FALSE;

} /* Process_Command_Line */

/* ====================================================================
 *
 * Prepare_Source
 *
 * Process the source argument and associated files, except the listing
 * file, which may not be opened until after Configure_Source has
 * determined whether it is required.
 *
 * ====================================================================
 */
void
Prepare_Source (void)
{
    char *fname = Last_Pathname_Component ( Src_File_Name );
    
    if (Err_File_Name && Err_File_Name[0] == 0)
	Err_File_Name = New_Extension (fname, ERR_FILE_EXTENSION);
    Set_Error_File ( Err_File_Name );

    /* Transformation log file */
    if ( Tlog_Enabled ) {
        if ( Tlog_File_Name == NULL ) {
	    /* Replace source file extension to get trace file: */
	    Tlog_File_Name = New_Extension (fname, TLOG_FILE_EXTENSION);
	}
        if ( (Tlog_File = fopen ( Tlog_File_Name, "w" ) ) == NULL ) {
	  ErrMsg ( EC_Tlog_Open, Tlog_File_Name, errno );
	  Tlog_File_Name = NULL;
	  Tlog_File = stdout;
        }
    }

    /* Trace file */
    if ( Trc_File_Name == NULL ) {
	if ( Tracing_Enabled ) {
	    /* Replace source file extension to get trace file: */
	    Trc_File_Name = New_Extension (fname, TRC_FILE_EXTENSION);
	}
    } else if ( *Trc_File_Name == '-' ) {
	/* Leave trace file on stdout: */
	Trc_File_Name = NULL;
    }
    Set_Trace_File ( Trc_File_Name );
    if ( Get_Trace (TKIND_INFO, TINFO_TIME) )
	Tim_File = TFile;
    else if ( Get_Trace (TKIND_INFO, TINFO_CTIME) )
	Tim_File = TFile;

    if (Irb_File_Name == NULL)
	Irb_File_Name = New_Extension (Src_File_Name, IRB_FILE_EXTENSION);

} /* Prepare_Source */

/* ====================================================================
 *
 * Prepare_Listing_File
 *
 * Determine the listing file name and open it.
 *
 * ====================================================================
 */
void
Prepare_Listing_File (void)
{
  char *fname = Last_Pathname_Component ( Src_File_Name );
  
  if ( List_Enabled ) {
    if (Lst_File_Name == NULL) {
      /* Replace source file extension to get listing file: */
      Lst_File_Name = New_Extension (fname, LST_FILE_EXTENSION);
    } else if ( *Lst_File_Name == '-' ) {
      /* Send listing file to stdout: */
      Lst_File_Name = NULL;
    }
    if (Lst_File_Name) {
      if ( (Lst_File = fopen ( Lst_File_Name, "w" ) ) == NULL ) {
	ErrMsg ( EC_Lst_Open, Lst_File_Name, errno );
	Lst_File_Name = NULL;
	Lst_File = stdout;
      }
    } else Lst_File = stdout;
  }
} /* Prepare_Listing_File */

/* perform initialization of the lowerer for lowering High WHIRL */
void
Lowering_Initialize (void)
{
    Create_Slink_Symbol();

   /*
    *  lowering specific initialization
    */
    Lower_Init();
}
