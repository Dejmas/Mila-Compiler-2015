#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gcc.h"
#include "opts.h"

#include "tm.h"
#include "intl.h"

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
      unsigned int *in_decoded_options_count,
      int *in_added_libraries)
{
  
  fprintf (stderr, "Incoming:");
  for( unsigned i = 1; i < *in_decoded_options_count; i++ )
    fprintf (stderr, " %s", (*in_decoded_options)[i].orig_option_with_args_text);
  fprintf (stderr, "\n");
  
  for( unsigned i = 1; i < *in_decoded_options_count; i++ ) {
    switch( (*in_decoded_options)[i].opt_index ) {
      case OPT_SPECIAL_input_file:
	continue;

      case OPT_nostdlib:
      case OPT_nodefaultlibs:
      case OPT_c:
      case OPT_S:
      case OPT_fsyntax_only:
      case OPT_E:
	/* These options disable linking entirely or linking of the
	    standard libraries.  */
	break;

      case OPT_static:
	break;

      case OPT_l:
	break;

      case OPT_o:
	break;

      case OPT_v:
	break;

      case OPT__version:
	printf ("GNU SampleFE\n");
	exit (0);
	break;

      case OPT__help:
	/* Let gcc.c handle this, as it has a really
	    cool facility for handling --help and --verbose --help.  */
	return;

      default:
	break;
      }
  }
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;	
