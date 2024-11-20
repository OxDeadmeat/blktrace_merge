#include "typ_blktrace.h"

/* 
 * 0 = nominal/expected secondary IMB type for initial/anchor type 
 * 1 = allowed/nomesssage
 * 2 = allowed/informational message
 * 3 = allowed/warning       message
 * 4 = allowed/error         message
 * 5 = disallowed/fatal exception
 *
 */

static char imb_matrix[5][6] =
{              /* none S0  Sx  00  0x  xx */
  { /* S0  */       -1, 0,  0,  1,  1,  1 },
  { /* Sx  */       -1, 1,  0,  1,  0,  1 },
  { /* 00  */       -1, 3,  3,  0,  0,  1 },
  { /* 0x  */       -1, 3,  3,  1,  0,  1 },
  { /* xx  */       -1, 3,  3,  3,  3,  3 }
};
static int ibm_crosscheck[6] = {0,0,0,0,0,0};
