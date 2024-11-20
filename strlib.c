/*
 *----------------------------------------------------------------------------------------
 *
 * All software provided below is unsupported and provided as-is, without warranty 
 * of any kind.
 *
 * To the extent possible under law, Red Hat, Inc. has dedicated all copyright
 * to this software to the public domain worldwide, pursuant to the CC0 Public
 * Domain Dedication. This software is distributed without any warranty.
 * See <http://creativecommons.org/publicdomain/zero/1.0/>.
 *
 *----------------------------------------------------------------------------------------
 */

/*
 * Maintainer: bubrown@redhat.com
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define VERSION "01.04-0028-04072021"		/* Sun Jul  4 12:36:03 2021 bubrown */


static int nodelimit(char c, char *list);

int upcase(char *string)
{
char *ptr = string;

    while(*ptr != 0)
    {
	if ((*ptr >= 'a') && (*ptr <='z'))
        {
	    *ptr = toupper(*ptr);
	}
	ptr++;
    }
    return(0);
}

int strskip(char **pptr, char *list)
{
char *ptr;
char *chk;

    ptr = *pptr;
    chk = list;
    while (1)
    {
        while (*chk != 0)
        {
            if (*chk == *ptr)
            {
                ptr++;
                chk = list;
                break;
            }
            chk++;
        }
        if ((*chk == 0) || (*ptr == 0))
            break;
    }
    *pptr = ptr;
    return(1);
}

int strtrim( char *bgn )
{
char *ptr;
char *chk;
int   len;

    if ((len = strlen(bgn)) == 0) return(0);
    ptr = &bgn[len-1];
    while (1)
    {
        if ((*ptr == ' ') || (*ptr == '\t') || (*ptr == '\n') || (*ptr == 0x0A) || (*ptr == 0x0D))
        {
            *ptr = 0;
	    --len;
            if (ptr == bgn) return(0);
            --ptr;
            continue;
        }
        break;
    }
    return(len);
}

int get_token(char **pptr, char *token)
{
char *bgn = token;
char *ptr = *pptr;

    *token = 0;
    while ((*ptr != 0) && (*ptr != ' ') && (*ptr != '\t'))
        *token++ = *ptr++;
    *token = 0;
    *pptr  = ptr;
    return(strlen(bgn));
}
int get_tokend(char **pptr, char *token, char *dlimits)
{
char *bgn = token;
char *ptr = *pptr;

    *token = 0;
    while ((*ptr != 0) && (nodelimit(*ptr, dlimits)==0))
        *token++ = *ptr++;
    *token = 0;
    *pptr  = ptr;
    *token = 0;
    *pptr  = ptr;
    return(strlen(bgn));
}
static int nodelimit(char c, char *list)
{
   while (*list != 0)
   {
	if (*list == c) return(1);
	list++;
   }
   return(0);
}
int skip_space(char **pptr)
{
char *ptr = *pptr;
    while ((ptr != NULL) && (*ptr != 0) && ((*ptr == ' ') || (*ptr == '\n') || (*ptr == '\t'))) ptr++;
    *pptr = ptr;
    return(0);
}

