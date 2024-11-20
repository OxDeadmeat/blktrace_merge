
#define       BGN_FUNCTION trace_printf( 0, __FUNCTION__, __LINE__ )
#define     TRACE_FUNCTION trace_printf( 0, __FUNCTION__, __LINE__ )
#define       END_FUNCTION trace_printf( 1, __FUNCTION__, __LINE__ )
#define _return(_status) return( trace_printf( 1, __FUNCTION__, __LINE__ )?(_status):(_status))
#define _exit(  _status)   exit( trace_printf( 1, __FUNCTION__, __LINE__ )?(_status):(_status))

#define  _assert fflush(stdout); assert
#define __assert printf("DBG[%05d]: %s()\n", __LINE__, __func__ ); fflush(stdout); assert

int trace_printf( int where, const char funcname[], int lineno );

extern int flag_function;
int trace_printf( int where, const char funcname[], int lineno )
{
    if (flag_function != 0)
       printf( "TRC[%05d]: %s: %-20s()\n", lineno, (where==0)?"bgn":"end", funcname );
    return(0);
}

