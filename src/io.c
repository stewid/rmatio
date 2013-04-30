/** @file io.c
 * MAT File I/O Utility Functions
 */
/*
 * Copyright (C) 2005-2011   Christopher C. Hulbert
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Changes in the R package rmatio:
 * 
 * - The io routines have been adopted to use R printing and error routines.
 *   See the R manual Writing R Extensions 
 */

#include <R.h>
#include "matio_private.h"

#if !defined(HAVE_VA_COPY) && defined(HAVE___VA_COPY)
#    define va_copy(d,s) __va_copy(d,s)
#elif !defined(HAVE_VA_COPY)
#    define va_copy(d,s) memcpy(&(d),&(s),sizeof(va_list))
#endif
#ifndef HAVE_VSNPRINTF
#    define vsnprintf mat_vsnprintf
#    ifdef  __cplusplus
         extern "C" int vsnprintf(char *,size_t,const char *,va_list);
#    else
         extern int vsnprintf(char *,size_t,const char *,va_list);
#    endif
#endif
#ifndef HAVE_SNPRINTF
#    define snprintf mat_snprintf
#    ifdef  __cplusplus
         extern "C" int snprintf(char *str,size_t size,const char *format,...);
#    else
         extern int snprintf(char *str,size_t size,const char *format,...);
#    endif
#endif
#ifndef HAVE_VASPRINTF
#    define vasprintf mat_vasprintf
#endif
#ifndef HAVE_ASPRINTF
#    define asprintf mat_asprintf
#endif

/** @cond 0 */
#define LOG_LEVEL_ERROR    1
#define LOG_LEVEL_CRITICAL 1 << 1
#define LOG_LEVEL_WARNING  1 << 2
#define LOG_LEVEL_MESSAGE  1 << 3
#define LOG_LEVEL_DEBUG    1 << 4
/** @endcond */

static void (*logfunc)(int log_level, char *message ) = NULL;
static const char *progname = NULL;

/** @brief Allocates and prints to a new string
 *
 * @ingroup mat_util
 * @param format format string
 * @param ap variable argument list
 * @return Newly allocated string with format printed to it
 */
char *
strdup_vprintf(const char* format, va_list ap)
{
  va_list ap2;
  int size;
  char* buffer;

  va_copy(ap2, ap);
  size = vsnprintf(NULL, 0, format, ap2)+1;
  va_end(ap2);

  buffer = malloc(size+1);
  if ( !buffer )
      return NULL;

  vsnprintf(buffer, size, format, ap);
  return buffer;
}

/** @brief Allocates and prints to a new string using printf format
 *
 * @ingroup mat_util
 * @param format format string
 * @return Pointer to resulting string, or NULL if there was an error
 */
char *
strdup_printf(const char* format, ...)
{
  char* buffer;
  va_list ap;
  va_start(ap, format);
  buffer = strdup_vprintf(format, ap);
  va_end(ap);
  return buffer;
}

static void
matio_error_func( int log_level, char *message )
{
  if ( progname ) {
    if ( log_level & LOG_LEVEL_CRITICAL) {
      REprintf("-E- %s: %s\n", progname, message);
    } else if ( log_level & LOG_LEVEL_ERROR ) {
      error("-E- %s: %s\n", progname, message);
    } else if ( log_level & LOG_LEVEL_WARNING ) {
      REprintf("-W- %s: %s\n", progname, message);
    } else if ( log_level & LOG_LEVEL_DEBUG ) {
      REprintf("-D- %s: %s\n", progname, message);
    } else if ( log_level & LOG_LEVEL_MESSAGE ) {
      Rprintf("%s\n", message);
    }
  } else {
    if ( log_level & LOG_LEVEL_CRITICAL) {
      REprintf("-E- : %s\n", message);
    } else if ( log_level & LOG_LEVEL_ERROR ) {
      error("-E- : %s\n", message);
    } else if ( log_level & LOG_LEVEL_WARNING ) {
      REprintf("-W- : %s\n", message);
    } else if ( log_level & LOG_LEVEL_DEBUG ) {
      REprintf("-D- : %s\n", message);
    } else if ( log_level & LOG_LEVEL_MESSAGE ) {
      Rprintf("%s\n", message);
    }
  }
}

static void
mat_log(int loglevel, const char *format, va_list ap)
{
    char* buffer;

    if ( !logfunc ) return;
    buffer = strdup_vprintf(format, ap);
    (*logfunc)(loglevel,buffer);
    free(buffer);
    return;
}

/** @var debug
 *  @brief holds the verbose level set in @ref SetVerbose
 *  This variable is used to determine if information should be printed to
 *  the screen
 *  @ingroup mat_util
 */
static int debug = 0;

/** @var verbose
 *  @brief holds the verbose level set in @ref SetVerbose
 *  This variable is used to determine if information should be printed to
 *  the screen
 *  @ingroup mat_util
 */
static int verbose = 0;

/** @var silent
 *  @brief holds the silent level set in @ref SetVerbose
 *  If set, all output which is not an error is not displayed regardless
 *  of verbose level
 *  @ingroup mat_util
 */
static int silent = 0;

/** @brief Sets verbose parameters
 *
 *  Sets the verbose level and silent level.  These values are used by
 *  programs to determine what information should be printed to the screen
 *  @ingroup mat_util
 *  @param verb sets logging verbosity level
 *  @param s sets logging silent level
 */
int
Mat_SetVerbose( int verb, int s )
{
    verbose = verb;
    silent  = s;

    return 0;
}

/** @brief Sets verbose parameters
 *
 *  Sets the verbose level and silent level.  These values are used by
 *  programs to determine what information should be printed to the screen
 *  @ingroup mat_util
 *  @param verb sets logging verbosity level
 *  @param s sets logging silent level
 */
int
Mat_SetDebug( int d )
{
    debug = d;
    return 0;
}

/** @brief Log a message unless silent
 *
 * Logs the message unless the silent option is set (See @ref SetVerbose).
 * To log a message based on the verbose level, use @ref Mat_VerbMessage
 * @ingroup mat_util
 * @param format message format
 */
int Mat_Message( const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( !logfunc ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_MESSAGE, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Log a message based on verbose level
 *
 *  If @e level is less than or equal to the set verbose level, the message
 *  is printed.  If the level is higher than the set verbose level nothing
 *  is displayed.
 *  @ingroup mat_util
 *  @param level verbose level
 *  @param format message format
 */
int Mat_DebugMessage( int level, const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( level > debug ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_DEBUG, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Log a message based on verbose level
 *
 *  If @e level is less than or equal to the set verbose level, the message
 *  is printed.  If the level is higher than the set verbose level nothing
 *  is displayed.
 *  @ingroup mat_util
 *  @param level verbose level
 *  @param format message format
 */
int Mat_VerbMessage( int level, const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( level > verbose ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_MESSAGE, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Logs a Critical message and returns to the user
 *
 * Logs a Critical message and returns to the user.  If the program should
 * stop running, use @ref Mat_Error
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void Mat_Critical( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log(LOG_LEVEL_CRITICAL, format, ap );
    va_end(ap);
}

/** @brief Logs a Critical message and aborts the program
 *
 * Logs an Error message and aborts
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void Mat_Error( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log( LOG_LEVEL_ERROR, format, ap );
    va_end(ap);
}

/** @brief Prints a helpstring to stdout and exits with status 1
 *
 * Prints the array of strings to stdout and exits with status 1.  The array
 * of strings should have NULL as its last element
 * @code
 * char *helpstr[] = {"My Help string line1","My help string line 2",NULL};
 * Mat_Help(helpstr);
 * @endcode
 * @ingroup mat_util
 * @param helpstr array of strings with NULL as its last element
 */
void Mat_Help( const char *helpstr[] )
{
    /* int i; */
    /* for (i = 0; helpstr[i] != NULL; i++) */
    /*     Rprintf("%s\n",helpstr[i]); */
    /* exit(EXIT_SUCCESS); */
}

/** @brief Closes the logging system
 *
 * @ingroup mat_util
 * @retval 1
 */
int
Mat_LogClose( void )
{
    logfunc = NULL;
    return 1;
}

/** @brief Intializes the logging system
 *
 * @ingroup mat_util
 * @param prog_name Name of the program initializing the logging functions
 * @return 0 on success
 */
int
Mat_LogInit( const char *prog_name )
{
    logfunc = &matio_error_func;

    verbose = 0;
    silent  = 0;

    return 0;
}

/** @brief Intializes the logging system
 *
 * @ingroup mat_util
 * @param prog_name Name of the program initializing the logging functions
 * @param log_func pointer to the function to do the logging
 * @return 0 on success
 */
int
Mat_LogInitFunc(const char *prog_name,
    void (*log_func)(int log_level,char *message))
{
    logfunc = log_func;
    progname = prog_name;

    verbose = 0;
    silent  = 0;
    return 0;
}

/** @brief Prints a warning message to stdout
 *
 * Logs a warning message then returns
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void
Mat_Warning( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log(LOG_LEVEL_WARNING, format, ap );
    va_end(ap);
}

/** @brief Calculate the size of MAT data types
 *
 * @ingroup mat_util
 * @param data_type Data type enumeration
 * @return size of the data type in bytes
 */
size_t
Mat_SizeOf(enum matio_types data_type)
{
    switch (data_type) {
        case MAT_T_DOUBLE:
            return sizeof(double);
        case MAT_T_SINGLE:
            return sizeof(float);
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            return sizeof(mat_int64_t);
#endif
#ifdef HAVE_MAT_INT64_T
        case MAT_T_UINT64:
            return sizeof(mat_uint64_t);
#endif
        case MAT_T_INT32:
            return sizeof(mat_int32_t);
        case MAT_T_UINT32:
            return sizeof(mat_uint32_t);
        case MAT_T_INT16:
            return sizeof(mat_int16_t);
        case MAT_T_UINT16:
            return sizeof(mat_uint16_t);
        case MAT_T_INT8:
            return sizeof(mat_int8_t);
        case MAT_T_UINT8:
            return sizeof(mat_uint8_t);
        default:
            return 0;
    }
}

void
Mat_PrintNumber(enum matio_types type, void *data)
{
    switch ( type ) {
        case MAT_T_DOUBLE:
            Rprintf("%g",*(double*)data);
            break;
        case MAT_T_SINGLE:
            Rprintf("%g",*(float*)data);
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            Rprintf("%lld",*(mat_int64_t*)data);
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            Rprintf("%llu",*(mat_uint64_t*)data);
            break;
#endif
        case MAT_T_INT32:
            Rprintf("%d",*(mat_int32_t*)data);
            break;
        case MAT_T_UINT32:
            Rprintf("%u",*(mat_uint32_t*)data);
            break;
        case MAT_T_INT16:
            Rprintf("%hd",*(mat_int16_t*)data);
            break;
        case MAT_T_UINT16:
            Rprintf("%hu",*(mat_uint16_t*)data);
            break;
        case MAT_T_INT8:
            Rprintf("%hhd",*(mat_int8_t*)data);
            break;
        case MAT_T_UINT8:
            Rprintf("%hhu",*(mat_uint8_t*)data);
            break;
        default:
            break;
    }
}

/** @brief Prints the variable information
 *
 * Prints to stdout the values of the @ref matvar_t structure
 * @ingroup MAT
 * @param matvar Pointer to the matvar_t structure
 * @param printdata set to 1 if the Variables data should be printed, else 0
 */
void
Mat_VarPrint( matvar_t *matvar, int printdata )
{
    size_t nmemb;
    int i, j;
    const char *class_type_desc[16] = {"Undefined","Cell Array","Structure",
       "Object","Character Array","Sparse Array","Double Precision Array",
       "Single Precision Array", "8-bit, signed integer array",
       "8-bit, unsigned integer array","16-bit, signed integer array",
       "16-bit, unsigned integer array","32-bit, signed integer array",
       "32-bit, unsigned integer array","64-bit, signed integer array",
       "64-bit, unsigned integer array"};
    const char *data_type_desc[23] = {"Unknown","8-bit, signed integer",
       "8-bit, unsigned integer","16-bit, signed integer",
       "16-bit, unsigned integer","32-bit, signed integer",
       "32-bit, unsigned integer","IEEE 754 single-precision","RESERVED",
       "IEEE 754 double-precision","RESERVED","RESERVED",
       "64-bit, signed integer","64-bit, unsigned integer", "Matlab Array",
       "Compressed Data","Unicode UTF-8 Encoded Character Data",
       "Unicode UTF-16 Encoded Character Data",
       "Unicode UTF-32 Encoded Character Data","","String","Cell Array",
       "Structure"};

    if ( matvar == NULL )
        return;
    if ( matvar->name )
        Rprintf("      Name: %s\n", matvar->name);
    Rprintf("      Rank: %d\n", matvar->rank);
    if ( matvar->rank == 0 )
        return;
    Rprintf("Dimensions: %zu",matvar->dims[0]);
    nmemb = matvar->dims[0];
    for ( i = 1; i < matvar->rank; i++ ) {
        Rprintf(" x %zu",matvar->dims[i]);
        nmemb *= matvar->dims[i];
    }
    Rprintf("\n");
    Rprintf("Class Type: %s",class_type_desc[matvar->class_type]);
    if ( matvar->isComplex )
        Rprintf(" (complex)");
    else if ( matvar->isLogical )
        Rprintf(" (logical)");
    Rprintf("\n");
    if ( matvar->data_type )
        Rprintf(" Data Type: %s\n", data_type_desc[matvar->data_type]);

    if ( MAT_C_STRUCT == matvar->class_type ) {
        matvar_t **fields = (matvar_t **)matvar->data;
        int nfields = matvar->internal->num_fields;
        if ( nmemb*nfields > 0 ) {
            Rprintf("Fields[%zu] {\n", nfields*nmemb);
            for ( i = 0; i < nfields*nmemb; i++ ) {
                if ( NULL == fields[i] ) {
                    Rprintf("      Name: %s\n      Rank: %d\n",
                           matvar->internal->fieldnames[i%nfields],0);
                } else {
                    Mat_VarPrint(fields[i],printdata);
                }
            }
            Rprintf("}\n");
        } else {
            Rprintf("Fields[%d] {\n", nfields);
            for ( i = 0; i < nfields; i++ )
                Rprintf("      Name: %s\n      Rank: %d\n",
                       matvar->internal->fieldnames[i],0);
            Rprintf("}\n");
        }
        return;
    } else if ( matvar->data == NULL || matvar->data_size < 1 ) {
        return;
    } else if ( MAT_C_CELL == matvar->class_type ) {
        matvar_t **cells = (matvar_t **)matvar->data;
        int ncells = matvar->nbytes / matvar->data_size;
        Rprintf("{\n");
        for ( i = 0; i < ncells; i++ )
            Mat_VarPrint(cells[i],printdata);
        Rprintf("}\n");
        return;
    } else if ( !printdata ) {
        return;
    }

    Rprintf("{\n");

    if ( matvar->rank > 2 ) {
        Rprintf("I can't print more than 2 dimensions\n");
    } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
        Rprintf("I won't print more than 15 elements in a vector\n");
    } else if ( matvar->rank==2 ) {
        switch( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
#ifdef HAVE_MAT_INT64_T
            case MAT_C_INT64:
#endif
#ifdef HAVE_MAT_UINT64_T
            case MAT_C_UINT64:
#endif
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
            {
                size_t stride = Mat_SizeOf(matvar->data_type);
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = matvar->data;
                    char *rp = complex_data->Re;
                    char *ip = complex_data->Im;
                   for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ ) {
                            size_t idx = matvar->dims[0]*j+i;
                            Mat_PrintNumber(matvar->data_type,rp+idx*stride);
                            Rprintf(" + ");
                            Mat_PrintNumber(matvar->data_type,ip+idx*stride);
                            Rprintf("i ");
                        }
                        if ( j < matvar->dims[1] )
                            Rprintf("...");
                        Rprintf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        Rprintf(".\n.\n.\n");
               } else {
                   char *data = matvar->data;
                   for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ ) {
                            size_t idx = matvar->dims[0]*j+i;
                            Mat_PrintNumber(matvar->data_type,
                                            data+idx*stride);
                            Rprintf(" ");
                        }
                        if ( j < matvar->dims[1] )
                            Rprintf("...");
                        Rprintf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        Rprintf(".\n.\n.\n");
                }
                break;
            }
            case MAT_C_CHAR:
            {
                char *data = matvar->data;
                if ( !printdata )
                    break;
                for ( i = 0; i < matvar->dims[0]; i++ ) {
                    j = 0;
                    for ( j = 0; j < matvar->dims[1]; j++ )
                        Rprintf("%c",data[j*matvar->dims[0]+i]);
                    Rprintf("\n");
                }
                break;
            }
            case MAT_C_SPARSE:
            {
                mat_sparse_t *sparse;
                size_t stride = Mat_SizeOf(matvar->data_type);
#if !defined(EXTENDED_SPARSE)
                if ( MAT_T_DOUBLE != matvar->data_type )
                    break;
#endif
                sparse = matvar->data;
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = sparse->data;
                    char *re,*im;
                    re = complex_data->Re;
                    im = complex_data->Im;
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for (j = sparse->jc[i];
                             j<sparse->jc[i+1] && j<sparse->ndata;j++ ) {
                            Rprintf("    (%d,%d)  ",sparse->ir[j]+1,i+1);
                            Mat_PrintNumber(matvar->data_type,re+j*stride);
                            Rprintf(" + ");
                            Mat_PrintNumber(matvar->data_type,im+j*stride);
                            Rprintf("i\n");
                        }
                    }
                } else {
                    char *data;
                    data = sparse->data;
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for (j = sparse->jc[i];
                             j<sparse->jc[i+1] && j<sparse->ndata;j++ ){
                            Rprintf("    (%d,%d)  ",sparse->ir[j]+1,i+1);
                            Mat_PrintNumber(matvar->data_type,data+j*stride);
                            Rprintf("\n");
                        }
                    }
                }
                break;
            } /* case MAT_C_SPARSE: */
            default:
                break;
        } /* switch( matvar->class_type ) */
    }

    Rprintf("}\n");

    return;
}
