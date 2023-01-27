dnl  configure.ac
dnl
dnl  NOTE:  Process this file with GNU autoconf to produce a configure script.
dnl
dnl Copyright (C) 2005-2013   Christopher C. Hulbert
dnl
dnl All rights reserved.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are met:
dnl
dnl    1. Redistributions of source code must retain the above copyright notice,
dnl       this list of conditions and the following disclaimer.
dnl
dnl    2. Redistributions in binary form must reproduce the above copyright
dnl       notice, this list of conditions and the following disclaimer in the
dnl       documentation and/or other materials provided with the distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY
dnl EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
dnl WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
dnl DISCLAIMED. IN NO EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE
dnl LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
dnl CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
dnl SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
dnl INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
dnl CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
dnl ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
dnl POSSIBILITY OF SUCH DAMAGE.

dnl Stefan Widgren
dnl Included several of the configure script from matio into this file
dnl Modified to be used to configure matio to build in the rmatio R package

AC_PREREQ([2.69])
AC_INIT([rmatio],[see.DESCRIPTION.file],[https://github.com/stewid/rmatio/issues])
AC_CONFIG_AUX_DIR([tools])

AC_CONFIG_HEADERS([src/matio/config.h])

# zlib include and library path
AC_ARG_WITH([zlib-include],
    AS_HELP_STRING([--with-zlib-include=INCLUDE_PATH],[the location of zlib header files]),
    [zlib_include_path=$withval])
if test [ -n "$zlib_include_path" ] ; then
  RMATIO_CPPFLAGS="-I. -I${zlib_include_path}"
else
  RMATIO_CPPFLAGS="-I."
fi

AC_ARG_WITH([zlib-lib],
    AS_HELP_STRING([--with-zlib-lib=LIB_PATH],[the location of zlib libraries]),
    [zlib_lib_path=$withval])
if test [ -n "$zlib_lib_path" ] ; then
  LIBS="-L${zlib_lib_path} ${LIBS}"
fi

# Find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

CPPFLAGS="${CPPFLAGS} ${RMATIO_CPPFLAGS}"

# Check for zlib
AC_CHECK_LIB(z, inflate,
[
        RMATIO_HAVE_ZLIB=TRUE
        AC_DEFINE_UNQUOTED([HAVE_ZLIB],[1],[Have zlib])
        LIBS="${LIBS} -lz"
],
[
        RMATIO_HAVE_ZLIB=FALSE
])
AC_SUBST(RMATIO_HAVE_ZLIB)

# Checks for header files.
AC_CHECK_HEADERS([inttypes.h stdint.h stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor memset strdup])

AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([size_t])
AC_CHECK_SIZEOF([void *])

SIZEOF_CHAR="$ac_cv_sizeof_char"
SIZEOF_SHORT="$ac_cv_sizeof_short"
SIZEOF_DOUBLE="$ac_cv_sizeof_double"
SIZEOF_FLOAT="$ac_cv_sizeof_float"
SIZEOF_INT="$ac_cv_sizeof_int"
SIZEOF_LONG="$ac_cv_sizeof_long"
SIZEOF_LONG_LONG="$ac_cv_sizeof_long_long"
SIZEOF_SIZE_T="$ac_cv_sizeof_size_t"
SIZEOF_VOID_P="$ac_cv_sizeof_void_p"

AC_SUBST(SIZEOF_CHAR)
AC_SUBST(SIZEOF_SHORT)
AC_SUBST(SIZEOF_DOUBLE)
AC_SUBST(SIZEOF_FLOAT)
AC_SUBST(SIZEOF_INT)
AC_SUBST(SIZEOF_LONG)
AC_SUBST(SIZEOF_LONG_LONG)
AC_SUBST(SIZEOF_SIZE_T)
AC_SUBST(SIZEOF_VOID_P)

AC_DEFUN([CHECK_MATIO_INT8_T],
[
    AC_MSG_CHECKING([for mat_int8_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int8_t i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        ac_have_mat_int8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[int8_t],[int8 type])
        AC_MSG_RESULT([int8_t])
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 8
            #matio sizeof(signed char) not 8 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[signed char],[int8 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 8
            #matio sizeof(short) not 8 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[short],[int8 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 8
            #matio sizeof(int) not 8 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[int],[int8 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 8
            #matio sizeof(long) not 8 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long],[int8 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 8
            #matio sizeof(long long) not 8 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long long],[int8 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_INT16_T],
[
    AC_MSG_CHECKING([for mat_int16_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int16_t i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        ac_have_mat_int16_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[int16_t],[int16 type])
        AC_MSG_RESULT([int16_t])
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 16
            #matio sizeof(signed char) not 16 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[signed char],[int16 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 16
            #matio sizeof(short) not 16 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[short],[int16 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 16
            #matio sizeof(int) not 16 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[int],[int16 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 16
            #matio sizeof(long) not 16 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[long],[int16 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 16
            #matio sizeof(long long) not 16 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int16_t=yes],[ac_have_mat_int16_t=no])

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[long long],[int16 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_INT32_T],
[
    AC_MSG_CHECKING([for mat_int32_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int32_t i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

    if test "x$ac_have_mat_int32_t" = "xyes"
    then
        ac_have_mat_int32_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[int32_t],[int32 type])
        AC_MSG_RESULT([int32_t])
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 32
            #matio sizeof(signed char) not 32 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[signed char],[int32 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 32
            #matio sizeof(short) not 32 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[short],[int32 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 32
            #matio sizeof(int) not 32 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[int],[int32 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 32
            #matio sizeof(long) not 32 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[long],[int32 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 32
            #matio sizeof(long long) not 32 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int32_t=yes],[ac_have_mat_int32_t=no])

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[long long],[int32 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_INT64_T],
[
    AC_MSG_CHECKING([for mat_int64_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int64_t i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        ac_have_mat_int64_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[int64_t],[int64 type])
        AC_MSG_RESULT([int64_t])
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 64
            #matio sizeof(signed char) not 64 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[signed char],[int64 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 64
            #matio sizeof(short) not 64 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[short],[int64 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 64
            #matio sizeof(int) not 64 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[int],[int64 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 64
            #matio sizeof(long) not 64 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[long],[int64 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 64
            #matio sizeof(long long) not 64 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[long long],[int64 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_UINT8_T],
[
    AC_MSG_CHECKING([for mat_uint8_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint8_t i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        ac_have_mat_uint8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[uint8_t],[int8 type])
        AC_MSG_RESULT([uint8_t])
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 8
            #matio sizeof(signed char) not 8 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned char],[int8 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 8
            #matio sizeof(short) not 8 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned short],[int8 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 8
            #matio sizeof(int) not 8 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned int],[int8 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 8
            #matio sizeof(long) not 8 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned long],[int8 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 8
            #matio sizeof(long long) not 8 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint8_t=yes],[ac_have_mat_uint8_t=no])

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned long long][int8 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_UINT16_T],
[
    AC_MSG_CHECKING([for mat_uint16_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint16_t i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        ac_have_mat_uint16_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[uint16_t],[int16 type])
        AC_MSG_RESULT([uint16_t])
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 16
            #matio sizeof(signed char) not 16 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned char],[int16 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 16
            #matio sizeof(short) not 16 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned short],[int16 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 16
            #matio sizeof(int) not 16 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned int],[int16 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 16
            #matio sizeof(long) not 16 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long],[int16 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 16
            #matio sizeof(long long) not 16 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long long],[int16 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_UINT32_T],
[
    AC_MSG_CHECKING([for mat_uint32_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint32_t i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        ac_have_mat_uint32_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[uint32_t],[int32 type])
        AC_MSG_RESULT([uint32_t])
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 32
            #matio sizeof(signed char) not 32 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned char],[int32 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 32
            #matio sizeof(short) not 32 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned short],[int32 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 32
            #matio sizeof(int) not 32 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned int],[int32 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 32
            #matio sizeof(long) not 32 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long],[int32type ])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 32
            #matio sizeof(long long) not 32 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long long],[int32 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

AC_DEFUN([CHECK_MATIO_UINT64_T],
[
    AC_MSG_CHECKING([for mat_uint64_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint64_t i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        ac_have_mat_uint64_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[uint64_t],[int64 type])
        AC_MSG_RESULT([uint64_t])
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 64
            #matio sizeof(signed char) not 64 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned char],[int64 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 64
            #matio sizeof(short) not 64 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned short],[int64 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 64
            #matio sizeof(int) not 64 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned int],[int64 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 64
            #matio sizeof(long) not 64 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned long],[int64 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 64
            #matio sizeof(long long) not 64 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint64_t=yes],[ac_have_mat_uint64_t=no])

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned long long],[int64 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])

CHECK_MATIO_INT64_T
CHECK_MATIO_UINT64_T
CHECK_MATIO_INT32_T
CHECK_MATIO_UINT32_T
CHECK_MATIO_INT16_T
CHECK_MATIO_UINT16_T
CHECK_MATIO_INT8_T
CHECK_MATIO_UINT8_T

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)

AC_CONFIG_FILES([R/have_lib.R src/Makevars])

AC_OUTPUT
