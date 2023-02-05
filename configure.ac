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
dnl Included several parts of the configure script from matio into
dnl this file. Modified to be used to configure matio to build in the
dnl rmatio R package

AC_PREREQ([2.69])
AC_INIT([rmatio],[see.DESCRIPTION.file],[https://github.com/stewid/rmatio/issues])
AC_CONFIG_AUX_DIR([tools])

m4_include([tools/pkg.m4])

AC_CONFIG_HEADERS([src/matio/config.h])

dnl Check for pkg-config
PKG_PROG_PKG_CONFIG

dnl Find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

AC_MSG_CHECKING([inttypes.h])
ac_have_inttypes_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <inttypes.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_inttypes_h=yes
AC_MSG_RESULT([$ac_have_inttypes_h])
if test "x$ac_have_inttypes_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_INTTYPES_H],
        [1],
        [Define to 1 if you have the <inttypes.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([stdlib.h])
ac_have_stdlib_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <stdlib.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_stdlib_h=yes
AC_MSG_RESULT([$ac_have_stdlib_h])
if test "x$ac_have_stdlib_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_STDLIB_H],
        [1],
        [Define to 1 if you have the <stdlib.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([stdint.h])
ac_have_stdint_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <stdint.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_stdint_h=yes
AC_MSG_RESULT([$ac_have_stdint_h])
if test "x$ac_have_stdint_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_STDINT_H],
        [1],
        [Define to 1 if you have the <stdint.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([strings.h])
ac_have_strings_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <strings.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_strings_h=yes
AC_MSG_RESULT([$ac_have_strings_h])
if test "x$ac_have_strings_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_STRINGS_H],
        [1],
        [Define to 1 if you have the <strings.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([string.h])
ac_have_string_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <string.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_string_h=yes
AC_MSG_RESULT([$ac_have_string_h])
if test "x$ac_have_string_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_STRING_H],
        [1],
        [Define to 1 if you have the <string.h> header file.])
    if test "x$ac_have_stdlib_h" = xyes; then
        AC_DEFINE_UNQUOTED(
            [STDC_HEADERS],
            [1],
            [Define to 1 if you have the ANSI C header files.])
    fi
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([sys/stat.h])
ac_have_sys_stat_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <sys/stat.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_sys_stat_h=yes
AC_MSG_RESULT([$ac_have_sys_stat_h])
if test "x$ac_have_sys_stat_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_SYS_STAT_H],
        [1],
        [Define to 1 if you have the <sys/stat.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([sys/types.h])
ac_have_sys_types_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <sys/types.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_sys_types_h=yes
AC_MSG_RESULT([$ac_have_sys_types_h])
if test "x$ac_have_sys_types_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_SYS_TYPES_H],
        [1],
        [Define to 1 if you have the <sys/types.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([unistd.h])
ac_have_unistd_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <unistd.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \ 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_unistd_h=yes
AC_MSG_RESULT([${have_unistd_h}])
if test "x$ac_have_unistd_h" = xyes; then
    AC_DEFINE_UNQUOTED(
        [HAVE_UNISTD_H],
        [1],
        [Define to 1 if you have the <unistd.h> header file.])
fi
rm -f conftest.o conftest.c conftest.so

dnl Check for zlib
ac_have_zlib=no

if test [ -n "$PKG_CONFIG" ] ; then
    PKG_CHECK_MODULES([zlib], [zlib],
                      [CPPFLAGS="${zlib_CFLAGS} ${CPPFLAGS}"
                       LIBS="${zlib_LIBS} ${LIBS}"
                       ac_have_zlib=yes], [ ])
fi

if test "x$ac_have_zlib" = xno; then
    AC_SEARCH_LIBS([inflate], [z], [have_zlib=yes])
fi

if test "x$ac_have_zlib" = xno; then
        AC_MSG_FAILURE([
  ---------------------------------------------
   The zlib library that is required to build
   rmatio was not found.
   Please install:
     zlib1g-dev (package on e.g. Debian and Ubuntu)
     zlib-devel (package on e.g. Fedora, CentOS and RHEL)
   and try again.
   If the zlib library is installed on your
   system but the rmatio configuration is
   unable to find it, you can specify the
   include and lib path to zlib with:
   R CMD INSTALL rmatio --configure-vars='LIBS=-L/path/to/libs CPPFLAGS=-I/path/to/headers'
  ---------------------------------------------])
fi

AC_MSG_CHECKING([size of char])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(char));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_CHAR=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_CHAR}])
AC_DEFINE_UNQUOTED(
  [SIZEOF_CHAR],
  [$SIZEOF_CHAR],
  [The size of `char', as computed by sizeof.])

AC_MSG_CHECKING([size of short])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(short));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_SHORT=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_SHORT}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_SHORT],
    [$SIZEOF_SHORT],
    [The size of `short', as computed by sizeof.])

AC_MSG_CHECKING([size of double])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(double));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_DOUBLE=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_DOUBLE}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_DOUBLE],
    [$SIZEOF_DOUBLE],
    [The size of `double', as computed by sizeof.])

AC_MSG_CHECKING([size of float])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(float));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_FLOAT=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_FLOAT}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_FLOAT],
    [$SIZEOF_FLOAT],
    [The size of `float', as computed by sizeof.])

AC_MSG_CHECKING([size of int])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(int));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_INT=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_INT}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_INT],
    [$SIZEOF_INT],
    [The size of `int', as computed by sizeof.])

AC_MSG_CHECKING([size of long])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(long));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_LONG=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_LONG}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_LONG],
    [$SIZEOF_LONG],
    [The size of `long', as computed by sizeof.])

AC_MSG_CHECKING([size of long long])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(long long));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_LONG_LONG=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_LONG_LONG}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_LONG_LONG],
    [$SIZEOF_LONG_LONG],
    [The size of `long long', as computed by sizeof.])

AC_MSG_CHECKING([size of size_t])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(size_t));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_SIZE_T=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_SIZE_T}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_SIZE_T],
    [$SIZEOF_SIZE_T],
    [The size of `size_t', as computed by sizeof.])

AC_MSG_CHECKING([size of void *])
cat >conftest.c <<EOF
[
  #include <Rinternals.h>
  SEXP sizeof_of_type()
  {
      return Rf_ScalarInteger(sizeof(void *));
  }
]
EOF
${R_HOME}/bin/R CMD SHLIB -o conftest.so conftest.c >& /dev/null
echo "dyn.load('conftest.so'); cat(.Call('sizeof_of_type'))" > conftest.R
SIZEOF_VOID_P=`${R_HOME}/bin/R --slave --vanilla < conftest.R`
rm -f conftest.o conftest.c conftest.so conftest.R
AC_MSG_RESULT([${SIZEOF_VOID_P}])
AC_DEFINE_UNQUOTED(
    [SIZEOF_VOID_P],
    [$SIZEOF_VOID_P],
    [The size of `void *', as computed by sizeof.])

AC_MSG_CHECKING([for mat_int8_t])
ac_have_mat_int8_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[int8_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_int8_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
    AC_DEFINE_UNQUOTED([_mat_int8_t],[int8_t],[int8 type])
    AC_MSG_RESULT([int8_t])
fi
if test "x$ac_have_mat_int8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[signed char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[signed char],[int8 type])
        AC_MSG_RESULT([signed char])
    fi
fi
if test "x$ac_have_mat_int8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[short],[int8 type])
        AC_MSG_RESULT([short])
    fi
fi
if test "x$ac_have_mat_int8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[int],[int8 type])
        AC_MSG_RESULT([int])
    fi
fi
if test "x$ac_have_mat_int8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[long],[int8 type])
        AC_MSG_RESULT([long])
    fi
fi
if test "x$ac_have_mat_int8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[long long],[int8 type])
        AC_MSG_RESULT([long long])
    fi
fi
if test "x$ac_have_mat_int8_t" != "xyes"; then
    AC_MSG_RESULT([])
fi

AC_MSG_CHECKING([for mat_int16_t])
ac_have_mat_int16_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[int16_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_int16_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
    AC_DEFINE_UNQUOTED([_mat_int16_t],[int16_t],[int16 type])
    AC_MSG_RESULT([int16_t])
fi
if test "x$ac_have_mat_int16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[signed char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[signed char],[int16 type])
        AC_MSG_RESULT([signed char])
    fi
fi
if test "x$ac_have_mat_int16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[short],[int16 type])
        AC_MSG_RESULT([short])
    fi
fi
if test "x$ac_have_mat_int16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[int],[int16 type])
        AC_MSG_RESULT([int])
    fi
fi
if test "x$ac_have_mat_int16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[long],[int16 type])
        AC_MSG_RESULT([long])
    fi
fi
if test "x$ac_have_mat_int16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int16_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_int32_t])
ac_have_mat_int32_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[int32_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_int32_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
    AC_DEFINE_UNQUOTED([_mat_int32_t],[int32_t],[int32 type])
    AC_MSG_RESULT([int32_t])
fi
if test "x$ac_have_mat_int32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[signed char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[signed char],[int32 type])
        AC_MSG_RESULT([signed char])
    fi
fi
if test "x$ac_have_mat_int32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[short i = 0;]])])
    if test "x$ac_have_mat_int32_t" = "xyes"
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
    rm -f conftest.o conftest.c conftest.so
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[short],[int32 type])
        AC_MSG_RESULT([short])
    fi
fi
if test "x$ac_have_mat_int32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[int],[int32 type])
        AC_MSG_RESULT([int])
    fi
fi
if test "x$ac_have_mat_int32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[long],[int32 type])
        AC_MSG_RESULT([long])
    fi
fi
if test "x$ac_have_mat_int32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int32_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_int64_t])
ac_have_mat_int64_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[int64_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_int64_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
    AC_DEFINE_UNQUOTED([_mat_int64_t],[int64_t],[int64 type])
    AC_MSG_RESULT([int64_t])
fi
if test "x$ac_have_mat_int64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[signed char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[signed char],[int64 type])
        AC_MSG_RESULT([signed char])
    fi
fi
if test "x$ac_have_mat_int64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[short],[int64 type])
        AC_MSG_RESULT([short])
    fi
fi
if test "x$ac_have_mat_int64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[int],[int64 type])
        AC_MSG_RESULT([int])
    fi
fi
if test "x$ac_have_mat_int64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[long],[int64 type])
        AC_MSG_RESULT([long])
    fi
fi
if test "x$ac_have_mat_int64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_int64_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_uint8_t])
ac_have_mat_uint8_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[uint8_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_uint8_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
    AC_DEFINE_UNQUOTED([_mat_uint8_t],[uint8_t],[int8 type])
    AC_MSG_RESULT([uint8_t])
fi
if test "x$ac_have_mat_uint8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned char],[int8 type])
        AC_MSG_RESULT([unsigned char])
    fi
fi
if test "x$ac_have_mat_uint8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned short],[int8 type])
        AC_MSG_RESULT([unsigned short])
    fi
fi
if test "x$ac_have_mat_uint8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned int],[int8 type])
        AC_MSG_RESULT([unsigned int])
    fi
fi
if test "x$ac_have_mat_uint8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned long],[int8 type])
        AC_MSG_RESULT([unsigned long])
    fi
fi
if test "x$ac_have_mat_uint8_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint8_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_uint16_t])
ac_have_mat_uint16_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[uint16_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_uint16_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
    AC_DEFINE_UNQUOTED([_mat_uint16_t],[uint16_t],[int16 type])
    AC_MSG_RESULT([uint16_t])
fi
if test "x$ac_have_mat_uint16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned char],[int16 type])
        AC_MSG_RESULT([unsigned char])
    fi
fi
if test "x$ac_have_mat_uint16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned short],[int16 type])
        AC_MSG_RESULT([unsigned short])
    fi
fi
if test "x$ac_have_mat_uint16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned int],[int16 type])
        AC_MSG_RESULT([unsigned int])
    fi
fi
if test "x$ac_have_mat_uint16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long],[int16 type])
        AC_MSG_RESULT([unsigned long])
    fi
fi
if test "x$ac_have_mat_uint16_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint16_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_uint32_t])
ac_have_mat_uint32_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[uint32_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_uint32_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
    AC_DEFINE_UNQUOTED([_mat_uint32_t],[uint32_t],[int32 type])
    AC_MSG_RESULT([uint32_t])
fi
if test "x$ac_have_mat_uint32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned char],[int32 type])
        AC_MSG_RESULT([unsigned char])
    fi
fi
if test "x$ac_have_mat_uint32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned short],[int32 type])
        AC_MSG_RESULT([unsigned short])
    fi
fi
if test "x$ac_have_mat_uint32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned int],[int32 type])
        AC_MSG_RESULT([unsigned int])
    fi
fi
if test "x$ac_have_mat_uint32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long],[int32type ])
        AC_MSG_RESULT([unsigned long])
    fi
fi
if test "x$ac_have_mat_uint32_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint32_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_MSG_CHECKING([for mat_uint64_t])
ac_have_mat_uint64_t=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
    #include <stdlib.h>
    #ifdef HAVE_INTTYPES_H
    #   include <inttypes.h>
    #endif
    #ifdef HAVE_STDINT_H
    #   include <stdint.h>
    #endif
]], [[uint64_t i = 0;]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
rm -f conftest.o conftest.c conftest.so
if test "x$ac_have_mat_uint64_t" = "xyes"
then
    AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
    AC_DEFINE_UNQUOTED([_mat_uint64_t],[uint64_t],[int64 type])
    AC_MSG_RESULT([uint64_t])
fi
if test "x$ac_have_mat_uint64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned char i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned char],[int64 type])
        AC_MSG_RESULT([unsigned char])
    fi
fi
if test "x$ac_have_mat_uint64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned short i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned short],[int64 type])
        AC_MSG_RESULT([unsigned short])
    fi
fi
if test "x$ac_have_mat_uint64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned int i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned int],[int64 type])
        AC_MSG_RESULT([unsigned int])
    fi
fi
if test "x$ac_have_mat_uint64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
    rm -f conftest.o conftest.c conftest.so
    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned long],[int64 type])
        AC_MSG_RESULT([unsigned long])
    fi
fi
if test "x$ac_have_mat_uint64_t" != "xyes"
then
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[
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
    ]], [[unsigned long long i = 0;]])])
    "${R_HOME}/bin/R" CMD SHLIB conftest.c \
    1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_have_mat_uint64_t=yes
    rm -f conftest.o conftest.c conftest.so
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

AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
