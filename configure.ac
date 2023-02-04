AC_PREREQ([2.69])
AC_INIT([rmatio],[see.DESCRIPTION.file],[https://github.com/stewid/rmatio/issues])
AC_CONFIG_AUX_DIR([tools])

m4_include([tools/pkg.m4])

AC_CONFIG_HEADERS([src/matio/config.h])

# Check for pkg-config
PKG_PROG_PKG_CONFIG

# Find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

AC_MSG_CHECKING([inttypes.h])
have_inttypes_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <inttypes.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && have_inttypes_h=yes
AC_MSG_RESULT([${have_inttypes_h}])
if test "x${have_inttypes_h}" = xyes; then
    AC_DEFINE_UNQUOTED([HAVE_INTTYPES_H])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([stdlib.h])
have_stdlib_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <stdlib.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && have_stdlib_h=yes
AC_MSG_RESULT([${have_stdlib_h}])
if test "x${have_stdlib_h}" = xyes; then
    AC_DEFINE_UNQUOTED([HAVE_STDLIB_H])
fi
rm -f conftest.o conftest.c conftest.so

AC_MSG_CHECKING([stdint.h])
have_stdint_h=no
AC_LANG_CONFTEST([AC_LANG_PROGRAM(
[[#include <stdint.h>]], [[]])])
"${R_HOME}/bin/R" CMD SHLIB conftest.c \
1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && have_stdint_h=yes
AC_MSG_RESULT([${have_stdint_h}])
if test "x${have_stdint_h}" = xyes; then
    AC_DEFINE_UNQUOTED([HAVE_STDINT_H])
fi
rm -f conftest.o conftest.c conftest.so

# Check for zlib
have_zlib=no

if test [ -n "$PKG_CONFIG" ] ; then
    PKG_CHECK_MODULES([zlib], [zlib],
                      [CPPFLAGS="${zlib_CFLAGS} ${CPPFLAGS}"
                       LIBS="${zlib_LIBS} ${LIBS}"
                       have_zlib=yes], [ ])
fi

if test "x${have_zlib}" = xno; then
    AC_SEARCH_LIBS([inflate], [z], [have_zlib=yes])
fi

if test "x${have_zlib}" = xno; then
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
AC_SUBST(SIZEOF_CHAR)

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
AC_SUBST(SIZEOF_SHORT)

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
AC_SUBST(SIZEOF_DOUBLE)

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
AC_SUBST(SIZEOF_FLOAT)

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
AC_SUBST(SIZEOF_INT)

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
AC_SUBST(SIZEOF_LONG)

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
AC_SUBST(SIZEOF_LONG_LONG)

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
AC_SUBST(SIZEOF_SIZE_T)

dnl AC_CHECK_SIZEOF([void *])
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
AC_SUBST(SIZEOF_VOID_P)

AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
