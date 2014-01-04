AC_INIT(rmatio, see.DESCRIPTION.file)

AC_DEFINE_UNQUOTED([MATIO_MAJOR_VERSION],1,[Matio major version number])
AC_DEFINE_UNQUOTED([MATIO_MINOR_VERSION],5,[Matio minor version number])
AC_DEFINE_UNQUOTED([MATIO_RELEASE_LEVEL],2,[Matio release level number])
AC_DEFINE_UNQUOTED([MATIO_VERSION],152,[Matio version number])

AC_CONFIG_HEADERS([src/matio/matioConfig.h src/matio/matio_pubconf.h])
AC_STDC_HEADERS
AC_CHECK_HEADER([inttypes.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_INTTYPES_H],[1],[Matio has access to inttypes.h])])
AC_CHECK_HEADER([stdint.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_STDINT_H],[1],[Matio has access to stdint.h])])
AC_CHECK_HEADER([strings.h],[AC_DEFINE_UNQUOTED([HAVE_STRINGS_H],[1],[Matio has access to strings.h])])
AC_CHECK_HEADER([unistd.h],[AC_DEFINE_UNQUOTED([HAVE_UNISTD_H],[1],[Matio has access to unistd.h])])

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [int8_t i = 0;],
    ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        ac_have_mat_int8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[int8_t],[int8 type])
        AC_MSG_RESULT([int8_t])
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [signed char i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[signed char],[int8 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [short i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[short],[int8 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [int i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[int],[int8 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long],[int8 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long long i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [int16_t i = 0;],
    ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

    if test "x$ac_have_mat_int16_t" = "xyes"
    then
        ac_have_mat_int16_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_int16_t],[int16_t],[int16 type])
        AC_MSG_RESULT([int16_t])
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [signed char i = 0;],
        ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[signed char],[int16 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [short i = 0;],
        ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[short],[int16 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [int i = 0;],
        ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[int],[int16 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long i = 0;],
        ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

        if test "x$ac_have_mat_int16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_int16_t],[long],[int16 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long long i = 0;],
        ac_have_mat_int16_t=yes,ac_have_mat_int16_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [int32_t i = 0;],
    ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

    if test "x$ac_have_mat_int32_t" = "xyes"
    then
        ac_have_mat_int32_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_int32_t],[int32_t],[int32 type])
        AC_MSG_RESULT([int32_t])
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [signed char i = 0;],
        ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[signed char],[int32 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [short i = 0;],
        ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[short],[int32 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [int i = 0;],
        ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[int],[int32 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long i = 0;],
        ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

        if test "x$ac_have_mat_int32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_int32_t],[long],[int32 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long long i = 0;],
        ac_have_mat_int32_t=yes,ac_have_mat_int32_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [int64_t i = 0;],
    ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        ac_have_mat_int64_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[int64_t],[int64 type])
        AC_MSG_RESULT([int64_t])
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [signed char i = 0;],
        ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[signed char],[int64 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [short i = 0;],
        ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[short],[int64 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [int i = 0;],
        ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[int],[int64 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long i = 0;],
        ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[long],[int64 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [long long i = 0;],
        ac_have_mat_int64_t=yes,ac_have_mat_int64_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [uint8_t i = 0;],
    ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

    if test "x$ac_have_mat_uint8_t" = "xyes"
    then
        ac_have_mat_uint8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_uint8_t],[uint8_t],[int8 type])
        AC_MSG_RESULT([uint8_t])
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned char i = 0;],
        ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned char],[int8 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned short i = 0;],
        ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned short],[int8 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned int i = 0;],
        ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned int],[int8 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long i = 0;],
        ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

        if test "x$ac_have_mat_uint8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_uint8_t],[unsigned long],[int8 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint8_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long long i = 0;],
        ac_have_mat_uint8_t=yes,ac_have_mat_uint8_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [uint16_t i = 0;],
    ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        ac_have_mat_uint16_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[uint16_t],[int16 type])
        AC_MSG_RESULT([uint16_t])
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned char i = 0;],
        ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned char],[int16 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned short i = 0;],
        ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned short],[int16 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned int i = 0;],
        ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned int],[int16 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long i = 0;],
        ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long],[int16 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long long i = 0;],
        ac_have_mat_uint16_t=yes,ac_have_mat_uint16_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [uint32_t i = 0;],
    ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        ac_have_mat_uint32_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[uint32_t],[int32 type])
        AC_MSG_RESULT([uint32_t])
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned char i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned char],[int32 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned short i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned short],[int32 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned int i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned int],[int32 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long],[int32type ])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long long i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

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

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [uint64_t i = 0;],
    ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

    if test "x$ac_have_mat_uint64_t" = "xyes"
    then
        ac_have_mat_uint64_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_uint64_t],[uint64_t],[int64 type])
        AC_MSG_RESULT([uint64_t])
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned char i = 0;],
        ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned char],[int64 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned short i = 0;],
        ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned short],[int64 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned int i = 0;],
        ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned int],[int64 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long i = 0;],
        ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

        if test "x$ac_have_mat_uint64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_uint64_t],[unsigned long],[int64 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint64_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long long i = 0;],
        ac_have_mat_uint64_t=yes,ac_have_mat_uint64_t=no)

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

AC_CACHE_CHECK([for va_copy()],matio_cv_have_va_copy,[
	AC_LINK_IFELSE([AC_LANG_SOURCE([[
    #include <stdarg.h>
	void f (int i, ...) {
	va_list args1, args2;
	va_start (args1, i);
	va_copy (args2, args1);
	if (va_arg (args2, int) != 42 || va_arg (args1, int) != 42)
	  exit (1);
	va_end (args1); va_end (args2);
	}
	int main() {
	  f (0, 42);
	  return 0;
	}]])],
	[matio_cv_have_va_copy=yes],
	[matio_cv_have_va_copy=no])
])
if test "$matio_cv_have_va_copy" = "no"
then
    AC_CACHE_CHECK([for __va_copy()],matio_cv_have___va_copy,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[#include <stdarg.h>
            void f (int i, ...) {
                va_list args1, args2;
                va_start (args1, i);
                __va_copy (args2, args1);
                if (va_arg (args2, int) != 42 || va_arg (args1, int) != 42)
                    exit (1);
                va_end (args1); va_end (args2);
            }
            int main() {
                f (0, 42);
                return 0;
            }]])],
	[matio_cv_have___va_copy=yes],
	[matio_cv_have___va_copy=no])
    ])
    if test "matio_cv_have___va_copy" = "yes"
    then
        AC_DEFINE_UNQUOTED([HAVE___VA_COPY],[],[Have va_copy])
    fi
else
    AC_DEFINE_UNQUOTED([HAVE_VA_COPY],[],[Have va_copy])
fi

AC_CACHE_CHECK([for vsnprintf()],matio_cv_have_vsnprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vsnprintf(NULL,0,"%d",ap);
          return 0;
        }]])],
        [matio_cv_have_vsnprintf=yes],
        [matio_cv_have_vsnprintf=no])
])

AC_CACHE_CHECK([for snprintf()],matio_cv_have_snprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          snprintf(NULL,0,"%d",0);
          return 0;
        }]])],
        [matio_cv_have_snprintf=yes],
        [matio_cv_have_snprintf=no])
])

AC_CACHE_CHECK([for vasprintf()],matio_cv_have_vasprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vasprintf(NULL,"%d",ap);
          return 0;
        }]])],
        [matio_cv_have_vasprintf=yes],
        [matio_cv_have_vasprintf=no])
])

AC_CACHE_CHECK([for asprintf()],matio_cv_have_asprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          asprintf(NULL,0,"%d",0);
          return 0;
        }]])],
        [matio_cv_have_asprintf=yes],
        [matio_cv_have_asprintf=no])
])

if test "$matio_cv_have_vsnprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VSNPRINTF],[],[Have vsnprintf])
fi
if test "$matio_cv_have_snprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_SNPRINTF],[],[Have snprintf])
fi
if test "$matio_cv_have_vasprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VASPRINTF],[],[Have vasprintf])
fi
if test "$matio_cv_have_asprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_ASPRINTF],[],[Have asprintf])
fi

AC_OUTPUT
