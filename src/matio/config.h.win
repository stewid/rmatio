/*
 * rmatio, a R interface to the C library matio, MAT File I/O Library.
 * Copyright (C) 2013-2014  Stefan Widgren

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * rmatio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef CONFIG_H
#define CONFIG_H 1

/* Have zlib */
#define HAVE_ZLIB 1

/* MAT v7.3 file support */
#undef MAT73

#define HAVE_MAT_INT16_T  1
#define HAVE_MAT_INT32_T  1
#define HAVE_MAT_INT64_T  1
#define HAVE_MAT_INT8_T   1
#define HAVE_MAT_UINT16_T 1
#define HAVE_MAT_UINT32_T 1
#define HAVE_MAT_UINT64_T 1
#define HAVE_MAT_UINT8_T  1

#define SIZEOF_CHAR      1
#define SIZEOF_DOUBLE    8
#define SIZEOF_FLOAT     4
#define SIZEOF_INT       4
#define SIZEOF_LONG      4
#define SIZEOF_LONG_LONG 8
#define SIZEOF_SHORT     2

typedef long long          mat_int64_t;
typedef unsigned long long mat_uint64_t;
typedef int                mat_int32_t;
typedef unsigned           mat_uint32_t;
typedef short              mat_int16_t;
typedef unsigned short     mat_uint16_t;
typedef signed char        mat_int8_t;
typedef unsigned char      mat_uint8_t;

/* Matio major version number */
#define MATIO_MAJOR_VERSION 1

/* Matio minor version number */
#define MATIO_MINOR_VERSION 5

/* Matio release level number */
#define MATIO_RELEASE_LEVEL 2

/* Matio version number */
#define MATIO_VERSION 152

/* MATIO_PLATFORM is defined to "rmatio" when building. The define is not */
/* really used, since rmatio always use the argument hdr_str initialized */
/* from R with the correct platform when calling Mat_CreateVer */
#define MATIO_PLATFORM "rmatio"

#endif
