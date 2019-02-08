/*
 * rmatio, a R interface to the C library matio, MAT File I/O Library.
 * Copyright (C) 2013-2019  Stefan Widgren

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

#include <Rdefines.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "matio/matio.h"

/*
 * -------------------------------------------------------------
 *
 *   Forward declarations
 *
 * -------------------------------------------------------------
 */

static int
read_mat_cell(SEXP list,
              int index,
              matvar_t *matvar);

static int
read_mat_struct(SEXP list,
                int index,
                matvar_t *matvar);

static int
write_elmt(const SEXP elmt,
           mat_t *mat,
           const char *name,
           matvar_t *mat_struct,
           matvar_t *mat_cell,
           size_t field_index,
           size_t index,
           int ragged,
           int compression);

/*
 * -------------------------------------------------------------
 *
 *   Help functions to map an R object to mat dimensions
 *
 * -------------------------------------------------------------
 */

/** @brief Map the dimensions and rank from an R object
 *
 *
 * Note: The function allocates memory for dims
 * @ingroup rmatio
 * @param elmt R object to determine dimension and rank from
 * @param rank The rank of the R object
 * @param dims Dimensions of the R object.
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_object_rank_and_dims(const SEXP elmt, int *rank, size_t **dims)
{
    if (Rf_isNull(elmt) || NULL == rank || NULL == dims)
        return 1;

    /* Check that the type of elmt is the expected */
    switch (TYPEOF(elmt)) {
    case REALSXP:
    case INTSXP:
    case CPLXSXP:
    case LGLSXP:
        break;
    default:
        return 1;
    }

    if (Rf_isNull(Rf_getAttrib(elmt, R_DimSymbol))) {
        *rank = 2;
        *dims = malloc((*rank)*sizeof(size_t));
        if (NULL == dims)
            return 1;
        (*dims)[0] = 1;
        (*dims)[1] = LENGTH(elmt);
    } else {
        *rank = LENGTH(GET_SLOT(elmt, R_DimSymbol));
        *dims = malloc((*rank)*sizeof(size_t));
        if (NULL == *dims)
            return 1;
        for (int i=0;i<*rank;i++)
            (*dims)[i] = INTEGER(GET_SLOT(elmt, R_DimSymbol))[i];
    }

    return 0;
}

/** @brief Map the length from an R object
 *
 *
 * @ingroup rmatio
 * @param elmt R object to determine dimension from
 * @param len The out value of the length
 * @return 0 on succes or 1 on failure.
 */
static int
map_vec_len(const SEXP elmt, int *len)
{
    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt))
        return 1;

    if (LENGTH(elmt)) {
        int first_lookup = 1;

        for (size_t i=0;i<LENGTH(elmt);i++) {
            SEXP item = VECTOR_ELT(elmt, i);
            switch (TYPEOF(item)) {
            case VECSXP:
                if (LENGTH(item)) {
                    if (first_lookup) {
                        *len = LENGTH(item);
                        first_lookup = 0;
                    } else if (*len != LENGTH(item)) {
                        return 1;
                    }
                } else if (first_lookup) {
                    *len = 0;
                    first_lookup = 0;
                } else if (*len) {
                    return 1;
                }
                break;

            case STRSXP:
            case REALSXP:
            case INTSXP:
            case CPLXSXP:
            case LGLSXP:
                if (first_lookup) {
                    if (!Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)))
                        *len = LENGTH(item);
                    else
                        *len = LENGTH(elmt);
                    first_lookup = 0;
                } else if (!Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol))) {
                    if (*len != LENGTH(item))
                        return 1;
                } else if (*len != LENGTH(elmt)) {
                    return 1;
                }
                break;

            case S4SXP:
            {
                /* Check that the S4 class is the expected */
                SEXP class_name = Rf_getAttrib(elmt, R_ClassSymbol);
                if ((strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0)
                    || (strcmp(CHAR(STRING_ELT(class_name, 0)), "lgCMatrix") == 0)) {
                    if (first_lookup) {
                        if (!Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)))
                            *len = 1;
                        else
                            *len = LENGTH(elmt);
                        first_lookup = 0;
                    } else if (!Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol))) {
                        return 1;
                    } else if (*len != LENGTH(elmt)) {
                        return 1;
                    }
                } else {
                    return 1;
                }
                break;
            }

            default:
                return 1;
            }
        }
    } else {
        *len = 0;
    }

    if (*len && !Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)))
        *len = 1;

    return 0;
}

/** @brief Set the dim for a a cell or structure array from an R object
 *
 *
 * @ingroup rmatio
 * @param elmt R object to determine dimension from
 * @param dims Dimensions of the R object.
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_object_dims(const SEXP elmt, size_t *dims)
{
    if (Rf_isNull(elmt) || NULL == dims)
        return 1;

    switch (TYPEOF(elmt)) {
    case VECSXP:
    {
        int tmp = 0;

        if (Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)))
            tmp = LENGTH(elmt);
        else if (map_vec_len(elmt, &tmp))
            return 1;
        dims[0] = tmp;
        dims[1] = 1;
        break;
    }
    case STRSXP:
        dims[0] = LENGTH(elmt);
        dims[1] = 1;
        break;
    case REALSXP:
    case INTSXP:
    case CPLXSXP:
    case LGLSXP:
        dims[0] = LENGTH(elmt) > 1;
        dims[1] = 1;
        break;
    case S4SXP:
    {
        /* Check that the S4 class is the expected */
        SEXP class_name = Rf_getAttrib(elmt, R_ClassSymbol);
        if ((strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0)
            || (strcmp(CHAR(STRING_ELT(class_name, 0)), "lgCMatrix") == 0)) {
            dims[0] = 1;
            dims[1] = 1;
        } else {
            return 1;
        }
        break;
    }
    default:
        return 1;
    }

    return 0;
}

/** @brief Set the dim for a a cell or structure array from an R object
 *
 *
 * @ingroup rmatio
 * @param elmt
 * @param dims
 * @param empty
 * @param ragged
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_vecsxp_dims(const SEXP elmt, size_t *dims, int *empty)
{
    size_t len=0;
    int vecsxp = 0;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || NULL == dims || NULL == empty)
        return 1;

    *empty = 0;

    if (LENGTH(elmt)) {
        for (int i=0;i<LENGTH(elmt);i++) {
            SEXP item = VECTOR_ELT(elmt, i);

            if (map_R_object_dims(item, dims))
                return 1;

            if (!i)
                len = dims[0];
            else if (len != dims[0])
                return 1;

            if (VECSXP == TYPEOF(item))
                vecsxp = 1;
        }
    }

    if (!LENGTH(elmt)) {
        if (Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol))) {
            dims[0] = 0;
            dims[1] = 0;
        } else {
            dims[0] = 1;
            dims[1] = 1;
        }
    } else if (!len) {
        if (Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)) || !vecsxp) {
            dims[0] = 1;
            dims[1] = LENGTH(elmt);
            *empty = 1;
        } else {
            dims[0] = 0;
            dims[1] = 1;
        }
    } else if (Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol))) {
        dims[0] = LENGTH(elmt);
        dims[1] = len;
    } else {
        dims[0] = len;
        dims[1] = 1;
    }

    return 0;
}

/** @brief Check if all strings have equal length
 *
 *
 * @ingroup rmatio
 * @param elmt R object to check
 * @param equal_length The out value is 1 if all lengths are equal else 0.
 * @return 0 on succes or 1 on failure.
 */
static int
check_string_lengths(const SEXP elmt, int *equal_length)
{
    size_t n;

    if (Rf_isNull(elmt) || STRSXP != TYPEOF(elmt) || NULL == equal_length)
        return 1;

    n = LENGTH(elmt);
    *equal_length = 1;
    if (n) {
        size_t len = LENGTH(STRING_ELT(elmt, 0));
        for (size_t i=1;i<n;i++) {
            if (len != LENGTH(STRING_ELT(elmt, i))) {
                *equal_length = 0;
                break;
            }
        }
    }

    return 0;
}

/** @brief Check if the R object VECSXP contains element of equal
 * length
 *
 *
 * @ingroup rmatio
 * @param elmt R object to check if it's ragged
 * @param ragged The out value is 0 if the VECSXP contains element of equal length
 * @return 0 on succes or 1 on failure.
 */
static int
check_ragged(const SEXP elmt, int *ragged)
{
    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || NULL == ragged)
        return 1;

    *ragged = 0;

    if (LENGTH(elmt)) {
        size_t len=0;

        for (int i=0;i<LENGTH(elmt);i++) {
            SEXP item = VECTOR_ELT(elmt, i);
            switch (TYPEOF(item)) {
            case VECSXP:
            {
                int tmp = 0;

                if (Rf_isNull(Rf_getAttrib(item, R_NamesSymbol)))
                    tmp = LENGTH(item);
                else if (map_vec_len(item, &tmp))
                    return 1;
                if (!i)
                    len = tmp;
                else if (len != tmp)
                    *ragged = 1;
                break;
            }

            case STRSXP:
                /* Check that all fields/cells have equal length */
                if (i && len != LENGTH(item)) {
                    return 1;
                } else {
                    int equal_length;

                    len = LENGTH(item);
                    if (check_string_lengths(item, &equal_length))
                        return 1;
                    if (0 == equal_length)
                        *ragged = 1;
                }
                break;

            case REALSXP:
            case INTSXP:
            case CPLXSXP:
            case LGLSXP:
                if(!i)
                    len = LENGTH(item) > 1;
                else if(len != (LENGTH(item) > 1))
                    *ragged = 1;
                break;

            case S4SXP:
            {
                /* Check that the S4 class is the expected */
                SEXP class_name = Rf_getAttrib(item, R_ClassSymbol);
                if ((strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0)
                    || (strcmp(CHAR(STRING_ELT(class_name, 0)), "lgCMatrix") == 0)) {
                    if(!i)
                        len = 1;
                    else if(1 != len)
                        *ragged = 1;
                } else {
                    return 1;
                }
                break;
            }
            default:
                return 1;
            }
        }
    }

    return 0;
}

/*
 * -------------------------------------------------------------
 *   Write functions
 * -------------------------------------------------------------
 */

/** @brief Create matvar_t pointer to an empty data structure
 *
 *
 * @ingroup rmatio
 * @param elmt R object to create empty mat variable from
 * @return
 */
static matvar_t *
Mat_VarCreateEmpty(const SEXP elmt)
{
    size_t dims_0_1[2] = {0, 1};
    const int rank = 2;

    if (Rf_isNull(elmt))
        return NULL;

    switch (TYPEOF(elmt)) {
    case REALSXP:
        return Mat_VarCreate(NULL,
                             MAT_C_DOUBLE,
                             MAT_T_DOUBLE,
                             rank,
                             dims_0_1,
                             NULL,
                             0);
    case INTSXP:
        return Mat_VarCreate(NULL,
                             MAT_C_INT32,
                             MAT_T_INT32,
                             rank,
                             dims_0_1,
                             NULL,
                             0);
    case CPLXSXP:
        return Mat_VarCreate(NULL,
                             MAT_C_DOUBLE,
                             MAT_T_DOUBLE,
                             rank,
                             dims_0_1,
                             NULL,
                             MAT_F_COMPLEX);
    case LGLSXP:
        return Mat_VarCreate(NULL,
                             MAT_C_UINT8,
                             MAT_T_UINT8,
                             rank,
                             dims_0_1,
                             NULL,
                             MAT_F_LOGICAL);
    case STRSXP:
        return Mat_VarCreate(NULL,
                             MAT_C_CHAR,
                             MAT_T_UINT16,
                             rank,
                             dims_0_1,
                             NULL,
                             0);
    default:
        return NULL;
    }
}

/** @brief Write the matvar data
 *
 *
 * @ingroup rmatio
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param matvar MAT variable pointer to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @return 0 on succes or 1 on failure.
 */
static int
write_matvar(mat_t *mat,
             matvar_t *matvar,
             matvar_t *mat_struct,
             matvar_t *mat_cell,
             size_t field_index,
             size_t index,
             int compression)
{
    if (NULL == matvar)
        return 1;

    if (mat_struct) {
        Mat_VarSetStructFieldByIndex(mat_struct, field_index, index, matvar);
    } else if(mat_cell) {
        Mat_VarSetCell(mat_cell, index, matvar);
    } else {
        Mat_VarWrite(mat, matvar, compression);
        Mat_VarFree(matvar);
    }

    return 0;
}

/** @brief Write CHARSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @return 0 on succes or 1 on failure.
 */
static int
write_charsxp(const SEXP elmt,
              mat_t *mat,
              const char *name,
              matvar_t *mat_struct,
              matvar_t *mat_cell,
              size_t field_index,
              size_t index,
              int compression)
{
    size_t dims[2];
    const int rank = 2;
    matvar_t *matvar;
    mat_uint16_t *buf;

    if (Rf_isNull(elmt) || CHARSXP != TYPEOF(elmt))
        return 1;

    dims[0] = 1;
    dims[1] = LENGTH(elmt);

    buf = malloc(dims[1]*sizeof(mat_uint16_t));
    if (NULL == buf)
        return 1;
    for (size_t i=0;i<dims[1];i++)
        buf[i] = CHAR(elmt)[i];

    matvar = Mat_VarCreate(name,
                           MAT_C_CHAR,
                           MAT_T_UINT16,
                           rank,
                           dims,
                           (void*)buf,
                           0);

    free(buf);

    if (NULL == matvar)
        return 1;

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write REALSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_realsxp(const SEXP elmt,
              mat_t *mat,
              const char *name,
              matvar_t *mat_struct,
              matvar_t *mat_cell,
              size_t field_index,
              size_t index,
              int compression)
{
    size_t *dims;
    int rank;
    matvar_t *matvar=NULL;

    if (Rf_isNull(elmt) || REALSXP != TYPEOF(elmt))
        return 1;

    if (map_R_object_rank_and_dims(elmt, &rank, &dims))
        return 1;

    matvar = Mat_VarCreate(name,
                           MAT_C_DOUBLE,
                           MAT_T_DOUBLE,
                           rank,
                           dims,
                           REAL(elmt),
                           0);

    free(dims);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write INTSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_intsxp(const SEXP elmt,
             mat_t *mat,
             const char *name,
             matvar_t *mat_struct,
             matvar_t *mat_cell,
             size_t field_index,
             size_t index,
             int compression)
{
    size_t *dims;
    int rank;
    matvar_t *matvar=NULL;

    if (Rf_isNull(elmt) || INTSXP != TYPEOF(elmt))
        return 1;

    if (map_R_object_rank_and_dims(elmt, &rank, &dims))
        return 1;

    matvar = Mat_VarCreate(name,
                           MAT_C_INT32,
                           MAT_T_INT32,
                           rank,
                           dims,
                           INTEGER(elmt),
                           0);

    free(dims);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write CPLXSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_cplxsxp(const SEXP elmt,
              mat_t *mat,
              const char *name,
              matvar_t *mat_struct,
              matvar_t *mat_cell,
              size_t field_index,
              size_t index,
              int compression)
{
    size_t *dims;
    int rank;
    matvar_t *matvar=NULL;
    double *re = NULL;
    double *im = NULL;
    struct mat_complex_split_t z;

    if (Rf_isNull(elmt) || CPLXSXP != TYPEOF(elmt))
        return 1;

    if (map_R_object_rank_and_dims(elmt, &rank, &dims))
        return 1;

    re = malloc(LENGTH(elmt)*sizeof(double));
    if (NULL == re) {
        free(dims);
        return 1;
    }

    im = malloc(LENGTH(elmt)*sizeof(double));
    if (NULL == im) {
        free(dims);
        free(re);
        return 1;
    }

    for (int i=0;i<LENGTH(elmt);i++) {
        re[i] = COMPLEX(elmt)[i].r;
        im[i] = COMPLEX(elmt)[i].i;
    }

    z.Re = re;
    z.Im = im;
    matvar = Mat_VarCreate(name,
                           MAT_C_DOUBLE,
                           MAT_T_DOUBLE,
                           rank,
                           dims,
                           &z,
                           MAT_F_COMPLEX);

    free(dims);
    free(re);
    free(im);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write LGLSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_lglsxp(const SEXP elmt,
             mat_t *mat,
             const char *name,
             matvar_t *mat_struct,
             matvar_t *mat_cell,
             size_t field_index,
             size_t index,
             int compression)
{
    size_t *dims, len;
    int rank;
    matvar_t *matvar = NULL;
    mat_uint8_t *logical = NULL;

    if (Rf_isNull(elmt) || LGLSXP != TYPEOF(elmt))
        return 1;

    if (map_R_object_rank_and_dims(elmt, &rank, &dims))
        return 1;

    len = dims[0];
    for (int i=1;i<rank;i++)
        len *= dims[i];

    logical = malloc(len*sizeof(mat_uint8_t));
    if (NULL == logical) {
        free(dims);
        return 1;
    }

    for (size_t i=0;i<len;i++)
        logical[i] = LOGICAL(elmt)[i] != 0;

    matvar = Mat_VarCreate(name,
                           MAT_C_UINT8,
                           MAT_T_UINT8,
                           rank,
                           dims,
                           logical,
                           MAT_F_LOGICAL);

    free(dims);
    free(logical);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write STRSXP
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param ragged
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_strsxp(const SEXP elmt,
             mat_t *mat,
             const char *name,
             matvar_t *mat_struct,
             matvar_t *mat_cell,
             size_t field_index,
             size_t index,
             int ragged,
             int compression)
{
    size_t dims[2] = {0, 0};
    matvar_t *matvar;
    const int rank = 2;
    int equal_length;

    if (Rf_isNull(elmt) || STRSXP != TYPEOF(elmt) ||
        !Rf_isNull(Rf_getAttrib(elmt, R_DimSymbol)))
        return 1;

    if (mat_struct
        && LENGTH(STRING_ELT(elmt, index)) != LENGTH(STRING_ELT(elmt, 0)))
        return 1;
    if (ragged
        && NULL == mat_cell)
        return 1;

    if (mat_struct || ragged)
        return write_elmt(STRING_ELT(elmt, index),
                          mat,
                          name,
                          mat_struct,
                          mat_cell,
                          field_index,
                          index,
                          ragged,
                          compression);

    dims[0] = LENGTH(elmt);
    if (dims[0])
        dims[1] = LENGTH(STRING_ELT(elmt, 0));

    if (check_string_lengths(elmt, &equal_length))
        return 1;

    if (equal_length) {
        mat_uint16_t *buf = malloc(dims[0]*dims[1]*sizeof(mat_uint16_t));
        if (NULL == buf)
            return 1;

        for (size_t i=0;i<dims[0];i++) {
            for (size_t j=0;j<dims[1];j++)
                buf[dims[0]*j + i] = CHAR(STRING_ELT(elmt, i))[j];
        }

        matvar = Mat_VarCreate(name,
                               MAT_C_CHAR,
                               MAT_T_UINT16,
                               rank,
                               dims,
                               (void*)buf,
                               0);

        free(buf);

        if (NULL == matvar)
            return 1;
    } else {
        /* Write strings in a cell */
        dims[1] = 1;
        matvar = Mat_VarCreate(name,
                               MAT_C_CELL,
                               MAT_T_CELL,
                               rank,
                               dims,
                               NULL,
                               0);

        if (NULL == matvar)
            return 1;

        for (size_t i=0;i<dims[0];i++) {
            if (write_elmt(STRING_ELT(elmt, i),
                           mat,
                           NULL,
                           NULL,
                           matvar,
                           0,
                           i,
                           0,
                           compression)) {
                Mat_VarFree(matvar);
                return 1;
            }
        }
    }

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write dgCMatrix
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_dgCMatrix(const SEXP elmt,
                mat_t *mat,
                const char *name,
                matvar_t *mat_struct,
                matvar_t *mat_cell,
                size_t field_index,
                size_t index,
                int compression)
{
    size_t dims[2];
    matvar_t *matvar;
    mat_sparse_t  sparse = {0,};

    if (Rf_isNull(elmt) || 2 != LENGTH(GET_SLOT(elmt, Rf_install("Dim"))))
        return 1;

    dims[0] = INTEGER(GET_SLOT(elmt, Rf_install("Dim")))[0];
    dims[1] = INTEGER(GET_SLOT(elmt, Rf_install("Dim")))[1];
    sparse.nzmax = LENGTH(GET_SLOT(elmt, Rf_install("i")));
    sparse.ir = INTEGER(GET_SLOT(elmt, Rf_install("i")));
    sparse.nir = LENGTH(GET_SLOT(elmt, Rf_install("i")));
    sparse.jc = INTEGER(GET_SLOT(elmt, Rf_install("p")));
    sparse.njc = LENGTH(GET_SLOT(elmt, Rf_install("p")));
    sparse.data = REAL(GET_SLOT(elmt, Rf_install("x")));
    sparse.ndata = LENGTH(GET_SLOT(elmt, Rf_install("x")));

    matvar = Mat_VarCreate(name,
                           MAT_C_SPARSE,
                           MAT_T_DOUBLE,
                           2,
                           dims,
                           &sparse,
                           0);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief Write lgCMatrix
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_lgCMatrix(const SEXP elmt,
                mat_t *mat,
                const char *name,
                matvar_t *mat_struct,
                matvar_t *mat_cell,
                size_t field_index,
                size_t index,
                int compression)
{
    size_t dims[2];
    matvar_t *matvar;
    mat_sparse_t  sparse = {0,};

    if (Rf_isNull(elmt) || 2 != LENGTH(GET_SLOT(elmt, Rf_install("Dim"))))
        return 1;

    dims[0] = INTEGER(GET_SLOT(elmt, Rf_install("Dim")))[0];
    dims[1] = INTEGER(GET_SLOT(elmt, Rf_install("Dim")))[1];
    sparse.nzmax = LENGTH(GET_SLOT(elmt, Rf_install("i")));
    sparse.ir = INTEGER(GET_SLOT(elmt, Rf_install("i")));
    sparse.nir = LENGTH(GET_SLOT(elmt, Rf_install("i")));
    sparse.jc = INTEGER(GET_SLOT(elmt, Rf_install("p")));
    sparse.njc = LENGTH(GET_SLOT(elmt, Rf_install("p")));
    sparse.ndata = LENGTH(GET_SLOT(elmt, Rf_install("x")));
    sparse.data = malloc(sparse.ndata*sizeof(mat_uint8_t));
    if (NULL == sparse.data)
        return 1;

    for (size_t i=0;i<sparse.ndata;i++)
        ((mat_uint8_t*)sparse.data)[i] =
            LOGICAL(GET_SLOT(elmt, Rf_install("x")))[i] != 0;

    matvar = Mat_VarCreate(name,
                           MAT_C_SPARSE,
                           MAT_T_UINT8,
                           2,
                           dims,
                           &sparse,
                           MAT_F_LOGICAL);

    free(sparse.data);

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param names
 * @param matvar
 * @param dims
 * @param ragged
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_ragged_data(SEXP elmt,
                  matvar_t *mat_struct,
                  matvar_t *mat_cell,
                  size_t len,
                  int compression)
{
    if (Rf_isNull(elmt))
        return 1;

    switch (TYPEOF(elmt)) {
    case STRSXP:
        for (size_t j=0;j<len;j++) {
            if (write_elmt(STRING_ELT(elmt, j),
                           NULL,
                           NULL,
                           NULL,
                           mat_cell,
                           0,
                           j,
                           0,
                           compression)) {
                return 1;
            }
        }
        break;
    case REALSXP:
    case INTSXP:
    case CPLXSXP:
    case LGLSXP:
    case VECSXP:
    case S4SXP:
        if (write_elmt(elmt,
                       NULL,
                       NULL,
                       NULL,
                       mat_cell,
                       0,
                       0,
                       0,
                       compression)) {
            return 1;
        }
        break;
    default:
        return 1;
    }

    return 0;
}

static int
write_ragged(const SEXP elmt,
             const SEXP names,
             matvar_t *matvar,
             int compression)
{
    size_t dims[2] = {0, 0};
    const int rank = 2;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || NULL == matvar)
        return 1;

    for (size_t i=0;i<LENGTH(elmt);i++) {
        matvar_t *cell;
        const char *fieldname = NULL;

        if (map_R_object_dims(VECTOR_ELT(elmt, i), dims))
            return 1;

        if (!Rf_isNull(names))
            fieldname = CHAR(STRING_ELT(names, i));

        cell = Mat_VarCreate(fieldname,
                             MAT_C_CELL,
                             MAT_T_CELL,
                             rank,
                             dims,
                             NULL,
                             0);

        if (NULL == cell)
            return 1;
        if (Rf_isNull(names))
            Mat_VarSetCell(matvar, i, cell);
        else
            Mat_VarSetStructFieldByIndex(matvar, i, 0, cell);

        write_ragged_data(VECTOR_ELT(elmt, i),
                          NULL,
                          cell,
                          dims[0],
                          compression);
    }

    return 0;
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param names
 * @param matvar
 * @param dims
 * @param ragged
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_vecsxp_data(const SEXP elmt,
                       matvar_t *mat_struct,
                       matvar_t *mat_cell,
                       size_t *dims,
                       int ragged,
                       int compression)
{
    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || !LENGTH(elmt) || NULL == dims)
        return 1;

    for (size_t i=0;i<LENGTH(elmt);i++) {
        size_t len;

        if (NULL == mat_struct)
            len = dims[1];
        else
            len = dims[0];

        for (size_t j=0;j<len;j++) {
            size_t field_index, index;
            SEXP item;

            item = VECTOR_ELT(elmt, i);
            if (VECSXP == TYPEOF(item)) {
                if ((mat_struct && VECSXP != TYPEOF(VECTOR_ELT(item, j)))
                    || (mat_cell && Rf_isNull(Rf_getAttrib(item, R_NamesSymbol))))
                    item = VECTOR_ELT(item, j);
            }

            if (NULL == mat_struct) {
                field_index = 0;
                index = j*dims[0]+i;
            } else {
                field_index = i;
                index = j;
            }

            if (write_elmt(item,
                           NULL,
                           NULL,
                           mat_struct,
                           mat_cell,
                           field_index,
                           index,
                           ragged,
                           compression)) {
                return 1;
            }
        }
    }

    return 0;
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat_cell
 * @return 0 on succes or 1 on failure.
 */
static int
write_cell_array_with_empty_arrays(const SEXP elmt, matvar_t *mat_cell)
{
    SEXP names;
    size_t len;
    size_t dims[2];
    size_t dims_0_1[2] = {0, 1};
    size_t dims_1_1[2] = {1, 1};
    matvar_t *cell;
    matvar_t *field;
    const int rank = 2;
    const char **fieldnames = NULL;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || !LENGTH(elmt) ||
        !Rf_isNull(Rf_getAttrib(elmt, R_NamesSymbol)))
        return 1;

    len = LENGTH(elmt);

    for (size_t i=0;i<len;i++) {
        SEXP item = VECTOR_ELT(elmt, i);
        if (TYPEOF(item) != VECSXP && LENGTH(item))
            return 1;

        switch (TYPEOF(item)) {
        case REALSXP:
        case INTSXP:
        case CPLXSXP:
        case LGLSXP:
        case STRSXP:
            cell = Mat_VarCreateEmpty(item);
            if(NULL == cell)
                return 1;
            break;

        case VECSXP:
            names = Rf_getAttrib(item, R_NamesSymbol);
            if (!Rf_isNull(names)) {
                if (LENGTH(item)) {
                    dims[0] = 1;
                    dims[1] = LENGTH(item);
                    fieldnames = malloc(LENGTH(item)*sizeof(char*));
                    if (NULL == fieldnames)
                        return 1;

                    for (size_t j=0;j<LENGTH(item);j++) {
                        fieldnames[j] = CHAR(STRING_ELT(names, j));
                        if (VECSXP == TYPEOF(VECTOR_ELT(item, j))) {
                            dims[0] = 0;
                            dims[1] = 1;
                        }
                    }

                    cell = Mat_VarCreateStruct(NULL,
                                               rank,
                                               dims,
                                               fieldnames,
                                               LENGTH(item));
                    free(fieldnames);
                    if (NULL == cell)
                        return 1;

                    if (1 == dims[0]) {
                        for (size_t j=0;j<LENGTH(item);j++) {
                            switch (TYPEOF(VECTOR_ELT(item, j))) {
                            case REALSXP:
                            case INTSXP:
                            case CPLXSXP:
                            case LGLSXP:
                            case STRSXP:
                                field = Mat_VarCreateEmpty(VECTOR_ELT(item, j));
                                if(NULL == field)
                                    return 1;
                                break;

                            default:
                                return 1;
                            }

                            Mat_VarSetStructFieldByIndex(cell, j, 0, field);
                        }
                    }
                } else {
                    cell = Mat_VarCreateStruct(NULL, rank, dims_1_1, NULL, 0);
                }
            } else {
                cell = Mat_VarCreate(NULL,
                                     MAT_C_CELL,
                                     MAT_T_CELL,
                                     rank,
                                     dims_0_1,
                                     NULL,
                                     0);
            }
            break;

        default:
            return 1;
        }

        if (NULL == cell)
            return 1;

        Mat_VarSetCell(mat_cell, i, cell);
    }

    return 0;
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_vecsxp_as_cell(const SEXP elmt,
                     mat_t *mat,
                     const char *name,
                     matvar_t *mat_struct,
                     matvar_t *mat_cell,
                     size_t field_index,
                     size_t index,
                     int compression)

{
    size_t dims[2] = {1, 1};
    matvar_t *matvar;
    const int rank = 2;
    int err = 1;
    int empty, ragged;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt))
        return 1;

    if (check_ragged(elmt, &ragged))
        return 1;

    if (ragged) {
        dims[0] = LENGTH(elmt);
    } else if (!LENGTH(elmt)) {
        /* Check if the target is an empty cell array, in that case no
         * more work to do. */
        if (NULL != mat_cell
            && 0 == mat_cell->dims[0]
            && 1 == mat_cell->dims[1])
            return 0;

        dims[0] = 0;
        dims[1] = 0;
    } else if (map_R_vecsxp_dims(elmt, dims, &empty)) {
        return 1;
    }

    matvar = Mat_VarCreate(name, MAT_C_CELL, MAT_T_CELL, rank, dims, NULL, 0);
    if (NULL == matvar)
        return 1;

    if (ragged) {
        err = write_ragged(elmt, R_NilValue, matvar, compression);
    } else if (dims[0] == 0 && dims[1] == 0) {
        err = 0;
    } else if (dims[0] && dims[1]) {
        if(empty)
            err = write_cell_array_with_empty_arrays(elmt, matvar);
        else
            err = write_vecsxp_data(elmt,
                                    NULL,
                                    matvar,
                                    dims,
                                    ragged,
                                    compression);
    }

    if (err) {
        Mat_VarFree(matvar);
        return 1;
    }

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index,
                        compression);
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param names
 * @param mat_struct
 * @return 0 on succes or 1 on failure.
 */
static int
write_structure_array_with_empty_fields(const SEXP elmt,
                                        const SEXP names,
                                        matvar_t *mat_struct)
{
    size_t len;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || !LENGTH(elmt) || Rf_isNull(names))
        return 1;

    len = LENGTH(elmt);
    for (size_t field_index=0;field_index<len;field_index++) {
        matvar_t *field;
        SEXP field_elmt = VECTOR_ELT(elmt, field_index);
        if (LENGTH(field_elmt))
            return 1;
        field = Mat_VarCreateEmpty(field_elmt);
        if(NULL == field)
            return 1;
        Mat_VarSetStructFieldByIndex(mat_struct, field_index, 0, field);
    }

    return 0;
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param names
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_vecsxp_as_struct(const SEXP elmt,
                       const SEXP names,
                       mat_t *mat,
                       const char *name,
                       matvar_t *mat_struct,
                       matvar_t *mat_cell,
                       size_t field_index,
                       size_t index,
                       int compression)
{
    size_t dims[2] = {1, 1};
    size_t nfields;
    matvar_t *matvar;
    const int rank = 2;
    const char **fieldnames = NULL;
    int err = 1;
    int empty, ragged;

    if (Rf_isNull(elmt) || VECSXP != TYPEOF(elmt) || Rf_isNull(names))
        return 1;

    if (check_ragged(elmt, &ragged))
        return 1;

    nfields = LENGTH(elmt);
    if (!ragged) {
        if (nfields && map_R_vecsxp_dims(elmt, dims, &empty))
            return 1;
    }

    if (nfields) {
        fieldnames = malloc(nfields*sizeof(char*));
        if (NULL == fieldnames)
            return 1;
        for (size_t i=0;i<nfields;i++)
            fieldnames[i] = CHAR(STRING_ELT(names, i));
    }

    matvar = Mat_VarCreateStruct(name,
                                 rank,
                                 dims,
                                 fieldnames,
                                 nfields);

    if (fieldnames)
        free(fieldnames);

    if (NULL == matvar)
        return 1;

    if (ragged) {
        err = write_ragged(elmt, names, matvar, compression);
    } else if (nfields && dims[0] && dims[1]) {
        if (empty)
            err = write_structure_array_with_empty_fields(elmt, names, matvar);
        else
            err = write_vecsxp_data(elmt,
                                    matvar,
                                    NULL,
                                    dims,
                                    ragged,
                                    compression);
    } else if (nfields == 0 && dims[0] == 1 && dims[1] == 1) {
        /* Empty structure array */
        err = 0;
    } else if (nfields && dims[0] == 0 && dims[1] == 1) {
        /* Empty structure array with fields */
        err = 0;
    }

    if (err) {
        Mat_VarFree(matvar);
        return 1;
    }

    return write_matvar(mat,
                        matvar,
                        mat_struct,
                        mat_cell,
                        field_index,
                        index, compression);
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_vecsxp(const SEXP elmt,
             mat_t *mat,
             const char *name,
             matvar_t *mat_struct,
             matvar_t *mat_cell,
             size_t field_index,
             size_t index,
             int compression)
{
    int error;
    SEXP names;

    PROTECT(names = Rf_getAttrib(elmt, R_NamesSymbol));
    if (Rf_isNull(names)) {
        error = write_vecsxp_as_cell(
            elmt,
            mat,
            name,
            mat_struct,
            mat_cell,
            field_index,
            index,
            compression);
    } else {
        error = write_vecsxp_as_struct(
            elmt,
            names,
            mat,
            name,
            mat_struct,
            mat_cell,
            field_index,
            index,
            compression);
    }

    UNPROTECT(1);

    return error;
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param elmt R object to write
 * @param mat MAT file pointer. If mat_struct and mat_cell
 *  equals NULL, then the matvar data are written to the mat
 *  file.
 * @param name Name of the variable to write
 * @param mat_struct MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_struct.
 * @param mat_cell MAT variable pointer to a struct field.
 *  If non-zero, the matvar data is written to the mat_cell.
 * @param field_index
 * @param index
 * @param compression Write the file with compression or not
 * @return 0 on succes or 1 on failure.
 */
static int
write_elmt(const SEXP elmt,
           mat_t *mat,
           const char *name,
           matvar_t *mat_struct,
           matvar_t *mat_cell,
           size_t field_index,
           size_t index,
           int ragged,
           int compression)
{
    SEXP class_name;

    if (Rf_isNull(elmt))
        return 0;

    switch (TYPEOF(elmt)) {
    case CHARSXP:
        return write_charsxp(elmt,
                             mat,
                             name,
                             mat_struct,
                             mat_cell,
                             field_index,
                             index,
                             compression);
    case REALSXP:
        return write_realsxp(elmt,
                             mat,
                             name,
                             mat_struct,
                             mat_cell,
                             field_index,
                             index,
                             compression);
    case INTSXP:
        return write_intsxp(elmt,
                            mat,
                            name,
                            mat_struct,
                            mat_cell,
                            field_index,
                            index,
                            compression);
    case CPLXSXP:
        return write_cplxsxp(elmt,
                             mat,
                             name,
                             mat_struct,
                             mat_cell,
                             field_index,
                             index,
                             compression);
    case LGLSXP:
        return write_lglsxp(elmt,
                            mat,
                            name,
                            mat_struct,
                            mat_cell,
                            field_index,
                            index,
                            compression);
    case STRSXP:
        return write_strsxp(elmt,
                            mat,
                            name,
                            mat_struct,
                            mat_cell,
                            field_index,
                            index,
                            ragged,
                            compression);
    case VECSXP:
        return write_vecsxp(elmt,
                            mat,
                            name,
                            mat_struct,
                            mat_cell,
                            field_index,
                            index,
                            compression);
    case S4SXP:
        class_name = Rf_getAttrib(elmt, R_ClassSymbol);
        if (strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0)
            return write_dgCMatrix(elmt,
                                   mat,
                                   name,
                                   mat_struct,
                                   mat_cell,
                                   field_index,
                                   index,
                                   compression);
        else if (strcmp(CHAR(STRING_ELT(class_name, 0)), "lgCMatrix") == 0)
            return write_lgCMatrix(elmt,
                                   mat,
                                   name,
                                   mat_struct,
                                   mat_cell,
                                   field_index,
                                   index,
                                   compression);
        return 1;
    default:
        return 1;
    }
}

/** @brief
 *
 *
 * @ingroup rmatio
 * @param m
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
set_dim(SEXP m,
        matvar_t *matvar)
{
    SEXP dim;

    /* Assign dimension to the allocated vector, if not   */
    /* the rank is two and one of the dimensions is <= 1  */
    if (!(matvar->rank == 2 && (matvar->dims[0] <= 1 || matvar->dims[1] <= 1))) {
        PROTECT(dim = Rf_allocVector(INTSXP, matvar->rank));
        for (size_t j=0;j<matvar->rank;j++)
            INTEGER(dim)[j] = matvar->dims[j];
        Rf_setAttrib(m, R_DimSymbol, dim);
        UNPROTECT(1);
    }

    return 0;
}

/** @brief Read character data
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_mat_char(SEXP list,
              int index,
              matvar_t *matvar)
{
    SEXP c;

    if (NULL == matvar
        || 2 != matvar->rank
        || NULL == matvar->dims
        || (matvar->dims[1] && NULL == matvar->data)
        || matvar->isComplex)
        return 1;

    PROTECT(c = Rf_allocVector(STRSXP, matvar->dims[0]));

    switch (matvar->data_type) {
    case MAT_T_UINT8:
    case MAT_T_UNKNOWN:
    {
        char *buf = malloc((matvar->dims[1]+1)*sizeof(char));
        if (NULL == buf) {
            UNPROTECT(1);
            return 1;
        }

        for (size_t i=0;i<matvar->dims[0];i++) {
            for (size_t j=0;j<matvar->dims[1];j++)
                buf[j] = ((char*)matvar->data)[matvar->dims[0]*j + i];
            buf[matvar->dims[1]] = 0;
            SET_STRING_ELT(c, i, Rf_mkChar(buf));
        }
        free(buf);
        break;
    }
    default:
        UNPROTECT(1);
        return 1;
    }

    SET_VECTOR_ELT(list, index, c);
    UNPROTECT(1);

    return 0;
}

/*
 * -------------------------------------------------------------
 *   Read functions
 * -------------------------------------------------------------
 */

/** @brief Read sparse data
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_sparse(SEXP list,
            int index,
            matvar_t *matvar)
{
    SEXP m, data;
    int *dims;
    mat_sparse_t *sparse;

    if (NULL == matvar
        || 2 != matvar->rank
        || NULL == matvar->dims
        || NULL == matvar->data)
        return 1;

    sparse = matvar->data;
    if (NULL == sparse->ir || NULL == sparse->jc)
        return 1;

    if (matvar->isComplex)  {
        size_t len;
        mat_complex_split_t *complex_data;

        complex_data = sparse->data;
        if (NULL == complex_data->Im || NULL == complex_data->Re)
            return 1;

        len = matvar->dims[0] * matvar->dims[1];
        PROTECT(m = Rf_allocVector(CPLXSXP, len));

        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = 0;
            COMPLEX(m)[j].i = 0;
        }

        for (size_t j=0,k=0;j<matvar->dims[1];j++) {
            while (k<sparse->jc[j+1]) {
                COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].r =
                    ((double*)complex_data->Re)[k];
                COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].i =
                    ((double*)complex_data->Im)[k];
                k++;
            }
        }

        if (set_dim(m, matvar)) {
            UNPROTECT(1);
            return 1;
        }
    } else {
        SEXP ir, jc, cls;
        int *ir_ptr;  /* Array of size nnzero where ir_ptr[k] is the
                       * row of data[k] */
        int *jc_ptr;  /* Array of size ncol+1, jc_ptr[k] index to data
                       * of first non-zero element in row k */

        if (matvar->isLogical)
            PROTECT(cls = MAKE_CLASS("lgCMatrix"));
        else
            PROTECT(cls = MAKE_CLASS("dgCMatrix"));
        PROTECT(m = NEW_OBJECT(cls));
        UNPROTECT(1);

        dims = INTEGER(GET_SLOT(m, Rf_install("Dim")));
        dims[0] = matvar->dims[0];
        dims[1] = matvar->dims[1];

        PROTECT(ir = Rf_allocVector(INTSXP, sparse->nir));
        SET_SLOT(m, Rf_install("i"), ir);
        ir_ptr = INTEGER(ir);
        for (int j=0; j<sparse->nir; ++j)
            ir_ptr[j] = sparse->ir[j];
        UNPROTECT(1);

        PROTECT(jc = Rf_allocVector(INTSXP, sparse->njc));
        SET_SLOT(m, Rf_install("p"), jc);
        jc_ptr = INTEGER(jc);
        for (int j=0; j<sparse->njc; ++j)
            jc_ptr[j] = sparse->jc[j];
        UNPROTECT(1);

        if (matvar->isLogical) {
            int *data_ptr;
            PROTECT(data = Rf_allocVector(LGLSXP, sparse->nir));
            SET_SLOT(m, Rf_install("x"), data);
            data_ptr = LOGICAL(data);
            for (int j=0; j<sparse->nir; ++j)
                data_ptr[j] = 1;
            UNPROTECT(1);
        } else {
            double *data_ptr;
            PROTECT(data = Rf_allocVector(REALSXP, sparse->ndata));
            SET_SLOT(m, Rf_install("x"), data);
            data_ptr = REAL(data);
            for (int j=0; j<sparse->ndata; ++j)
                data_ptr[j] = ((double*)sparse->data)[j];
            UNPROTECT(1);
        }
    }

    SET_VECTOR_ELT(list, index, m);
    UNPROTECT(1);

    return 0;
}

/** @brief Read complex data
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_mat_complex(SEXP list,
                 int index,
                 matvar_t *matvar)
{
    SEXP m;
    size_t len;
    mat_complex_split_t *complex_data;

    if (NULL == matvar
        || 2 > matvar->rank
        || NULL == matvar->dims
        || NULL == matvar->data
        || !matvar->isComplex)
        return 1;

    complex_data = matvar->data;
    if (NULL == complex_data->Im || NULL == complex_data->Re)
        return 1;

    len = matvar->dims[0];
    for (size_t j=1;j<matvar->rank;j++)
        len *= matvar->dims[j];

    PROTECT(m = Rf_allocVector(CPLXSXP, len));

    switch (matvar->data_type) {
    case MAT_T_SINGLE:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((float*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((float*)complex_data->Im)[j];
        }
        break;

    case MAT_T_DOUBLE:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((double*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((double*)complex_data->Im)[j];
        }
        break;

    case MAT_T_INT64:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_int64_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_int64_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_INT32:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_int32_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_int32_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_INT16:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_int16_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_int16_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_INT8:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_int8_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_int8_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_UINT64:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_uint64_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_uint64_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_UINT32:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_uint32_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_uint32_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_UINT16:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_uint16_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_uint16_t*)complex_data->Im)[j];
        }
        break;

    case MAT_T_UINT8:
        for (size_t j=0;j<len;j++) {
            COMPLEX(m)[j].r = ((mat_uint8_t*)complex_data->Re)[j];
            COMPLEX(m)[j].i = ((mat_uint8_t*)complex_data->Im)[j];
        }
        break;

    default:
        UNPROTECT(1);
        return 1;
    }

    if (set_dim(m, matvar)) {
        UNPROTECT(1);
        return 1;
    }

    SET_VECTOR_ELT(list, index, m);
    UNPROTECT(1);

    return 0;
}

/** @brief Read data
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_mat_data(SEXP list,
              int index,
              matvar_t *matvar)
{
    SEXP m;
    size_t len;

    if (NULL == matvar
        || 2 > matvar->rank
        || NULL == matvar->dims
        || NULL == matvar->data
        || matvar->isComplex)
        return 1;

    len = matvar->dims[0];
    for (size_t j=1;j<matvar->rank;j++)
        len *= matvar->dims[j];

    switch (matvar->data_type) {
    case MAT_T_SINGLE:
        PROTECT(m = Rf_allocVector(REALSXP, len));
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((float*)matvar->data)[j];
        break;

    case MAT_T_DOUBLE:
        PROTECT(m = Rf_allocVector(REALSXP, len));
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((double*)matvar->data)[j];
        break;

    case MAT_T_INT64:
        PROTECT(m = Rf_allocVector(REALSXP, len));
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_int64_t*)matvar->data)[j];
        break;

    case MAT_T_INT32:
        PROTECT(m = Rf_allocVector(INTSXP, len));
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int32_t*)matvar->data)[j];
        break;

    case MAT_T_INT16:
        PROTECT(m = Rf_allocVector(INTSXP, len));
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int16_t*)matvar->data)[j];
        break;

    case MAT_T_INT8:
        PROTECT(m = Rf_allocVector(INTSXP, len));
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int8_t*)matvar->data)[j];
        break;

    case MAT_T_UINT64:
        PROTECT(m = Rf_allocVector(REALSXP, len));
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_uint64_t*)matvar->data)[j];
        break;

    case MAT_T_UINT32:
        PROTECT(m = Rf_allocVector(REALSXP, len));
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_uint32_t*)matvar->data)[j];
        break;

    case MAT_T_UINT16:
        PROTECT(m = Rf_allocVector(INTSXP, len));
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_uint16_t*)matvar->data)[j];
        break;

    case MAT_T_UINT8:
        PROTECT(m = Rf_allocVector(INTSXP, len));
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_uint8_t*)matvar->data)[j];
        break;

    default:
        return 1;
    }

    if (set_dim(m, matvar)) {
        UNPROTECT(1);
        return 1;
    }

    SET_VECTOR_ELT(list, index, m);
    UNPROTECT(1);

    return 0;
}

/** @brief Read logical data
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_logical(SEXP list,
             int index,
             matvar_t *matvar)
{
    SEXP m;
    size_t len;

    if (NULL == matvar
        || 2 > matvar->rank
        || NULL == matvar->dims
        || NULL == matvar->data
        || !matvar->isLogical
        || MAT_T_UINT8 != matvar->data_type)
        return 1;

    len = matvar->dims[0];
    for (size_t j=1;j<matvar->rank;j++)
        len *= matvar->dims[j];

    PROTECT(m = Rf_allocVector(LGLSXP, len));
    for (size_t j=0;j<len;j++)
        LOGICAL(m)[j] = (0 != ((mat_uint8_t*)matvar->data)[j]);

    if (set_dim(m, matvar)) {
        UNPROTECT(1);
        return 1;
    }

    SET_VECTOR_ELT(list, index, m);
    UNPROTECT(1);

    return 0;
}

/*
 * -------------------------------------------------------------
 *   Read structure arrays
 * -------------------------------------------------------------
 */

/** @brief Read empty structure array
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_empty_structure_array(SEXP list,
                           int index,
                           matvar_t *matvar)
{
    SEXP names;
    SEXP struc;

    if (NULL == matvar
        || matvar->class_type != MAT_C_STRUCT
        || 2 != matvar->rank
        || 1 != matvar->dims[0]
        || 1 != matvar->dims[1]
        || Mat_VarGetNumberOfFields(matvar))
        return 1;

    PROTECT(struc = Rf_allocVector(VECSXP, 0));
    PROTECT(names = Rf_allocVector(STRSXP, 0));
    Rf_setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);
    UNPROTECT(2);

    return 0;
}

/** @brief Read empty structure array with fields
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_empty_structure_array_with_fields(SEXP list,
                                       int index,
                                       matvar_t *matvar)
{
    SEXP names;
    SEXP struc;
    size_t nfields = 0;
    char * const * fieldnames;
    int err = 0;

    if (NULL == matvar
        || MAT_C_STRUCT != matvar->class_type
        || 2 != matvar->rank
        || 0 != matvar->dims[0]
        || 1 != matvar->dims[1])
        return 1;

    nfields = Mat_VarGetNumberOfFields(matvar);
    if (!nfields)
        return 1;

    fieldnames = Mat_VarGetStructFieldnames(matvar);
    PROTECT(struc = Rf_allocVector(VECSXP, nfields));
    PROTECT(names = Rf_allocVector(STRSXP, nfields));

    for (size_t i=0;i<nfields;i++) {
        SEXP s;

        if (!fieldnames[i]) {
            err = 1;
            goto cleanup;
        }
        SET_STRING_ELT(names, i, Rf_mkChar(fieldnames[i]));
        PROTECT(s = Rf_allocVector(VECSXP, 0));
        SET_VECTOR_ELT(struc, i, s);
        UNPROTECT(1);
    }

    Rf_setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);

cleanup:
    UNPROTECT(2);

    return err;
}

/** @brief Read structure array with empty fields
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_structure_array_with_empty_fields(SEXP list,
                                       int index,
                                       matvar_t *matvar)
{
    SEXP names;
    SEXP struc;
    char * const * field_names;
    int err = 0;

    if (NULL == matvar
        || matvar->class_type != MAT_C_STRUCT
        || 2 != matvar->rank
        || 1 != matvar->dims[0]
        || 1 > matvar->dims[1])
        return 1;

    field_names = Mat_VarGetStructFieldnames(matvar);
    PROTECT(struc = Rf_allocVector(VECSXP, matvar->dims[1]));
    PROTECT(names = Rf_allocVector(STRSXP, matvar->dims[1]));

    for (size_t i=0;i<matvar->dims[1];i++) {
        SEXP s;
        matvar_t *field = NULL;

        if (!field_names[i]) {
            err = 1;
            goto cleanup;
        }
        SET_STRING_ELT(names, i, Rf_mkChar(field_names[i]));

        field = Mat_VarGetStructFieldByIndex(matvar, i, 0);
        if (NULL == field) {
            err = 1;
            goto cleanup;
        }

        if (field->isComplex) {
            PROTECT(s = Rf_allocVector(CPLXSXP, 0));
        } else if (field->isLogical) {
            PROTECT(s = Rf_allocVector(LGLSXP, 0));
        } else {
            switch (field->class_type) {
            case MAT_C_CHAR:
                PROTECT(s = Rf_allocVector(STRSXP, 0));
                break;

            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
                PROTECT(s = Rf_allocVector(REALSXP, 0));
                break;

            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                PROTECT(s = Rf_allocVector(INTSXP, 0));
                break;

            default:
                err = 1;
                goto cleanup;
            }
        }

        SET_VECTOR_ELT(struc, i, s);
        UNPROTECT(1);
    }

    Rf_setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);

cleanup:
    UNPROTECT(2);

    return err;
}

/** @brief Read structure array with fields
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_structure_array_with_fields(SEXP list,
                                 int index,
                                 matvar_t *matvar)
{
    SEXP names;
    SEXP struc;
    char * const * fieldnames;
    size_t nfields = 0;
    size_t fieldlen = 0;
    int err = 0;
    int protected = 0;

    if (NULL == matvar
        || MAT_C_STRUCT != matvar->class_type
        || 2 != matvar->rank
        || NULL == matvar->dims)
        return 1;

    nfields = Mat_VarGetNumberOfFields(matvar);
    if (!nfields)
        return 1;

    /* Check that one dimension == 1 and that the other dimension >= 1 */
    if (1 == matvar->dims[0]) {
        if (1 > matvar->dims[1])
            return 1;
        fieldlen = matvar->dims[1];
    } else if (1 == matvar->dims[1]) {
        if (1 > matvar->dims[0])
            return 1;
        fieldlen = matvar->dims[0];
    } else {
        return 1;
    }

    fieldnames = Mat_VarGetStructFieldnames(matvar);
    PROTECT(struc = Rf_allocVector(VECSXP, nfields));
    protected++;
    PROTECT(names = Rf_allocVector(STRSXP, nfields));
    protected++;

    for (size_t i=0;i<nfields;i++) {
        SEXP s = R_NilValue;

        if (fieldnames[i])
            SET_STRING_ELT(names, i, Rf_mkChar(fieldnames[i]));

        switch (Mat_VarGetStructFieldByIndex(matvar, i, 0)->class_type) {
        case MAT_C_CHAR:
            PROTECT(s = Rf_allocVector(STRSXP, fieldlen));
            protected++;
            break;

        case MAT_C_CELL:
        case MAT_C_STRUCT:
            s = R_NilValue;
            break;

        default:
            PROTECT(s = Rf_allocVector(VECSXP, fieldlen));
            protected++;
            break;
        }

        for (size_t j=0;j<fieldlen;j++) {
            matvar_t *field = Mat_VarGetStructFieldByIndex(matvar, i, j);
            if (NULL == field) {
                err = 1;
                goto cleanup;
            }

            switch (field->class_type) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                if (field->isLogical)
                    err = read_logical(s, j, field);
                else if (field->isComplex)
                    err = read_mat_complex(s, j, field);
                else
                    err = read_mat_data(s, j, field);
                break;

            case MAT_C_SPARSE:
                err = read_sparse(s, j, field);
                break;

            case MAT_C_CHAR:
                switch (field->data_type) {
                case MAT_T_UINT8:
                case MAT_T_UNKNOWN:
                {
                    char* buf = (char*)malloc((field->dims[1]+1)*sizeof(char));
                    if (NULL == buf) {
                        err = 1;
                        goto cleanup;
                    }
                    for (size_t k=0;k<field->dims[1];k++)
                        buf[k] = ((char*)field->data)[k];
                    buf[field->dims[1]] = 0;
                    SET_STRING_ELT(s, j, Rf_mkChar(buf));
                    free(buf);
                    break;
                }

                default:
                    err = 1;
                    goto cleanup;
                }
                break;

            case MAT_C_CELL:
                err = read_mat_cell(struc, i, field);
                break;

            case MAT_C_STRUCT:
                err = read_mat_struct(struc, i, field);
                break;

            case MAT_C_EMPTY:
                err = 0;
                break;

            case MAT_C_FUNCTION:
            case MAT_C_OPAQUE:
                err = 0;
                Rf_warning("Function class type read as NULL: %s", fieldnames[i]);
                break;

            default:
                err = 1;
                goto cleanup;
            }

            if (err)
                goto cleanup;
        }

        if (!Rf_isNull(s)) {
            SET_VECTOR_ELT(struc, i, s);
            UNPROTECT(1);
            protected--;
        }
    }

    Rf_setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);

cleanup:
    if (protected)
        UNPROTECT(protected);

    return err;
}

/** @brief Read struct
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_mat_struct(SEXP list,
                int index,
                matvar_t *matvar)
{
    if (NULL == matvar
        || MAT_C_STRUCT != matvar->class_type
        || 2 != matvar->rank
        || NULL == matvar->dims)
        return 1;

    if (Mat_VarGetNumberOfFields(matvar)) {
        if (matvar->dims[0] == 0 && matvar->dims[1] == 1) {
            return read_empty_structure_array_with_fields(list,
                                                          index,
                                                          matvar);
        } else if (matvar->dims[0] && matvar->dims[1]) {
            matvar_t *field = Mat_VarGetStructFieldByIndex(matvar, 0, 0);
            if (field == NULL)
                return 1;

            if (!field->dims[0])
                return read_structure_array_with_empty_fields(list,
                                                              index,
                                                              matvar);
            else
                return read_structure_array_with_fields(list,
                                                        index,
                                                        matvar);
        }
    } else if (matvar->dims[0] == 1 && matvar->dims[1] == 1) {
        return read_empty_structure_array(list, index, matvar);
    }

    return 1;
}

/*
 * -------------------------------------------------------------
 *   Read cell arrays
 * -------------------------------------------------------------
 */

/** @brief Read empty cell array
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_empty_cell_array(SEXP list,
                      int index,
                      matvar_t *matvar)
{
    SEXP cell;

    if (NULL == matvar
        || MAT_C_CELL != matvar->class_type
        || MAT_T_CELL != matvar->data_type
        || 2 != matvar->rank
        || NULL == matvar->dims
        || 0 != matvar->dims[0])
        return 1;

    PROTECT(cell = Rf_allocVector(VECSXP, 0));
    SET_VECTOR_ELT(list, index, cell);
    UNPROTECT(1);

    return 0;
}

/** @brief Read cell array with empty arrays
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_cell_array_with_empty_arrays(SEXP list,
                                  int index,
                                  matvar_t *matvar)
{
    SEXP cell_array;
    char * const * fieldnames;
    int err = 0;
    int protected = 0;

    if (NULL == matvar
        || MAT_C_CELL != matvar->class_type
        || MAT_T_CELL != matvar->data_type
        || 2 != matvar->rank
        || NULL == matvar->dims
        || 1 != matvar->dims[0]
        || 1 > matvar->dims[1])
        return 1;

    PROTECT(cell_array = Rf_allocVector(VECSXP, matvar->dims[1]));
    protected++;

    for (size_t i=0;i<matvar->dims[1];i++) {
        SEXP cell_item;
        SEXP names;
        matvar_t *cell = Mat_VarGetCell(matvar, i);

        if (cell->isComplex) {
            PROTECT(cell_item = Rf_allocVector(CPLXSXP, 0));
            protected++;
        } else if (cell->isLogical) {
            PROTECT(cell_item = Rf_allocVector(LGLSXP, 0));
            protected++;
        } else {
            switch (cell->class_type) {
            case MAT_C_CHAR:
                PROTECT(cell_item = Rf_allocVector(STRSXP, 0));
                protected++;
                break;

            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
                PROTECT(cell_item = Rf_allocVector(REALSXP, 0));
                protected++;
                break;

            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                PROTECT(cell_item = Rf_allocVector(INTSXP, 0));
                protected++;
                break;

            case MAT_C_STRUCT:
                if (cell->dims[0] == 0 && cell->dims[1] == 1) {
                    PROTECT(cell_item = Rf_allocVector(VECSXP,
                                                    Mat_VarGetNumberOfFields(cell)));
                    protected++;

                    PROTECT(names = Rf_allocVector(STRSXP,
                                                Mat_VarGetNumberOfFields(cell)));
                    Rf_setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);

                    fieldnames = Mat_VarGetStructFieldnames(cell);
                    for (size_t j=0;j<Mat_VarGetNumberOfFields(cell);j++) {
                        SEXP field_item;
                        SET_STRING_ELT(names, j, Rf_mkChar(fieldnames[j]));
                        PROTECT(field_item = Rf_allocVector(VECSXP, 0));
                        SET_VECTOR_ELT(cell_item, j, field_item);
                        UNPROTECT(1);
                    }
                } else if (cell->dims[0] == 1 && cell->dims[1] == 1) {
                    PROTECT(cell_item = Rf_allocVector(VECSXP, 0));
                    protected++;
                    PROTECT(names = Rf_allocVector(STRSXP, 0));
                    Rf_setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);
                } else if (cell->dims[0] == 1 && cell->dims[1] > 1) {
                    fieldnames = Mat_VarGetStructFieldnames(cell);
                    PROTECT(cell_item = Rf_allocVector(VECSXP, cell->dims[1]));
                    protected++;
                    PROTECT(names = Rf_allocVector(STRSXP, cell->dims[1]));
                    Rf_setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);

                    for (size_t j=0;j<cell->dims[1];j++) {
                        SEXP field_item;
                        matvar_t *field = Mat_VarGetStructFieldByIndex(cell, j, 0);
                        if (NULL == field) {
                            err = 1;
                            goto cleanup;
                        }

                        if (fieldnames[j])
                            SET_STRING_ELT(names, j, Rf_mkChar(fieldnames[j]));

                        if (field->isComplex) {
                            PROTECT(field_item = Rf_allocVector(CPLXSXP, 0));
                        } else if (field->isLogical) {
                            PROTECT(field_item = Rf_allocVector(LGLSXP, 0));
                        } else {
                            switch (field->class_type) {
                            case MAT_C_CHAR:
                                PROTECT(field_item = Rf_allocVector(STRSXP, 0));
                                break;

                            case MAT_C_DOUBLE:
                            case MAT_C_SINGLE:
                                PROTECT(field_item = Rf_allocVector(REALSXP, 0));
                                break;

                            case MAT_C_INT64:
                            case MAT_C_INT32:
                            case MAT_C_INT16:
                            case MAT_C_INT8:
                            case MAT_C_UINT64:
                            case MAT_C_UINT32:
                            case MAT_C_UINT16:
                            case MAT_C_UINT8:
                                PROTECT(field_item = Rf_allocVector(INTSXP, 0));
                                break;

                            default:
                                err = 1;
                                goto cleanup;
                            }
                        }

                        SET_VECTOR_ELT(cell_item, j, field_item);
                        UNPROTECT(1);
                    }
                } else {
                    err = 1;
                    goto cleanup;
                }
                break;

            case MAT_C_CELL:
                if (cell->dims[0] == 0 && cell->dims[1] == 1) {
                    PROTECT(cell_item = Rf_allocVector(VECSXP, 0));
                    protected++;
                } else {
                    err = 1;
                    goto cleanup;
                }
                break;

            default:
                err = 1;
                goto cleanup;
            }
        }

        SET_VECTOR_ELT(cell_array, i, cell_item);
        UNPROTECT(1);
        protected--;
    }

    SET_VECTOR_ELT(list, index, cell_array);

cleanup:
    if (protected)
        UNPROTECT(protected);

    return err;
}

/** @brief Read cell array with arrays
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_cell_array_with_arrays(SEXP list,
                            int index,
                            matvar_t *matvar)
{
    SEXP cell;
    int err = 0;
    int protected = 0;

    if (NULL == matvar || NULL == matvar->dims)
        return 1;

    PROTECT(cell = Rf_allocVector(VECSXP, matvar->dims[0]));
    protected++;

    for (size_t i=0;i<matvar->dims[0];i++) {
        SEXP cell_row = R_NilValue;
        if (matvar->dims[1] > 1) {
            PROTECT(cell_row = Rf_allocVector(VECSXP, matvar->dims[1]));
            protected++;
        }

        for (size_t j=0;j<matvar->dims[1];j++) {
            matvar_t *mat_cell = Mat_VarGetCell(matvar, j*matvar->dims[0] + i);
            if (NULL == mat_cell) {
                err = 1;
                goto cleanup;
            }

            switch (mat_cell->class_type) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                if (Rf_isNull(cell_row)) {
                    if (mat_cell->isLogical)
                        err = read_logical(cell, i, mat_cell);
                    else if (mat_cell->isComplex)
                        err = read_mat_complex(cell, i, mat_cell);
                    else
                        err = read_mat_data(cell, i, mat_cell);
                } else {
                    if (mat_cell->isLogical)
                        err = read_logical(cell_row, j, mat_cell);
                    else if (mat_cell->isComplex)
                        err = read_mat_complex(cell_row, j, mat_cell);
                    else
                        err = read_mat_data(cell_row, j, mat_cell);
                }
                break;

            case MAT_C_SPARSE:
                if (Rf_isNull(cell_row))
                    err = read_sparse(cell, i, mat_cell);
                else
                    err = read_sparse(cell_row, j, mat_cell);
                break;

            case MAT_C_CHAR:
                if (Rf_isNull(cell_row))
                    err = read_mat_char(cell, i, mat_cell);
                else
                    err = read_mat_char(cell_row, j, mat_cell);
                break;

            case MAT_C_STRUCT:
                if (Rf_isNull(cell_row))
                    err = read_mat_struct(cell, i, mat_cell);
                else
                    err = read_mat_struct(cell_row, j, mat_cell);
                break;

            case MAT_C_CELL:
                if (Rf_isNull(cell_row))
                    err = read_mat_cell(cell, i, mat_cell);
                else
                    err = read_mat_cell(cell_row, j, mat_cell);
                break;

            default:
                err = 1;
                break;
            }

            if (err)
                goto cleanup;
        }

        if (!Rf_isNull(cell_row)) {
            SET_VECTOR_ELT(cell, i, cell_row);
            UNPROTECT(1);
            protected--;
        }
    }

    SET_VECTOR_ELT(list, index, cell);

cleanup:
    if (protected)
        UNPROTECT(protected);

    return err;
}

/** @brief Read cell
 *
 *
 * @ingroup rmatio
 * @param list The list to hold the read data
 * @param index The position in the list where to store the read data
 * @param matvar MAT variable pointer
 * @return 0 on succes or 1 on failure.
 */
static int
read_mat_cell(SEXP list,
              int index,
              matvar_t *matvar)
{
    if (NULL == matvar
        || MAT_C_CELL != matvar->class_type
        || MAT_T_CELL != matvar->data_type
        || NULL == matvar->dims)
        return 1;

    if (0 == matvar->dims[0]) {
        return read_empty_cell_array(list, index, matvar);
    } else if (matvar->dims[0] && matvar->dims[1]) {
        matvar_t *cell = Mat_VarGetCell(matvar, 0);

        if (NULL == cell || NULL == cell->dims)
            return 1;

        if (MAT_C_CELL == cell->class_type
            && 0 == cell->dims[0]
            && 1 == cell->dims[1]) {
            return read_cell_array_with_empty_arrays(list, index, matvar);
        } else if (MAT_C_STRUCT == cell->class_type
                   && 1 == cell->dims[0]
                   && 1 == cell->dims[1]) {

            if(Mat_VarGetNumberOfFields(cell))
                return read_cell_array_with_arrays(list, index, matvar);
            else
                return read_cell_array_with_empty_arrays(list, index, matvar);
        } else if (cell->dims[0] && cell->dims[1]) {
            return read_cell_array_with_arrays(list, index, matvar);
        } else {
            return read_cell_array_with_empty_arrays(list, index, matvar);
        }
    }

    return 1;
}

/*
 * -------------------------------------------------------------
 *   Functions to interface R
 * -------------------------------------------------------------
 */

/** @brief Number of variables in MAT-file
 *
 *
 * @ingroup rmatio
 * @param mat MAT file pointer
 * @return 0 on succes or 1 on failure.
 */
static int
number_of_variables(mat_t *mat)
{
    int len=0;
    matvar_t *matvar;

    if (mat != NULL) {
        if (!Mat_Rewind(mat)) {
            while ((matvar = Mat_VarReadNextInfo(mat)) != NULL) {
                len++;
                Mat_VarFree(matvar);
                matvar = NULL;
            }
        }
    }

    return len;
}

/** @brief Read matlab file
 *
 *
 * @ingroup rmatio
 * @param filename The file to read
 * @return a named list (VECSXP).
 */
SEXP read_mat(const SEXP filename)
{
    mat_t *mat = NULL;
    matvar_t *matvar = NULL;
    int i = 0, n = 0, err = 0;
    SEXP list, names;

    const char err_reading_mat_file[] = "Error reading MAT file";
    const char err_mat_c_empty[] = "Not implemented support to read matio class type MAT_C_EMPTY";
    const char err_mat_c_object[] = "Not implemented support to read matio class type MAT_C_OBJECT";
    const char *err_msg = NULL;

    if (Rf_isNull(filename))
        Rf_error("'filename' equals R_NilValue.");
    if (!Rf_isString(filename))
        Rf_error("'filename' must be a string.");

    mat = Mat_Open(CHAR(STRING_ELT(filename, 0)), MAT_ACC_RDONLY);
    if (!mat)
        Rf_error("Unable to open file.");

    n = number_of_variables(mat);
    PROTECT(list = Rf_allocVector(VECSXP, n));
    PROTECT(names = Rf_allocVector(STRSXP, n));

    if (Mat_Rewind(mat)) {
        err = 1;
        err_msg = err_reading_mat_file;
        goto cleanup;
    }

    while ((matvar = Mat_VarReadNext(mat)) != NULL) {
        if (matvar->name != NULL)
            SET_STRING_ELT(names, i, Rf_mkChar(matvar->name));

        switch (matvar->class_type) {
        case MAT_C_EMPTY:
            err = 1;
            err_msg = err_mat_c_empty;
            goto cleanup;

        case MAT_C_CELL:
            err = read_mat_cell(list, i, matvar);
            break;

        case MAT_C_STRUCT:
            err = read_mat_struct(list, i, matvar);
            break;

        case MAT_C_OBJECT:
            err = 1;
            err_msg = err_mat_c_object;
            goto cleanup;

        case MAT_C_CHAR:
            err = read_mat_char(list, i, matvar);
            break;

        case MAT_C_SPARSE:
            err = read_sparse(list, i, matvar);
            break;

        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_INT32:
        case MAT_C_INT16:
        case MAT_C_INT8:
        case MAT_C_UINT64:
        case MAT_C_UINT32:
        case MAT_C_UINT16:
        case MAT_C_UINT8:
            if (matvar->isLogical)
                err = read_logical(list, i, matvar);
            else if (matvar->isComplex)
                err = read_mat_complex(list, i, matvar);
            else
                err = read_mat_data(list, i, matvar);
            break;

        case MAT_C_FUNCTION:
        case MAT_C_OPAQUE:
            err = 0;
            Rf_warning("Function class type read as NULL: %s",
                       matvar->name == NULL ? "" : matvar->name);
            break;

        default:
            err = 1;
            break;
        }

        if (err) {
            err_msg = err_reading_mat_file;
            goto cleanup;
        }

        Mat_VarFree(matvar);
        matvar = NULL;
        i++;
    }

    Rf_setAttrib(list, R_NamesSymbol, names);

cleanup:
    if (matvar)
        Mat_VarFree(matvar);
    if (mat)
        Mat_Close(mat);
    UNPROTECT(2);
    if (err)
        Rf_error(err_msg);

    return list;
}

/** @brief Write matlab file
 *
 *
 * @ingroup rmatio
 * @param list List of variables to write
 * @param filename Name of MAT file to create
 * @param version MAT file version to create
 * @param compression Write the file with compression or not
 * @return R_NilValue.
 */
SEXP
write_mat(const SEXP list,
          const SEXP filename,
          const SEXP compression,
          const SEXP version,
          const SEXP header)
{
    SEXP names;    /* names in list */
    mat_t *mat;
    int use_compression = MAT_COMPRESSION_NONE;

    if (Rf_isNull(list))
        Rf_error("'list' equals R_NilValue.");
    if (Rf_isNull(filename))
        Rf_error("'filename' equals R_NilValue.");
    if (Rf_isNull(compression))
        Rf_error("'compression' equals R_NilValue.");
    if (Rf_isNull(version))
        Rf_error("'version' equals R_NilValue.");
    if (Rf_isNull(header))
        Rf_error("'header' equals R_NilValue.");
    if (!Rf_isNewList(list))
        Rf_error("'list' must be a list.");
    if (!Rf_isString(filename))
        Rf_error("'filename' must be a string.");

    mat = Mat_CreateVer(CHAR(STRING_ELT(filename, 0)),
                        CHAR(STRING_ELT(header, 0)),
                        INTEGER(version)[0]);
    if (!mat)
        Rf_error("Unable to open file.");

    if (INTEGER(compression)[0])
        use_compression = MAT_COMPRESSION_ZLIB;

    PROTECT(names = Rf_getAttrib(list, R_NamesSymbol));

    for (int i = 0; i < Rf_length(list); i++) {
        if (write_elmt(VECTOR_ELT(list, i),
                       mat,
                       CHAR(STRING_ELT(names, i)),
                       NULL,
                       NULL,
                       0,
                       0,
                       0,
                       use_compression)) {
            Mat_Close(mat);
            Rf_error("Unable to write list");
        }
    }

    Mat_Close(mat);

    UNPROTECT(1);

    return R_NilValue;
}

static const R_CallMethodDef callMethods[] =
{
    {"read_mat", (DL_FUNC)&read_mat, 1},
    {"write_mat", (DL_FUNC)&write_mat, 5},
    {NULL, NULL, 0}
};

/** @brief Register routines to R
 *
 *
 * @ingroup rmatio
 * @param info Information about the DLL being loaded
 */
void
R_init_rmatio(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}
