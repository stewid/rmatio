/*
 * rmatio, a R interface to the C library matio, MAT File I/O Library.
 * Copyright (C) 2013  Stefan Widgren

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
#include "matio.h"

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
 * @ingroup
 * @param elmt
 * @param rank
 * @param dims
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_object_rank_and_dims(const SEXP elmt, int *rank, size_t **dims)
{
    if (R_NilValue == elmt
        || NULL == rank
        || NULL == dims)
        return 1;

    if (isNull(getAttrib(elmt, R_DimSymbol))) {
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt
 * @param empty
 * @return 0 on succes or 1 on failure.
 */
static int
map_vec_len(const SEXP elmt,
        int *len)
{
    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt))
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
            case S4SXP:
                if (first_lookup) {
                    if (getAttrib(elmt, R_NamesSymbol) != R_NilValue)
                        *len = LENGTH(item);
                    else
                        *len = LENGTH(elmt);
                    first_lookup = 0;
                } else if (getAttrib(elmt, R_NamesSymbol) != R_NilValue) {
                    if (*len != LENGTH(item))
                        return 1;
                } else if (*len != LENGTH(elmt)) {
                    return 1;
                }
                break;

            default:
                return 1;
            }
        }
    } else {
        *len = 0;
    }

    if (*len && getAttrib(elmt, R_NamesSymbol) != R_NilValue)
        *len = 1;

    return 0;
}

/** @brief Set the dim for a a cell or structure array from an R object
 *
 *
 * @ingroup
 * @param elmt
 * @param dims
 * @param empty
 * @param ragged
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_object_dims(const SEXP elmt, const SEXP names, size_t *dims)
{
    if (R_NilValue == elmt
        || NULL == dims)
        return 1;

    switch (TYPEOF(elmt)) {
    case VECSXP:
    {
        int tmp = 0;

        if (R_NilValue == getAttrib(elmt, R_NamesSymbol))
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
    case S4SXP:
        dims[0] = LENGTH(elmt) > 1;
        dims[1] = 1;
        break;
    default:
        return 1;
    }

    return 0;
}

/** @brief Set the dim for a a cell or structure array from an R object
 *
 *
 * @ingroup
 * @param elmt
 * @param dims
 * @param empty
 * @param ragged
 * @return 0 on succes or 1 on failure.
 */
static int
map_R_vecsxp_dims(const SEXP elmt,
                  size_t *dims,
                  int *empty)
{
    SEXP names;
    size_t len=0;
    int vecsxp = 0;

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || NULL == dims
        || NULL == empty)
        return 1;

    *empty = 0;
    names = getAttrib(elmt, R_NamesSymbol);

    if (LENGTH(elmt)) {
        for (int i=0;i<LENGTH(elmt);i++) {
            SEXP item = VECTOR_ELT(elmt, i);

            if (map_R_object_dims(item, names, dims))
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
        if (R_NilValue == names) {
            dims[0] = 0;
            dims[1] = 0;
        } else {
            dims[0] = 1;
            dims[1] = 1;
        }
    } else if (!len) {
        if (R_NilValue == names || !vecsxp) {
            dims[0] = 1;
            dims[1] = LENGTH(elmt);
            *empty = 1;
        } else {
            dims[0] = 0;
            dims[1] = 1;
        }
    } else if (R_NilValue == names) {
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
 * @ingroup
 * @param elmt R object to check
 * @return 1 if all lengths are equal else 0.
 */
static int
all_strings_have_equal_length(const SEXP elmt)
{
    size_t len;
    size_t n = LENGTH(elmt);

    if (n) {
        len = LENGTH(STRING_ELT(elmt, 0));
        for (size_t i=1;i<n;i++) {
            if (len != LENGTH(STRING_ELT(elmt, i)))
                return 0;
        }
    }

    return 1;
}

/** @brief Check if the R object VECSXP contains element of equal
 * length
 *
 *
 * @ingroup
 * @param elmt
 * @param ragged
 * @return 0 on succes or 1 on failure.
 */
static int
check_ragged(const SEXP elmt, int *ragged)
{
    size_t len=0;

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || NULL == ragged)
        return 1;

    *ragged = 0;

    if (LENGTH(elmt)) {
        for (int i=0;i<LENGTH(elmt);i++) {
            SEXP item = VECTOR_ELT(elmt, i);
            switch (TYPEOF(item)) {
            case VECSXP:
            {
                int tmp = 0;

                if (R_NilValue == getAttrib(item, R_NamesSymbol))
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
                    len = LENGTH(item);
                    *ragged = (0 == all_strings_have_equal_length(item));
                }
                break;

            case REALSXP:
            case INTSXP:
            case CPLXSXP:
            case LGLSXP:
            case S4SXP:
                if(!i)
                    len = LENGTH(item) > 1;
                else if(len != (LENGTH(item) > 1))
                    *ragged = 1;
                break;

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
 * @ingroup
 * @param elmt R object to create empty mat variable from
 * @return
 */
static matvar_t *
Mat_VarCreateEmpty(const SEXP elmt)
{
    size_t dims_0_1[2] = {0, 1};
    const int rank = 2;

    if (R_NilValue == elmt)
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

/** @brief
 *
 *
 * @ingroup
 * @param mat MAT file pointer
 * @param matvar
 * @param mat_struct
 * @param mat_cell
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || CHARSXP != TYPEOF(elmt))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || REALSXP != TYPEOF(elmt))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || INTSXP != TYPEOF(elmt))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || CPLXSXP != TYPEOF(elmt))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || LGLSXP != TYPEOF(elmt))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || STRSXP != TYPEOF(elmt)
        || !isNull(getAttrib(elmt, R_DimSymbol)))
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

    if (all_strings_have_equal_length(elmt)) {
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || 2 != LENGTH(GET_SLOT(elmt, Rf_install("Dim"))))
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

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || 2 != LENGTH(GET_SLOT(elmt, Rf_install("Dim"))))
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
 * @ingroup
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
    if (R_NilValue == elmt)
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

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || NULL == matvar)
        return 1;

    for (size_t i=0;i<LENGTH(elmt);i++) {
        matvar_t *cell;
        const char *fieldname = NULL;

        if (map_R_object_dims(VECTOR_ELT(elmt, i), names, dims))
            return 1;

        if (R_NilValue != names)
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
        if (R_NilValue == names)
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
 * @ingroup
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
    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || !LENGTH(elmt)
        || NULL == dims)
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
                    || (mat_cell && getAttrib(item, R_NamesSymbol) == R_NilValue))
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
 * @ingroup
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

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || !LENGTH(elmt)
        || R_NilValue != getAttrib(elmt, R_NamesSymbol))
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
            names = getAttrib(item, R_NamesSymbol);
            if (R_NilValue != names) {
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
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt))
        return 1;

    if (check_ragged(elmt, &ragged))
        return 1;

    if (ragged) {
        dims[0] = LENGTH(elmt);
    } else if (!LENGTH(elmt)) {
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
 * @ingroup
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

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || !LENGTH(elmt)
        || R_NilValue == names)
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
 * @ingroup
 * @param elmt R object to write
 * @param names
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt
        || VECSXP != TYPEOF(elmt)
        || R_NilValue == names)
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
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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
    SEXP names = R_NilValue;

    names = getAttrib(elmt, R_NamesSymbol);
    if (R_NilValue == names)
        return write_vecsxp_as_cell(elmt,
                                    mat,
                                    name,
                                    mat_struct,
                                    mat_cell,
                                    field_index,
                                    index,
                                    compression);

    return write_vecsxp_as_struct(elmt,
                                  names,
                                  mat,
                                  name,
                                  mat_struct,
                                  mat_cell,
                                  field_index,
                                  index,
                                  compression);
}

/** @brief
 *
 *
 * @ingroup
 * @param elmt R object to write
 * @param mat MAT file pointer
 * @param name Name of the variable to write
 * @param mat_struct
 * @param mat_cell
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

    if (R_NilValue == elmt)
        return 1;

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
        class_name = getAttrib(elmt, R_ClassSymbol);
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
 * @ingroup
 * @param m
 * @param matvar
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
        PROTECT(dim = allocVector(INTSXP, matvar->rank));
        if (R_NilValue == dim)
            return 1;
        for (size_t j=0;j<matvar->rank;j++)
            INTEGER(dim)[j] = matvar->dims[j];
        setAttrib(m, R_DimSymbol, dim);
        UNPROTECT(1);
    }

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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

    PROTECT(c = allocVector(STRSXP, matvar->dims[0]));
    if (R_NilValue == c)
        return 1;

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
            SET_STRING_ELT(c, i, mkChar(buf));
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

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_sparse(SEXP list,
            int index,
            matvar_t *matvar)
{
    SEXP m, data;
    int *dims;
    int *ir;              /* Array of size nnzero where ir[k] is the row of data[k] */
    int *jc;              /* Array of size ncol+1, jc[k] index to data of first non-zero element in row k */
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
        PROTECT(m = allocVector(CPLXSXP, len));
        if (R_NilValue == m)
            return 1;

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
        if (matvar->isLogical)
            PROTECT(m = NEW_OBJECT(MAKE_CLASS("lgCMatrix")));
        else
            PROTECT(m = NEW_OBJECT(MAKE_CLASS("dgCMatrix")));

        if (R_NilValue == m)
            return 1;

        dims = INTEGER(GET_SLOT(m, Rf_install("Dim")));
        dims[0] = matvar->dims[0];
        dims[1] = matvar->dims[1];

        SET_SLOT(m, Rf_install("i"), allocVector(INTSXP, sparse->nir));
        ir = INTEGER(GET_SLOT(m, Rf_install("i")));
        for (int j=0;j<sparse->nir;++j)
            ir[j] = sparse->ir[j];

        SET_SLOT(m, Rf_install("p"), allocVector(INTSXP, sparse->njc));
        jc = INTEGER(GET_SLOT(m, Rf_install("p")));
        for (int j=0;j<sparse->njc;++j)
            jc[j] = sparse->jc[j];

        if (matvar->isLogical) {
            SET_SLOT(m, Rf_install("x"), allocVector(LGLSXP, sparse->nir));
            data = GET_SLOT(m, Rf_install("x"));
            for (int j=0;j<sparse->nir;++j)
                LOGICAL(data)[j] = 1;
        } else {
            SET_SLOT(m, Rf_install("x"), allocVector(REALSXP, sparse->ndata));
            data = GET_SLOT(m, Rf_install("x"));
            for (int j=0;j<sparse->ndata;++j)
                REAL(data)[j] = ((double*)sparse->data)[j];
        }
    }

    SET_VECTOR_ELT(list, index, m);
    UNPROTECT(1);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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

    PROTECT(m = allocVector(CPLXSXP, len));
    if (R_NilValue == m)
        return 1;

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

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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
        PROTECT(m = allocVector(REALSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((float*)matvar->data)[j];
        break;

    case MAT_T_DOUBLE:
        PROTECT(m = allocVector(REALSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((double*)matvar->data)[j];
        break;

    case MAT_T_INT64:
        PROTECT(m = allocVector(REALSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_int64_t*)matvar->data)[j];
        break;

    case MAT_T_INT32:
        PROTECT(m = allocVector(INTSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int32_t*)matvar->data)[j];
        break;

    case MAT_T_INT16:
        PROTECT(m = allocVector(INTSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int16_t*)matvar->data)[j];
        break;

    case MAT_T_INT8:
        PROTECT(m = allocVector(INTSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_int8_t*)matvar->data)[j];
        break;

    case MAT_T_UINT64:
        PROTECT(m = allocVector(REALSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_uint64_t*)matvar->data)[j];
        break;

    case MAT_T_UINT32:
        PROTECT(m = allocVector(REALSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            REAL(m)[j] = ((mat_uint32_t*)matvar->data)[j];
        break;

    case MAT_T_UINT16:
        PROTECT(m = allocVector(INTSXP, len));
        if (R_NilValue == m)
            return 1;
        for (size_t j=0;j<len;j++)
            INTEGER(m)[j] = ((mat_uint16_t*)matvar->data)[j];
        break;

    case MAT_T_UINT8:
        PROTECT(m = allocVector(INTSXP, len));
        if (R_NilValue == m)
            return 1;
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

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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

    PROTECT(m = allocVector(LGLSXP, len));
    if (R_NilValue == m)
        return 1;
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

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_empty_structure_array(SEXP list,
                           int index,
                           matvar_t *matvar)
{
    SEXP names, struc;

    if (NULL == matvar
        || matvar->class_type != MAT_C_STRUCT
        || 2 != matvar->rank
        || 1 != matvar->dims[0]
        || 1 != matvar->dims[1]
        || Mat_VarGetNumberOfFields(matvar))
        return 1;

    PROTECT(struc = allocVector(VECSXP, 0));
    if (R_NilValue == struc)
        return 1;
    PROTECT(names = allocVector(STRSXP, 0));
    if (R_NilValue == names) {
        UNPROTECT(1);
        return 1;
    }
    setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);
    UNPROTECT(2);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_empty_structure_array_with_fields(SEXP list,
                                       int index,
                                       matvar_t *matvar)
{
    SEXP names, struc, s;
    size_t nfields;
    char * const * fieldnames;

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
    PROTECT(struc = allocVector(VECSXP, nfields));
    if (R_NilValue == struc)
        return 1;
    PROTECT(names = allocVector(STRSXP, nfields));
    if (R_NilValue == names) {
        UNPROTECT(1);
        return 1;
    }

    for (size_t i=0;i<nfields;i++) {
        if (fieldnames[i]) {
            SET_STRING_ELT(names, i, mkChar(fieldnames[i]));
        } else {
            UNPROTECT(2);
            return 1;
        }

        PROTECT(s = allocVector(VECSXP, 0));
        if (R_NilValue == s) {
            UNPROTECT(2);
            return 1;
        }
        SET_VECTOR_ELT(struc, i, s);
        UNPROTECT(1);
    }

    setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);
    UNPROTECT(2);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_structure_array_with_empty_fields(SEXP list,
                                       int index,
                                       matvar_t *matvar)
{
    SEXP names, struc, s;
    char * const * field_names;
    matvar_t *field;

    if (NULL == matvar
        || matvar->class_type != MAT_C_STRUCT
        || 2 != matvar->rank
        || 1 != matvar->dims[0]
        || 1 > matvar->dims[1])
        return 1;

    field_names = Mat_VarGetStructFieldnames(matvar);
    PROTECT(struc = allocVector(VECSXP, matvar->dims[1]));
    if (R_NilValue == struc)
        return 1;
    PROTECT(names = allocVector(STRSXP, matvar->dims[1]));
    if (R_NilValue == names) {
        UNPROTECT(1);
        return 1;
    }

    for (size_t i=0;i<matvar->dims[1];i++) {
        if (field_names[i]) {
            SET_STRING_ELT(names, i, mkChar(field_names[i]));
        } else {
            UNPROTECT(2);
            return 1;
        }

        field = Mat_VarGetStructFieldByIndex(matvar, i, 0);
        if (field->isComplex) {
            PROTECT(s = allocVector(CPLXSXP, 0));
        } else if (field->isLogical) {
            PROTECT(s = allocVector(LGLSXP, 0));
        } else {
            switch (field->class_type) {
            case MAT_C_CHAR:
                PROTECT(s = allocVector(STRSXP, 0));
                break;

            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
                PROTECT(s = allocVector(REALSXP, 0));
                break;

            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                PROTECT(s = allocVector(INTSXP, 0));
                break;

            default:
                UNPROTECT(2);
                return 1;
            }
        }

        if (R_NilValue == s) {
            UNPROTECT(2);
            return 1;
        }

        SET_VECTOR_ELT(struc, i, s);
        UNPROTECT(1);
    }

    setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);
    UNPROTECT(2);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_structure_array_with_fields(SEXP list,
                                 int index,
                                 matvar_t *matvar)
{
    SEXP names, struc, s;
    char * const * fieldnames;
    size_t nfields, fieldlen;
    int err = 0;

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
    PROTECT(struc = allocVector(VECSXP, nfields));
    if (R_NilValue == struc)
        return 1;
    PROTECT(names = allocVector(STRSXP, nfields));
    if (R_NilValue == names) {
        UNPROTECT(1);
        return 1;
    }

    for (size_t i=0;i<nfields;i++) {
        if (fieldnames[i])
            SET_STRING_ELT(names, i, mkChar(fieldnames[i]));

        switch (Mat_VarGetStructFieldByIndex(matvar, i, 0)->class_type) {
        case MAT_C_CHAR:
            PROTECT(s = allocVector(STRSXP, fieldlen));
            if (R_NilValue == s) {
                UNPROTECT(2);
                return 1;
            }
            break;

        case MAT_C_CELL:
            s = R_NilValue;
            break;

        default:
            PROTECT(s = allocVector(VECSXP, fieldlen));
            if (R_NilValue == s) {
                UNPROTECT(2);
                return 1;
            }
            break;
        }

        for (size_t j=0;j<fieldlen;j++) {
            matvar_t *field = Mat_VarGetStructFieldByIndex(matvar, i, j);

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
                    } else {
                        for (size_t k=0;k<field->dims[1];k++)
                            buf[k] = ((char*)field->data)[k];
                        buf[field->dims[1]] = 0;
                        SET_STRING_ELT(s, j, mkChar(buf));
                        free(buf);
                        err = 0;
                    }
                    break;
                }

                default:
                    err = 1;
                    break;
                }
                break;

            case MAT_C_CELL:
                err = read_mat_cell(struc, i, field);
                break;

            default:
                err = 1;
                break;
            }
        }

        if (R_NilValue != s) {
            SET_VECTOR_ELT(struc, i, s);
            UNPROTECT(1);
        }

        if (err) {
            UNPROTECT(2);
            return 1;
        }
    }

    setAttrib(struc, R_NamesSymbol, names);
    SET_VECTOR_ELT(list, index, struc);
    UNPROTECT(2);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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

    PROTECT(cell = allocVector(VECSXP, 0));
    if (R_NilValue == cell)
        return 1;
    SET_VECTOR_ELT(list, index, cell);
    UNPROTECT(1);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_cell_array_with_empty_arrays(SEXP list,
                                  int index,
                                  matvar_t *matvar)
{
    SEXP cell_array, cell_item, field_item;
    char * const * fieldnames;

    if (NULL == matvar
        || MAT_C_CELL != matvar->class_type
        || MAT_T_CELL != matvar->data_type
        || 2 != matvar->rank
        || NULL == matvar->dims
        || 1 != matvar->dims[0]
        || 1 > matvar->dims[1])
        return 1;

    PROTECT(cell_array = allocVector(VECSXP, matvar->dims[1]));
    if (R_NilValue == cell_array)
        return 1;

    for (size_t i=0;i<matvar->dims[1];i++) {
        matvar_t *cell = Mat_VarGetCell(matvar, i);
        SEXP names = R_NilValue;

        if (cell->isComplex) {
            PROTECT(cell_item = allocVector(CPLXSXP, 0));
            if (R_NilValue == cell_item) {
                UNPROTECT(1);
                return 1;
            }
        } else if (cell->isLogical) {
            PROTECT(cell_item = allocVector(LGLSXP, 0));
            if (R_NilValue == cell_item) {
                UNPROTECT(1);
                return 1;
            }
        } else {
            switch (cell->class_type) {
            case MAT_C_CHAR:
                PROTECT(cell_item = allocVector(STRSXP, 0));
                if (R_NilValue == cell_item) {
                    UNPROTECT(1);
                    return 1;
                }
                break;

            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
                PROTECT(cell_item = allocVector(REALSXP, 0));
                if (R_NilValue == cell_item) {
                    UNPROTECT(1);
                    return 1;
                }
                break;

            case MAT_C_INT64:
            case MAT_C_INT32:
            case MAT_C_INT16:
            case MAT_C_INT8:
            case MAT_C_UINT64:
            case MAT_C_UINT32:
            case MAT_C_UINT16:
            case MAT_C_UINT8:
                PROTECT(cell_item = allocVector(INTSXP, 0));
                if (R_NilValue == cell_item) {
                    UNPROTECT(1);
                    return 1;
                }
                break;

            case MAT_C_STRUCT:
                if (cell->dims[0] == 0 && cell->dims[1] == 1) {
                    PROTECT(cell_item = allocVector(VECSXP,
                                                    Mat_VarGetNumberOfFields(cell)));
                    if (R_NilValue == cell_item) {
                        UNPROTECT(1);
                        return 1;
                    }
                    PROTECT(names = allocVector(STRSXP,
                                                Mat_VarGetNumberOfFields(cell)));
                    if (R_NilValue == names) {
                        UNPROTECT(2);
                        return 1;
                    }
                    setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);

                    fieldnames = Mat_VarGetStructFieldnames(cell);
                    for (size_t j=0;j<Mat_VarGetNumberOfFields(cell);j++) {
                        SET_STRING_ELT(names, j, mkChar(fieldnames[j]));
                        PROTECT(field_item = allocVector(VECSXP, 0));
                        if (R_NilValue == field_item) {
                            UNPROTECT(3);
                            return 1;
                        }
                        SET_VECTOR_ELT(cell_item, j, field_item);
                        UNPROTECT(1);
                    }
                } else if (cell->dims[0] == 1 && cell->dims[1] == 1) {
                    PROTECT(cell_item = allocVector(VECSXP, 0));
                    if (R_NilValue == cell_item) {
                        UNPROTECT(1);
                        return 1;
                    }
                    PROTECT(names = allocVector(STRSXP, 0));
                    if (R_NilValue == names) {
                        UNPROTECT(2);
                        return 1;
                    }
                    setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);
                } else if (cell->dims[0] == 1 && cell->dims[1] > 1) {
                    fieldnames = Mat_VarGetStructFieldnames(cell);
                    PROTECT(cell_item = allocVector(VECSXP, cell->dims[1]));
                    if (R_NilValue == cell_item) {
                        UNPROTECT(1);
                        return 1;
                    }
                    PROTECT(names = allocVector(STRSXP, cell->dims[1]));
                    if (R_NilValue == names) {
                        UNPROTECT(2);
                        return 1;
                    }
                    setAttrib(cell_item, R_NamesSymbol, names);
                    UNPROTECT(1);

                    for (size_t j=0;j<cell->dims[1];j++) {
                        matvar_t *field = Mat_VarGetStructFieldByIndex(cell, j, 0);
                        if (NULL == field) {
                            UNPROTECT(2);
                            return 1;
                        }

                        if (fieldnames[j])
                            SET_STRING_ELT(names, j, mkChar(fieldnames[j]));

                        if (field->isComplex) {
                            PROTECT(field_item = allocVector(CPLXSXP, 0));
                        } else if (field->isLogical) {
                            PROTECT(field_item = allocVector(LGLSXP, 0));
                        } else {
                            switch (field->class_type) {
                            case MAT_C_CHAR:
                                PROTECT(field_item = allocVector(STRSXP, 0));
                                break;

                            case MAT_C_DOUBLE:
                            case MAT_C_SINGLE:
                                PROTECT(field_item = allocVector(REALSXP, 0));
                                break;

                            case MAT_C_INT64:
                            case MAT_C_INT32:
                            case MAT_C_INT16:
                            case MAT_C_INT8:
                            case MAT_C_UINT64:
                            case MAT_C_UINT32:
                            case MAT_C_UINT16:
                            case MAT_C_UINT8:
                                PROTECT(field_item = allocVector(INTSXP, 0));
                                break;

                            default:
                                field_item = R_NilValue;
                                break;
                            }
                        }

                        if (R_NilValue == field_item) {
                            UNPROTECT(2);
                            return 1;
                        }

                        SET_VECTOR_ELT(cell_item, j, field_item);
                        UNPROTECT(1);
                    }
                } else {
                    UNPROTECT(1);
                    return 1;
                }
                break;

            case MAT_C_CELL:
                if (cell->dims[0] == 0 && cell->dims[1] == 1) {
                    PROTECT(cell_item = allocVector(VECSXP, 0));
                    if (R_NilValue == cell_item) {
                        UNPROTECT(1);
                        return 1;
                    }
                } else {
                    UNPROTECT(1);
                    return 1;
                }
                break;

            default:
                UNPROTECT(1);
                return 1;
            }
        }

        SET_VECTOR_ELT(cell_array, i, cell_item);
        UNPROTECT(1);
    }

    SET_VECTOR_ELT(list, index, cell_array);
    UNPROTECT(1);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
 * @return 0 on succes or 1 on failure.
 */
static int
read_cell_array_with_arrays(SEXP list,
                            int index,
                            matvar_t *matvar)
{
    SEXP cell;
    int err;

    if (NULL == matvar
        || NULL == matvar->dims)
        return 1;

    PROTECT(cell = allocVector(VECSXP, matvar->dims[0]));
    if (R_NilValue == cell)
        return 1;

    for (size_t i=0;i<matvar->dims[0];i++) {
        SEXP cell_row = R_NilValue;;
        if (matvar->dims[1] > 1) {
            PROTECT(cell_row = allocVector(VECSXP, matvar->dims[1]));
            if (R_NilValue == cell_row) {
                UNPROTECT(1);
                return 1;
            }
        }

        for (size_t j=0;j<matvar->dims[1];j++) {
            matvar_t *mat_cell = Mat_VarGetCell(matvar, j*matvar->dims[0] + i);

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
                if (R_NilValue == cell_row) {
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
                if (R_NilValue == cell_row)
                    err = read_sparse(cell, i, mat_cell);
                else
                    err = read_sparse(cell_row, j, mat_cell);
                break;

            case MAT_C_CHAR:
                if (R_NilValue == cell_row)
                    err = read_mat_char(cell, i, mat_cell);
                else
                    err = read_mat_char(cell_row, j, mat_cell);
                break;

            case MAT_C_STRUCT:
                if (R_NilValue == cell_row)
                    err = read_mat_struct(cell, i, mat_cell);
                else
                    err = read_mat_struct(cell_row, j, mat_cell);
                break;

            case MAT_C_CELL:
                if (R_NilValue == cell_row)
                    err = read_mat_cell(cell, i, mat_cell);
                else
                    err = read_mat_cell(cell_row, j, mat_cell);
                break;

            default:
                err = 1;
                break;
            }

            if (err) {
                UNPROTECT(1);
                return 1;
            }
        }

        if (R_NilValue != cell_row) {
            SET_VECTOR_ELT(cell, i, cell_row);
            UNPROTECT(1);
        }
    }

    SET_VECTOR_ELT(list, index, cell);
    UNPROTECT(1);

    return 0;
}

/** @brief
 *
 *
 * @ingroup
 * @param list
 * @param index
 * @param matvar
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
 * @ingroup
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

/** @brief
 *
 *
 * @ingroup
 * @param filename
 * @return a named list (VECSXP).
 */
SEXP read_mat(const SEXP filename)
{
    mat_t *mat;
    matvar_t *matvar;
    int i=0, n, err;
    SEXP list, names;

    if (filename == R_NilValue)
        error("'filename' equals R_NilValue.");
    if (!isString(filename))
        error("'filename' must be a string.");

    mat = Mat_Open(CHAR(STRING_ELT(filename, 0)), MAT_ACC_RDONLY);
    if (!mat)
        error("Unable to open file.");

    n = number_of_variables(mat);
    PROTECT(list = allocVector(VECSXP, n));
    if (R_NilValue == list) {
        Mat_Close(mat);
        error("Error reading MAT file");
    }
    PROTECT(names = allocVector(STRSXP, n));
    if (R_NilValue == names) {
        Mat_Close(mat);
        UNPROTECT(1);
        error("Error reading MAT file");
    }

    if (Mat_Rewind(mat)) {
        Mat_Close(mat);
        UNPROTECT(2);
        error("Error reading MAT file");
    }

    while ((matvar = Mat_VarReadNext(mat)) != NULL) {
        SET_STRING_ELT(names, i, mkChar(matvar->name));

        switch (matvar->class_type) {
        case MAT_C_EMPTY:
            Mat_VarFree(matvar);
            Mat_Close(mat);
            UNPROTECT(2);
            error("Not implemented support to read matio class type MAT_C_EMPTY");

        case MAT_C_CELL:
            err = read_mat_cell(list, i, matvar);
            break;

        case MAT_C_STRUCT:
            err = read_mat_struct(list, i, matvar);
            break;

        case MAT_C_OBJECT:
            Mat_VarFree(matvar);
            Mat_Close(mat);
            UNPROTECT(2);
            error("Not implemented support to read matio class type MAT_C_OBJECT");

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
            Mat_VarFree(matvar);
            Mat_Close(mat);
            UNPROTECT(2);
            error("Not implemented support to read matio class type MAT_C_FUNCTION");

        default:
            err = 1;
        }

        if (err) {
            Mat_VarFree(matvar);
            Mat_Close(mat);
            UNPROTECT(2);
            error("Error reading MAT file");
        }

        Mat_VarFree(matvar);
        matvar = NULL;
        i++;
    }

    setAttrib(list, R_NamesSymbol, names);
    Mat_Close(mat);
    UNPROTECT(2);

    return list;
}

/** @brief
 *
 *
 * @ingroup
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

    if (R_NilValue == list)
        error("'list' equals R_NilValue.");
    if (R_NilValue == filename)
        error("'filename' equals R_NilValue.");
    if (R_NilValue == compression)
        error("'compression' equals R_NilValue.");
    if (R_NilValue == version)
        error("'version' equals R_NilValue.");
    if (R_NilValue == header)
        error("'header' equals R_NilValue.");
    if (!isNewList(list))
        error("'list' must be a list.");
    if (!isString(filename))
        error("'filename' must be a string.");

    mat = Mat_CreateVer(CHAR(STRING_ELT(filename, 0)),
                        CHAR(STRING_ELT(header, 0)),
                        INTEGER(version)[0]);
    if (!mat)
        error("Unable to open file.");

    if (INTEGER(compression)[0])
        use_compression = MAT_COMPRESSION_ZLIB;

    names = getAttrib(list, R_NamesSymbol);
    for (int i = 0; i < length(list); i++) {
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
            error("Unable to write list");
        }
    }

    Mat_Close(mat);

    return R_NilValue;
}

static const R_CallMethodDef callMethods[] =
{
    {"read_mat", (DL_FUNC)&read_mat, 1},
    {"write_mat", (DL_FUNC)&write_mat, 5},
    {NULL, NULL, 0}
};

/** @brief
 *
 *
 * @ingroup
 * @param info
 */
void
R_init_rmatio(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
