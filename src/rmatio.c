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

int write_sparse(SEXP slot, const char* name, mat_t* mat)
{
  size_t dims[2];
  matvar_t *matvar;
  mat_sparse_t  sparse = {0,};

  if(slot == R_NilValue || LENGTH(GET_SLOT(slot, Rf_install("Dim"))) != 2)
    return 1;

  dims[0] = INTEGER(GET_SLOT(slot, Rf_install("Dim")))[0];
  dims[1] = INTEGER(GET_SLOT(slot, Rf_install("Dim")))[1];
  sparse.nzmax = LENGTH(GET_SLOT(slot, Rf_install("i")));
  sparse.ir = INTEGER(GET_SLOT(slot, Rf_install("i")));
  sparse.nir = LENGTH(GET_SLOT(slot, Rf_install("i")));
  sparse.jc = INTEGER(GET_SLOT(slot, Rf_install("p")));
  sparse.njc = LENGTH(GET_SLOT(slot, Rf_install("p")));
  sparse.data = REAL(GET_SLOT(slot, Rf_install("x")));
  sparse.ndata = LENGTH(GET_SLOT(slot, Rf_install("x")));

  matvar = Mat_VarCreate(name,
  			 MAT_C_SPARSE,
  			 MAT_T_DOUBLE,
  			 2,
  			 dims,
  			 &sparse,
  			 MAT_F_DONT_COPY_DATA);

  if(matvar == NULL)
    return 1;

  Mat_VarWrite(mat,matvar,MAT_COMPRESSION_ZLIB);
  Mat_VarFree(matvar);

  return 0;
}

/* Write a R vector or matrix to mat file.         */
/* Currently, only integer and real are supported. */
/* Vectors are written as 1 x length               */
int write_vector(SEXP elmt, mat_t *mat, const char *name)
{
  size_t* dims;
  int rank;
  matvar_t *matvar;
  int do_unprotect=0;
  double* re = NULL;
  double* im = NULL;
  struct mat_complex_split_t z;

  if(elmt == R_NilValue || !isVector(elmt))
    return 1;

  if(isNull(getAttrib(elmt, R_DimSymbol))) {
    rank = 2;
    dims = malloc(rank*sizeof(size_t));
    if(dims == NULL)
      return 1;
    dims[0] = 1;
    dims[1] = LENGTH(elmt);
  } else {
    rank = LENGTH(GET_SLOT(elmt, R_DimSymbol));
    dims = malloc(rank*sizeof(size_t));
    if(dims == NULL)
      return 1;
    for(int i=0;i<rank;i++)
      dims[i] = INTEGER(GET_SLOT(elmt, R_DimSymbol))[i];
  }

  if(isComplex(elmt)) {
    re = malloc(LENGTH(elmt)*sizeof(double));
    im = malloc(LENGTH(elmt)*sizeof(double));
    for(int i=0;i<LENGTH(elmt);i++) {
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
  } else {
    if(isInteger(elmt)) {
      PROTECT(elmt = AS_NUMERIC(elmt));
      do_unprotect=1;
    } else if(!isReal(elmt)) {
      free(dims);
      return 1;
    }

    matvar = Mat_VarCreate(name,
			   MAT_C_DOUBLE,
			   MAT_T_DOUBLE,
			   rank,
			   dims,
			   REAL(elmt),
			   0);
  }

  if(matvar == NULL) {
    if(do_unprotect)
      UNPROTECT(1);
    free(dims);
    if(re)
      free(re);
    if(im)
      free(im);
    return 1;
  }

  Mat_VarWrite(mat, matvar, MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
  free(dims);
  if(re)
    free(re);
  if(im)
    free(im);
  if(do_unprotect)
    UNPROTECT(1);

  return 0;
}

int number_of_variables(mat_t *mat)
{
  int len=0;
  matvar_t *matvar;

  if(mat != NULL) {
    if(!Mat_Rewind(mat)) {
      while((matvar = Mat_VarReadNextInfo(mat)) != NULL) {
	len++;
	Mat_VarFree(matvar);
	matvar = NULL;
      }
    }
  }
   
 return len;
}

void set_dim(SEXP m, matvar_t *matvar)
{
  SEXP dim;

  /* Assign dimension to the allocated vector, if not the rank is two and one */
  /* of the dimensions is one. */
  if(!(matvar->rank == 2 && (matvar->dims[0] == 1 || matvar->dims[1] == 1))) {
    PROTECT(dim = allocVector(INTSXP, matvar->rank));
    for(size_t j=0;j<matvar->rank;j++)
      INTEGER(dim)[j] = matvar->dims[j];
    setAttrib(m, R_DimSymbol, dim);
    UNPROTECT(1);
  }
}

int read_mat_sparse(SEXP list, int i, matvar_t *matvar)
{
  SEXP m;
  int *dims;
  int *ir;              /* Array of size nnzero where ir[k] is the row of data[k] */
  int *jc;              /* Array of size ncol+1, jc[k] index to data of first non-zero element in row k */
  double *data;         /* Array of data elements */
  mat_sparse_t *sparse;

  if(matvar == NULL || matvar->rank != 2)
    return 1;

  sparse = matvar->data;

  if(matvar->isComplex)  {
    size_t len = matvar->dims[0] * matvar->dims[1];
    PROTECT(m = allocVector(CPLXSXP, len));
    mat_complex_split_t *complex_data = sparse->data;

    for(size_t j=0;j<len;j++) {
      COMPLEX(m)[j].r = 0;
      COMPLEX(m)[j].i = 0;
    }

    for(int j=0,k=0;k<sparse->ndata;++k) {
      while(sparse->jc[j] < k || sparse->jc[j] == sparse->jc[j+1])
	j++;

      COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].r = ((double*)complex_data->Re)[k];
      COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].i = ((double*)complex_data->Im)[k];
    }

    set_dim(m, matvar);
  }
  else {
    PROTECT(m = NEW_OBJECT(MAKE_CLASS("dgCMatrix")));
    if(m == R_NilValue)
      return 1;

    dims = INTEGER(GET_SLOT(m, Rf_install("Dim")));
    dims[0] = matvar->dims[0];
    dims[1] = matvar->dims[1];

    SET_SLOT(m, Rf_install("i"), allocVector(INTSXP, sparse->nir));
    ir = INTEGER(GET_SLOT(m, Rf_install("i")));
    for(int j=0;j<sparse->nir;++j)
      ir[j] = sparse->ir[j];

    SET_SLOT(m, Rf_install("p"), allocVector(INTSXP, sparse->njc));
    jc = INTEGER(GET_SLOT(m, Rf_install("p")));
    for(int j=0;j<sparse->njc;++j)
      jc[j] = sparse->jc[j];

    SET_SLOT(m, Rf_install("x"), allocVector(REALSXP, sparse->ndata));
    data = REAL(GET_SLOT(m, Rf_install("x")));
    for(int j=0;j<sparse->ndata;++j)
      data[j] = ((double*)sparse->data)[j];
  }

  SET_VECTOR_ELT(list, i, m);
  UNPROTECT(1);

  return 0;
}

int read_mat_complex(SEXP list, int i, matvar_t *matvar)
{
  SEXP m;
  size_t j, len;
  mat_complex_split_t *complex_data;

  if(matvar == NULL || matvar->rank < 2 || !matvar->isComplex)
    return 1;

  len = matvar->dims[0];
  for(j=1;j<matvar->rank;j++)
    len *= matvar->dims[j];

  PROTECT(m = allocVector(CPLXSXP, len));
  if(m == R_NilValue)
    return 1;

  complex_data = matvar->data;
  switch(matvar->data_type) {
  case MAT_T_SINGLE:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((float*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((float*)complex_data->Im)[j];
    }
    break;

  case MAT_T_DOUBLE:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((double*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((double*)complex_data->Im)[j];
    }
    break;

  case MAT_T_INT64:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_int64_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_int64_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_INT32:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_int32_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_int32_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_INT16:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_int16_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_int16_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_INT8:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_int8_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_int8_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_UINT64:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_uint64_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_uint64_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_UINT32:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_uint32_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_uint32_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_UINT16:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_uint16_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_uint16_t*)complex_data->Im)[j];
    }
    break;

  case MAT_T_UINT8:
    for(j=0;j<len;j++) {
      COMPLEX(m)[j].r = ((mat_uint8_t*)complex_data->Re)[j];
      COMPLEX(m)[j].i = ((mat_uint8_t*)complex_data->Im)[j];
    }
    break;

  default:
    error("Unimplemented Matlab data type");
    break;
  }

  set_dim(m, matvar);
  SET_VECTOR_ELT(list, i, m);
  UNPROTECT(1);
  
  return 0;
}

int read_mat_data(SEXP list, int i, matvar_t *matvar)
{
  SEXP m;
  size_t j, len;

  if(matvar == NULL || matvar->rank < 2 || matvar->isComplex)
    return 1;

  len = matvar->dims[0];
  for(j=1;j<matvar->rank;j++)
    len *= matvar->dims[j];

  switch(matvar->data_type) {
  case MAT_T_SINGLE:
    PROTECT(m = allocVector(REALSXP, len));
    for(j=0;j<len;j++)
      REAL(m)[j] = ((float*)matvar->data)[j];
    break;

  case MAT_T_DOUBLE:
    PROTECT(m = allocVector(REALSXP, len));
    for(j=0;j<len;j++)
      REAL(m)[j] = ((double*)matvar->data)[j];
    break;

  case MAT_T_INT64:
    PROTECT(m = allocVector(REALSXP, len));
    for(j=0;j<len;j++)
      REAL(m)[j] = ((mat_int64_t*)matvar->data)[j];
    break;

  case MAT_T_INT32:
    PROTECT(m = allocVector(INTSXP, len));
    for(j=0;j<len;j++)
      INTEGER(m)[j] = ((mat_int32_t*)matvar->data)[j];
    break;

  case MAT_T_INT16:
    PROTECT(m = allocVector(INTSXP, len));
    for(j=0;j<len;j++)
      INTEGER(m)[j] = ((mat_int16_t*)matvar->data)[j];
    break;

  case MAT_T_INT8:
    PROTECT(m = allocVector(INTSXP, len));
    for(j=0;j<len;j++)
      INTEGER(m)[j] = ((mat_int8_t*)matvar->data)[j];
    break;

  case MAT_T_UINT64:
    PROTECT(m = allocVector(REALSXP, len));
    for(j=0;j<len;j++)
      REAL(m)[j] = ((mat_uint64_t*)matvar->data)[j];
    break;

  case MAT_T_UINT32:
    PROTECT(m = allocVector(REALSXP, len));
    for(j=0;j<len;j++)
      REAL(m)[j] = ((mat_uint32_t*)matvar->data)[j];
    break;

  case MAT_T_UINT16:
    PROTECT(m = allocVector(INTSXP, len));
    for(j=0;j<len;j++)
      INTEGER(m)[j] = ((mat_uint16_t*)matvar->data)[j];
    break;

  case MAT_T_UINT8:
    PROTECT(m = allocVector(INTSXP, len));
    for(j=0;j<len;j++)
      INTEGER(m)[j] = ((mat_uint8_t*)matvar->data)[j];
    break;

  default:
    error("Unimplemented Matlab data type");
    break;
  }

  set_dim(m, matvar);
  SET_VECTOR_ELT(list, i, m);
  UNPROTECT(1);
  
  return 0;
}

int read_mat_char(SEXP list, int i, matvar_t *matvar)
{
  SEXP m;
  size_t j;

  if(matvar == NULL || matvar->rank != 2 || matvar->isComplex)
    return 1;

  PROTECT(m = allocVector(STRSXP, matvar->dims[0]));

  for(j=0;j<matvar->dims[0];j++)
    SET_STRING_ELT(m, j, mkChar("Under construction"));

  SET_VECTOR_ELT(list, i, m);
  UNPROTECT(1);
  
  return 0;
}

SEXP read_mat(SEXP filename)
{
  mat_t *mat;
  matvar_t *matvar;
  int i=0, n, err;
  SEXP list, names;

  if(filename == R_NilValue)
    error("'filename' equals R_NilValue.");
  if(!isString(filename))
    error("'filename' must be a string.");

  mat = Mat_Open(CHAR(STRING_ELT(filename, 0)), MAT_ACC_RDONLY);
  if(!mat)
    error("Unable to open file.");

  n = number_of_variables(mat);
  PROTECT(list = allocVector(VECSXP, n));
  PROTECT(names = allocVector(STRSXP, n));

  if(Mat_Rewind(mat)) {
    Mat_Close(mat);
    error("Error reading MAT file");
  }

  while((matvar = Mat_VarReadNext(mat)) != NULL) {
    SET_STRING_ELT(names, i, mkChar(matvar->name));

    switch(matvar->class_type) {
    case MAT_C_EMPTY:
      Mat_VarFree(matvar);
      Mat_Close(mat);
      UNPROTECT(2);
      error("Not implemented support to read matio class type MAT_C_EMPTY");

    case MAT_C_CELL:
      Mat_VarFree(matvar);
      Mat_Close(mat);
      UNPROTECT(2);
      error("Not implemented support to read matio class type MAT_C_CELL");

    case MAT_C_STRUCT:
      Mat_VarFree(matvar);
      Mat_Close(mat);
      UNPROTECT(2);
      error("Not implemented support to read matio class type MAT_C_STRUCT");

    case MAT_C_OBJECT:
      Mat_VarFree(matvar);
      Mat_Close(mat);
      UNPROTECT(2);
      error("Not implemented support to read matio class type MAT_C_OBJECT");

    case MAT_C_CHAR:
	err = read_mat_char(list, i, matvar);
	break;

    case MAT_C_SPARSE:
      err = read_mat_sparse(list, i, matvar);
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
      if(matvar->isComplex)
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

    if(err) {
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

int write_element(SEXP elmt, const char* name, mat_t* mat)
{
  SEXP class_name;
  int *dims;
  int err = 1;

  if(elmt != R_NilValue) {
    if(isVector(elmt))
      err = write_vector(elmt, mat, name);
    else if(isS4(elmt)) {
      class_name = getAttrib(elmt, R_ClassSymbol);
      
      if(strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0) {
	err = write_sparse(elmt, name, mat);
      }
    }
  }
  
  return err;
}

int write_list(SEXP list, mat_t* mat)
{
  SEXP names;    /* names in list */
  SEXP elmt;     /* element in list */

  if(list == R_NilValue || !isNewList(list))
    return 1;

  names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++) {
    elmt = VECTOR_ELT(list, i);
    if(elmt == R_NilValue
       || write_element(elmt, CHAR(STRING_ELT(names, i)), mat))
      return 1;
  }

  return 0;
}

SEXP write_mat(SEXP list, SEXP filename, SEXP version)
{
  mat_t *mat;

  if(list == R_NilValue)
    error("'list' equals R_NilValue.");
  if(filename == R_NilValue)
    error("'filename' equals R_NilValue.");
  if(!isNewList(list))
    error("'list' must be a list.");
  if(!isString(filename))
    error("'filename' must be a string.");

  mat = Mat_CreateVer(CHAR(STRING_ELT(filename, 0)), NULL, INTEGER(version)[0]);
  if(!mat)
    error("Unable to open file.");

  if(write_list(list, mat)) {
    Mat_Close(mat);
    error("Unable to write list");
  }

  Mat_Close(mat);
  
  return R_NilValue;
}

static const R_CallMethodDef callMethods[] =
  {
    {"read_mat", (DL_FUNC)&read_mat, 1},
    {"write_mat", (DL_FUNC)&write_mat, 3},
    {NULL, NULL, 0}
  };

void R_init_rmatio(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
