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

int write_sparse(SEXP elmt, const char* name, mat_t* mat)
{
  size_t dims[2];
  matvar_t *matvar;
  mat_sparse_t  sparse = {0,};

  if(elmt == R_NilValue || LENGTH(GET_SLOT(elmt, Rf_install("Dim"))) != 2)
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
  			 MAT_F_DONT_COPY_DATA);

  if(matvar == NULL)
    return 1;

  Mat_VarWrite(mat,matvar,MAT_COMPRESSION_ZLIB);
  Mat_VarFree(matvar);

  return 0;
}

int write_vector(SEXP elmt, mat_t *mat, const char *name)
{
  size_t *dims, len;
  int rank;
  matvar_t *matvar=NULL;
  double *re = NULL;
  double *im = NULL;
  mat_uint8_t *logical = NULL;
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

  switch(TYPEOF(elmt)) {
  case REALSXP:
    matvar = Mat_VarCreate(name,
			   MAT_C_DOUBLE,
			   MAT_T_DOUBLE,
			   rank,
			   dims,
			   REAL(elmt),
			   0);
    break;

  case INTSXP:
    matvar = Mat_VarCreate(name,
			   MAT_C_INT32,
			   MAT_T_INT32,
			   rank,
			   dims,
			   INTEGER(elmt),
			   0);
    break;

  case CPLXSXP:
    re = malloc(LENGTH(elmt)*sizeof(double));
    if(re == NULL) {
      free(dims);
      return 1;
    }
    
    im = malloc(LENGTH(elmt)*sizeof(double));
    if(im == NULL) {
      free(dims);
      free(re);
      return 1;
    }

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
    break;

  case LGLSXP:
    len = dims[0];
    for(int i=1;i<rank;i++)
      len *= dims[i];

    logical = malloc(len*sizeof(mat_uint8_t));
    if(logical == NULL) {
      free(dims);
      return 1;
    }

    for(size_t i=0;i<len;i++)
      logical[i] = LOGICAL(elmt)[i] != 0;

    matvar = Mat_VarCreate(name,
			   MAT_C_UINT8,
			   MAT_T_UINT8,
			   rank,
			   dims,
			   logical,
			   MAT_F_LOGICAL);
    break;
  }

  if(matvar == NULL) {
    free(dims);
    if(re)
      free(re);
    if(im)
      free(im);
    if(logical)
      free(logical);
    return 1;
  }

  Mat_VarWrite(mat, matvar, MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
  free(dims);
  if(re)
    free(re);
  if(im)
    free(im);
  if(logical)
    free(logical);

  return 0;
}

int write_string(SEXP elmt, mat_t *mat, const char *name)
{
  size_t* dims;
  matvar_t *matvar;
  int rank = 2;
  mat_uint8_t *buf;

  if(elmt == R_NilValue
     || TYPEOF(elmt) != STRSXP
     || !isNull(getAttrib(elmt, R_DimSymbol)))
    return 1;

  dims = calloc(rank, sizeof(size_t));
  if(dims == NULL)
    return 1;
  dims[0] = LENGTH(elmt);
  if(dims[0])
    dims[1] = LENGTH(STRING_ELT(elmt, 0));
  
  /* Copy data and check that all strings have equal length */
  buf = malloc(dims[0]*dims[1]*sizeof(mat_uint8_t));
  for(size_t i=0;i<dims[0];i++) {
    if(dims[1] != LENGTH(STRING_ELT(elmt, i))) {
      free(buf);
      free(dims);
      return 1;
    }

    for(size_t j=0;j<dims[1];j++)
      buf[dims[0]*j + i] = CHAR(STRING_ELT(elmt, i))[j];
  }

  matvar = Mat_VarCreate(name,
  			 MAT_C_CHAR,
  			 MAT_T_UINT8,
  			 rank,
  			 dims,
  			 buf,
  			 0);

  Mat_VarWrite(mat, matvar, MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
  free(buf);
  free(dims);

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

  /* Assign dimension to the allocated vector, if not   */
  /* the rank is two and one of the dimensions is <= 1  */
  if(!(matvar->rank == 2 && (matvar->dims[0] <= 1 || matvar->dims[1] <= 1))) {
    PROTECT(dim = allocVector(INTSXP, matvar->rank));
    for(size_t j=0;j<matvar->rank;j++)
      INTEGER(dim)[j] = matvar->dims[j];
    setAttrib(m, R_DimSymbol, dim);
    UNPROTECT(1);
  }
}

int read_mat_char(SEXP list, int index, matvar_t *matvar)
{
  SEXP c;
  size_t i,j;
  char *buf;

  if(matvar == NULL || matvar->rank != 2 || matvar->isComplex)
    return 1;

  PROTECT(c = allocVector(STRSXP, matvar->dims[0]));

  switch(matvar->data_type) {
  case MAT_T_UINT8:
  case MAT_T_UNKNOWN:
    buf = (char*)malloc((matvar->dims[1]+1)*sizeof(char));
    for(i=0;i<matvar->dims[0];i++) {
      for(j=0;j<matvar->dims[1];j++)
	buf[j] = ((char*)matvar->data)[matvar->dims[0]*j + i];
      buf[matvar->dims[1]] = 0;
      SET_STRING_ELT(c, i, mkChar(buf));
    }
    free(buf);
    break;

  default:
    error("Unimplemented Matlab character data type");
    break;
  }

  SET_VECTOR_ELT(list, index, c);
  UNPROTECT(1);
  
  return 0;
}

int read_mat_sparse(SEXP list, int index, matvar_t *matvar)
{
  SEXP m, data;
  int *dims;
  int *ir;              /* Array of size nnzero where ir[k] is the row of data[k] */
  int *jc;              /* Array of size ncol+1, jc[k] index to data of first non-zero element in row k */
  mat_sparse_t *sparse;

  if(matvar == NULL || matvar->rank != 2)
    return 1;

  sparse = matvar->data;

  if(matvar->isLogical) {
    PROTECT(m = NEW_OBJECT(MAKE_CLASS("lgCMatrix")));
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

    SET_SLOT(m, Rf_install("x"), allocVector(LGLSXP, sparse->nir));
    data = GET_SLOT(m, Rf_install("x"));
    for(int j=0;j<sparse->nir;++j)
      LOGICAL(data)[j] = 1;
  } else if(matvar->isComplex)  {
    size_t len = matvar->dims[0] * matvar->dims[1];
    PROTECT(m = allocVector(CPLXSXP, len));
    mat_complex_split_t *complex_data = sparse->data;

    for(size_t j=0;j<len;j++) {
      COMPLEX(m)[j].r = 0;
      COMPLEX(m)[j].i = 0;
    }

    for(size_t j=0,k=0;j<matvar->dims[1];j++) {
      while(k<sparse->jc[j+1]) {
      	COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].r = ((double*)complex_data->Re)[k];
      	COMPLEX(m)[matvar->dims[0]*j+sparse->ir[k]].i = ((double*)complex_data->Im)[k];
	k++;
      }
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
    data = GET_SLOT(m, Rf_install("x"));
    for(int j=0;j<sparse->ndata;++j)
      REAL(data)[j] = ((double*)sparse->data)[j];
  }

  SET_VECTOR_ELT(list, index, m);
  UNPROTECT(1);

  return 0;
}

int read_mat_complex(SEXP list, int index, matvar_t *matvar)
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
  SET_VECTOR_ELT(list, index, m);
  UNPROTECT(1);
  
  return 0;
}

int read_mat_struct(SEXP list, int index, matvar_t *matvar)
{
  SEXP names, struc, s, elmt;
  matvar_t *field;
  char * const * field_names;
  size_t i, j, n_fields, field_len;
  int err = 0;
  char* buf;

  if(matvar == NULL || matvar->class_type != MAT_C_STRUCT)
    return 1;

  n_fields = Mat_VarGetNumberOfFields(matvar);
  field_names = Mat_VarGetStructFieldnames(matvar);
  PROTECT(struc = allocVector(VECSXP, n_fields));
  PROTECT(names = allocVector(STRSXP, n_fields));

  for(i=0;i<n_fields;i++) {
    if(field_names[i])
      SET_STRING_ELT(names, i, mkChar(field_names[i]));

    field_len = 0;
    if(matvar->dims[0] && matvar->dims[1]) {
      field = Mat_VarGetStructFieldByIndex(matvar, i, 0);
      if(field->dims[0] && field->dims[1]) {
	while(Mat_VarGetStructFieldByIndex(matvar, i, field_len) != NULL)
	  field_len++;
      }

      if(field->class_type == MAT_C_CHAR) {
	if(field_len)
	  PROTECT(s = allocVector(STRSXP, field_len));
	else
	  PROTECT(s = allocVector(STRSXP, 0));
      } else if(field_len) {
	PROTECT(s = allocVector(VECSXP, field_len));
      } else {
	PROTECT(s = allocVector(VECSXP, 1));

	if(field->data_type != MAT_T_DOUBLE)
	  error("Unimplemented Matlab data type");

	PROTECT(elmt = allocVector(REALSXP, 0));
	SET_VECTOR_ELT(s, 0, elmt);
	UNPROTECT(1);
      }
    } else {
      PROTECT(s = allocVector(VECSXP, 0));
    }

    for(j=0;j<field_len;j++) {
      field = Mat_VarGetStructFieldByIndex(matvar, i, j);

      switch(field->class_type) {
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
	if(field->isLogical)
	  err = read_mat_logical(s, j, field);
	else if(field->isComplex)
	  err = read_mat_complex(s, j, field);
	else
	  err = read_mat_data(s, j, field);
	break;

      case MAT_C_SPARSE:
      	err = read_mat_sparse(s, j, field);
      	break;

      case MAT_C_CHAR:
	switch(field->data_type) {
	case MAT_T_UINT8:
	case MAT_T_UNKNOWN:
	  buf = (char*)malloc((field->dims[1]+1)*sizeof(char));
	  for(size_t k=0;k<field->dims[1];k++)
	    buf[k] = ((char*)field->data)[field->dims[0]*k + j];
	  buf[field->dims[1]] = 0;
	  SET_STRING_ELT(s, j, mkChar(buf));
	  free(buf);
	  break;
	    
	default:
	  error("Unimplemented Matlab character data type");
	  break;
	}
      	break;

      default:
	err = 1;
	break;
      }
    }

    SET_VECTOR_ELT(struc, i, s);
    UNPROTECT(1);

    if(err) {
      UNPROTECT(2);
      return 1;
    }
  }

  setAttrib(struc, R_NamesSymbol, names);
  SET_VECTOR_ELT(list, index, struc);
  UNPROTECT(2);
  
  return 0;
}

int read_mat_cell(SEXP list, int index, matvar_t *matvar)
{
  SEXP cell, cell_row, s;
  matvar_t *mat_cell;
  int err;

  if(matvar == NULL || matvar->data_type != MAT_T_CELL)
    return 1;

  PROTECT(cell = allocVector(VECSXP, matvar->dims[0]));

  for(size_t i=0;i<matvar->dims[0];i++) {
    PROTECT(cell_row = allocVector(VECSXP, matvar->dims[1]));

    for(size_t j=0;j<matvar->dims[1];j++) {
      mat_cell = Mat_VarGetCell(matvar, j*matvar->dims[0] + i);

      if(mat_cell->dims[0] == 0 && mat_cell->dims[1] == 0) {
      	switch(mat_cell->data_type) {
      	case MAT_T_INT8:
      	case MAT_T_UINT8:
      	case MAT_T_INT16:
      	case MAT_T_UINT16:
      	case MAT_T_INT32:
      	case MAT_T_UINT32:
      	case MAT_T_SINGLE:
      	case MAT_T_DOUBLE:
      	case MAT_T_INT64:
      	case MAT_T_UINT64:
      	  err = read_mat_data(cell_row, j, mat_cell);
      	  break;

      	default:
      	  err = 1;
      	  break;
      	}
      } else if(mat_cell->dims[0] == 0 || mat_cell->dims[1] == 0) {
	PROTECT(s = allocVector(VECSXP, 0));
	SET_VECTOR_ELT(cell_row, j, s);
	UNPROTECT(1);
      } else {
      	switch(mat_cell->class_type) {
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
	  if(mat_cell->isLogical)
	    err = read_mat_logical(cell_row, j, mat_cell);
      	  else if(mat_cell->isComplex)
      	    err = read_mat_complex(cell_row, j, mat_cell);
      	  else
      	    err = read_mat_data(cell_row, j, mat_cell);
      	  break;

      	case MAT_C_SPARSE:
      	  err = read_mat_sparse(cell_row, j, mat_cell);
      	  break;

      	case MAT_C_CHAR:
      	  err = read_mat_char(cell_row, j, mat_cell);
      	  break;

      	case MAT_C_STRUCT:
      	  err = read_mat_struct(cell_row, j, mat_cell);
      	  break;

      	default:
      	  err = 1;
      	  break;
      	}
      }

      if(err) {
      	UNPROTECT(1);
      	return 1;
      }
    }

    SET_VECTOR_ELT(cell, i, cell_row);
    UNPROTECT(1);
  }

  SET_VECTOR_ELT(list, index, cell);
  UNPROTECT(1);

  return 0;
}

int read_mat_data(SEXP list, int index, matvar_t *matvar)
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
  SET_VECTOR_ELT(list, index, m);
  UNPROTECT(1);
  
  return 0;
}

int read_mat_logical(SEXP list, int index, matvar_t *matvar)
{
  SEXP m;
  size_t j, len;

  if(matvar == NULL || matvar->rank < 2 || !matvar->isLogical)
    return 1;

  len = matvar->dims[0];
  for(j=1;j<matvar->rank;j++)
    len *= matvar->dims[j];

  switch(matvar->data_type) {
  case MAT_T_UINT8:
    PROTECT(m = allocVector(LGLSXP, len));
    for(j=0;j<len;j++)
      LOGICAL(m)[j] = ((mat_uint8_t*)matvar->data)[j] != 0;
    break;

  default:
    error("Unimplemented Matlab logical data type");
    break;
  }

  set_dim(m, matvar);
  SET_VECTOR_ELT(list, index, m);
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
      if(matvar->isLogical)
	err = read_mat_logical(list, i, matvar);
      else if(matvar->isComplex)
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

SEXP write_mat(SEXP list, SEXP filename, SEXP version)
{
  SEXP names;    /* names in list */
  SEXP elmt;     /* element in list */
  SEXP class_name;
  mat_t *mat;
  int err;

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

  names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++) {
    elmt = VECTOR_ELT(list, i);

    switch(TYPEOF(elmt)) {
    case REALSXP:
    case INTSXP:
    case CPLXSXP:
    case LGLSXP:
      err = write_vector(elmt, mat, CHAR(STRING_ELT(names, i)));
      break;

    case STRSXP:
      err= write_string(elmt, mat, CHAR(STRING_ELT(names, i)));
      break;

    case S4SXP:
      class_name = getAttrib(elmt, R_ClassSymbol);      
      if(strcmp(CHAR(STRING_ELT(class_name, 0)), "dgCMatrix") == 0)
	err= write_sparse(elmt, CHAR(STRING_ELT(names, i)), mat);
      else
	err = 1;
      break;

    default:
      Mat_Close(mat);
      error("Unsupported SEXPTYPE: %i\n", TYPEOF(elmt));
    }

    if(err) {
      Mat_Close(mat);
      error("Unable to write list");
    }
  }

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
