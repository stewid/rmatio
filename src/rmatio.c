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

int write_dgCMatrix(SEXP slot, const char* name, mat_t* mat)
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
/* Matrices must have rank equal to 2              */
int write_vector(SEXP elmt, mat_t *mat, const char *name)
{
  size_t* dims;
  int rank;
  matvar_t *matvar;
  int do_unprotect=0;

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
			 0); /* :TODO:CHECK: Is that the correct option? */

  if(matvar == NULL) {
    if(do_unprotect)
      UNPROTECT(1);
    free(dims);
    return 1;
  }

  Mat_VarWrite(mat, matvar, MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
  free(dims);
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

int read_dgCMatrix(SEXP list,
		   int i,
		   const char* name,
		   mat_t* mat)
{
  SEXP m;
  int *dims;
  int *ir;              /* Array of size nnzero where ir[k] is the row of data[k] */
  int *jc;              /* Array of size ncol+1, jc[k] index to data of first non-zero element in row k */
  double *data;         /* Array of data elements */
  matvar_t *matvar;
  mat_sparse_t *sparse;

  matvar = Mat_VarRead(mat, name);
  if(matvar == NULL)
    return 1;

  PROTECT(m = NEW_OBJECT(MAKE_CLASS("dgCMatrix")));
  if(m == R_NilValue)
    return 1;

  dims = INTEGER(GET_SLOT(m, Rf_install("Dim")));
  dims[0] = matvar->dims[0];
  dims[1] = matvar->dims[1];

  sparse = matvar->data;
  SET_SLOT(m, Rf_install("i"), allocVector(INTSXP, sparse->nir));
  ir = INTEGER(GET_SLOT(m, Rf_install("i")));
  for(int i=0;i<sparse->nir;++i)
    ir[i] = sparse->ir[i];

  SET_SLOT(m, Rf_install("p"), allocVector(INTSXP, sparse->njc));
  jc = INTEGER(GET_SLOT(m, Rf_install("p")));
  for(int i=0;i<sparse->njc;++i)
    jc[i] = sparse->jc[i];

  SET_SLOT(m, Rf_install("x"), allocVector(REALSXP, sparse->ndata));
  data = REAL(GET_SLOT(m, Rf_install("x")));
  for(int i=0;i<sparse->ndata;++i)
    data[i] = ((double*)sparse->data)[i];

  Mat_VarFree(matvar);
  SET_VECTOR_ELT(list, i, m);
  UNPROTECT(1);

  return 0;
}

int read_matrix(SEXP list,
		int i,
		const char* name,
		mat_t* mat)
{
  SEXP m;
  SEXP dim = R_NilValue;
  matvar_t *matvar;
  double* data;
  int len;

  matvar = Mat_VarRead(mat, name);
  if(matvar == NULL || matvar->rank < 2)
    return 1;

  len = matvar->dims[0];
  for(size_t j=1;j<matvar->rank;j++)
    len *= matvar->dims[j];
  if(len <= 0)
    return 1;

  if(matvar->isComplex)  
    PROTECT(m = allocVector(CPLXSXP, len));
  else {
    PROTECT(m = allocVector(REALSXP, len));
    data = REAL(m);
    for(size_t j=0;j<len;j++)
      data[j] = ((double*)matvar->data)[j];
  }

  /* Assign dimension to the allocated vector, if not the rank is two and one */
  /* of the dimensions is one. */
  if(!(matvar->rank == 2 && (matvar->dims[0] == 1 || matvar->dims[1] == 1))) {
    PROTECT(dim = allocVector(INTSXP, matvar->rank));
    for(size_t j=0;j<matvar->rank;j++)
      INTEGER(dim)[j] = matvar->dims[j];
    setAttrib(m, R_DimSymbol, dim);
  }

  Mat_VarFree(matvar);
  SET_VECTOR_ELT(list, i, m);
  if(dim == R_NilValue)
    UNPROTECT(1);
  else
    UNPROTECT(2);
  
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

  while((matvar = Mat_VarReadNextInfo(mat)) != NULL) {
    SET_STRING_ELT(names, i, mkChar(matvar->name));

    switch(matvar->class_type) {
    case MAT_C_SPARSE:
      err = read_dgCMatrix(list, i, matvar->name, mat);
      break;

    case MAT_C_DOUBLE:
      err = read_matrix(list, i, matvar->name, mat);
      break;

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
	err = write_dgCMatrix(elmt, name, mat);
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
