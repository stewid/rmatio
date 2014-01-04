/** @file mat4.c
 * Matlab MAT version 4 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2011   Christopher C. Hulbert
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Changes in the R package rmatio:
 *
 * - The io routines have been adopted to use R printing and error routines.
 *   See the R manual Writing R Extensions
 *
 */

#include <Rdefines.h>
#define Mat_Critical error

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "matio_private.h"
#include "mat4.h"

/** @if mat_devman
 * @brief Reads the data of a version 4 MAT file variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @endif
 */
void
Read4(mat_t *mat,matvar_t *matvar)
{
    unsigned int N;

    if ( fseek(mat->fp,matvar->internal->datapos,SEEK_SET) )
        return;

    N = matvar->dims[0]*matvar->dims[1];
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
            matvar->data_size = sizeof(double);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                matvar->nbytes   = N*sizeof(double);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                    complex_data->Re != NULL && complex_data->Im != NULL ) {
                    ReadDoubleData(mat, complex_data->Re, matvar->data_type, N);
                    ReadDoubleData(mat, complex_data->Im, matvar->data_type, N);
                }
            } else {
                matvar->nbytes = N*sizeof(double);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    ReadDoubleData(mat, matvar->data, matvar->data_type, N);
            }
            /* Update data type to match format of matvar->data */
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_CHAR:
            matvar->data_size = 1;
            matvar->nbytes = N;
            matvar->data = malloc(matvar->nbytes);
            if ( NULL == matvar->data )
                Mat_Critical("Memory allocation failure");
            else
                ReadUInt8Data(mat,matvar->data,matvar->data_type,N);
            matvar->data_type = MAT_T_UINT8;
            break;
        case MAT_C_SPARSE:
	{
	    mat_sparse_t *data;
            double* buf;
	    size_t i, j;

	    /* Read data into temporary buffer */
	    buf = malloc(N*sizeof(double));
	    if ( NULL == buf) {
                Mat_Critical("ReadData: Allocation of temporary buffer failed");
                break;
	    }
	    ReadDoubleData(mat, buf, matvar->data_type, N);

	    matvar->nbytes    = matvar->dims[0] - 1;
            matvar->data_size = sizeof(mat_sparse_t);
            matvar->data      = calloc(1, matvar->data_size);
            if ( NULL == matvar->data ) {
	        free(buf);
                Mat_Critical("ReadData: Allocation of data pointer failed");
                break;
            }
	    data        = matvar->data;
	    data->nzmax = matvar->dims[0] - 1;
            data->nir   = matvar->dims[0] - 1;
            data->ndata = matvar->dims[0] - 1;
            data->njc   = buf[2 * matvar->dims[0] - 1] + 1;

	    data->ir = malloc(data->nir*sizeof(mat_int32_t));
	    data->jc = calloc(data->njc, sizeof(mat_int32_t));
            if ( NULL == data->ir || NULL == data->jc) {
	        free(buf);
                Mat_Critical("ReadData: Allocation of data pointer failed");
                break;
            }

            if ( matvar->isComplex )
                data->data = malloc(sizeof(mat_complex_split_t));
	    else
	        data->data = malloc(data->ndata*sizeof(double));
	    if ( NULL == data->data ) {
	        free(buf);
	        Mat_Critical("ReadData: Allocation of data pointer failed");
		break;
	    }

            if ( matvar->isComplex ) {
	        mat_complex_split_t *complex_data = data->data;
                complex_data->Re = malloc(data->ndata*sizeof(double));
                complex_data->Im = malloc(data->ndata*sizeof(double));
                if ( NULL == complex_data->Re || NULL == complex_data->Im ) {
  	            free(buf);
		    Mat_Critical("ReadData: Allocation of data pointer failed");
		    break;
		}
	    }

	    /* Copy data from temporary buffer */
	    for ( i = 0; i < data->nzmax; i++ ) {
	        data->ir[i] = (mat_int32_t)buf[i] - 1;
		j = buf[matvar->dims[0] + i] - 1;
		if (j && 0 == data->jc[j] )
		    data->jc[j] = i;
		if ( matvar->isComplex ) {
		    mat_complex_split_t *complex_data = data->data;
		    ((double*)complex_data->Re)[i] = (double)buf[2*matvar->dims[0] + i];
		    ((double*)complex_data->Im)[i] = (double)buf[3*matvar->dims[0] + i];
		} else {
	    	    ((double*)data->data)[i] = (double)buf[2*matvar->dims[0] + i];
		}
	    }

	    /* Make sure jc vector is non-decreasing */
	    i = data->njc - 1;
	    data->jc[i--] = data->nzmax;
	    while ( i ) {
	        if ( 0 == data->jc[i]  )
		  data->jc[i] = data->jc[i+1];
	        i--;
	    }

	    /* Update dimension */
	    matvar->dims[0] = buf[matvar->dims[0] - 1];
	    matvar->dims[1] = data->njc - 1;
	    free(buf);
            break;
	}
        default:
            Mat_Critical("MAT V4 data type error");
            return;
    }

    return;
}

/** @if mat_devman
 * @brief Reads a slab of data from a version 4 MAT file for the @c matvar variable
 *
 * @ingroup mat_internal
 * @param mat Version 4 MAT file pointer
 * @param matvar pointer to the mat variable
 * @param data pointer to store the read data in (must be of size
 *             edge[0]*...edge[rank-1]*Mat_SizeOfClass(matvar->class_type))
 * @param start index to start reading data in each dimension
 * @param stride write data every @c stride elements in each dimension
 * @param edge number of elements to read in each dimension
 * @retval 0 on success
 * @endif
 */
int
ReadData4(mat_t *mat,matvar_t *matvar,void *data,
      int *start,int *stride,int *edge)
{
    int err = 0;
    enum matio_classes class_type = MAT_C_EMPTY;

    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

    switch( matvar->data_type ) {
        case MAT_T_DOUBLE:
            class_type = MAT_C_DOUBLE;
            break;
        case MAT_T_SINGLE:
            class_type = MAT_C_SINGLE;
            break;
        case MAT_T_INT32:
            class_type = MAT_C_INT32;
            break;
        case MAT_T_INT16:
            class_type = MAT_C_INT16;
            break;
        case MAT_T_UINT16:
            class_type = MAT_C_UINT16;
            break;
        case MAT_T_UINT8:
            class_type = MAT_C_UINT8;
            break;
        default:
            return 1;
    }

    if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] )
            err = 1;
        else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] )
            err = 1;
        if ( matvar->isComplex ) {
            mat_complex_split_t *cdata = data;
            long nbytes = edge[0]*edge[1]*Mat_SizeOf(matvar->data_type);

            ReadDataSlab2(mat,cdata->Re,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlab2(mat,data,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
        }
    } else {
        if ( matvar->isComplex ) {
            int i;
            mat_complex_split_t *cdata = data;
            long nbytes = Mat_SizeOf(matvar->data_type);

            for ( i = 0; i < matvar->rank; i++ )
                nbytes *= edge[i];

            ReadDataSlabN(mat,cdata->Re,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlabN(mat,data,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
        }
    }
    return err;
}

/** @brief Reads a subset of a MAT variable using a 1-D indexing
 *
 * Reads data from a MAT variable using a linear (1-D) indexing mode. The
 * variable must have been read by Mat_VarReadInfo.
 * @ingroup MAT
 * @param mat MAT file to read data from
 * @param matvar MAT variable information
 * @param data pointer to store data in (must be pre-allocated)
 * @param start starting index
 * @param stride stride of data
 * @param edge number of elements to read
 * @retval 0 on success
 */
int
Mat_VarReadDataLinear4(mat_t *mat,matvar_t *matvar,void *data,int start,
                       int stride,int edge)
{
    size_t i, nmemb = 1;
    int err = 0;
    enum matio_classes class_type = MAT_C_EMPTY;

    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

    switch( matvar->data_type ) {
        case MAT_T_DOUBLE:
            class_type = MAT_C_DOUBLE;
            break;
        case MAT_T_SINGLE:
            class_type = MAT_C_SINGLE;
            break;
        case MAT_T_INT32:
            class_type = MAT_C_INT32;
            break;
        case MAT_T_INT16:
            class_type = MAT_C_INT16;
            break;
        case MAT_T_UINT16:
            class_type = MAT_C_UINT16;
            break;
        case MAT_T_UINT8:
            class_type = MAT_C_UINT8;
            break;
        default:
            return 1;
    }
    matvar->data_size = Mat_SizeOf(matvar->data_type);

    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( stride*(edge-1)+start+1 > nmemb ) {
        return 1;
    }
    if ( matvar->isComplex ) {
            mat_complex_split_t *complex_data = data;
            long nbytes = nmemb*matvar->data_size;

            ReadDataSlab1(mat,complex_data->Re,matvar->class_type,
                          matvar->data_type,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab1(mat,complex_data->Im,matvar->class_type,
                          matvar->data_type,start,stride,edge);
    } else {
        ReadDataSlab1(mat,data,matvar->class_type,matvar->data_type,start,
                      stride,edge);
    }

    return err;
}

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable in a version 4 MAT file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @retuen pointer to the MAT variable or NULL
 * @endif
 */
matvar_t *
Mat_VarReadNextInfo4(mat_t *mat)
{
    int       tmp,M,O,data_type,class_type;
    long      nBytes;
    size_t    err;
    matvar_t *matvar = NULL;
    union {
        mat_uint32_t u;
        mat_uint8_t  c[4];
    } endian;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;
    else if ( NULL == (matvar = Mat_VarCalloc()) )
        return NULL;

    matvar->internal->fp   = mat;
    matvar->internal->fpos = ftell(mat->fp);

    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        free(matvar);
        return NULL;
    }

    endian.u = 0x01020304;

    /* See if MOPT may need byteswapping */
    if ( tmp < 0 || tmp > 4052 ) {
        if ( Mat_int32Swap(&tmp) > 4052 ) {
            Mat_VarFree(matvar);
            return NULL;
        }
    }

    M = floor(tmp / 1000.0);
    tmp -= M*1000;
    O = floor(tmp / 100.0);
    tmp -= O*100;
    data_type = floor(tmp / 10.0);
    tmp -= data_type*10;
    class_type = floor(tmp);

    switch ( M ) {
        case 0:
            /* IEEE little endian */
            mat->byteswap = (endian.c[0] != 4);
            break;
        case 1:
            /* IEEE big endian */
            mat->byteswap = (endian.c[0] != 1);
            break;
        default:
            /* VAX, Cray, or bogus */
            Mat_VarFree(matvar);
            return NULL;
    }
    /* O must be zero */
    if ( 0 != O ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    /* Convert the V4 data type */
    switch ( data_type ) {
        case 0:
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case 1:
            matvar->data_type = MAT_T_SINGLE;
            break;
        case 2:
            matvar->data_type = MAT_T_INT32;
            break;
        case 3:
            matvar->data_type = MAT_T_INT16;
            break;
        case 4:
            matvar->data_type = MAT_T_UINT16;
            break;
        case 5:
            matvar->data_type = MAT_T_UINT8;
            break;
        default:
            Mat_VarFree(matvar);
            return NULL;
    }
    switch ( class_type ) {
        case 0:
            matvar->class_type = MAT_C_DOUBLE;
            break;
        case 1:
            matvar->class_type = MAT_C_CHAR;
            break;
        case 2:
            matvar->class_type = MAT_C_SPARSE;
            break;
        default:
            Mat_VarFree(matvar);
            return NULL;
    }
    matvar->rank = 2;
    matvar->dims = malloc(2*sizeof(*matvar->dims));
    if ( NULL == matvar->dims ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[0] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[1] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    err = fread(&(matvar->isComplex),sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    /* Check that the length of the variable name is at least 1 */
    if ( tmp < 1 ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    matvar->name = malloc(tmp);
    if ( NULL == matvar->name ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(matvar->name,1,tmp,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    matvar->internal->datapos = ftell(mat->fp);
    nBytes = matvar->dims[0]*matvar->dims[1]*Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex )
        nBytes *= 2;
    fseek(mat->fp,nBytes,SEEK_CUR);

    /* Check if sparse complex matrix */
    if ( !matvar->isComplex
    	 && MAT_T_DOUBLE == matvar->data_type
    	 && MAT_C_SPARSE == matvar->class_type
    	 && 4 == matvar->dims[1] )
        matvar->isComplex = 1;

    return matvar;
}
