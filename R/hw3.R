library(Matrix)

#' @title bis620_sparse_matrix
#' @description create a sparse matrix class
#' @rdname sparse-matrix-class
#' @export
bis620_sparse_matrix <- setClass(Class = "bis620_sparse_matrix",
                                 slots=list(i="numeric",
                                            j="numeric",
                                            x="numeric")
)

#############################################################################

#' @title mat_to_df
#' @description transform matrix to dataframe
#' @param mat a matrix
#' @return a dataframe
#' @rdname mat_to_df
#' @export
mat_to_df <- function(mat){
  i <- c()
  j <- c()
  x <- c()
  for (c in 1:ncol(mat)){
    for (r in 1:nrow(mat)){
      if (mat[r,c] != 0){
        i <- c(i, r)
        j <- c(j, c)
        x <- c(x, mat[r,c])}
    }
  }
  data.frame(i=i,j=j,x=x)
}

#############################################################################

#' @title bis620_sparse_transpose
#' @description get the transpose of the matrix
#' @param a sparse matrix or dgeMatrix or dgCMatrix or matrix
#' @return transpose of the input matrix
#' @rdname bis620_sparse_transpose
#' @export
bis620_sparse_transpose <- function(a){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  # Matrix::sparseMatrix:print(sparseMatrix(i=a$j,j=a$i,x=a$x))
  return(bis620_sparse_matrix(i=a$j,j=a$i,x=a$x))
}

#' @title bis620_sparse_transpose
#' @param x sparse matrix
#' @rdname bis620_sparse_transpose_sparse
#' @export
setMethod(f = "t",signature = c(x = "bis620_sparse_matrix"), function(x){bis620_sparse_transpose(x)})

#' @title bis620_sparse_transpose
#' @param x dgCMatrix
#' @rdname bis620_sparse_transpose_dgCMatrix
#' @export
setMethod(f = "t",signature = c(x = "dgCMatrix"), function(x){bis620_sparse_transpose(x)})

#' @title bis620_sparse_transpose
#' @param x dgeMatrix
#' @rdname bis620_sparse_transpose_dgeMatrix
#' @export
setMethod(f = "t",signature = c(x = "dgeMatrix"), function(x){bis620_sparse_transpose(x)})

#' @title bis620_sparse_transpose
#' @param x matrix
#' @rdname bis620_sparse_transpose_matrix
#' @export
setMethod(f = "t",signature = c(x = "matrix"), function(x){bis620_sparse_transpose(x)})

#############################################################################

#' @title bis620_sparse_add
#' @description add two input matrix together
#' @param a sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @param b sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @return a sparse matrix
#' @rdname bis620_sparse_add
#' @export
bis620_sparse_add <- function(a, b){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  ifelse(class(b) == "bis620_sparse_matrix",
         b <- data.frame(i=b@i,j=b@j,x=b@x),
         b <- mat_to_df(b))
  ab <- merge(a, b, by= c("i", "j"), all = TRUE, suffixes = c("", "2"))
  ab$x[is.na(ab$x)] <- 0
  ab$x2[is.na(ab$x2)] <- 0
  ab$x <- ab$x + ab$x2
  result <- ab[, c("i", "j", "x")]
  # Matrix::sparseMatrix:print(sparseMatrix(i=result$i,j=result$j,x=result$x))
  return(bis620_sparse_matrix(i=result$i,j=result$j,x=result$x))
}

#' @title bis620_sparse_add_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "bis620_sparse_matrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 bis620_sparse_matrix
#' @param e2 matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "bis620_sparse_matrix", e2 = "matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgCMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgCMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgCMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgCMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgCMatrix
#' @param e2 bis620_sparse_matrix
#'
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgCMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgCMatrix
#' @param e2 matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgCMatrix", e2 = "matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgeMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgeMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgeMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgeMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgeMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgeMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 dgeMatrix
#' @param e2 matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "dgeMatrix", e2 = "matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 matrix
#' @param e2 dgeMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "matrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 matrix
#' @param e2 dgCMatrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "matrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#' @title bis620_sparse_add_1
#' @param e1 matrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_sparse_add_1
#' @export
setMethod(f = '+',signature = c(e1 = "matrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_add(e1,e2)})

#############################################################################

#' @title bis620_multiply
#' @description multiply two input matrix together
#' @param a sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @param b sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @return a sparse matrix
#' @rdname bis620_sparse_multiply
#' @export
bis620_sparse_multiply <- function(a, b){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  ifelse(class(b) == "bis620_sparse_matrix",
         b <- data.frame(i=b@i,j=b@j,x=b@x),
         b <- mat_to_df(b))
  ab <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("", "2"))
  ab$x[is.na(ab$x)] <- 0
  ab$x2[is.na(ab$x2)] <- 0
  ab$x <- ab$x * ab$x2
  ab<-ab[ab$x!=0, ]
  result <- ab[, c("i", "j", "x")]
  # Matrix::sparseMatrix:print(sparseMatrix(i=result$i,j=result$j,x=result$x))
  return(bis620_sparse_matrix(i=result$i,j=result$j,x=result$x))
}

#' @title bis620_multiply_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "bis620_sparse_matrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgCMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgCMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgCMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgCMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgCMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgCMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgeMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgeMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgeMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgeMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#' @title bis620_multiply_1
#' @param e1 dgeMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_multiply_1
#' @export
setMethod(f = '*',signature = c(e1 = "dgeMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_multiply(e1,e2)})

#############################################################################

#' @title bis620_minus
#' @description the first input matrix minus the second matrix
#' @param a sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @param b sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @return a sparse matrix
#' @rdname bis620_sparse_minus
#' @export
bis620_sparse_minus <-  function(a, b){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  ifelse(class(b) == "bis620_sparse_matrix",
         b <- data.frame(i=b@i,j=b@j,x=b@x),
         b <- mat_to_df(b))
  ab <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("", "2"))
  ab$x[is.na(ab$x)] <- 0
  ab$x2[is.na(ab$x2)] <- 0
  ab$x <- ab$x - ab$x2
  ab<-ab[ab$x!=0, ]
  result <- ab[, c("i", "j", "x")]
  # Matrix::sparseMatrix:print(sparseMatrix(i=result$i,j=result$j,x=result$x))
  return(bis620_sparse_matrix(i=result$j,j=result$i,x=result$x))
}

#' @title bis620_minus_1
#' @param e1 dgCMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "bis620_sparse_matrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgCMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgCMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgCMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgCMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgCMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgCMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgeMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgeMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgeMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgeMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#' @title bis620_minus_1
#' @param e1 dgeMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_minus_1
#' @export
setMethod(f = '-',signature = c(e1 = "dgeMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_minus(e1,e2)})

#############################################################################

#' @title bis620_divide
#' @description the first input matrix divide the second matrix
#' @param a sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @param b sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @return a sparse matrix
#' @rdname bis620_sparse_divide
#' @export
bis620_sparse_divide <- function(a, b){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  ifelse(class(b) == "bis620_sparse_matrix",
         b <- data.frame(i=b@i,j=b@j,x=b@x),
         b <- mat_to_df(b))
  ab <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("", "2"))
  ab$x[is.na(ab$x)] <- 0
  ab$x2[is.na(ab$x2)] <- 0
  ab<-ab[(ab$x!=0 & ab$x2!=0), ]
  ab$x <- ab$x / ab$x2
  result <- ab[, c("i", "j", "x")]
  # Matrix::sparseMatrix:print(sparseMatrix(i=result$i,j=result$j,x=result$x))
  return(bis620_sparse_matrix(i=result$j,j=result$i,x=result$x))
}

#' @title bis620_divide_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "bis620_sparse_matrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "bis620_sparse_matrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgCMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgCMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgCMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgCMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgCMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgCMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgeMatrix
#' @param e2 dgeMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgeMatrix", e2 = "dgeMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgeMatrix
#' @param e2 dgCMatrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgeMatrix", e2 = "dgCMatrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#' @title bis620_divide_1
#' @param e1 dgeMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname bis620_divide_1
#' @export
setMethod(f = '/',signature = c(e1 = "dgeMatrix", e2 = "bis620_sparse_matrix"), function(e1,e2){bis620_sparse_divide(e1,e2)})

#############################################################################

#' @title bis620_matrix_multiply
#' @description matrix multiplication of the two input matrixs
#' @param a sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @param b sparse matrixs or dgeMatrixs or dgCMatrixs or matrixs
#' @return a sparse matrix
#' @rdname bis620_sparse_matrix_multiply
#' @export
bis620_sparse_matrix_multiply <- function(a, b){
  ifelse(class(a) == "bis620_sparse_matrix",
         a <- data.frame(i=a@i,j=a@j,x=a@x),
         a <- mat_to_df(a))
  ifelse(class(b) == "bis620_sparse_matrix",
         b <- data.frame(i=b@i,j=b@j,x=b@x),
         b <- mat_to_df(b))
  i <- c()
  j <- c()
  x <- c()
  for (p in unique(a$i)){
    for (q in unique(b$j)){
      m <-  a[a$i == p,]
      n <-  b[b$j == q,]
      mn <- merge(m,n,by.x="j",by.y="i")
      mn$x <- mn$x.x * mn$x.y
      i <- c(i, p)
      j <- c(j, q)
      x <- c(x, sum(mn$x)) }
  }
  result <- data.frame(i=i,j=j,x=x)
  result1 <- result[result$x != 0, ]
  # Matrix::sparseMatrix:print(sparseMatrix(i=result$i,j=result$j,x=result$x))
  return(bis620_sparse_matrix(i=result1$j,j=result1$i,x=result1$x))
}

#' @title bis620_matrix_multiply_1
#' @param x bis620_sparse_matrix
#' @param y matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "bis620_sparse_matrix", y = "matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x bis620_sparse_matrix
#' @param y dgeMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "bis620_sparse_matrix", y = "dgeMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x bis620_sparse_matrix
#' @param y dgCMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "bis620_sparse_matrix", y = "dgCMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x bis620_sparse_matrix
#' @param y bis620_sparse_matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "bis620_sparse_matrix", y = "bis620_sparse_matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgCMatrix
#' @param y matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgCMatrix", y = "matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgCMatrix
#' @param y dgeMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgCMatrix", y = "dgeMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgCMatrix
#' @param y dgCMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgCMatrix", y = "dgCMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgCMatrix
#' @param y bis620_sparse_matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgCMatrix", y = "bis620_sparse_matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgeMatrix
#' @param y matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgeMatrix", y = "matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgeMatrix
#' @param y dgeMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgeMatrix", y = "dgeMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgeMatrix
#' @param y dgCMatrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgeMatrix", y = "dgCMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#' @title bis620_matrix_multiply_1
#' @param x dgeMatrix
#' @param y bis620_sparse_matrix
#' @rdname bis620_matrix_multiply_1
#' @export
setMethod(f = '%*%',signature = c(x = "dgeMatrix", y = "bis620_sparse_matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})
# setMethod(f = '%*%',signature = c(x = "matrix", y = "matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})
# setMethod(f = '%*%',signature = c(x = "matrix", y = "dgeMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})
# setMethod(f = '%*%',signature = c(x = "matrix", y = "dgCMatrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})
# setMethod(f = '%*%',signature = c(x = "matrix", y = "bis620_sparse_matrix"), function(x,y){bis620_sparse_matrix_multiply(x,y)})

#############################################################################


#bis620_sparse_print <- function(x){
#  print(sparseMatrix(i=x@i,j=x@j,x=x@x))
#  return()
#}
#setMethod(f = "print",signature(x = "bis620_sparse_matrix"), function(x){print(sparseMatrix(i=x@i,j=x@j,x=x@x))})


#' @title Print bis620_sparse_matrix
#'
#' @description Print a bis620_sparse_matrix in general matrix layout.
#' @param x A bis620_sparse_matrix.
#' @return A sparse matrix in general matrix layout.
#' @importFrom Matrix sparseMatrix
#' @rdname print_bis620_sparse_matrix
#' @export
setMethod(
  f="print",
  signature = c(x="bis620_sparse_matrix"),
  function(x) {
    print(sparseMatrix(
      i = x@i,
      j = x@j,
      x = x@x)
    )
    invisible(x)
  }
)
