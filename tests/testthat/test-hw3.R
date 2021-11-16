library(Matrix)
library(bis620)

x1 <- bis620_sparse_matrix(
  i = c(1, 2, 5, 6), 
  j = c(2, 2, 6, 1), 
  x = c(4.3, 5.6, 7, 10)
)

set.seed(1)

x2 <- matrix(rnorm(36), ncol = 6)

library(Matrix)

set.seed(1)

x3 <- Matrix(rnorm(36), ncol = 6)

x4 <- sparseMatrix(  
  i = c(1, 1, 3, 6), 
  j = c(2, 3, 5, 1), 
  x = c(4.3, 5.6, 7, 10),
  dims = c(6, 6)
)

# test the mat_to_df function
set.seed(1)
df <- data.frame(ID = 1:36,                                
                 i = rep(c(1:6),6),                   
                 j = c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)),     
                 x = rnorm(36)) 

test_that("mat_to_df works", {
  expect_equal(class(mat_to_df(x2)), class(df))
})

# test the bis620_sparse_transpose function
x5 <- bis620_sparse_matrix(
  i = c(2, 2, 6, 1), 
  j = c(1, 2, 5, 6), 
  x = c(4.3, 5.6, 7, 10)
)
test_that("bis620_sparse_transpose works", {
  expect_equal(bis620_sparse_transpose(x1), x5)
})



# test the bis620_sparse_add function
x6 <- bis620_sparse_matrix(
  i = c(1, 2, 5, 6), 
  j = c(2, 2, 6, 1), 
  x = c(8.6, 11.2, 14, 20)
)
test_that("bis620_sparse_add works", {
  expect_equal(x1 + x1, x6)
})

# test the bis620_sparse_matrix_multiply function
x7 <- bis620_sparse_matrix(
  i = c(2, 2, 1, 2), 
  j = c(1, 2, 5, 6), 
  x = c(24.08, 31.36, 70.00, 43.00)
)
test_that("bis620_sparse_multiply works", {
  expect_equal(x1 %*% x1, x7)
})


# test the bis620_sparse_minus function
x8 <- bis620_sparse_matrix(
  i = c(3, 2, 5, 6), 
  j = c(1, 2, 3, 5), 
  x = c(-5.6,  5.6, -7.0,  7.0)
)
test_that("bis620_sparse_minus works", {
  expect_equal(x1 - x4, x8)
})

# test the bis620_sparse_divide function
x9 <- bis620_sparse_matrix(
  i = c(2, 2, 6, 1), 
  j = c(1, 2, 5, 6), 
  x = c(8.82179669, 7.58473875, -5.08329503, -12.18816007)
)
test_that("bis620_sparse_divide works", {
  expect_equal(x1 / x3, x9)
})

# test the bis620_sparse_multiply function
x10 <- bis620_sparse_matrix(
  i = c(1,6), 
  j = c(2,1), 
  x = c(18.49, 100.00)
)
test_that("bis620_sparse_multiply works", {
  expect_equal(x1 * x4, x10)
})

# test the print function
test_that("print works", {
  expect_true(inherits(print(x1), "dgCMatrix"))
})

