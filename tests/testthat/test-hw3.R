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

#x12 <- bis620_sparse_matrix(
#  i = rep(1:6,c(rep(6,6))),
#  j = rep(1:6,6),
#  x = c(-0.62645381,4.48364332,-0.83562861,1.59528080,0.32950777,-0.82046838,
#        0.48742905,6.33832471,0.57578135,-0.30538839,1.51178117,0.38984324,
#        -0.62124058,-2.21469989,1.12493092,-0.04493361,-0.01619026,0.94383621,
#        0.82122120,0.59390132,0.91897737,0.78213630,0.07456498,-1.98935170,
#        0.61982575,-0.05612874,-0.15579551,-1.47075238,-0.47815006,7.41794156,
#        11.35867955,-0.10278773)
#)
#test_that("bis620_sparse_add works", {
#  expect_equal(x1 + t(x2), x12)
#})

x13 <- bis620_sparse_matrix(
  i = rep(1:6,c(rep(6,6))),
  j = rep(1:6,6),
  x = c(-0.62645381,4.78742905,-0.62124058,0.82122120,0.61982575,1.35867955,
        0.18364332,6.33832471,-2.21469989,0.59390132,-0.05612874,-0.10278773,
        -0.83562861,0.57578135,1.12493092,0.91897737,-0.15579551,0.38767161,
        1.59528080,-0.30538839,-0.04493361,0.78213630,-1.47075238,-0.05380504,
        0.32950777,1.51178117,-0.01619026,0.07456498,-0.47815006,5.62294044,
        9.17953162,0.38984324,0.94383621,-1.98935170,0.41794156,-0.41499456)
)
test_that("bis620_sparse_add works", {
  expect_equal(x3 + x1, x13)
})

x15 <- bis620_sparse_matrix(
  i = c(1, 1, 2, 3, 5, 6),
  j = c(2, 3, 2, 5, 6, 1),
  x = c(8.6, 5.6, 5.6, 7.0, 7.0, 20.0)
)
test_that("bis620_sparse_add works", {
  expect_equal(x1 + x4, x15)
})

# test the bis620_sparse_matrix_multiply function
x7 <- bis620_sparse_matrix(
  i = c(2, 2, 1, 2),
  j = c(1, 2, 5, 6),
  x = c(24.08, 31.36, 70.00, 43.00)
)
test_that("bis620_sparse_matrix_multiply works", {
  expect_equal(x1 %*% x1, x7)
})

x11 <- bis620_sparse_matrix(
  i = c(2, 6, 1),
  j = c(2, 6, 1),
  x = c(49.85, 49.00, 100.00)
)
test_that("bis620_sparse_matrix_multiply works", {
  expect_equal(bis620_sparse_transpose(x1) %*% x1, x11)
})

x14 <- bis620_sparse_matrix(
  i = rep(1:6,6),
  j = rep(1:6,c(rep(6,6))),
  x = c(1.40064606,0.91275189,-0.15374789,-2.81024569,-1.25519858,-2.60365648,
        2.88448954,-0.94684247,-4.36343087,-0.78114779,-0.47217717,-0.59696048,
        0.78581550,0.30047560,0.83654099,0.62544093,-1.84059165,-0.75421852,
        -0.21065645,-1.95704815,-0.42737224,1.69652258,0.54336178,4.18700565,
        1.17596792,-0.01499189,-4.86638588,3.91569161,-0.33466417,1.51192778,
        -2.89848855,1.50893232,0.39900829,-0.27410373,1.87508721,-1.08519899)
)
test_that("bis620_sparse_matrix_multiply works", {
  expect_equal(x3 %*% x3, x14)
})

x16 <- bis620_sparse_matrix(
  i = c(1, 2, 3),
  j = c(5, 6, 6),
  x = c(70, 43, 56)
)
test_that("bis620_sparse_matrix_multiply works", {
  expect_equal(x1 %*% x4, x16)
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
#test_that("print works", {
#  expect_true(inherits(print(x1), "dgCMatrix"))
#})
