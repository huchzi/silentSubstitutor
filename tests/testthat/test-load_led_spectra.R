library(here)
i_am("tests/testthat/test-load_led_spectra.R")

spec_matrix <- load_led_spectra(here("tests", "testthat", "MCVS.csv"), wavelengths)

# print(head(mcvs))
#
# test_that("mcvs: returns matrix", {
#   expect_equal(class(mcvs), c("matrix", "array"))
# })
#
# test_that("mcvs: matrix has correct dimension", {
#   expect_equal(dim(mcvs), c(length(wavelengths), 5))
# })

test_that("is_matrix", {
  expect_true(is.matrix(spec_matrix))
})

test_that("columns_sorted", {
  expect_equal(colnames(spec_matrix), c("RED.LED", "AMBER.LED", "GREEN.LED", "CYAN", "BLUE.LED"))
})

col_sums <- apply(spec_matrix, 2 , sum)
names(col_sums) <- colnames(spec_matrix)

test_that("is_normal", {
  expect_equal(col_sums,
               c(RED.LED = 1, AMBER.LED = 1, GREEN.LED = 1, CYAN = 1, BLUE.LED = 1))
})
