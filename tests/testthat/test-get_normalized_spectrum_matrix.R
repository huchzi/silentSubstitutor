spec_matrix <- get_normalized_spectrum_matrix(four_primary_LEDs)

test_that("is_matrix", {
  expect_true(is.matrix(spec_matrix))
})

test_that("columns_sorted", {
  expect_equal(colnames(spec_matrix), c("red", "green", "cyan", "blue"))
})

col_sums <- apply(spec_matrix, 2 , sum)
names(col_sums) <- colnames(spec_matrix)

test_that("is_normal", {
  expect_equal(col_sums,
               c(red = 1, green = 1, cyan = 1, blue = 1))
})
