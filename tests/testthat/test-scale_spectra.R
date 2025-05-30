test_that("scale1234_2", {
  spectra_matrix <- as.matrix(four_primary_LEDs[, -1])
  scaled_matrix <- scale_spectra(c(.1, 2, 3, 4), spectra_matrix)
  expect_equal(scaled_matrix[, 1] / spectra_matrix[, 1], rep(.1, nrow(spectra_matrix)))
})

test_that("scale1234_2", {
  spectra_matrix <- as.matrix(four_primary_LEDs[, -1])
  scaled_matrix <- scale_spectra(c(1, 2, 3, 4), spectra_matrix)
  expect_true(all((scaled_matrix[, 2] / spectra_matrix[, 2]) == 2))
})

test_that("scale1234_3", {
  spectra_matrix <- as.matrix(four_primary_LEDs[, -1])
  scaled_matrix <- scale_spectra(c(1, 2, 3, 4), spectra_matrix)
  expect_equal(scaled_matrix[, 3] / spectra_matrix[, 3], rep(3.0, nrow(spectra_matrix)))
})

test_that("scale1234_4", {
  spectra_matrix <- as.matrix(four_primary_LEDs[, -1])
  scaled_matrix <- scale_spectra(c(1, 2, 3, 4), spectra_matrix)
  expect_true(all((scaled_matrix[, 4] / spectra_matrix[, 4]) == 4))
})
