load_led_spectra <- function(path, wavelengths) {
  try({
    colorSpec::readSpectra(path) |>
    colorSpec::resample(wavelengths) |>
    data.frame() |>
    get_normalized_spectrum_matrix()
  })
}