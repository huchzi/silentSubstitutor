calculate_primary_RGBs <- function(n_primaries, led_spectra, wavelengths) {
  rgb_specs <-
    calculate_power_spectra(rep(10000, n_primaries), led_spectra) |>
    colorSpec::colorSpec(wavelengths) |>
    colorSpec::product(colorSpec::BT.709.RGB, wavelength = "auto") |>
    colorSpec::DisplayRGBfromLinearRGB()

  rgb_specs |>
    t() |>
    data.frame() |>
    lapply(function(x)
      grDevices::rgb(x[1], x[2], x[3]))
}
