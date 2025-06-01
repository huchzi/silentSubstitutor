#' Emission Spectra of LEDs (Example Dataset)
#'
#' This dataset contains emission spectra of four different LEDs (red, green, blue, cyan)
#' measured across the wavelength range from 390 nm to 780 nm in 2-nm increments.
#' The values represent the normalized spectral radiant power for each LED at the respective wavelengths.
#'
#' @format A data frame with 196 rows and 5 columns:
#' \describe{
#'   \item{wavelength_nm}{Numeric. Wavelength in nanometers (nm), from 390 to 780 in 2-nm steps.}
#'   \item{red}{Numeric. Emission spectrum of the red LED (normalized intensity).}
#'   \item{green}{Numeric. Emission spectrum of the green LED (normalized intensity).}
#'   \item{blue}{Numeric. Emission spectrum of the blue LED (normalized intensity).}
#'   \item{cyan}{Numeric. Emission spectrum of the cyan LED (normalized intensity).}
#' }
#'
#' @details
#' The spectral intensities for each LED are normalized so that the values reflect the relative distribution of radiant power
#' across the visible spectrum. Small negative values may occur due to measurement noise or background correction and are typically negligible.
#'
#' @usage data(led_spectra)
#'
#' @examples
#' data(led_spectra)
#' head(led_spectra)
#' matplot(led_spectra$wavelength_nm, led_spectra[,2:5], type = "l",
#'         lty = 1, col = c("red", "green", "blue", "cyan"),
#'         xlab = "Wavelength (nm)", ylab = "Normalized Intensity",
#'         main = "Emission Spectra of LEDs")
#'
#' @source Own measurements or example values for demonstration purposes.
"four_primary_LEDs"


#' Optical Density of the Macular Pigment
#'
#' This dataset contains the spectral optical density of the macular pigment as a function of wavelength.
#' Measurements were taken from 390 nm to 780 nm in 2-nm increments.
#'
#' @format A data frame with 196 rows and 2 columns:
#' \describe{
#'   \item{Wavelength}{Numeric. Wavelength in nanometers (nm).}
#'   \item{macss_1.optical_density}{Numeric. Measured optical density of the macular pigment.}
#' }
#'
#' @details
#' The macular pigment is a yellowish pigment located in the macula, the central area of the retina.
#' It primarily absorbs short-wavelength (blue) light, thereby protecting the retina from phototoxic damage.
#' The optical density indicates how strongly the pigment absorbs light at a given wavelength.
#' A value of 0 means no absorption, while higher values indicate stronger absorption.
#'
#' @usage data(mpod_spectrum)
#'
#' @examples
#' data(mpod_spectrum)
#' head(mpod_spectrum)
#' plot(mpod_spectrum$Wavelength,
#'      mpod_spectrum$macss_1.optical_density,
#'      type = "l", xlab = "Wavelength (nm)", ylab = "Optical Density",
#'      main = "Optical Density of the Macular Pigment")
#'
#' @source Bone, R. A., Landrum, J. T., & Cains, A. (1992). Optical density spectra of the macular pigment in vivo and in vitro. Vision Research, 32, 105-110.
#' @source Stockman, A., Sharpe, L. T., & Fach, C. C. (1999). The spectral sensitivity of the human short-wavelength cones. Vision Research, 39, 2901-2927.
#' @source www.cvrl.org
"mpod_spectrum"

#' Spectral Sensitivity Functions of Human Photoreceptors
#'
#' This dataset contains the normalized spectral sensitivity functions for the five main human photoreceptor types:
#' melanopsin-containing intrinsically photosensitive retinal ganglion cells (ipRGCs), rods, S-cones, M-cones, and L-cones.
#' The data represent the relative sensitivity of each photoreceptor type across the visible spectrum.
#'
#' @format A data frame with 196 rows and 5 columns:
#' \describe{
#'   \item{melanopsin}{Numeric. Relative spectral sensitivity of melanopsin-containing ipRGCs.}
#'   \item{rod}{Numeric. Relative spectral sensitivity of rods.}
#'   \item{scone}{Numeric. Relative spectral sensitivity of short-wavelength-sensitive cones (S-cones).}
#'   \item{mcone}{Numeric. Relative spectral sensitivity of medium-wavelength-sensitive cones (M-cones).}
#'   \item{lcone}{Numeric. Relative spectral sensitivity of long-wavelength-sensitive cones (L-cones).}
#' }
#'
#' @details
#' The spectral sensitivities are normalized and typically derived from physiological measurements or established template functions.
#' These functions are widely used in vision science, chronobiology, and lighting research to model the response of the human visual and non-visual system to light stimuli.
#'
#' @usage data(photoreceptor_sensitivities)
#'
#' @examples
#' data(photoreceptor_sensitivities)
#' head(photoreceptor_sensitivities)
#' matplot(photoreceptor_sensitivities, type = "l", lty = 1,
#'         col = c("black", "grey", "blue", "green", "red"),
#'         xlab = "Index (arbitrary, e.g., wavelength)", ylab = "Relative Sensitivity",
#'         main = "Spectral Sensitivity Functions of Human Photoreceptors")
#'
#' @source S-cones: Stockman, A., Sharpe, L. T., & Fach, C. C. (1999). The spectral sensitivity of the human short-wavelength cones. Vision Research, 39, 2901-2927.
#' @source L-/M-cones: Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of the middle- and long-wavelength sensitive cones derived from measurements in observers of known genotype. Vision Research, 40, 1711-1737.
#' @source Rods: Wyszecki, G., & Stiles, W. S. (1982). Color Science: concepts and methods, quantitative data and formulae. (2nd ed.). New York: Wiley.
#' @source Melanopsin: DOI: 10.25039/CIE.DS.vqqhzp5a
#' @source www.cvrl.org
"photoreceptor_spectral_sensitivities"

#' CIE 1924 Photopic Luminous Efficiency Function (V-lambda)
#'
#' This dataset provides the CIE 1924 photopic luminous efficiency function V(λ), together with the corresponding wavelengths and frequencies.
#' The data cover the visible spectrum from 390 nm to 780 nm in 2-nm increments.
#'
#' @format A data frame with 196 rows and 3 columns:
#' \describe{
#'   \item{wavelength_nm}{Numeric. Wavelength in nanometers (nm).}
#'   \item{frequency_THz}{Numeric. Frequency in terahertz (THz) corresponding to each wavelength.}
#'   \item{vlambda}{Numeric. Value of the photopic luminous efficiency function V(λ) at each wavelength.}
#' }
#'
#' @details
#' The photopic luminous efficiency function V(λ) describes the average spectral sensitivity of human visual perception of brightness under well-lit (photopic) conditions.
#' It is a key standard function in color science and photometry, and is used to convert spectral power distributions to luminous quantities such as lumens.
#'
#' @usage data(vlambda_cie1924)
#'
#' @examples
#' data(vlambda_cie1924)
#' head(vlambda_cie1924)
#' plot(vlambda_cie1924$wavelength_nm, vlambda_cie1924$vlambda, type = "l",
#'      xlab = "Wavelength (nm)", ylab = "V(lambda)",
#'      main = "CIE 1924 Photopic Luminous Efficiency Function")
#'
#' @source CIE (1926). Commission Internationale de l'Eclairage Proceedings, 1924. Cambridge: Cambridge University Press.
#' @source www.cvrl.org
"vlambda"
