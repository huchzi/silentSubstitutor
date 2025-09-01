wavelengths <- seq(390, 780, 2)

# is_valid_spectra_matrix checks whether an object contains LED spectra
#' @export
is_valid_spectra_matrix <- function(x) {
  is_valid <- all(
    is.matrix(x),
    ncol(x) >= 3,
    ncol(x) <= 5,
    nrow(x) == length(wavelengths),
    !any(as.logical(sapply(data.frame(x), function(col) col == wavelengths)))
  )

  return(is_valid)
}

# scale_spectra scales spectra to a set of factors
#' @export
scale_spectra <- function(factors, spectra) {
  stopifnot(
    is.vector(factors),
    is.matrix(spectra),
    length(factors) == ncol(spectra)
  )

  for (i in 1:length(factors)) {
    spectra[, i] <- spectra[, i] * factors[i] / sum(spectra[, i])
  }

  return(
    spectra
  )
}

# get_normalized_spectrum_matrix converts a dataframe to a spectra matrix
#' @export
get_normalized_spectrum_matrix <- function(dtab) {
  stopifnot(
    is.data.frame(dtab),
    ncol(dtab) >= 4,
    ncol(dtab) <= 6,
    names(dtab)[1] %in% c("Wavelength", "wavelength", "wavelength_nm", "wl"),
    as.vector(unlist(dtab[, 1])) == wavelengths
  )

  for (i in 2:ncol(dtab)) {
    dtab[, i] <- dtab[, i] / sum(dtab[, i])
  }

  normalized_spectra <- as.matrix(dtab[, -1])

  ordered_spectra <- normalized_spectra[, order(apply(normalized_spectra, 2, which.max),
    decreasing = TRUE
  )]

  return(
    ordered_spectra
  )
}

# calculate_power_spectra calculates power spectra for a given set of luminances and spectra
#' @export
calculate_power_spectra <- function(luminances, spectra) {
  stopifnot(
    is_valid_spectra_matrix(spectra),
    is.vector(luminances),
    length(luminances) == ncol(spectra)
  )

  led_powers <- convert_luminance_to_power(luminances, spectra)
  scale_spectra(led_powers, spectra)

  return(
    scale_spectra(led_powers, spectra)
  )
}



# create_A_matrix calculates the A-matrix
# old way of normalizing: crossprod(P_led, s_receptors)              # t(P_led) %*% s_receptors
#' @export
create_A_matrix <- function(P_led, s_receptors, normalize = TRUE) {
  stopifnot(
    is_valid_spectra_matrix(P_led),
    is.matrix(s_receptors)
    # dim(P_led) == dim(s_receptors)
  )

  a_matrix <- crossprod(P_led, s_receptors)

  return(
    if (normalize) {
      return(t(t(a_matrix) / apply(a_matrix, 2, sum)))
    } else {
      return(a_matrix)
    }
  )
}

# convert_luminance_to_power converts cd/m^2 to W/m^2
#' @export
convert_luminance_to_power <-
  function(luminances, normalized_spectra) {
    # define v_lambda
    v_lambda <- as.matrix(silentSubstitutor::vlambda$vlambda)

    stopifnot(
      is.vector(luminances),
      is_valid_spectra_matrix(normalized_spectra),
      length(luminances) == ncol(normalized_spectra)
    )

    return(
      as.vector(
        luminances / (683 * crossprod(v_lambda, normalized_spectra))
      )
    )
  }



# find_LED_contrasts calculates LED contrasts by multiplying the pursued #####
# photoreceptor contrast with the inverse A-Matrix (crossproduct)
#' @export
find_LED_contrasts <- function(photoreceptor_contrasts, A_matrix) {
  stopifnot(
    is.vector(photoreceptor_contrasts),
    is.matrix(A_matrix),
    !is.null(names(photoreceptor_contrasts)),
    colnames(A_matrix) == names(photoreceptor_contrasts),
    all(photoreceptor_contrasts <= 1)
  )

  contrast_matrix <- crossprod(photoreceptor_contrasts, solve(A_matrix))
  contrasts <- as.vector(contrast_matrix)
  names(contrasts) <- colnames(contrast_matrix)
  return(
    contrasts
  )
}

##### Modifying fundamentals #####

# shift_cone_fundamentals shifts the cone fundamentals by a given amount
#' @export
shift_cone_fundamentals <- function(fundamentals, receptor, dlambda) {
  if ((dlambda %% 2) > 0) {
    warning("Can only shift by multiples of 2!")
  }
  dlambda <- (dlambda %/% 2)
  fundamentals[, receptor] <- data.table::shift(fundamentals[, receptor],
    dlambda,
    fill = 0
  )
  return(fundamentals)
}

# macular_pigment calculates the effect of the macular pigment on fundamentals
#' @export
macular_pigment <- function(fundamentals, optical_density, mpod_spectrum) {
  mpod.OD <- colorSpec::multiply(mpod_spectrum, optical_density / max(mpod_spectrum[, 2]))
  mpod.trans <- colorSpec::linearize(mpod.OD)

  fundamentals * mpod.trans[, 2]
}

# lens_age calculates the effect of the lens age on fundamentals
#' @export
lens_age <- function(fundamentals, age) {
  lens.trans <- data.frame(colorSpec::linearize(colorSpec::lensAbsorbance(c(30, age), wavelength = 390:780)))
  lens.age <- lens.trans[seq(1, 391, 2), 3] / lens.trans[seq(1, 391, 2), 2]

  fundamentals * lens.age
}
