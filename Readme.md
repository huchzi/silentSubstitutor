# SilentSubstiTutor

SilentSubstiTutor is an interactive web application built with R and Shiny.
The app enables the calculation of photoreceptor-directed, periodic stimuli based on dual, triple, or quadruple silent substitution for use in electroretinography.

## Features

- Import of emission spectra from the visual stimulator
- Bookmark custom settings for the stimulator
- Calculate photoreceptor-directed stimuli based on the silent substitution paradigm
- Check how changes in the cone fundamentals affect the quality of isolation

## Getting Started

### Install the app

```r
install.packages("devtools")
library(devtools)
install_github("huchzi/silentSubstitutor")
```

### Run the app

```r
library(silentSubstitutor)
silent_substitutor()
```

## Import individual emission spectra

You can import individual emission spectra from a CSV file under `Advanced Settings` > `Upload Spectra`. 

The import process uses the `readSpectra()` function from the **colorSpec** package.

The function searches the file for a header line containing a column that denotes wavelength (the column name must begin with "wave" or "wv", case-insensitive, matching the pattern `^(wave|wv?l)`). The spectrum name should follow in subsequent columns. All lines before this header are ignored; lines after the header must contain the emission spectra data. Each spectrum is normalized so that its values sum to one, and then scaled according to the luminance values set by the sliders.

``` 
Wavelength,RED.LED,GREEN.LED,BLUE.LED,AMBER.LED,CYAN
390,-2.75e-10,1.2e-10,6e-09,4.9e-11,2.27e-09
392,-1.46e-10,1.1e-10,5.6e-09,6.88e-11,2.1e-09
394,-8.77e-11,1.16e-10,5.76e-09,4.99e-11,2.34e-09
396,6.48e-11,8.56e-11,6.29e-09,4.65e-11,2.25e-09
398,1.05e-10,9.62e-11,7.75e-09,2.16e-11,1.87e-09
400,5.76e-11,1e-10,9.83e-09,4.34e-11,2.01e-09
```

The spectra are automatically resampled to the necessary stepsizes using `resampleSpectra()` from the **colorSpec** package.

Importing from Excel files is also supported. For detailed information on the required file format, please consult the documentation for the `readSpectra()` function.

Further details and examples can be found in the colorSpec User Guide:  
[https://cran.r-project.org/web/packages/colorSpec/vignettes/colorSpec-guide.html#spectrum-file-import](https://cran.r-project.org/web/packages/colorSpec/vignettes/colorSpec-guide.html#spectrum-file-import)

## Contribute

Contributions are welcome. To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes
4. Push to your fork
5. Submit a pull request

Please ensure your code is well-documented and tested where appropriate.

### Set Up the Environment

This project uses [`renv`](https://rstudio.github.io/renv/) to manage package dependencies. In R, run:

```r
install.packages("renv")
renv::restore()
```

This will install the exact package versions specified in `renv.lock`.

## Versioning

This project follows [Semantic Versioning](https://semver.org/).
Current version: `v0.9.0`

See the [CHANGELOG.md](CHANGELOG.md) file for detailed release notes.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Further reading

The applications allows to create simple periodic silent substitution stimuli.

For more advanced use cases, please refer to the following resources:

  - PySilentSubstitution: https://pypi.org/project/pysilsub/
  - Protocol for isolation of melanopsin and rhodopsin in the human eye using silent substitution: https://doi.org/10.1016/j.xpro.2023.102126

## Contact

For questions, suggestions, or issues, please use the GitHub Issues section or email:

  c.huchzermeyer@posteo.de
