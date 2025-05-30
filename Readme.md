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

## Contribute

### Clone the Repository

```bash
git clone https://github.com/huchzi/silentSubstitutor.git
cd MyShinyApp
```

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

## Contributing

Contributions are welcome. To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes
4. Push to your fork
5. Submit a pull request

Please ensure your code is well-documented and tested where appropriate.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

For questions, suggestions, or issues, please use the GitHub Issues section or email:

  c.huchzermeyer@posteo.de
