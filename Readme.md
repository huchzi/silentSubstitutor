# SilentSubstiTutor

SilentSubstiTutor is an interactive web application built with R and Shiny.
The app enables the calculation of photoreceptor-directed, periodic stimuli based on dual, triple, or quadruple silent substitution for use in electroretinography.

## Features

- Import of emission spectra from the apparatus
- Interactive filters and inputs
- File upload and data handling
- Modular and extensible code structure

## Getting Started

### Clone the Repository

```bash
git clone https://github.com/huchzi/MyShinyApp.git
cd MyShinyApp
```

### Set Up the Environment

This project uses [`renv`](https://rstudio.github.io/renv/) to manage package dependencies. In R, run:

  ```r
install.packages("renv")
renv::restore()
```

This will install the exact package versions specified in `renv.lock`.

## Running the App

In R, start the application by running:

```r
shiny::runApp()
```

If you are using a single `app.R` file, simply:

  ```r
source("app.R")
```

## Dependencies

All dependencies are listed in `renv.lock`. However, the main packages used include:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "readr"))
```

## Versioning

This project follows [Semantic Versioning](https://semver.org/).
Current version: `v1.0.0`

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

  your.email@example.com
