# Changelog

All notable changes to this project will be documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)  
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [0.1.0] - 2025-05-28

### Added
- Initial working version of the Shiny app
- Stimulus computation based on dual, triple, and quadruple silent substitution
- Core structure for photoreceptor-targeted stimuli
- Modularized UI and server setup
- Basic plotting and preview of stimuli
- Deployable `app.R` under `inst/app/`
- Initial unit tests using `testthat`

### Changed
- Improved UI layout for better usability
- Refactored computational functions into separate scripts

### Known Limitations
- Input validation is still limited
- Output preview only; export functionality not yet implemented

---

## [0.9.0] - 2025-05-31

### Added
- Local deployment as a package is functional
- Deployment to shinyapps.io with `rsconnect::deployApp()` is functional

### Changed
- Moved app.R to root directory

### Known Limitations
- Better documentation of datasets in the app is needed
- Bookmarking  does not function well, yet

---

## [Unreleased]

### Planned
- Export functionality for computed stimuli (CSV, JSON)
