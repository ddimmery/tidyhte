# Contributing to tidyhte

Thank you for your interest in contributing to tidyhte! We welcome contributions from the community.

## Project Vision

`tidyhte` aims to make modern heterogeneous treatment effect estimation with DR-learner accessible to applied researchers through a user-friendly interface. The focus is on providing reliable implementations with sensible defaults while maintaining flexibility for advanced users. We particularly value contributions that improve usability, add well-tested features, and enhance documentation.

## Governance

This project is maintained by Drew Dimmery, who makes decisions about the project direction. Substantial contributors may be invited to join a core development team, with decisions made by consensus among team members.

## Versioning

This project uses [semantic versioning](https://semver.org/). The API is stable as of version 1.0.0, and there are no current plans for breaking changes. Patch versions (1.0.x) fix bugs, minor versions (1.x.0) add backwards-compatible features, and major versions (x.0.0) would introduce breaking changes (which are not currently planned).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Getting Started

### Prerequisites

- R (>= 3.5.0)
- RStudio (recommended)
- Familiarity with the tidyverse and R package development

### Setting Up Your Development Environment

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/tidyhte.git
   cd tidyhte
   ```

3. Install development dependencies:
   ```r
   install.packages("devtools")
   devtools::install_dev_deps()
   ```

4. Load the package for development:
   ```r
   devtools::load_all()
   ```

## How to Contribute

### Reporting Bugs

If you find a bug, please [open an issue](https://github.com/ddimmery/tidyhte/issues/new) with:

- A clear, descriptive title
- A minimal reproducible example (reprex) demonstrating the bug
- Your R version and platform information (`sessionInfo()`)
- What you expected to happen vs. what actually happened

### Suggesting Enhancements

Enhancement suggestions are welcome! Please open an issue describing:

- The motivation for the enhancement
- How it would benefit users
- Any implementation ideas you have

### Pull Requests

1. Create a new branch for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes, following the coding style guidelines below

3. Add tests for your changes in `tests/testthat/`

4. Run R CMD check to ensure everything passes:
   ```r
   devtools::check()
   ```

5. Update documentation if needed:
   ```r
   devtools::document()
   ```

6. Commit your changes with a clear commit message

7. Push to your fork and submit a pull request

## Coding Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use roxygen2 for function documentation
- Keep functions focused and modular
- Write clear, descriptive variable and function names
- Run `styler::style_pkg()` before committing
- Ensure `lintr::lint_package()` passes

## Testing

- All new functionality should include unit tests
- Tests use the testthat framework
- Run tests with `devtools::test()`
- Aim to maintain or improve code coverage (current: 80%+)
- Check coverage with `covr::package_coverage()`

### Running Tests Locally

```r
# Run all tests
devtools::test()

# Run tests for a specific file
testthat::test_file("tests/testthat/test-api-mcate.R")

# Check test coverage
covr::package_coverage()
```

## Documentation

- Document all exported functions using roxygen2
- Include `@examples` for user-facing functions
- Update vignettes if adding major features
- Keep README.md up to date
- Run `devtools::document()` to regenerate .Rd files

### Building Vignettes

When modifying vignettes:

1. Edit the .Rmd source files in `vignettes/`
2. Rebuild vignettes with:
   ```r
   devtools::build_vignettes()
   ```

## Pull Request Checklist

Before submitting a pull request, ensure:

- [ ] All tests pass locally (`devtools::test()`)
- [ ] R CMD check passes with no errors, warnings, or notes (`devtools::check()`)
- [ ] New tests added for new functionality
- [ ] Documentation updated (roxygen2, vignettes if needed)
- [ ] Code follows tidyverse style guide
- [ ] NEWS.md updated with user-facing changes
- [ ] Test coverage maintained or improved

## Package Building

To build and install the package locally:

```r
devtools::build()
devtools::install()
```

To check the package as CRAN does:

```r
devtools::check(cran = TRUE)
```

## Version Control

- We use semantic versioning (MAJOR.MINOR.PATCH)
- Update NEWS.md with notable changes
- Only maintainers increment version numbers

## Questions?

- Open a GitHub issue for technical questions
- Join the [Experimentation Community Discord](https://discord.com/invite/MrxjbHc3jD) for real-time discussion (use the #tidyhte channel)
- Email the maintainer at cran@ddimmery.com for other inquiries

## Recognition

Contributors will be recognized in the package documentation and may be listed as contributors (ctb) in the DESCRIPTION file for substantial contributions.

## License

By contributing, you agree that your contributions will be licensed under the MIT License that covers this project.
