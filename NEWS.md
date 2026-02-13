# mhpfilter 0.1.0

## Initial CRAN Release (2026-02-06)

This is the first official release of **mhpfilter** to CRAN.

### Overview

**mhpfilter** implements the Modified Hodrick-Prescott (HP) Filter for decomposing macroeconomic time series into trend and cyclical components. The package automatically selects the optimal smoothing parameter λ using generalized cross-validation (GCV), based on the methodology of Choudhary, Hanif & Iqbal (2014).

### Core Features

#### Filtering Functions
* `mhp_filter()` - Modified HP filter with automatic λ selection via GCV
* `hp_filter()` - Standard HP filter with fixed λ parameter
* `mhp_compare()` - Compare HP vs Modified HP for single series
* `mhp_batch()` - Batch processing for multiple time series
* `batch_compare()` - Batch comparison of HP vs Modified HP

#### Visualization Functions
* `autoplot.mhp()` - ggplot2-based visualization for mhp objects
* `plot_comparison()` - Visual comparison of HP vs Modified HP results
* `plot_batch()` - Visualization of batch processing results

#### Utility Functions
* `get_lambda()` - Extract optimal λ from results
* `get_gcv()` - Extract GCV value from results
* `print.mhp()` - Print method for mhp class objects

### Technical Implementation

#### High Performance
* Fast C++ implementation using RcppArmadillo for matrix operations
* Optimized grid search algorithm for λ selection
* Efficient memory management for large time series
* Linear scaling for batch processing of multiple series

#### Modern R Integration
* Full compatibility with `data.table` (≥ 1.14.0)
* Integration with `collapse` (≥ 2.0.0) for fast statistics
* Support for `kit` (≥ 0.0.11) utilities
* Works seamlessly with `tidyverse` (≥ 2.0.0) and `fastverse` (≥ 0.3.0)
* ggplot2-based professional visualizations

### Documentation

#### Comprehensive Vignettes
* **Introduction** - Quick start guide with practical examples
  - Basic usage and workflow
  - Comparison with standard HP filter
  - Real-world applications
  
* **Methodology** - Mathematical theory and derivations
  - Standard HP filter formulation
  - Generalized cross-validation criterion
  - Optimal λ selection procedure
  - Performance characteristics

#### Function Documentation
* Complete roxygen2 documentation for all functions
* Mathematical formulas properly rendered with LaTeX
* Extensive examples with commentary
* Author affiliations and ORCID identifiers

#### Package Website
* Professional pkgdown site with full documentation
* Download statistics and badges
* Interactive examples and tutorials
* API reference with search functionality

### Methodology Reference

This implementation is based on:

**Choudhary, M.A., Hanif, M.N., & Iqbal, J. (2014).** "On smoothing macroeconomic time series using the modified HP filter." *Applied Economics*, 46(19), 2205-2214. https://doi.org/10.1080/00036846.2014.896982

### Authors and Contributors

#### Package Author and Maintainer
**Muhammad Yaseen**  
Postdoctoral Research Fellow  
School of Mathematical and Statistical Sciences  
Clemson University

#### Original Methodology Authors
**Javed Iqbal** - State Bank of Pakistan  
**M. Nadim Hanif** - State Bank of Pakistan

### Installation

```r
# From CRAN (recommended)
install.packages("mhpfilter")

# From GitHub (development version)
devtools::install_github("myaseen208/mhpfilter")
```

### System Requirements

* R ≥ 4.0.0
* C++ compiler for building from source
* Operating systems: Windows, macOS, Linux

### Testing and Quality Assurance

* Comprehensive test suite with testthat (≥ 3.0.0)
* Continuous integration via GitHub Actions
* R CMD check passes on all platforms
* CRAN policy compliance verified
* No NOTEs, WARNINGs, or ERRORs

### License

MIT License

### Bug Reports and Issues

Please report bugs at: https://github.com/myaseen208/mhpfilter/issues

### Acknowledgments

* Original methodology: Choudhary, Hanif & Iqbal (2014)
* RcppArmadillo team for excellent C++ integration
* R Core Team for the R statistical computing environment
* CRAN team for review and hosting

---

For more information, visit: https://github.com/myaseen208/mhpfilter
