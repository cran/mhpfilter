

# mhpfilter 1.0.0

## Changes visible to users

- Every function keeps its name, its arguments and its results, so
  existing scripts run unchanged; the results of every example of
  version 0.1.0 are reproduced to 1e-8. New arguments are appended with
  defaults that give the old behaviour (`log_scale = FALSE`,
  `criterion = "approximate"`, `fmhp = FALSE`), and the frequency
  `"monthly"` (lambda = 129600) is added to the comparisons.
- Every function that takes a series or a result is an S3 generic with a
  `.default` method. A `ts` keeps its time and frequency in the results.
- With `as_dt = FALSE` the filters return lists of class
  `c("mhp_<filter>", "mhp", "mhp_result", "list")` with `print()`,
  `summary()`, `plot()`, `ggplot2::autoplot()` and `as.data.table()`
  methods; the printed result ends with the sources of the method.
- No startup message.

## Speed

- Every filter is one pentadiagonal solve, O(T), instead of a dense one,
  O(T^3): `mhp_filter()` searches 100000 values of lambda for 200
  observations in 1.3 s (6.4 s for 0.1.0), and 3000 values for 500
  observations 20 times faster than 0.1.0 (vignette “Validation and
  speed”).

## Corrections

- `plot_batch()` passed `linewidth` twice (a warning) and ignored
  `highlight` in the faceted plot.
- The help page of `mhp_filter()` printed the factor of the criterion as
  `1 + 2/(T lambda)`; the code used, and still uses, `1 + 2T/lambda`
  (eq. 6 of Choudhary et al. 2014).
- `mhp_batch()` and `batch_compare()` accept series of different lengths
  (leading and trailing missing values).

## New

- `fmhp_filter()` and `fmhp_batch()`: the fully modified HP filter of
  Hanif, Iqbal and Choudhary (2017), with endogenous lambda and
  endogenous linear or quadratic end-point weights; it reproduces the
  published MATLAB code (`form = "rows"`, the default) and offers the
  penalised least-squares form (`form = "differences"`) and a fixed
  lambda (`lambda =`).
- `fmhp_compare()`: HP, modified HP and fully modified HP filters
  compared by the moments of the cycles and the end-point loss.
- `hp_loss()`, `hp_gain()`, `hp_weights()`, `hp_schedule()`: the
  end-point loss of Bloechl (2014), the gain functions, the filter
  weights and the end-point schedules of any HP-type filter. The
  end-point losses of the HP filter in Tables 3a and 3b of Hanif et
  al. (2017) are reproduced, and so are the annual losses of the paper’s
  own scheme (quadratic weights, k = 20, alpha = 10) and the weights of
  its Figure 1a; the quarterly losses of its scheme are not, because the
  paper does not report the weights behind them.
- `mhp_filter(criterion = "exact")`: exact generalised cross-validation
  (eq. 5 of Choudhary et al. 2014) beside the published approximation;
  on the US series it is smallest at lambda = 1, which the help page
  explains.
- `mhp_designs()`, `mhp_simulate()`, `fmhp_simulation()`: the simulation
  designs of the 2014 and 2017 papers, simulated trend-plus-cycle series
  and the power comparison of the filters. The results of the
  comparison, for every design, are shipped and discussed in a vignette;
  they do not confirm that the fully modified filter wins in every
  replicate.
- `mhp_interpret()`, `mhp_cite()`, `mhp_app()`: a plain-language
  interpretation of every result, the references of every method and a
  built-in shiny application; `mhp_theme()`, `mhp_save()` (with WebP),
  `mhp_notation()`, `mhp_reference()`.
- Data `us_macro`: quarterly US real GDP, consumption and investment,
  1959 to 2009.
- Quarto vignettes in HTML and PDF; complete help pages with the
  formulas; tests against both MATLAB codes of the authors and against
  the published values.

## Internal

- collapse is no longer imported; the help pages, README and NEWS are
  generated from roxygen and Quarto.

# mhpfilter 0.1.0

## Initial CRAN Release (2026-02-06)

This is the first official release of **mhpfilter** to CRAN.

### Overview

**mhpfilter** implements the Modified Hodrick-Prescott (HP) Filter for
decomposing macroeconomic time series into trend and cyclical
components. The package automatically selects the optimal smoothing
parameter λ using generalized cross-validation (GCV), based on the
methodology of Choudhary, Hanif & Iqbal (2014).

### Core Features

#### Filtering Functions

- `mhp_filter()` - Modified HP filter with automatic λ selection via GCV
- `hp_filter()` - Standard HP filter with fixed λ parameter
- `mhp_compare()` - Compare HP vs Modified HP for single series
- `mhp_batch()` - Batch processing for multiple time series
- `batch_compare()` - Batch comparison of HP vs Modified HP

#### Visualization Functions

- `autoplot.mhp()` - ggplot2-based visualization for mhp objects
- `plot_comparison()` - Visual comparison of HP vs Modified HP results
- `plot_batch()` - Visualization of batch processing results

#### Utility Functions

- `get_lambda()` - Extract optimal λ from results
- `get_gcv()` - Extract GCV value from results
- `print.mhp()` - Print method for mhp class objects

### Technical Implementation

#### High Performance

- Fast C++ implementation using RcppArmadillo for matrix operations
- Optimized grid search algorithm for λ selection
- Efficient memory management for large time series
- Linear scaling for batch processing of multiple series

#### Modern R Integration

- Full compatibility with `data.table` (≥ 1.14.0)
- Integration with `collapse` (≥ 2.0.0) for fast statistics
- Support for `kit` (≥ 0.0.11) utilities
- Works seamlessly with `tidyverse` (≥ 2.0.0) and `fastverse` (≥ 0.3.0)
- ggplot2-based professional visualizations

### Documentation

#### Comprehensive Vignettes

- **Introduction** - Quick start guide with practical examples
  - Basic usage and workflow
  - Comparison with standard HP filter
  - Real-world applications
- **Methodology** - Mathematical theory and derivations
  - Standard HP filter formulation
  - Generalized cross-validation criterion
  - Optimal λ selection procedure
  - Performance characteristics

#### Function Documentation

- Complete roxygen2 documentation for all functions
- Mathematical formulas properly rendered with LaTeX
- Extensive examples with commentary
- Author affiliations and ORCID identifiers

#### Package Website

- Professional pkgdown site with full documentation
- Download statistics and badges
- Interactive examples and tutorials
- API reference with search functionality

### Methodology Reference

This implementation is based on:

**Choudhary, M.A., Hanif, M.N., & Iqbal, J. (2014).** “On smoothing
macroeconomic time series using the modified HP filter.” *Applied
Economics*, 46(19), 2205-2214.
https://doi.org/10.1080/00036846.2014.896982

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

``` r
# From CRAN (recommended)
install.packages("mhpfilter")

# From GitHub (development version)
devtools::install_github("myaseen208/mhpfilter")
```

### System Requirements

- R ≥ 4.0.0
- C++ compiler for building from source
- Operating systems: Windows, macOS, Linux

### Testing and Quality Assurance

- Comprehensive test suite with testthat (≥ 3.0.0)
- Continuous integration via GitHub Actions
- R CMD check passes on all platforms
- CRAN policy compliance verified
- No NOTEs, WARNINGs, or ERRORs

### License

MIT License

### Bug Reports and Issues

Please report bugs at: https://github.com/myaseen208/mhpfilter/issues

### Acknowledgments

- Original methodology: Choudhary, Hanif & Iqbal (2014)
- RcppArmadillo team for excellent C++ integration
- R Core Team for the R statistical computing environment
- CRAN team for review and hosting

------------------------------------------------------------------------

For more information, visit: https://github.com/myaseen208/mhpfilter
