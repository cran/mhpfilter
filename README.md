# mhpfilter: Modified and Fully Modified Hodrick-Prescott Filters


[![CRAN
release](https://www.r-pkg.org/badges/version-last-release/mhpfilter)](https://CRAN.R-project.org/package=mhpfilter)
[![CRAN
checks](https://badges.cranchecks.info/worst/mhpfilter.svg)](https://cran.r-project.org/web/checks/check_results_mhpfilter.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mhpfilter?color=green)](https://CRAN.R-project.org/package=mhpfilter)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/mhpfilter?color=green)](https://CRAN.R-project.org/package=mhpfilter)
[![r-universe](https://myaseen208.r-universe.dev/badges/mhpfilter)](https://myaseen208.r-universe.dev/mhpfilter)
[![R-CMD-check](https://github.com/myaseen208/mhpfilter/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/myaseen208/mhpfilter/actions/workflows/R-CMD-check.yml)
[![Development
version](https://img.shields.io/badge/devel%20version-1.0.0-orange.svg)](https://github.com/myaseen208/mhpfilter)
[![Minimal R
version](https://img.shields.io/badge/R%3E%3D-4.2-6666ff.svg)](https://cran.r-project.org/)
[![Dependencies](https://tinyverse.netlify.app/badge/mhpfilter)](https://CRAN.R-project.org/package=mhpfilter)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last
commit](https://img.shields.io/github/last-commit/myaseen208/mhpfilter)](https://github.com/myaseen208/mhpfilter/commits)

## Overview

**mhpfilter** splits macroeconomic time series into a trend and a cycle
with the Hodrick-Prescott (HP) filter and two versions whose settings
come from the data:

- `hp_filter()`: the HP filter with a fixed smoothing parameter (1600
  quarterly, 100 annual);
- `mhp_filter()`: the modified HP filter of McDermott (1997), assessed
  by Choudhary, Hanif and Iqbal (2014): every series gets the smoothing
  parameter that minimises a generalised cross-validation criterion (the
  published approximation, or the exact criterion);
- `fmhp_filter()`: the fully modified HP filter of Hanif, Iqbal and
  Choudhary (2017), which also reduces the end-point bias of the HP
  filter by raising the smoothing parameter at the first and last
  observations, with linear or quadratic weights chosen to minimise the
  end-point loss of Bloechl (2014).

Around them:

- many series at once (`mhp_batch()`, `fmhp_batch()`), the filters
  compared (`mhp_compare()`, `batch_compare()`, `fmhp_compare()`);
- the weights, gain functions and end-point loss of any HP-type filter
  (`hp_weights()`, `hp_gain()`, `hp_loss()`, `hp_schedule()`);
- simulated trend-plus-cycle series and the power comparison of the
  papers (`mhp_designs()`, `mhp_simulate()`, `fmhp_simulation()`);
- `ggplot2` graphics, a plain-language interpretation of every result
  (`mhp_interpret()`), the references of every method (`mhp_cite()`) and
  an application that runs all of this without code (`mhp_app()`).

Version 1.0.0 keeps every function, argument and result of version 0.1.0
and adds the fully modified HP filter. Every filter is one banded linear
solve, so the search over 100000 values of the smoothing parameter for
200 observations takes 1.3 s instead of 6.4 s, and longer series gain
more; the fully modified HP filter reproduces the published MATLAB code,
and the end-point losses of the HP filter in Tables 3a and 3b of Hanif
et al. (2017) are reproduced.

## Installation

``` r
install.packages("mhpfilter")
# development version
# install.packages("remotes")
remotes::install_github("myaseen208/mhpfilter")
```

## Quick start

``` r
library(mhpfilter)
data(us_macro)
gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)
fm <-
  fmhp_filter(
      x               = gdp
    , max_lambda      = 3000
    , k_linear        = 15
    , k_quadratic     = NULL
    , alpha_linear    = seq(from = 0, to = 2000, by = 10)
    , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
    , schemes         = c("linear", "quadratic")
    , form            = "rows"
    , lambda          = NULL
    , log_scale       = TRUE
    , as_dt           = FALSE
    )
fm
```

    Fully modified HP Filter Result
    ==============================
    Observations: 203 
    Fully modified lambda: 1448  (modified HP lambda: 683 )
    End-point weights: quadratic, alpha = 15 on 22 end values; loss = 0.6163
    GCV: 3.336e-04 
    Filtered on the log scale; cycle = series - trend in levels

    Cycle Statistics:
      Mean: -2.603 
      SD:   120.4 
      AR1:  0.9522 
      Range: 900 

    Sources: Hanif et al. (2017); Hanif (2017); Bloechl (2014); McDermott (1997);
    Hodrick and Prescott (1997). Full references: mhp_cite(x).

``` r
fmhp_compare(
    x          = gdp
  , frequency  = "quarterly"
  , max_lambda = 3000
  , log_scale  = TRUE
  , end_share  = 0.1
  )
```

                  method lambda   cycle_sd    cycle_mean       ar1 cycle_range
    1:                HP   1600 0.01543904  4.942713e-12 0.8668207  0.08590516
    2:       Modified HP    683 0.01333176 -1.634336e-13 0.8280266  0.07801575
    3: Fully modified HP   1448 0.01618038 -4.145448e-05 0.8815215  0.08713978
                gcv      loss    sd_ends  sd_middle
    1:           NA 1.7617778 0.01458541 0.01568213
    2: 0.0002819926 1.7652928 0.01202396 0.01366805
    3: 0.0003335623 0.7898641 0.01930994 0.01538061

``` r
plot(x = fm)
```

![](README_files/figure-commonmark/quick-plot-1.png)

## Learn more

- Vignettes: `browseVignettes("mhpfilter")`, starting with
  `mhpfilter_01_getting_started`; PDF versions on the site.
- Site: <https://myaseen208.com/mhpfilter/>
- Notation: `mhp_notation()`
- References of the methods behind a result: `mhp_cite()`

## Citation

    Yaseen M (2026). _mhpfilter: Modified and Fully Modified
    Hodrick-Prescott Filters_. R package version 1.0.0,
    <https://CRAN.R-project.org/package=mhpfilter>.

BibTeX:

    @Manual{mhpfilter,
      title = {{mhpfilter}: Modified and Fully Modified Hodrick-Prescott Filters},
      author = {Muhammad Yaseen},
      year = {2026},
      note = {R package version 1.0.0},
      url = {https://CRAN.R-project.org/package=mhpfilter},
    }

Please also cite the methods you use: `citation("mhpfilter")` lists them
with the package, and `mhp_cite(x)` gives the references of a single
result.
