
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **trimsib**: Survey Estimates of Wartime Mortality

This package implements the methodology outlined in [Survey Estimates of
Wartime Mortality](https://gking.harvard.edu/sibs)

## Installation

Please install the most recent development version of `trimsib` as
follows.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("mmukaigawara/trimsib", dependencies=TRUE)
```

## Workflows

Consider a dataframe `dat` with sibship-based mortality data. The
dataframe `dat` has the following variables:

- `Bj`: the number of siblings before war reported by each respondent
- `Sj`: the number of survivors among `Bj` after war
- `Dj`: the number of deaths (`Bj` - `Sj`)
- `Mj`: sibship-level mortality (`Dj` / `Bj`)
- `cluster`: cluster ID

The first task is to construct weights (`Wj`), normalized weights
(`Zj`), and weighted mortality (`Zj`) as follows.

``` r
library(ggplot2)
library(dplyr)
library(trimsib)

dat$Wj <- dat$Bj / dat$Sj
dat$Zj <- dat$Wj / sum(dat$Wj)
dat$Vj <- dat$Zj * dat$Mj
```

We next examine how trimming affects the prediction errors. To do so, we
can simply run the `suggest_trim` function and then visualize the
results using the `plot_highlight` function.

The `suggest_trim` function requires `data`, the name of the variable to
trim (in this case, `Vj`), the name of the variable for cluster IDs
(`cluster`), and a formula for prediction (by default a quadratic
function: `x + I(x^2)`).

``` r
result <- suggest_trim(data = dat, vec_var = "Vj", 
                       cl_var = "cluster", formula = "x + I(x^2)")
```

We then plot the results with some criteria to highlight potentially
contaminated clusters. The argument `criteria` indicates the maximum
mean absolute prediction error at the cluster level. With this argument,
clusters with the maximum mean absolute prediction error greater than
`criteria` are colored in orange.

``` r
plot_highlight(data_list = result, criteria = 6*10^-6)
```

<img src="man/figures/README-fig_trim.png" style="width: 50%"/>

The plot suggests that clusters in orange color are suspected of data
contamination. It also suggests that, for these clusters to exhibit the
trends in other clusters, we need to trim 20% of observations. With
this, we proceed to trim 20% of data (total) from each of these
clusters. We can perform this procedure by running the
`get_trimmed_data` function by setting `trim_level = 0.2`.

``` r
dat_tr <- get_trimmed_data(result_iraq, criteria = 6*10^-6, 
                           trim_level = 0.2, vec_var = "Vj")
```

The final output `dat_tr` has trimmed 20% of observations (total, from
above and below) from each of the clusters with suspected data
contamination.
