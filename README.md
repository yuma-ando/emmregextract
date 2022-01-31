
# emmregextract

<!-- badges: start -->
<!-- badges: end -->

The goal of emmregextract is to ...

## Installation

You can install the development version of emmregextract from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yuma-ando/emmregextract")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(emmregextract)
## basic example code
#Extraction of a sigle survey
emm_extract("https://registry.ethmigsurveydatahub.eu/surveys/2870")

#Extraction of multiple surveys
list<-c("https://registry.ethmigsurveydatahub.eu/surveys/2870","https://registry.ethmigsurveydatahub.eu/surveys/2872","not_a_valid_url")

emm_extract(list)

```

