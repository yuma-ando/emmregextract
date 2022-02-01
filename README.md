
# emmregextract

<!-- badges: start -->
<!-- badges: end -->

The goal of emmregextract is to extract a selection of surveys from the EMM Survey Registry (https://ethmigsurveydatahub.eu/), and export an Excel file formatted in the same way as the internal Excel files.

## Author
Ando, Yuma, assistant statisticien au Centre d'études européennes et de politique comparées (UMR8239, Sciences Po/CNRS)

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

#the output file in xlsx is created in the working directory.

```

