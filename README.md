
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication: The Effect of Mandatory COVID-19 Certificates on Vaccine Uptake

This repository provides all materials for replication of results in

> Mills, M. C., and Rüttenauer, T. (2021). The Effect of Mandatory
> COVID-19 Certificates on Vaccine Uptake: Synthetic-Control Modelling
> of Six Countries. *The Lancet Public Health*, OnlineFirst.
> <https://doi.org/10.1016/S2468-2667(21)00273-5>

Date: 2021-12-23

## Set up

The code for replication of the results requires the following folders:
“01\_Script”, “02\_Data”, “03\_Output”. All R Scripts are required in
folder “01\_Script”, all data are required in folder “02\_Data”.

To reproduce the results of the paper, the scripts need to be executed
in order.

The following packages are necessary for reproduction of main results:

``` r
install.packages(Synth)
install.packages(SCtools)
install.packages(ggplot2)
install.packages(viridis)
install.packages(cowplot)
install.packages(texreg)
install.packages(grid)
install.packages(gridtext)
install.packages(gridExtra)
install.packages(colorspace)
install.packages(parallel)
install.packages(doParallel)
install.packages(future.apply)
install.packages(ISOweek)
install.packages(WDI)
install.packages(wpp2019)
```

### Scripts:

-   *01\_Download\_Data*: Downloads and prepares data from Our World in
    Data and OxCGRT.

-   *02\_Synthetic\_Control\_bootstrap*: Performs the main synthetic
    control analysis.

-   *02b\_Synthetic\_Control\_placebo*: Performs the placebo analysis
    (appendix).

-   *02c\_Synthetic\_Control\_resample*: Performs the resample analysis
    (appendix).

-   *03\_Decriptive\_age*: Performs age specific analyses from country
    specific data (in 02\_Data).

## Data:

All data for this paper are freely available and accessible online. The
sources are documented in the code.

## System and version information

Platform: x86\_64-pc-linux-gnu

Version: R version 4.0.5
