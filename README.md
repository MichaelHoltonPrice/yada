
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yada

[![CRAN
status](https://www.r-pkg.org/badges/version/yada)](https://cran.r-project.org/package=yada)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/eehh-stanford/yada.svg?branch=identif)](https://travis-ci.org/eehh-stanford/yada)
[![Coverage
status](https://codecov.io/gh/eehh-stanford/yada/branch/identif/graph/badge.svg)](https://codecov.io/github/eehh-stanford/yada?branch=identif)

`yada`, which stands for Yet Another Demographic Analysis package,
implements age estimation using a mixed cumulative probit (MCP) model.

A vignette for the general pipeline of optimizing a model using the MCP algorithm is provided [HERE](inst/doc/) and can be downloaded for reference. 

## Installation

You can install yada with:

``` r
devtools::install_github("MichaelHoltonPrice/yada")
```

## Running yada in a Docker container
To guarantee reproducibility, yada can be run inside a Docker container using
the Dockerfile that is included in the github repository. For example, the
yada test scripts (we have added over 1000 tests to ensure that yada functions
as intended) can be run inside this Docker container. The Docker Engine must be
installed and available from the command line; see this link:

[https://docs.docker.com/engine/install/](https://docs.docker.com/engine/install/)

Open a terminal/command window and enter the following commands to clone the
repository, change into the yada directory, and list its contents:

```console
git clone https://github.com/MichaelHoltonPrice/yada
cd yada
ls
```

The container runs Ubuntu 20.04 and, as of this writing, installs R version
4.1. Create and run the Docker container:

```console
docker build -t michaelholtonprice/yada .
docker run -itv /yada_docker_data:/data michaelholtonprice/yada
```

This mirrors the /data directory inside the container to the directory
yada_docker_container in the cloned directory on the host machine. If desired,
the container can be started without mirroring the directory:

```console
docker run -it michaelholtonprice/yada
```

The test scripts can be run by changing to the yada directory and running the
test_yada.R file:

```console
#cd yada  # use this line only if yada is not the root directory
Rscript test_yada.R
```


## Contributing

Please note that the ‘yada’ project is released with a [Contributor Code
of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
