README
================

-   [Introduction](#introduction)
-   [Installation](#installation)
    -   [From CRAN](#from-cran)
    -   [Dependencies](#dependencies)
    -   [From github](#from-github)
-   [Documentation](#documentation)
-   [TKCat: Tailored Knowledge
    Catalog](#tkcat-tailored-knowledge-catalog)
-   [Acknowledgments](#acknowledgments)

<img src="https://github.com/patzaw/ReDaMoR/raw/master/supp/logo/ReDaMoR.png" width="100px"/>

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ReDaMoR)](https://cran.r-project.org/package=ReDaMoR)
[![](http://cranlogs.r-pkg.org/badges/ReDaMoR)](https://cran.r-project.org/package=ReDaMoR)

The ReDaMoR package allows the manipulation of relational data models in
R. It provides functions to create, import and save relational data
models. These functions are accessible through a graphical user
interface made with [Shiny](https://shiny.rstudio.com/).

A public instance of the Shiny app is available
[here](https://pgodard.shinyapps.io/ReDaMoR).

The main features of ReDaMoR are the following:

-   Create data model from scratch with a graphical user interface
-   Save data model in a json format
-   Import SQL data model generated with [MySQL
    Workbench](https://www.mysql.com/products/workbench/)
-   Document data types as R types (character, numeric, logical, …)
-   Add comments to tables and fields to improve model documentation
-   Check and auto-correct model constraints (types, keys, uniqueness,
    mandatory fields)
-   Confront data to the model to check their compatibility
-   Support of matrices (\>= 0.5.0)

The [TKCat](https://github.com/patzaw/TKCat) package relies on ReDaMoR
to facilitate the management of data from knowledge resources which are
manipulated as modeled database (MDB) objects.

The package can also be tested in [Rstudio
Cloud](https://rstudio.cloud/project/1033803).

``` r
library(ReDaMoR)
m <- model_relational_data()
```

You can also run the code documented in the [package
vignette](https://patzaw.github.io/ReDaMoR/ReDaMoR.html).

# Introduction

In R, data are often stored in data frames which are tables in which
each row represents a record and each column a variable. Because data
frames are highly used they have been improved in different objects such
as [tibble](https://tibble.tidyverse.org/),
[data.table](https://rdatatable.gitlab.io/data.table/) or
[AnnotatedDataFrame](https://bioconductor.org/packages/release/bioc/html/Biobase.html).

However, in many projects, the data do not fit in only one table but
they are organized in different data frames, each of them dealing with a
specific concept. These tables are often related to each other by some
variables. Depending on the diversity of the concepts, it can be
difficult to remember what these tables represent and how they are
connected. Fortunately, because they are tables, a set of data frames
can be directly documented using a relational data model.

The [datamodelr](https://github.com/bergant/datamodelr) R package
provides tools to document relational data. The generate data models are
leveraged by the [dm](https://github.com/cynkra/dm) R package to
interact more easily with relational data.

Here we present the ReDaMoR package which also allows the manipulation
of relational data models in R but with an approach quite different from
the one implemented in datamodelr. It provides functions to create,
import and save relational data models. These functions are accessible
through a graphical user interface made with
[Shiny](https://shiny.rstudio.com/).

The main features of ReDaMoR are the following:

-   Create data model from scratch with a graphical user interface
-   Save data model in a json format
-   Import SQL data model generated with [MySQL
    Workbench](https://www.mysql.com/products/workbench/)
-   Document data types as R types (character, numeric, logical, …)
-   Add comments to tables and fields to improve model documentation
-   Check and auto-correct model constraints (types, keys, uniqueness,
    mandatory fields)
-   Confront data to the model to check their compatibility
-   Support of matrices (\>= 0.5.0)

The [TKCat](https://github.com/patzaw/TKCat) package relies on ReDaMoR
to facilitate the management of data from knowledge resources which are
manipulated as modeled database (MDB) objects.

# Installation

## From CRAN

``` r
install.packages("ReDaMoR")
```

## Dependencies

The following R packages available on CRAN are required:

-   [dplyr](https://CRAN.R-project.org/package=dplyr): A Grammar of Data
    Manipulation
-   [magrittr](https://CRAN.R-project.org/package=magrittr): A
    Forward-Pipe Operator for R
-   [visNetwork](https://CRAN.R-project.org/package=visNetwork): Network
    Visualization using ‘vis.js’ Library
-   [readr](https://CRAN.R-project.org/package=readr): Read Rectangular
    Text Data
-   [shiny](https://CRAN.R-project.org/package=shiny): Web Application
    Framework for R
-   [shinyjs](https://CRAN.R-project.org/package=shinyjs): Easily
    Improve the User Experience of Your Shiny Apps in Seconds
-   [jsonlite](https://CRAN.R-project.org/package=jsonlite): A Simple
    and Robust JSON Parser and Generator for R
-   [DT](https://CRAN.R-project.org/package=DT): A Wrapper of the
    JavaScript Library ‘DataTables’
-   [colourpicker](https://CRAN.R-project.org/package=colourpicker): A
    Colour Picker Tool for Shiny and for Selecting Colours in Plots
-   [rintrojs](https://CRAN.R-project.org/package=rintrojs): Wrapper for
    the ‘Intro.js’ Library
-   [markdown](https://CRAN.R-project.org/package=markdown): Render
    Markdown with the C Library ‘Sundown’
-   [rstudioapi](https://CRAN.R-project.org/package=rstudioapi): Safely
    Access the RStudio API
-   [crayon](https://CRAN.R-project.org/package=crayon): Colored
    Terminal Output
-   [igraph](https://CRAN.R-project.org/package=igraph): Network
    Analysis and Visualization
-   [utils](https://CRAN.R-project.org/package=utils): The R Utils
    Package
-   [graphics](https://CRAN.R-project.org/package=graphics): The R
    Graphics Package
-   [stats](https://CRAN.R-project.org/package=stats): The R Stats
    Package
-   [Matrix](https://CRAN.R-project.org/package=Matrix): Sparse and
    Dense Matrix Classes and Methods

And those are suggested:

-   [knitr](https://CRAN.R-project.org/package=knitr): A General-Purpose
    Package for Dynamic Report Generation in R
-   [rmarkdown](https://CRAN.R-project.org/package=rmarkdown): Dynamic
    Documents for R
-   [base64enc](https://CRAN.R-project.org/package=base64enc): Tools for
    base64 encoding

## From github

``` r
devtools::install_github("patzaw/ReDaMoR")
```

# Documentation

Documentation is available in [this
vignette](https://patzaw.github.io/ReDaMoR).

# TKCat: Tailored Knowledge Catalog

The aim of [TKCat](https://github.com/patzaw/TKCat) (Tailored Knowledge
Catalog) is to facilitate the management of data from knowledge
resources that are frequently used alone or together in research
environments. In TKCat, knowledge resources are manipulated as modeled
database (MDB) objects. These objects provide access to the data tables
along with a general description of the resource and a detail data model
generated with ReDaMoR documenting the tables, their fields and their
relationships. These MDB are then gathered in catalogs that can be
easily explored an shared. TKCat provides tools to easily subset, filter
and combine MDBs and create new catalogs suited for specific needs.

# Acknowledgments

This work was entirely supported by [UCB Pharma](https://www.ucb.com/)
(Early Solutions department).
