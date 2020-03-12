-   [Introduction](#introduction)
-   [Installation](#installation)
    -   [From CRAN](#from-cran)
    -   [Dependencies](#dependencies)
    -   [From github](#from-github)
-   [Documentation](#documentation)
-   [Future work](#future-work)
-   [Acknowledgments](#acknowledgments)

<img src="https://github.com/patzaw/ReDaMoR/raw/master/supp/logo/ReDaMoR.png" width="100px" />

The ReDaMoR package allows the manipulation of relational data models in
R. It provides functions to create, import and save relational data
models. These functions are accessible through a graphical user
interface made with [Shiny](https://shiny.rstudio.com/).

A plublic instance of the Shiny app is available
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

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
Introduction
============

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
leveraged by the [dm](https://github.com/krlmlr/dm) R package to
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

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
Installation
============

<!---->
From CRAN
---------

<!----------->
This package is not yet available on CRAN.

<!---->
Dependencies
------------

<!----------->
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
-   [DT](https://CRAN.R-project.org/package=DT): A Wrapper of the
    JavaScript Library ‘DataTables’
-   [rintrojs](https://CRAN.R-project.org/package=rintrojs): Wrapper for
    the ‘Intro.js’ Library
-   [colourpicker](https://CRAN.R-project.org/package=colourpicker): A
    Colour Picker Tool for Shiny and for Selecting Colours in Plots
-   [jsonlite](https://CRAN.R-project.org/package=jsonlite): A Robust,
    High Performance JSON Parser and Generator for R
-   [markdown](https://CRAN.R-project.org/package=markdown): Render
    Markdown with the C Library ‘Sundown’
-   [rstudioapi](https://CRAN.R-project.org/package=rstudioapi): Safely
    Access the RStudio API
-   [crayon](https://CRAN.R-project.org/package=crayon): Colored
    Terminal Output
-   [utils](https://CRAN.R-project.org/package=utils): The R Utils
    Package
-   [graphics](https://CRAN.R-project.org/package=graphics): The R
    Graphics Package
-   [stats](https://CRAN.R-project.org/package=stats): The R Stats
    Package

<!---->
From github
-----------

<!----------->
``` r
devtools::install_github("patzaw/ReDaMoR")
```

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
Documentation
=============

Documentation is available in [this
vignette](https://patzaw.github.io/ReDaMoR).

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
Future work
===========

We are working on `internalMDB` objects which bind the data to their
relational model. These objects will allow the manipulation and the
combination of datasets in a consistent manner. We hope to release the
corresponding package soon.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
Acknowledgments
===============

This work was entirely supported by [UCB Pharma](https://www.ucb.com/)
(Early Solutions department).
