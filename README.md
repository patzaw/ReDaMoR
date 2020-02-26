-   [Introduction](#introduction)
-   [Documentation](#documentation)
-   [Future work](#future-work)

<img src="https://github.com/patzaw/ReDaMoR/raw/master/supp/logo/ReDaMoR.png" width="100px" />

The ReDaMoR package allows the manipulation of relational data models in
R. It provides functions to create, import and save relational data
models. These functions are accessible through a graphical user
interface made with [Shiny](https://shiny.rstudio.com/).

The main features of ReDaMoR are the following:

-   Create data model from scratch with a graphical user interface
-   Save data model in a json format
-   Import SQL data model generated with [MySQL
    Workbench](https://www.mysql.com/products/workbench/)
-   Document data types as R types (character, numeric, logical, …)
-   Add comments to tables and fields to improve model documation
-   Check and auto-correct model constraints (keys, uniqueness,
    mandatory fields)
-   Confront data to the model to check their compatibility

A plublic instance of the Shiny app is available [here]().

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
-   Add comments to tables and fields to improve model documation
-   Check and auto-correct model constraints (keys, uniqueness,
    mandatory fields)
-   Confront data to the model to check their compatibility

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
