# Convert a list of 5 normalized tibbles in a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md) object

Convert a list of 5 normalized tibbles in a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
object

## Usage

``` r
fromDBM(dbm)
```

## Arguments

- dbm:

  a list with the following tibbles:

  - **tables**: The tables in the model with the following information

    - **name**: the name of the table

    - **x**: the x coordinate of the table in the model drawing (NA ==\>
      position undefined)

    - **y**: the y coordinate of the table in the model drawing (NA ==\>
      position undefined)

    - **color**: the color of the table in the model drawing (NA ==\>
      undefined)

    - **comment**: comment about the table

  - **fields**: The fields in the model with the following information

    - **name**: the name of the field

    - **type**: the type of the field

    - **nullable**: a logical indicating if the field can be null

    - **comment**: comment about the field

    - **table**: the name of the table to which the field belongs

  - **primaryKeys**: The primary keys in the model with the following
    information

    - **table**: the name of the relevant table

    - **field**: the name of the field participating to the primary key

  - **foreignKeys**: The foreign keys in the model with the following
    information

    - **table**: the name of the referring table

    - **fki**: the identifier of the foreign key (by referring table)

    - **field**: the name of the referring field

    - **refTable**: the name of the referred table

    - **refField**: the name of the referred field

  - **indexes**: The indexes in the model with the following information

    - **table**: the name of the relevant table

    - **idx**: the identifier of the index (by table)

    - **field**: the name of the field participating to the index

    - **unique**: a logical indicating if the field is unique

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
object
