
# galeriaVis

Principales visualizaciones para el  [Informes de evolución del mercado de telecomunicaciones](https://www.gub.uy/unidad-reguladora-servicios-comunicaciones/sites/unidad-reguladora-servicios-comunicaciones/files/2023-04/informe%20telecom%20dic%2022.pdf) 

## Installation

You can install the development version of galeriaVis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rlabuonora/galeriaVis")
```

## Ejemplo


``` r
library(galeriaVis)
tibble::tribble(
  ~year, ~semester, ~ventas,
  2018, 1, 2594825643,
  2018, 2, 2583094540,
  2019, 1, 2617190545,
  2019, 2, 2583844809,
  2020, 1, 2560643364,
  2020, 2, 2569283200,
  2021, 1, 2541786752,
  2021, 2, 2374896171,
  2022, 1, 2306935190,
  2022, 2, 2180212868
) %>% serie_historica_semestral_flujo(ventas)

```

