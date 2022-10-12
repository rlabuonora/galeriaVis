test_that("data frame vacío", {

  df <- tibble::tribble(
    ~depto, ~lineas
  )

  p <- barras_horizontales(df, lineas, depto)
  expect_true(ggplot2::is.ggplot(p))
  #expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  #expect_error(print(p), NA)
})


test_that("Grafico 10", {

  df <- tibble::tribble(
    ~depto, ~lineas,
    "Artigas",   16956,
    "Canelones",  164294,
    "Cerro Largo",   21300,
    "Colonia",   43492,
    "Durazno",   14413,
    "Flores",    7943,
    "Florida",   19332,
    "Lavalleja",   18845,
    "Maldonado",   89297,
    "Montevideo",  556391,
    "Paysandú",   29095,
    "Río Negro",   14355,
    "Rivera",   25133,
    "Rocha",   25041,
    "Salto",   28940,
    "San José",   31719,
    "Soriano",   23361,
    "Tacuarembó",   22538,
    "Treinta y Tres",   12928
  )

  p <- barras_horizontales(df, lineas, depto)
  expect_true(ggplot2::is.ggplot(p))
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
  #expect_error(print(p), NA)


})


test_that("Ceros", {

  skip("Empty")

  df <- tibble::tribble(
    ~medio,            ~servicios,
    "DSL",              105600,
    "Fibra óptica",     934604,
    "LMDS",             10402,
    "Otros",            71785
  )


  p <- barras_horizontales(df, servicios, medio)
})




