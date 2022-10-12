test_that("Hay que ajustar muchos", {
  df <- tibble::tribble(
    ~depto, ~lineas,
    "Artigas",   18921,
    "Canelones",  184240,
    "Cerro Largo",   22997,
    "Colonia",   45969,
    "Durazno",   15894,
    "Flores",    8435,
    "Florida",   20688,
    "Lavalleja",   19922,
    "Maldonado",   98032,
    "Montevideo",  582340,
    "Paysandú",   31773,
    "Río Negro",   15594,
    "Rivera",   28294,
    "Rocha",   27352,
    "Salto",   31269,
    "San José",   34984,
    "Soriano",   25021,
    "Tacuarembó",   24897,
    "Treinta y Tres",   13696
  )

  percents <- percent(df, lineas, depto)
  expect_true(all(percents$percent_num_round  >=0))

})


test_that("Suma menos de 100%", {

  df <- tibble::tribble(
    ~medio,            ~servicios,
    "DSL",              105600,
    "Fibra óptica",     934604,
    "LMDS",             10402,
    "Otros",            71785
  )

  percents <- percent(df, servicios, medio)

  expect_equal(percents$percent,
               c("9%",  "84%", "1%",  "6%"))

  expect_true(all(percents$percent_num_round >=0))

})


test_that("Redondeo", {
  df <- tibble::tribble(
    ~tipo_red,  ~numeric,
    "Al Exterior",  14238134,
    "Otra Red Móvil", 229192670,
    "Red Fija", 241914472,
    "Red Propia", 749810736
  )


  percents <- percent(df, numeric, tipo_red)
  expected <- c("1%",  "19%", "20%", "60%")

  expect_true(all(percents$percent_num_round >=0))

  expect_equal(percents$percent, expected)

})

