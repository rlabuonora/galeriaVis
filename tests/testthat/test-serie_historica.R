test_that("serie_historica", {

  serie <- structure(list(
    fecha =
      structure(c(1322697600, 1354320000, 1385856000,
                  1417392000, 1448928000, 1480550400,
                  1512086400, 1543622400, 1575158400),
                tzone = "UTC",
                class = c("POSIXct", "POSIXt")),
    lineas = c(11193, 8846, 6874, 5474, 3563, 1986, 1914, 1762, 1729)),
    row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame"))

  p <- serie_historica(serie, lineas, fecha, nudge_text = 5e2)
  expect_true(ggplot2::is.ggplot(p))
  expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})


test_that("serie_historica_anual", {

  serie <- data.frame(
    stringsAsFactors = FALSE,
    year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
    servicio = c("Mn", "Mn", "Mn", "Mn", "Mn", "Mn", "Mn", "Mn", "Mn"),
    sentido = c("desde","desde","desde",
                "desde","desde","desde","desde","desde","desde"),
    val = c(161947390,164858515,157409608,
            146673364,124414817,102077806,83858473,74820344,
            68859034))

  p <- serie_historica_anual(serie, val, nudge_text=c(10e6, 16e6, rep(10e6, 7)))
  expect_true(ggplot2::is.ggplot(p))
  expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})
