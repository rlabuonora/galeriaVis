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
  #expect_error(print(p), NA)
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
  #expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})

test_that("serie_historica_mensual", {

  serie <- tibble::tribble(
    ~trafico, ~year, ~month,
    96437,  2018,      1,
    93333,  2018,      2,
    101713,  2018,      3,
    101240,  2018,      4,
    109307,  2018,      5,
    108967,  2018,      6,
    112982,  2018,      7,
    115621,  2018,      8,
    115825,  2018,      9,
    114350,  2018,     10,
    114150,  2018,     11,
    111965,  2018,     12,
    120039,  2019,      1,
    118777,  2019,      2,
    111572,  2019,      3,
    126237,  2019,      4,
    134348,  2019,      5,
    142193,  2019,      6,
    150576,  2019,      7,
    151067,  2019,      8,
    146393,  2019,      9,
    155685,  2019,     10,
    143900,  2019,     11,
    155142,  2019,     12
  )

  p <- serie %>%
    dplyr::filter(year > 2018) %>%
    serie_historica_mensual(trafico, nudge_text=1e4)

  expect_true(ggplot2::is.ggplot(p))
  #expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})

test_that("serie_historica_semestral_flujo", {

  serie_141 <- data.frame(
    year = c(2011,2011,2012,2012,2013,2013,2014,
             2014,2015,2015,2016,2016,2017,2017,2018,2018,2019,2019),
    semester = c(1L,2L,1L,2L,1L,2L,1L,2L,1L,2L,1L,
                 2L,1L,2L,1L,2L,1L,2L),
    lineas = c(964859,979923,1010803,1010953,1029629,
               1048445,1059309,1082903,1096565,1106431,1112235,1113566,
               1120072,1136977,1149210,1153533,1164984,1165373)
  )

  p <-  serie_141 %>%
    dplyr::mutate(lineas = lineas / 1e3) %>%
    serie_historica_semestral_flujo(lineas, nudge_text=4e1)

  expect_true(ggplot2::is.ggplot(p))
  #expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))

  etiquetas <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$range$range

  expect_equal("2011\nI", etiquetas[[1]])
  expect_equal("2011\nII", etiquetas[[2]])


})


test_that("serie_historica_semestral_stock ", {

  serie_141 <- data.frame(
    year = c(2011,2011,2012,2012,2013,2013,2014,
             2014,2015,2015,2016,2016,2017,2017,2018,2018,2019,2019),
    semester = c(1L,2L,1L,2L,1L,2L,1L,2L,1L,2L,1L,
                 2L,1L,2L,1L,2L,1L,2L),
    lineas = c(964859,979923,1010803,1010953,1029629,
               1048445,1059309,1082903,1096565,1106431,1112235,1113566,
               1120072,1136977,1149210,1153533,1164984,1165373)
  )

  p <-  serie_141 %>%
    dplyr::mutate(lineas = lineas / 1e3) %>%
    serie_historica_semestral_stock(lineas, nudge_text=4e1)

  expect_true(ggplot2::is.ggplot(p))
  #expect_error(print(p), NA)
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))


  etiquetas <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$range$range

  expect_equal("6-2011", etiquetas[[1]])

  expect_equal("12-2011", etiquetas[[2]])




})


