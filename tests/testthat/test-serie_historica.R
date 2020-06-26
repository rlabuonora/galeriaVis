test_that("serie 12", {

  serie <- structure(list(
    fecha =
      structure(c(1322697600, 1354320000, 1385856000,
                  1417392000, 1448928000, 1480550400, 1512086400, 1543622400, 1575158400),
                tzone = "UTC",
                class = c("POSIXct", "POSIXt")),
    lineas = c(11193, 8846, 6874, 5474, 3563, 1986, 1914, 1762, 1729)),
    row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame"))

  p <- serie_historica(serie, lineas, fecha, nudge_text = 5e2)
  expect_true(ggplot2::is.ggplot(p))
  expect_true("GeomCol" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})
