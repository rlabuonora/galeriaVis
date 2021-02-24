
test_that("data frame vacío", {

  df <- tibble::tribble(
    ~empresa, ~minutos
  )

  p <- pie_chart(df,  minutos, empresa)
  expect_error(print(p), NA)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("simple pie chart", {



  serie <- structure(
    list(empresa = c("AMWU", "Ancel", "TMU"),
         minutos = c(23223127, 145676036, 61796380)),
         row.names = c(NA, -3L),
         class = c("tbl_df", "tbl", "data.frame"))

  p <- pie_chart(serie,  minutos, empresa)
  expect_error(print(p), NA)
  expect_true(ggplot2::is.ggplot(p))
  expect_true("GeomArcBar" %in% class(p$layers[[1]]$geom))
  expect_true("GeomText" %in% class(p$layers[[2]]$geom))
})
