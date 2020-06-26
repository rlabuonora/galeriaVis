



formato_numero <- scales::number_format(big.mark=".", decimal.mark = ",")
pal <- RColorBrewer::brewer.pal(n = 8, name = "Blues")

year_label <- function(x) {
  y <- as.Date(x)
  lubridate::year(y)
}


#' Serie Hist&oacute;rica
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'

serie_historica <- function(df, valor, fecha, nudge_text=0, size_text=5)  {

  valor_quo <- rlang::enquo(valor)
  fecha_quo <- rlang::enquo(fecha)

  ggplot2::ggplot(df, ggplot2::aes(factor(!! fecha_quo), !! valor_quo)) +
    ggplot2::geom_col(fill = pal[5], width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label=formato_numero(!!valor_quo)),
                       size=ggplot2::rel(size_text),
                       nudge_y = nudge_text) +
    ggplot2::scale_x_discrete(labels = year_label,
                              expand = ggplot2::expand_scale(add = c(0.5, 0.5),
                                                    mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x="", y="")
}



