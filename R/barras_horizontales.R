#' Title
#'
#' @param df
#' @param valor
#' @param cat
#' @param nudge_text
#' @param espacio_extra
#'
#' @return
#' @export
#'
#' @examples
barras_horizontales <- function(df, valor, cat, nudge_text=0, espacio_extra=0.15) {
  valor_quo <- rlang::enquo(valor)
  cat_quo <- rlang::enquo(cat)

  df %>%
    ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(!!cat_quo, !! valor_quo), !! valor_quo)) +
    ggplot2::geom_col(fill = pal[5], width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label=formato_numero(!! valor_quo)),
              nudge_y = nudge_text, size = 5) +
    ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = NULL,
                       expand = ggplot2::expand_scale(add = c(1, 1),
                                             mult = c(0, espacio_extra))) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x="", y="") +
    ggplot2::coord_flip()
}
