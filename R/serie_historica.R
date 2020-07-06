



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


#' Serie Historica Anual
#'
#' @param df
#' @param valor
#' @param nudge_text
#' @param size_text
#'
#' @return
#' @export
#'
#' @examples
serie_historica_anual <- function(df, valor, nudge_text=0, size_text=5) {

  valor_quo <- rlang::enquo(valor)

  df %>%
    dplyr::mutate(lbl = forcats::fct_inorder(factor(year))) %>%
    ggplot2::ggplot(ggplot2::aes(factor(lbl), !! valor_quo)) +
    ggplot2::geom_col(fill = pal[5], width = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label=formato_numero(!!valor_quo)),
              size=ggplot2::rel(size_text),
              nudge_y = nudge_text) +
    ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x="", y="")
}


# TODO refactorear con semestral
#' Title
#'
#' @param df
#' @param valor
#' @param nudge_text
#' @param size_text
#' @param col_width
#'
#' @return
#' @export
#'
#' @examples
serie_historica_mensual <- function(df, valor,  nudge_text=0, size_text=5, col_width=0.5) {

  valor_quo <- rlang::enquo(valor)

  df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lbl = forcats::fct_inorder(glue::glue("{month}/{year}"))) %>%
    ggplot2::ggplot(ggplot2::aes(factor(lbl), !! valor_quo)) +
    ggplot2::geom_col(fill = pal[5], width = col_width) +
    ggplot2::geom_text(ggplot2::aes(label=formato_numero(!!valor_quo)),
              size=ggplot2::rel(size_text),
              nudge_y = nudge_text) +
    ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x="", y="") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14))
}

serie_historica_semestral <- function(df, valor, nudge_text=0, size_text=5) {

  valor_quo <- rlang::enquo(valor)

  df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lbl = forcats::fct_inorder(glue::glue("{semester} Sem.\n{year}"))) %>%
    ggplot2::ggplot(ggplot2::aes(factor(lbl), !! valor_quo)) +
    ggplot2::geom_col(fill = pal[5], width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label=formato_numero(!!valor_quo)),
              size=ggplot2::rel(size_text),
              nudge_y = nudge_text) +
    ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x="", y="")
}


