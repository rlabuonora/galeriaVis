formato_numero <- function(x) {
  scales::number_format(big.mark=".",
                                        accuracy = .1,
                                        decimal.mark = ",")(x)
}

pal <- RColorBrewer::brewer.pal(n = 8, name = "Blues")




#' Serie Hist&oacute;rica
#' @param df data frame
#' @param valor valor a graficar
#' @param fecha fecha
#'
#' @return grafico
#' @export
#'


serie_historica <- function(df, valor, fecha)  {

  year_label <- function(x) {
    y <- as.Date(x)
    lubridate::year(y)
  }

  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * 0.08

  ggplot(df, aes(factor( {{ fecha }}, {{ valor }}))) +
    geom_col(fill = pal[5], width = 0.5) +
    geom_text(aes(label=formato_numero({{ valor }})),
                       nudge_y = auto_nudge,
                      family="Agency FB") +
    scale_x_discrete(labels = year_label) +
    scale_y_continuous(breaks = NULL) +
    geom_hline(yintercept = 0) +
    labs(x="", y="") +
    ggplot2::expand_limits(y=0)
}


#' Serie Historica Anual
#'
#' @param df datos
#' @param valor valor a graficar
#'
#' @export
#' @importFrom dplyr mutate pull
serie_historica_anual <- function(df, valor) {


  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * 0.08

  df %>%
    mutate(lbl = forcats::fct_inorder(factor(.data$year))) %>%
    ggplot(aes(factor(.data$lbl), {{ valor }})) +
    geom_col(fill = pal[5], width = 0.25) +
    geom_text(aes(label=formato_numero({{ valor }})),
              nudge_y = auto_nudge,
              family="Agency FB") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    scale_y_continuous(breaks = NULL) +
    geom_hline(yintercept = 0) +
    labs(x="", y="")
}


# TODO refactorear con semestral
#' Title
#'
#' @param df datos
#' @param valor valores de la serie
#' @param col_width ancho de las columnas
#'
#' @export
#'
serie_historica_mensual <- function(df, valor, col_width=0.5) {


  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{month}/{year}"))) %>%
    ggplot(aes(factor(.data$lbl), {{ valor }})) +
    geom_col(fill = pal[5], width = col_width) +
    geom_text(aes(label=formato_numero({{ valor }})),
              nudge_y = auto_nudge,
              family="Agency FB") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    scale_y_continuous(breaks = NULL) +
    geom_hline(yintercept = 0) +
    labs(x="", y="")
}


# TODO refactorear con semestral
#' Title
#'
#' @param df datos
#' @param valor a graficar
#'
#' @export
#' @importFrom ggplot2 geom_hline labs theme
#'
serie_historica_semestral_flujo <- function(df, valor, accuracy = .1, label_size = 4.5) {


  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{year}\n{as.roman(semester)}"))) %>%
    ggplot(aes(factor(.data$lbl), {{ valor }})) +
    geom_col(fill = pal[5]) +
    geom_text(aes(label=scales::number({{ valor }},
                                       big.mark=".",
                                       decimal.mark=",",
                                       accuracy = accuracy)),
                       nudge_y = auto_nudge,
                       size=label_size,
                       family="Agency FB") +
    scale_x_discrete(expand = expansion(0, 0)) +
    scale_y_continuous(breaks = NULL,
                                expand = expansion(mult = c(0.05, 0.18))) +
    geom_hline(yintercept = 0) +
    labs(x="", y="") +
    coord_cartesian(clip = "off") +
    theme(axis.text.x = ggplot2::element_text(vjust=6),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(t = 18, r = 10, b = 10, l = 10))
}


# TODO refactorear con semestral
#' Title
#'
#' @param df datos
#' @param valor valor a graficar
#'
#' @return grafico
#' @export
#' @importFrom ggplot2 geom_col geom_text aes scale_x_discrete scale_y_discrete
#' @importFrom dplyr if_else
serie_historica_semestral_stock <- function(df, valor, accuracy = .1) {

  # Auto nudge
  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(semester_month = if_else(.data$semester == 1, "6", "12")) %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{semester_month}-{year}"))) %>%
    ggplot(aes(factor(.data$lbl), {{ valor }})) +
    geom_col(fill = pal[5]) +
    geom_text(aes(label=scales::number({{ valor }},
                                       big.mark=".",
                                       decimal.mark=",",
                                       accuracy = accuracy)),
                       nudge_y = auto_nudge,
                       size=4.5,
                       family="Agency FB") +
    scale_x_discrete(expand = expansion(0, 0)) +
    scale_y_continuous(breaks = NULL,
                                expand = expansion(mult = c(0.05, 0.18))) +
    geom_hline(yintercept = 0) +
    labs(x="", y="") +
    coord_cartesian(clip = "off") +
    theme(axis.text.x = ggplot2::element_text(vjust=6),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(t = 18, r = 10, b = 10, l = 10))
}
