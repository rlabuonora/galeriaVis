



formato_numero <- scales::number_format(big.mark=".",
                                        accuracy = 1,
                                        decimal.mark = ",")

pal <- RColorBrewer::brewer.pal(n = 8, name = "Blues")




#' Serie Hist&oacute;rica
#' @param df data frame
#' @param valor valor a graficar
#' @param fecha fecha
#'
#' @return grafico
#' @export
#'
#' @examples
#'
#'
#'

serie_historica <- function(df, valor, fecha)  {

  year_label <- function(x) {
    y <- as.Date(x)
    lubridate::year(y)
  }

  valor_quo <- rlang::enquo(valor)
  fecha_quo <- rlang::enquo(fecha)

  valores <- dplyr::pull(df, !! valor_quo)
  auto_nudge <- max(valores) * 0.08

  ggplot(df, aes(factor(!! fecha_quo), !! valor_quo)) +
    geom_col(fill = pal[5], width = 0.5) +
    geom_text(aes(label=formato_numero(!!valor_quo)),
                       nudge_y = auto_nudge,
                      family="Agency FB") +
    scale_x_discrete(labels = year_label,
                              expand = expansion(add = c(0.5, 0.5),
                                                    mult = c(0, 0))) +
    scale_y_continuous(breaks = NULL) +
    geom_hline(yintercept = 0) +
    labs(x="", y="")
}


#' Serie Historica Anual
#'
#' @param df datos
#' @param valor valor a graficar
#'
#' @return
#' @export
#' @importFrom dplyr mutate pull
#' @examples
serie_historica_anual <- function(df, valor) {

  valor_quo <- rlang::enquo(valor)

  valores <- pull(df, !! valor_quo)
  auto_nudge <- max(valores) * 0.08

  df %>%
    mutate(lbl = forcats::fct_inorder(factor(year))) %>%
    ggplot(aes(factor(lbl), !! valor_quo)) +
    geom_col(fill = pal[5], width = 0.25) +
    geom_text(aes(label=formato_numero(!!valor_quo)),
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
#' @return
#' @export
#'
#' @examples
serie_historica_mensual <- function(df, valor, col_width=0.5) {

  valor_quo <- rlang::enquo(valor)

  valores <- pull(df, !! valor_quo)
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{month}/{year}"))) %>%
    ggplot(aes(factor(lbl), !! valor_quo)) +
    geom_col(fill = pal[5], width = col_width) +
    geom_text(aes(label=formato_numero(!!valor_quo)),
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
#' @return
#' @export
#' @importFrom ggplot2 geom_hline labs
#'
#' @examples
serie_historica_semestral_flujo <- function(df, valor) {

  valor_quo <- rlang::enquo(valor)

  valores <- pull(df, !! valor_quo)
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{year}\n{as.roman(semester)}"))) %>%
    ggplot(aes(factor(lbl), !! valor_quo)) +
    geom_col(fill = pal[5]) +
    geom_text(aes(label=formato_numero(!!valor_quo)),
                       nudge_y = auto_nudge,
                       family="Agency FB") +
    scale_x_discrete(expand = expansion(0, 0)) +
    scale_y_continuous(breaks = NULL,
                                expand = expansion(.05, 0)) +
    geom_hline(yintercept = 0) +
    labs(x="", y="")
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
#' @examples
serie_historica_semestral_stock <- function(df, valor) {

  valor_quo <- rlang::enquo(valor)

  # Auto nudge
  valores <- pull(df, !! valor_quo)
  auto_nudge <- max(valores) * 0.08

  df %>%
    dplyr::ungroup() %>%
    mutate(semester_month = if_else(semester == 1, "6", "12")) %>%
    mutate(lbl = forcats::fct_inorder(glue::glue("{semester_month}-{year}"))) %>%
    ggplot(aes(factor(lbl), !! valor_quo)) +
    geom_col(fill = pal[5]) +
    geom_text(aes(label=formato_numero(!!valor_quo)),
                       nudge_y = auto_nudge,
                       family="Agency FB") +
    scale_x_discrete(expand = expansion(0, 0)) +
    scale_y_continuous(breaks = NULL,
                                expand = expansion(.05, 0)) +
    geom_hline(yintercept = 0) +
    labs(x="", y="")
}


