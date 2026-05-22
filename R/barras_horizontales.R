# TODO: refactorear con la de pie char
etiqueta_barras <- function(df, values, labels,
                            accuracy=.1,
                            percent_accuracy=.1) {

  val <- rlang::enquo(values)
  lbls <- rlang::enquo(labels)
  df %>%
    percent({{values }}, {{ labels }}) %>%
    mutate(lbls = {{ labels }},
         val_num = scales::number({{ values }},
                                  big.mark = ".",
                                  decimal.mark = ',',
                                  accuracy=accuracy),
         percent = scales::percent(.data$percent_num_round,
                                   decimal.mark = ',',
                                   accuracy = percent_accuracy),
         lbl = glue::glue("{val_num} ({percent})"))

  # mutate(df,  percent_num = if_else(cum_sum == max(cum_sum),
  #                                   1-dplyr::lag(cum_sum, n=1),
  #                                   percent_num))

}



#' Barras horizontales
#'
#' @param df datos
#' @param valor columna con los valores a graficar
#' @param nudge valor para mover el texto
#' @param accuracy precision para los numeros (no los porcentajes!) de las etiquetas
#' @param cat columnas con las categorias
#' @param espacio_extra espacio
#'
#' @return plot object
#' @export
#' @importFrom ggplot2 expansion coord_flip
barras_horizontales <- function(df, valor, cat,
                                nudge = 0.15,
                                accuracy=.1,
                                percent_accuracy=.1,
                                espacio_extra=0.15) {

  if (nrow(df) == 0) {
    return(ggplot())
  }

  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * nudge

  df %>%
    etiqueta_barras({{ valor }}, {{ cat }},
                    accuracy=accuracy,
                    percent_accuracy=percent_accuracy) %>%
    ggplot(aes(forcats::fct_reorder( {{ cat }}, {{ valor }}),
               {{ valor }})) +
    geom_col(fill = pal[5], width = 0.5) +
    geom_text(aes(label=.data$lbl),
              nudge_y = auto_nudge,
              family="Agency FB") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5),
                                           mult = c(0, 0))) +
    scale_y_continuous(breaks = NULL,
                       expand = expansion(add = c(1, 1),
                                          mult = c(0, espacio_extra))) +
    geom_hline(yintercept = 0) +
    labs(x="", y="") +
    coord_flip()
}

#' Barras Horizonales factor ordernado
#'
#' @param df
#' @param valor
#' @param cat una columna de df que es un factor ordenado
#' @param nudge
#' @param accuracy
#' @param espacio_extra
#'
#' @return
#' @export
#'
#' @examples
barras_horizontales_2 <- function(df, valor, cat,
                                  nudge = 0.12,
                                  accuracy=.1,
                                  percent_accuracy=.1,
                                  espacio_extra=0.15) {

  if (nrow(df) == 0) {
    return(ggplot())
  }

  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * nudge

  df %>%
    etiqueta_barras({{ valor }}, {{ cat }},
                    accuracy=accuracy,
                    percent_accuracy=percent_accuracy) %>%
    ggplot(aes(forcats::fct_reorder({{ cat }}, {{ valor }}),
               {{ valor }})) +
    geom_col(fill = pal[5], width = 0.5) +
    geom_text(aes(label=.data$lbl),
              nudge_y = auto_nudge,
              family="Agency FB") +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5),
                                        mult = c(0, 0))) +
    scale_y_continuous(breaks = NULL,
                       expand = expansion(add = c(1, 1),
                                          mult = c(0, espacio_extra))) +
    geom_hline(yintercept = 0) +
    labs(x="", y="") +
    coord_flip()
}
