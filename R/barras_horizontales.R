
etiqueta_barras <- function(df, values, labels, percent_accuracy=.1) {

  val <- rlang::enquo(values)
  lbls <- rlang::enquo(labels)
  mutate(df,
         lbls = {{ labels }},
         percent = scales::percent({{ values }} / sum({{ values }}),
                                   decimal.mark = ',',
                                   accuracy=percent_accuracy),
         val_num = scales::number({{ values }},
                                  big.mark = ".",
                                  decimal.mark = ','),
         lbl = glue::glue("{val_num} ({percent})"))
}



#' Title
#'
#' @param df datos
#' @param valor columna con los valores a graficar
#' @param cat columnas con las categorias
#' @param espacio_extra espacio
#'
#' @return plot object
#' @export
#' @importFrom ggplot2 expansion coord_flip
#' @examples
barras_horizontales <- function(df, valor, cat,
                                nudge = 0.08,
                                espacio_extra=0.15) {

  if (nrow(df) == 0) {
    return(ggplot())
  }

  valores <- pull(df, {{ valor }})
  auto_nudge <- max(valores) * nudge

  df %>%
    etiqueta_barras({{ valor }}, {{ cat }}) %>%
    ggplot(aes(forcats::fct_reorder( {{ cat }}, {{ valor }}),
               {{ valor }})) +
    geom_col(fill = pal[5], width = 0.5) +
    geom_text(aes(label=lbl),
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
