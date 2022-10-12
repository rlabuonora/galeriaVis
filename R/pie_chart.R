


# Helpers ------

paleta_cats <- c(
  "azul" = "#80b1d3",
  "rojo" = "#fb8072",
  "verde" = "#8dd3c7",
  "violeta" = "#decbe4",
  "naranja" = "#fed9a6",
  "amarillo" = "#ffffcc"
)



# Helper para armar las etiquetas (Devuelve df con columna lbl) ---------

etiqueta_pie_chart <- function(df, values, labels) {

  val <- rlang::enquo(values)
  lbls <- rlang::enquo(labels)
  df %>%
    percent({{ values}}, {{ labels }}) %>%
    mutate(lbls = {{ labels }},
         val_num = scales::number({{ values }},
                                   big.mark = ".",
                                   decimal.mark = ','),
          lbl = glue::glue("{lbls}\n{val_num}\n{percent}"))
}


#' Make pie chart
#'
#' Crear grafico de tortas
#'
#' @param df data_frame
#' @param values variable a graficar
#' @param labels variable para agrupar
#' @param nudge_radio distancia del centro para las etiquetas
#' @param nudge_x vector para mover las  etiquetas en eje x
#' @param nudge_y vector para mover las etiquetas en eje  y
#' @param percent_accuracy redondeo para la etiqueta de porcentajes
#'
#' @return grafica de torta
#' @export
#' @importFrom ggplot2 ggplot coord_fixed
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_manual scale_color_manual
#' @examples
pie_chart <- function(df, values, labels,
                        nudge_radio = .7,
                        nudge_x = 0,
                        nudge_y = 0

) {

  if (nrow(df) == 0) {
    return(ggplot())
  }

  lab_char <- rlang::as_label(labs) %>%
    stringr::str_to_title()

  n <- nrow(df)

  p <- df %>%
    tibble::add_column(nudge_radio=nudge_radio, nudge_x=nudge_x, nudge_y=nudge_y) %>%
    etiqueta_pie_chart({{ values }}, {{ labels }}) %>%
    mutate(total = sum({{ values }}),
           end_angle = 2*pi*cumsum({{ values }}) / total,
           start_angle = dplyr::lag(end_angle, default = 0),
           mid_angle = (end_angle + start_angle) / 2) %>%
    ggplot() +
    ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0,
                     r0=0, r = 10,
                     fill = {{ labels }},
                     color = {{ labels }},
                     start = start_angle,
                     end   = end_angle),
    ) +
    coord_fixed(clip = "off", ratio=1) +
    scale_x_continuous(
      expand = c(.6, .6),
      name = "",
      breaks = NULL,
      labels = NULL
    ) +
    scale_y_continuous(
      name = "",
      breaks = NULL,
      labels = NULL,
      expand=c(.1, .1)
    ) +
    scale_fill_manual(name = lab_char,
                      values = unname(paleta_cats[1:n])) +
    scale_color_manual(name = lab_char,
                       values = unname(paleta_cats[1:n])
    ) +
    geom_text(aes(label = lbl, # esto viene de etiqueta_pie_chart
                  x = 10 * nudge_radio * sin(mid_angle) + nudge_x,
                  y = 10 * nudge_radio * cos(mid_angle) + nudge_y),
              show.legend = FALSE,
              family="Agency FB") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#EEEEEE",
                                         color = "white"),
          axis.line.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "none")

}
