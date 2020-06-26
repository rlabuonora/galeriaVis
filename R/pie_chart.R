
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
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'

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
  dplyr::mutate(df,
                lbls = !!lbls,
                percent = scales::percent(!!val / sum(!!val), decimal.mark = ','),
                val_num = scales::number(!!val, big.mark = ".", decimal.mark = ','),
                lbl = glue::glue("{lbls}\n{val_num}\n{percent}"))
}


# Hacer el plot --------
pie_chart <- function(df, values, labels,
                        nudge_radio = .7,
                        nudge_x = 0,
                        nudge_y = 0
) {

  val <- rlang::enquo(values)
  labs <- rlang::enquo(labels)
  lab_char <- stringr::str_to_title(as.character(labs)[2])

  n <- nrow(df)

  p <- df %>%
    tibble::add_column(nudge_radio=nudge_radio, nudge_x=nudge_x, nudge_y=nudge_y) %>%
    etiqueta_pie_chart(!!val, !!labs) %>%
    dplyr::mutate(total = sum(!!val),
           end_angle = 2*pi*cumsum(!!val) / total,
           start_angle = dplyr::lag(end_angle, default = 0),
           mid_angle = (end_angle + start_angle) / 2) %>%
    ggplot2::ggplot() +
    ggforce::geom_arc_bar(ggplot2::aes(x0 = 0, y0 = 0,
                     r0=0, r = 10,
                     fill = !!labs,
                     color = !!labs,
                     start = start_angle,
                     end   = end_angle),
    ) +
    ggplot2::coord_fixed(clip = "off", ratio=1) +
    ggplot2::scale_x_continuous(
      expand = c(.6, .6),
      name = "",
      breaks = NULL,
      labels = NULL
    ) +
    ggplot2::scale_y_continuous(
      name = "",
      breaks = NULL,
      labels = NULL,
      expand=c(.1, .1)
    ) +
    ggplot2::scale_fill_manual(name = lab_char,
                      values = unname(paleta_cats[1:n])) +
    ggplot2::scale_color_manual(name = lab_char,
                       values = unname(paleta_cats[1:n])
    ) +
    ggplot2::geom_text(ggplot2::aes(label = lbl,
                  x = 10 * nudge_radio * sin(mid_angle) + nudge_x,
                  y = 10 * nudge_radio * cos(mid_angle) + nudge_y),
              size = 6,
              show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#EEEEEE",
                                         color = "white"),
          axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
          axis.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
          axis.line.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "none")

}
