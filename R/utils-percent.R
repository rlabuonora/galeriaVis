utils::globalVariables(".data")

percent <- function(df, numerica, categorica) {

  df <- mutate(df,
         percent_num = {{ numerica }} / sum( {{ numerica }}),
         percent_num_round = round(.data$percent_num, 2),
         round_diff = .data$percent_num-.data$percent_num_round,
         diff       = sum(.data$percent_num_round)-1,
         # le agrego o le pongo la diferencia al mayor
         percent_num_round = if_else(.data$percent_num == max(.data$percent_num),
                                     .data$percent_num_round -.data$diff,
                                     .data$percent_num_round ),
         percent = scales::percent(.data$percent_num_round,
                                   decimal.mark = ',',
                                   accuracy=1)
         )

  df
}
