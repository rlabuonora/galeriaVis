percent <- function(df, numerica, categorica) {

  df <- mutate(df,
         percent_num = {{ numerica }} / sum( {{ numerica }}),
         percent_num_round = round(percent_num, 2),
         round_diff = percent_num-percent_num_round,
         diff       = sum(percent_num_round)-1,
         # le agrego o le pongo la diferencia al mayor
         percent_num_round = if_else(percent_num == max(percent_num),
                                     percent_num_round -diff,
                                     percent_num_round ),
         percent = scales::percent(percent_num_round,
                                   decimal.mark = ',',
                                   accuracy=1))

  df
}
