library(tidyverse)
library(wisconsink12)

x <- read_rds("data/internal/prepped_data_1.rda")

# how VAM is converted to 100 pt scale
# i.e., average VAM of 3 = 66.0

map_dbl(1:5, function(i) {
  ((i*.19) + .09) * 100
})

# break into quantiles

quantile(x$sch_growth, probs = seq(0, 1, 0.2), na.rm = TRUE, )

# recode based on quantiles 

w_growth_rating <- x |> 
  mutate(growth_rating = case_when(
    sch_growth <= 53.7 ~ "Well Below Average Growth",
    sch_growth <= 62.2 ~ "Below Average Growth",
    sch_growth <= 69.8 ~ "Average Growth",
    sch_growth <= 79.3 ~ "Above Average Growth",
    sch_growth <= 101 ~ "Well Above Average Growth",
    is.na(sch_growth) ~ "No Growth Rating",
    TRUE ~ "ERROR"
  ) |> 
    factor(levels = c(
      "No Growth Rating",
      "Well Below Average Growth",
      "Below Average Growth",
      "Average Growth",
      "Above Average Growth",
      "Well Above Average Growth"
    )))
  # group_by(growth_rating) |> 
  # count() |> 
  # ungroup() |> 
  # # summarise(total = sum(school_enrollment, na.rm = TRUE)) |> 
  # mutate(perc = n / sum(n))

write_csv(w_growth_rating, "data/wisconsin_schools_data_2023-24.csv")
