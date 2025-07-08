library(tidyverse)
library(wisconsink12)
library(scales)

sy <- "2023-24"

mps <- c("Traditional Public",
         "Instrumentality Charter",
         "Partnership")

charters <- c("Non-Instrumentality Charter",
              "2r/2x Charter")


# city_charters <- c(
#   "8007_8105",
#   "8105_1211", #central city cyber
#   "8127_0400", # fuller first code
#   "8011_8127", # fuller second code
#   "8109_0100",
#   "8009_8109",
#   "8006_8101",
#   "8101_1056",
#   "8026_8131",
#   "8131_0400",
#   "8008_8106",
#   "8106_1251",
#   "8012_8128",
#   "8128_0800"
# )


#######
# ACT #
#######

mke_schools <- schools %>%
  filter(school_year == sy) |> 
  mutate(accurate_agency_type = case_when(accurate_agency_type %in% mps ~ "Traditional Public Schools",
                                          accurate_agency_type %in% charters ~ "Public Charter Schools",
                                          accurate_agency_type == "Private" ~ "Private Choice Schools",
                                          TRUE ~ accurate_agency_type))

mke_tested_students_aat <- act %>%
  filter(group_by_value == "All Students" & 
           test_subject %in% c("ELA", "Mathematics") &
           school_year == sy) %>%
  right_join(., schools) %>%
  filter(!accurate_agency_type %in% c( "Private Choice Schools", "Partnership")) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic",
                                           "Developing", "Approaching") ~ "bb",
                        test_result %in% c("Proficient", "Advanced",
                                           "Meeting") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED")



# private ACT

private  <- schools %>%
  filter(accurate_agency_type == "Private Choice Schools" &
           school_year == sy) %>%
  select(dpi_true_id, school_year, accurate_agency_type) %>%
  left_join(., forward_exam) %>%
  filter(test_group == "ACT") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic",
                                           "Developing", "Approaching") ~ "bb",
                        test_result %in% c("Proficient", "Advanced",
                                           "Meeting") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED" & test_subject %in% c("ELA", "Mathematics"))

##########
# Aspire #
##########

aspire <- forward_exam |> 
  filter(group_by_value == "All Students" & 
           ((test_group %in% c("Aspire", "DLM") &
               grade %in% c("9", "10")) |
              test_group == "PreACT") &
           school_year == sy) %>%
  right_join(., schools) %>%
  filter(accurate_agency_type != "Partnership") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic",
                                           "Developing", "Approaching") ~ "bb",
                        test_result %in% c("Proficient", "Advanced",
                                           "Meeting") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED")

all <- bind_rows(aspire, private, mke_tested_students_aat)

hs <- all |> 
  group_by(school_year, dpi_true_id, accurate_agency_type, test_subject, pa) %>%
  summarise(total_count = sum(student_count, na.rm = TRUE))

hs |> 
  filter(school_year > "2020-21") |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) |> 
  left_join(schools |> 
              filter(accurate_agency_type != "Partnership") |> 
              select(dpi_true_id,
                     school_year,
                     broad_agency_type)) |> 
  group_by(broad_agency_type, school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)


elem <- forward_exam %>%
  filter(group_by_value == "All Students" &
           test_subject %in% c("ELA", "Mathematics") &
           grade %in% c("3", "4", "5", "6", "7", "8") &
           school_year == sy) %>%
  right_join(., schools) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic",
                                           "Developing", "Approaching") ~ "bb",
                        test_result %in% c("Proficient", "Advanced",
                                           "Meeting") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  group_by(school_year, dpi_true_id, accurate_agency_type, test_subject, pa) |> 
  summarise(total_count = sum(student_count, na.rm = TRUE))

elem |> 
  filter(school_year == sy) |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) |> 
  left_join(schools |> 
              select(dpi_true_id,
                     school_year,
                     broad_agency_type)) |> 
  group_by(broad_agency_type, school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)


all_prof <- bind_rows(hs, elem) |> 
  # mutate(dpi_true_id = case_when(dpi_true_id == "8105_1211" ~ "8007_8105",
  #                                dpi_true_id == "8127_0400" ~ "8011_8127",
  #                                dpi_true_id == "8109_0100" ~ "8009_8109",
  #                                dpi_true_id == "8101_1056" ~ "8006_8101",
  #                                dpi_true_id == "8131_0400" ~ "8026_8131",
  #                                dpi_true_id == "8106_1251" ~ "8008_8106",
  #                                dpi_true_id == "8128_0800" ~ "8012_8128",
  #                                dpi_true_id == "3619_0213" ~ "8027_8152",
  #                                dpi_true_id == "8152_8152" ~ "8027_8152",
  #                                TRUE ~ dpi_true_id))  |> 
  group_by(school_year,
           dpi_true_id,
           accurate_agency_type,
           test_subject,
           pa) |> 
  summarise(total_count = sum(total_count)) |> 
  ungroup() %>%
  group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  mutate(perc = (total_count / sum(total_count))) |>
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics"))

all_prof_tmp <- bind_rows(hs, elem) |> 
  # mutate(dpi_true_id = case_when(dpi_true_id == "8105_1211" ~ "8007_8105",
  #                                dpi_true_id == "8127_0400" ~ "8011_8127",
  #                                dpi_true_id == "8109_0100" ~ "8009_8109",
  #                                dpi_true_id == "8101_1056" ~ "8006_8101",
  #                                dpi_true_id == "8131_0400" ~ "8026_8131",
  #                                dpi_true_id == "8106_1251" ~ "8008_8106",
  #                                dpi_true_id == "8128_0800" ~ "8012_8128",
  #                                dpi_true_id == "3619_0213" ~ "8027_8152",
  #                                dpi_true_id == "8152_8152" ~ "8027_8152",
  #                                TRUE ~ dpi_true_id))  |> 
  group_by(school_year,
           dpi_true_id,
           accurate_agency_type,
           test_subject,
           pa) |> 
  summarise(total_count = sum(total_count)) |> 
  ungroup() %>%
  group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  mutate(perc = (total_count / sum(total_count)))

all_prof <- all_prof_tmp |> 
  select(-total_count) |>
  filter(pa != "REDACTED") |> 
  pivot_wider(names_from = pa, values_from = perc) |> 
  filter(!(is.na(bb) & is.na(pa) & is.na(no_test))) |> 
  filter(is.na(no_test) | no_test != 1) |> 
  mutate_at(c("bb", "no_test", "pa"), function(x) replace_na(x, 0)) |> 
  select(-c(bb, no_test)) |>
  mutate(pa = round(pa, 3)) |> 
  # filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) 

prof_counts <- all_prof_tmp |> 
  ungroup() |> 
  select(-c(perc)) |> 
  filter(pa == "pa" & test_subject %in% c("ELA", "Mathematics")) |> 
  select(-c(pa, accurate_agency_type)) |> 
  pivot_wider(names_from = test_subject, values_from = total_count, 
              names_prefix = "count_")



# saveRDS(all_prof, "../report_cards_2021-22/data/all_school_prof.rda")

x <- all_prof |> 
  pivot_wider(names_from = test_subject, values_from = pa) |> 
  select(-accurate_agency_type) |> 
  right_join(make_wi_rc(private_type = "all", exclude_milwaukee = FALSE) |> 
               filter(school_year == "2023-24") |> 
               select(dpi_true_id,
                      school_year,
                      school_name,
                      accurate_agency_type,
                      grade_band,
                      school_enrollment,
                      overall_rating,
                      overall_score, 
                      sch_ach,
                      sch_growth,
                      starts_with("per_"),
                      report_card_type)) |> 
  left_join(prof_counts) |> 
  select(school_year, 
         dpi_true_id,
         school_name:school_enrollment,
         ELA,
         Mathematics,
         count_ELA,
         count_Mathematics,
         overall_rating:sch_growth,
         starts_with("per_"),
         report_card_type) |> 
  mutate(accurate_agency_type = case_when(accurate_agency_type %in% mps ~ "Traditional Public School",
                                          accurate_agency_type %in% charters ~ "Public Charter School",
                                          accurate_agency_type == "Private" ~ "Private School",
                                          TRUE ~ accurate_agency_type),
         accurate_agency_type = ifelse(
           accurate_agency_type == "Private School" &
             report_card_type == "Private - Choice Students",
           "Private School*", accurate_agency_type
         )) |> 
  mutate(accurate_agency_type = str_remove(accurate_agency_type, "\\*")) |>
  filter(!is.na(accurate_agency_type)) |> 
  group_by(accurate_agency_type) |> 
  left_join(geocodes) |> 
  select(school_year,
         dpi_true_id,
         lat,
         long,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment,
         ELA,
         Mathematics,
         overall_rating,
         overall_score,
         sch_ach,
         sch_growth,
         per_ed,
         per_swd,
         per_lep,
         per_b_aa,
         per_hisp_lat) |> 
  left_join(schools |> 
              select(dpi_true_id,
                     school_year,
                     milwaukee_indicator))

saveRDS(x, "data/internal/prepped_data_1.rda")

