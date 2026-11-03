library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(reshape2)
library(haven)



setwd("C:/Users/LENOVO/Documents/GitHub/mixedlogit_uk/")

# upload the UK census cross-repeated ethnicity x social class dataset
df_final <- read_excel("df_final.xlsx")

df_individall <-  df_final %>%
  pivot_longer(
    cols = matches("^(white|mixed|asian|black).*_sc[1-7]$"),
    names_to = "category",
    values_to = "n"
  ) %>%
  filter(!is.na(n), n > 0) %>%
  uncount(weights = n) %>%
  mutate(
    ID = row_number(),
    .before = 1
  ) %>%
  select(
    ID,
    category,
    everything()
  )


###
# Select the category variables
# categories <- names(df_final) %>%
#   grep(
#     "^(white|mixed|asian|black).*_sc[1-7]$",
#     x = .,
#     value = TRUE
#   )
# 
# # validation numbers between long individual and aggregated row
# # Original aggregated counts
# df_original <- df_final %>%
#   pivot_longer(
#     cols = all_of(categories),
#     names_to = "category",
#     values_to = "n_original"
#   )
# 
# # Reconstructed individual counts
# df_check <- df_individall %>%
#   count(borough_year, category, name = "n_individual")
# 
# # Compare all categories and borough-years
# df_validation <- df_original %>%
#   left_join(
#     df_check,
#     by = c("borough_year", "category")
#   ) %>%
#   mutate(
#     n_individual = replace_na(n_individual, 0L),
#     difference = n_original - n_individual,
#     match = !is.na(difference) & difference == 0
#   )
# 
# all(df_validation$match)

######### homophily variables

# subset

df_individall <- df_individall %>% select(
  ID, category, borough_year, borough, year,
  matches("^frac_total_(bangladeshi|indian|pakistani|chinese|african|carribean|ewsnib|irish|whiteasian|whiteblcarribean|whiteblafrican)$"),
  matches("^frac_total_sc[1-7]$"),
  matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$"),
  shannon_ethnic_norm,
  shannon_status_norm
)

# homoethnic

# Create contextual lookup table
ethnic_lookup <- df_individall %>%
  select(
    borough_year,
    matches(
      "^frac_total_(bangladeshi|indian|pakistani|chinese|african|carribean|ewsnib|irish|whiteasian|whiteblcarribean|whiteblafrican)$"
    )
  ) %>%
  distinct() %>%
  pivot_longer(
    cols = matches(
      "^frac_total_(bangladeshi|indian|pakistani|chinese|african|carribean|ewsnib|irish|whiteasian|whiteblcarribean|whiteblafrican)$"
    ),
    names_to = "ethnic_group",
    names_prefix = "frac_total_",
    values_to = "homo_ethnic"
  )

# Assign the appropriate contextual value to each individual
df_individall <- df_individall %>%
  mutate(
    ethnic_group = sub(
      "^(white|mixed|black|asian)_(.*)_sc[1-7]$",
      "\\2",
      category
    )
  ) %>%
  left_join(
    ethnic_lookup,
    by = c("borough_year", "ethnic_group")
  )
 
# homo status

sc_lookup <- df_individall %>%
  select(
    borough_year,
    matches("^frac_total_sc[1-7]$")
  ) %>%
  distinct() %>%
  pivot_longer(
    cols = matches("^frac_total_sc[1-7]$"),
    names_to = "sc_group",
    names_prefix = "frac_total_",
    values_to = "homo_sc"
  )

# Assign the appropriate contextual value to each individual
df_individall <- df_individall %>%
  mutate(
    sc_group = sub(
      "^.*_(sc[1-7])$",
      "\\1",
      category
    )
  ) %>%
  left_join(
    sc_lookup,
    by = c("borough_year", "sc_group"),
    relationship = "many-to-one"
  )

# Reorder variables
df_individall <- df_individall %>%
  select(
    ID, category, borough_year, borough, year,
    homo_ethnic, ethnic_group,
    homo_sc, sc_group,
    everything()
  )
 
# specific homogroup

# Create contextual lookup table
subgroup_lookup <- df_individall %>%
  select(
    borough_year,
    matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$")
  ) %>%
  distinct() %>%
  pivot_longer(
    cols = matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$"),
    names_to = "specific_group",
    names_prefix = "frac_",
    values_to = "homo_group"
  )

df_individall <- df_individall %>%
  left_join(
    subgroup_lookup %>%
      select(borough_year, specific_group, homo_group),
    by = c(
      "borough_year",
      "category" = "specific_group"
    ),
    relationship = "many-to-one"
  )

# sanity check: for each of them, it computes the target value and the assigned value #####

# near() checks whether they are equivalent

# ethnic group
# df_check_eth <- df_individall %>%
#   select(
#     ID, borough_year, category, ethnic_group, homo_ethnic,
#     matches("^frac_total_(bangladeshi|indian|pakistani|chinese|african|carribean|ewsnib|irish|whiteasian|whiteblcarribean|whiteblafrican)$")
#   ) %>%
#   pivot_longer(
#     cols = matches("^frac_total_(bangladeshi|indian|pakistani|chinese|african|carribean|ewsnib|irish|whiteasian|whiteblcarribean|whiteblafrican)$"),
#     names_to = "ethnic_expected",
#     names_prefix = "frac_total_",
#     values_to = "expected_value"
#   ) %>%
#   filter(ethnic_group == ethnic_expected) %>%
#   mutate(
#     check = !is.na(homo_ethnic) &
#       !is.na(expected_value) &
#       near(homo_ethnic, expected_value)
#   )
# 
# all(df_check_eth$check)
# nrow(df_check_eth) == nrow(df_individall)
# df_check_eth %>%
#   group_by(borough_year, category) %>%
#   summarise(
#     n_individuals = n(),
#     n_errors = sum(!check),
#     all_correct = all(check),
#     .groups = "drop"
#   ) %>%
#   filter(!all_correct)

# social class
# df_check_sc <- df_individall %>%
#   select(
#     ID, borough_year, category, sc_group, homo_sc,
#     matches("^frac_total_sc[1-7]$")
#   ) %>%
#   pivot_longer(
#     cols = matches("^frac_total_sc[1-7]$"),
#     names_to = "sc_expected",
#     names_prefix = "frac_total_",
#     values_to = "expected_value"
#   ) %>%
#   filter(sc_group == sc_expected) %>%
#   mutate(
#     check = !is.na(homo_sc) &
#       !is.na(expected_value) &
#       near(homo_sc, expected_value)
#   )
# 
# all(df_check_sc$check)
# nrow(df_check_sc) == nrow(df_individall)
# df_check_sc %>%
#   group_by(borough_year, category) %>%
#   summarise(
#     n_individuals = n(),
#     n_errors = sum(!check),
#     all_correct = all(check),
#     .groups = "drop"
#   ) %>%
#   filter(!all_correct)

# specific groups

# df_check_group <- df_individall %>%
#   select(
#     ID, borough_year, category, homo_group,
#     matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$")
#   ) %>%
#   pivot_longer(
#     cols = matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$"),
#     names_to = "expected_category",
#     names_prefix = "frac_",
#     values_to = "expected_value"
#   ) %>%
#   filter(category == expected_category) %>%
#   mutate(
#     check = !is.na(homo_group) &
#       !is.na(expected_value) &
#       near(homo_group, expected_value)
#   )
# all(df_check_group$check)
# nrow(df_check_group) == nrow(df_individall)



df_individall <- df_individall %>% select(ID, category, borough_year, borough, year, homo_ethnic, homo_sc, homo_group, 
                                          ethnic_group,sc_group,shannon_ethnic_norm, shannon_status_norm)


write_dta(
  df_individall,
  "df_individall.dta",
  version = 14
)


# verification sum fractions from df_final ####
# dfcheckprop <- df_individall %>%
#   select(
#     borough_year,
#     matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$")
#   ) %>%
#   distinct() %>%
#   rowwise() %>%
#   mutate(
#     total_frac = sum(c_across(
#       matches("^frac_(white|mixed|black|asian)_.*_sc[1-7]$")
#     ), na.rm = TRUE),
#     check = near(total_frac, 1)
#   ) %>%
#   ungroup() %>%
#   select(borough_year, total_frac, check)
# unique(dfcheckprop$check)