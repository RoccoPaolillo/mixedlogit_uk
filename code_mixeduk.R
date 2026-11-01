library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(reshape2)


setwd("C:/Users/LENOVO/Documents/GitHub/mixedlogit_uk/")

df_final <- read_excel("df_final.xlsx")

df_sample <- df_final %>% slice_sample(prop = 0.05)
# write_xlsx(df_sample, "df_sample.xlsx")

df_individual <- df_sample %>%
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
categories <- names(df_final) %>%
  grep(
    "^(white|mixed|asian|black).*_sc[1-7]$",
    x = .,
    value = TRUE
  )

# Original aggregated counts
df_original <- df_final %>%
  pivot_longer(
    cols = all_of(categories),
    names_to = "category",
    values_to = "n_original"
  )

# Reconstructed individual counts
df_check <- df_individall %>%
  count(borough_year, category, name = "n_individual")

# Compare all categories and borough-years
df_validation <- df_original %>%
  left_join(
    df_check,
    by = c("borough_year", "category")
  ) %>%
  mutate(
    n_individual = replace_na(n_individual, 0L),
    difference = n_original - n_individual,
    match = !is.na(difference) & difference == 0
  )

all(df_validation$match)

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

# verification sum fractions

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

df_individall <- df_individall %>% select(ID, category, borough_year, borough, year, homo_ethnic,ethnic_group, everything())

# homo status

# Create contextual lookup table
sc_lookup <- df_individall %>%
  select(
    borough_year,
    matches(
      "^frac_total_sc[1-7]$"
    )
  ) %>%
  distinct() %>%
  pivot_longer(
    cols = matches(
      "^frac_total_sc[1-7]$"
    ),
    names_to = "sc_group",
    names_prefix = "frac_total_",
    values_to = "homo_sc"
  )

# Assign the appropriate contextual value to each individual
df_individall <- df_individall %>%
  mutate(
    sc_group = sub(
      "^(white|mixed|black|asian)_(.*)_sc[1-7]$",
      "\\3",
      category
    )
  ) %>%
  left_join(
    sc_lookup,
    by = c("borough_year", "sc_group")
  )

df_individall <- df_individall %>% select(ID, category, borough_year, borough, year, homo_sc,sc_group,ethnic_group, everything())