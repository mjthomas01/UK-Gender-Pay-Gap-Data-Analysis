library(readxl)    
library(tidyverse)

source("DataLoading.R")

# --- Age ---
glimpse(df_age)          # column names, types, and first few values
head(df_age)             # first 6 rows
nrow(df_age)             # should be 27 (9 age groups × 3 employment types)

# --- Industry ---
glimpse(df_industry)
head(df_industry, 10)

# --- Check all 3 employment types loaded correctly ---
unique(df_age$emp_type)         # should print: "All" "Full-Time" "Part-Time"
table(df_age$emp_type)          # should show equal row counts for each type

# --- Check for NA values (these come from suppressed "x" cells) ---
colSums(is.na(df_age))
colSums(is.na(df_industry))
colSums(is.na(df_region))

df_age %>%
  filter(emp_type == "All", description == "All employees") %>%
  select(description, gpg_median, gpg_mean)

df_industry %>%
  filter(emp_type == "All",
         description == "FINANCIAL AND INSURANCE ACTIVITIES") %>%
  select(description, gpg_median, gpg_mean)


# ── 6. Save cleaned data to csv

write_csv(df_age,        "clean_age.csv")
write_csv(df_occupation, "clean_occupation.csv")
write_csv(df_industry,   "clean_industry.csv")
write_csv(df_pubpriv,    "clean_pubpriv.csv")
write_csv(df_region,     "clean_region.csv")
