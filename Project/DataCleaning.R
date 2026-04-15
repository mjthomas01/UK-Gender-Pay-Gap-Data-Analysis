# ── Phase 2: Data Cleaning & Wrangling ────────────────────────────────────────

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("DataLoading.R")

# ── 2. Clean df_age ───────────────────────────────────────────────────────────

age_order <- c("All employees", "16-17", "18-21", "22-29",
               "30-39", "40-49", "50-59", "60+")

df_age_clean <- df_age %>%
  mutate(description = str_replace(description, "16-17b", "16-17")) %>%
  filter(description != "Not Classified") %>%
  mutate(description = factor(description, levels = age_order, ordered = TRUE))

# Check it looks right
levels(df_age_clean$description)   # should print age groups in correct order
glimpse(df_age_clean)
nrow(df_age_clean)

# ── 3. Clean df_industry ──────────────────────────────────────────────────────

top_industries <- c(
  "AGRICULTURE, FORESTRY AND FISHING",
  "MINING AND QUARRYING",
  "MANUFACTURING",
  "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
  "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
  "CONSTRUCTION",
  "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
  "TRANSPORTATION AND STORAGE",
  "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
  "INFORMATION AND COMMUNICATION",
  "FINANCIAL AND INSURANCE ACTIVITIES",
  "REAL ESTATE ACTIVITIES",
  "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
  "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
  "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY",
  "EDUCATION",
  "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
  "ARTS, ENTERTAINMENT AND RECREATION",
  "OTHER SERVICE ACTIVITIES"
)

# Short readable labels — in the same order as top_industries above
industry_short_labels <- c(
  "Agriculture", "Mining", "Manufacturing", "Electricity & gas",
  "Water & sewerage", "Construction", "Wholesale & retail",
  "Transport", "Accommodation & food", "Info & comms",
  "Finance & insurance", "Real estate", "Prof, sci & tech",
  "Admin & support", "Public admin", "Education",
  "Health & social work", "Arts & entertainment", "Other services"
)

df_industry_clean <- df_industry %>%
  
  # Keep only the top-level sector rows
  filter(description %in% top_industries) %>%
  
  # Add the short label as a new column
  mutate(
    short_label = industry_short_labels[match(description, top_industries)]
  ) %>%
  
  # Convert short_label to a factor ordered by All-employee median GPG
  # (this makes charts automatically sort from highest to lowest gap)
  mutate(
    short_label = reorder(short_label,
                          ifelse(emp_type == "All", gpg_median, NA),
                          mean, na.rm = TRUE)
  )

glimpse(df_industry_clean)
nrow(df_industry_clean)

# ── 4. Clean df_occupation ────────────────────────────────────────────────────

top_occupations <- c(
  "Managers, directors and senior officials",
  "Professional occupations",
  "Associate professional occupations",
  "Administrative and secretarial occupations",
  "Skilled trades occupations",
  "Caring, leisure and other service occupations",
  "Sales and customer service occupations",
  "Process, plant and machine operatives",
  "Elementary occupations"
)

df_occupation_clean <- df_occupation %>%
  filter(description %in% top_occupations) %>%
  filter(nchar(trimws(as.character(code))) == 1) %>%  # keep only single-digit SOC codes
  mutate(description = recode(description,
                              "Managers, directors and senior officials"      = "Managers & directors",
                              "Associate professional occupations"            = "Associate professionals",
                              "Administrative and secretarial occupations"    = "Admin & secretarial",
                              "Caring, leisure and other service occupations" = "Caring & leisure",
                              "Sales and customer service occupations"        = "Sales & customer service",
                              "Process, plant and machine operatives"         = "Process & plant operatives"
  ))

glimpse(df_occupation_clean)
nrow(df_occupation_clean)

# ── 5. Clean df_pubpriv ───────────────────────────────────────────────────────


df_pubpriv_clean <- df_pubpriv %>%
  filter(description != "Not classified") %>%
  mutate(description = recode(description,
                              "Non-profit body or mutual association" = "Non-profit / mutual"
  ))

glimpse(df_pubpriv_clean)

# ── 6. Clean df_region ────────────────────────────────────────────────────────

major_regions <- c(
  "North East", "North West", "Yorkshire and The Humber",
  "East Midlands", "West Midlands", "East", "London",
  "South East", "South West", "Wales", "Scotland", "Northern Ireland"
)

df_region_clean <- df_region %>%
  mutate(description = str_trim(description)) %>%   # remove any stray whitespace
  filter(description %in% major_regions) %>%
  
  # Order regions geographically north to south for charts
  mutate(description = factor(description,
                              levels = c("Scotland", "Northern Ireland", "North East",
                                         "North West", "Yorkshire and The Humber",
                                         "East Midlands", "West Midlands", "Wales",
                                         "East", "London", "South East", "South West")))

glimpse(df_region_clean)
nrow(df_region_clean)

# ── 7. Create a long-format summary table ─────────────────────────────────────

df_age_long <- df_age_clean %>%
  pivot_longer(
    cols      = c(gpg_median, gpg_mean),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = recode(metric,
                         gpg_median = "Median GPG",
                         gpg_mean   = "Mean GPG"
  ))

# Check the structure — rows should be double the wide version
glimpse(df_age_long)
nrow(df_age_long)

cat("NAs in df_age_clean:\n");        print(colSums(is.na(df_age_clean)))
cat("NAs in df_industry_clean:\n");   print(colSums(is.na(df_industry_clean)))
cat("NAs in df_occupation_clean:\n"); print(colSums(is.na(df_occupation_clean)))
cat("NAs in df_pubpriv_clean:\n");    print(colSums(is.na(df_pubpriv_clean)))
cat("NAs in df_region_clean:\n");     print(colSums(is.na(df_region_clean)))

write_csv(df_age_clean,        "clean_age.csv")
write_csv(df_industry_clean,   "clean_industry.csv")
write_csv(df_occupation_clean, "clean_occupation.csv")
write_csv(df_pubpriv_clean,    "clean_pubpriv.csv")
write_csv(df_region_clean,     "clean_region.csv")
write_csv(df_age_long,         "clean_age_long.csv")
