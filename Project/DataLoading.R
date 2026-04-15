library(readxl)    
library(tidyverse)

# ── 1. Data loading 

load_gpg_table <- function(filename, sheet = "All") {
  raw <- read_excel(filename, sheet = sheet, skip = 3, col_names = TRUE)
  raw <- raw[, 1:4]
  colnames(raw) <- c("description", "code", "gpg_median", "gpg_mean")
  raw %>%
    filter(!is.na(description)) %>%
    filter(!str_starts(description, "\\^")) %>%
    filter(!str_starts(description, "Source")) %>%
    filter(!str_starts(description, "Key")) %>%
    filter(!str_starts(description, "Estimates")) %>%
    filter(!str_starts(description, "x =")) %>%
    filter(!str_starts(description, "Descri")) %>% 
    mutate(
      gpg_median  = as.numeric(gpg_median),
      gpg_mean    = as.numeric(gpg_mean),
      description = str_trim(description)
    )
}
# Load all 3 employment-type sheets for each table
load_all_sheets <- function(filename) {
  sheets <- c("All", "Full-Time", "Part-Time")
  map_dfr(sheets, function(s) {
    load_gpg_table(filename, sheet = s) %>%
      mutate(emp_type = s)
  })
}

data_path <- "D:/Maynooth/M.Sc Final Project/DataSet/ashegenderpaygap2025provisional/"

df_total      <- load_all_sheets(paste0(data_path, "PROV - Total Table 1.12  Gender pay gap 2025.xlsx"))
df_age        <- load_all_sheets(paste0(data_path, "PROV - Age Group Table 6.12  Gender pay gap 2025.xlsx"))
df_occupation <- load_all_sheets(paste0(data_path, "PROV - Occupation SOC20 (2) Table 2.12  Gender pay gap 2025.xlsx"))
df_industry   <- load_all_sheets(paste0(data_path, "PROV - SIC07 Industry (2) SIC2007 Table 4.12  Gender pay gap 2025.xlsx"))
df_pubpriv    <- load_all_sheets(paste0(data_path, "PROV - PubPriv Table 13.12  Gender pay gap 2025.xlsx"))
df_region     <- load_all_sheets(paste0(data_path, "PROV - Work Geography Table 7.12  Gender pay gap 2025.xlsx")) 

message("Data loaded successfully.")