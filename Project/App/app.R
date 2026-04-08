library(shiny)
library(shinydashboard)
library(plotly)
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


# Age groups in the right order (not alphabetical)
age_order <- c("16-17", "18-21", "22-29", "30-39", "40-49", "50-59", "60+")

# Top-level industry rows only (all-caps rows are sector headings)
top_industries <- c(
  "AGRICULTURE, FORESTRY AND FISHING", "MINING AND QUARRYING", "MANUFACTURING",
  "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
  "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
  "CONSTRUCTION",
  "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
  "TRANSPORTATION AND STORAGE", "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
  "INFORMATION AND COMMUNICATION", "FINANCIAL AND INSURANCE ACTIVITIES",
  "REAL ESTATE ACTIVITIES", "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
  "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
  "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY",
  "EDUCATION", "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
  "ARTS, ENTERTAINMENT AND RECREATION", "OTHER SERVICE ACTIVITIES"
)

# Short labels for industries (for chart readability)
industry_labels <- c(
  "Agriculture", "Mining", "Manufacturing", "Electricity & gas",
  "Water & sewerage", "Construction", "Wholesale & retail",
  "Transport", "Accommodation & food", "Info & comms",
  "Finance & insurance", "Real estate", "Prof, sci & tech",
  "Admin & support", "Public admin", "Education",
  "Health & social work", "Arts & entertainment", "Other services"
)

# Top-level occupation rows only (single-digit codes)
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

# Major regions only
major_regions <- c(
  "North East", "North West", "Yorkshire and The Humber",
  "East Midlands", "West Midlands", "East", "London",
  "South East", "South West", "Wales", "Scotland", "Northern Ireland"
)


# ── 2. UI


ui <- dashboardPage(
  
  dashboardHeader(title = "UK Gender Pay Gap 2025"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",         tabName = "overview",    icon = icon("home")),
      menuItem("By age",           tabName = "age",         icon = icon("users")),
      menuItem("By industry",      tabName = "industry",    icon = icon("industry")),
      menuItem("By occupation",    tabName = "occupation",  icon = icon("briefcase")),
      menuItem("By region",        tabName = "region",      icon = icon("map")),
      menuItem("Public vs private",tabName = "pubpriv",     icon = icon("building"))
    ),
    hr(),
    # Global filter — applies to every tab
    selectInput(
      inputId  = "emp_type",
      label    = "Employment type",
      choices  = c("All", "Full-Time", "Part-Time"),
      selected = "All"
    ),
    selectInput(
      inputId  = "metric",
      label    = "Metric",
      choices  = c("Median GPG" = "gpg_median", "Mean GPG" = "gpg_mean"),
      selected = "gpg_median"
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── Overview tab ──────────────────────────────────────────────────────────
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("box_overall",    width = 3),
                valueBoxOutput("box_fulltime",   width = 3),
                valueBoxOutput("box_parttime",   width = 3),
                valueBoxOutput("box_finance",    width = 3)
              ),
              fluidRow(
                box(title = "GPG by age group",    plotlyOutput("plot_age_overview"),    width = 6),
                box(title = "GPG by industry",     plotlyOutput("plot_ind_overview"),    width = 6)
              ),
              fluidRow(
                box(title = "GPG by sector",       plotlyOutput("plot_pubpriv_overview"), width = 6),
                box(title = "GPG by occupation",   plotlyOutput("plot_occ_overview"),    width = 6)
              )
      ),
      
      # ── Age tab ───────────────────────────────────────────────────────────────
      tabItem(tabName = "age",
              fluidRow(
                box(
                  title = "Gender pay gap by age group",
                  width = 12,
                  plotlyOutput("plot_age", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "All employment types side by side",
                  width = 12,
                  plotlyOutput("plot_age_compare", height = "350px")
                )
              )
      ),
      
      # ── Industry tab ──────────────────────────────────────────────────────────
      tabItem(tabName = "industry",
              fluidRow(
                box(
                  title = "Gender pay gap by industry (sorted)",
                  width = 12,
                  plotlyOutput("plot_industry", height = "500px")
                )
              )
      ),
      
      # ── Occupation tab ────────────────────────────────────────────────────────
      tabItem(tabName = "occupation",
              fluidRow(
                box(
                  title = "Gender pay gap by occupation group",
                  width = 12,
                  plotlyOutput("plot_occupation", height = "400px")
                )
              )
      ),
      
      # ── Region tab ────────────────────────────────────────────────────────────
      tabItem(tabName = "region",
              fluidRow(
                box(
                  title = "Gender pay gap by UK region",
                  width = 12,
                  plotlyOutput("plot_region", height = "420px")
                )
              )
      ),
      
      # ── Public vs Private tab ─────────────────────────────────────────────────
      tabItem(tabName = "pubpriv",
              fluidRow(
                box(
                  title = "Public vs private sector — all employment types",
                  width = 12,
                  plotlyOutput("plot_pubpriv", height = "400px")
                )
              )
      )
    )
  )
)
# ── 3. Server ──────────────────────────────────────────────────────────────────

server <- function(input, output) {
  
  # Reactive helpers — filter data based on sidebar inputs
  # These automatically re-run whenever the user changes a dropdown
  
  age_data <- reactive({
    df_age %>%
      filter(emp_type == input$emp_type) %>%
      filter(description %in% c("All employees", age_order)) %>%
      filter(description != "All employees") %>%
      mutate(description = factor(description, levels = age_order))
  })
  
  industry_data <- reactive({
    df_industry %>%
      filter(emp_type == input$emp_type) %>%
      filter(description %in% top_industries) %>%
      mutate(
        short_label = industry_labels[match(description, top_industries)]
      ) %>%
      arrange(desc(.data[[input$metric]]))
  })
  
  occ_data <- reactive({
    df_occupation %>%
      filter(emp_type == input$emp_type) %>%
      filter(description %in% top_occupations) %>%
      arrange(desc(.data[[input$metric]]))
  })
  
  pubpriv_data <- reactive({
    df_pubpriv %>%
      filter(description %in% c("Public sector", "Private sector",
                                "Non-profit body or mutual association"))
  })
  
  region_data <- reactive({
    df_region %>%
      filter(emp_type == input$emp_type) %>%
      filter(str_trim(description) %in% major_regions) %>%
      arrange(desc(.data[[input$metric]]))
  })
  
  
  # ── Value boxes ──────────────────────────────────────────────────────────────
  
  output$box_overall <- renderValueBox({
    val <- df_age %>%
      filter(emp_type == "All", description == "All employees") %>%
      pull(gpg_median)
    valueBox(paste0(val, "%"), "Overall median GPG", icon = icon("percent"), color = "red")
  })
  
  output$box_fulltime <- renderValueBox({
    val <- df_age %>%
      filter(emp_type == "Full-Time", description == "All employees") %>%
      pull(gpg_median)
    valueBox(paste0(val, "%"), "Full-time median GPG", icon = icon("clock"), color = "yellow")
  })
  
  output$box_parttime <- renderValueBox({
    val <- df_age %>%
      filter(emp_type == "Part-Time", description == "All employees") %>%
      pull(gpg_median)
    valueBox(paste0(val, "%"), "Part-time median GPG", icon = icon("clock"), color = "green")
  })
  
  output$box_finance <- renderValueBox({
    val <- df_industry %>%
      filter(emp_type == "All", description == "FINANCIAL AND INSURANCE ACTIVITIES") %>%
      pull(gpg_median)
    valueBox(paste0(val, "%"), "Finance sector GPG", icon = icon("pound-sign"), color = "red")
  })
  
  
  # ── Chart helper — shared ggplot → plotly theme ───────────────────────────────
  
  make_bar <- function(df, x_col, y_col, title = "", flip = FALSE) {
    metric_label <- if (y_col == "gpg_median") "Median GPG (%)" else "Mean GPG (%)"
    
    p <- ggplot(df, aes(
      x    = reorder(.data[[x_col]], .data[[y_col]]),
      y    = .data[[y_col]],
      fill = .data[[y_col]],
      text = paste0(.data[[x_col]], ": ", round(.data[[y_col]], 1), "%")
    )) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradient2(low = "#1D9E75", mid = "#f5f4f0", high = "#E24B4A", midpoint = 0) +
      labs(x = NULL, y = metric_label, title = title) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(size = 13))
    
    if (flip) p <- p + coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 10, r = 10, t = 30, b = 10))
  }
  
  
  # ── Overview charts ───────────────────────────────────────────────────────────
  
  output$plot_age_overview <- renderPlotly({
    make_bar(age_data(), "description", input$metric, flip = TRUE)
  })
  
  output$plot_ind_overview <- renderPlotly({
    d <- industry_data() %>% head(10)
    make_bar(d, "short_label", input$metric, flip = TRUE)
  })
  
  output$plot_pubpriv_overview <- renderPlotly({
    d <- pubpriv_data() %>% filter(emp_type == input$emp_type)
    make_bar(d, "description", input$metric)
  })
  
  output$plot_occ_overview <- renderPlotly({
    make_bar(occ_data(), "description", input$metric, flip = TRUE)
  })
  
  
  # ── Age tab ───────────────────────────────────────────────────────────────────
  
  output$plot_age <- renderPlotly({
    metric_label <- if (input$metric == "gpg_median") "Median GPG (%)" else "Mean GPG (%)"
    d <- age_data()
    p <- ggplot(d, aes(
      x    = description,
      y    = .data[[input$metric]],
      fill = .data[[input$metric]],
      text = paste0(description, ": ", round(.data[[input$metric]], 1), "%")
    )) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      scale_fill_gradient2(low = "#1D9E75", mid = "#f0efe8", high = "#E24B4A", midpoint = 0) +
      labs(x = "Age group", y = metric_label) +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_age_compare <- renderPlotly({
    d <- df_age %>%
      filter(description %in% age_order) %>%
      mutate(description = factor(description, levels = age_order))
    
    p <- ggplot(d, aes(
      x      = description,
      y      = .data[[input$metric]],
      fill   = emp_type,
      text   = paste0(emp_type, " — ", description, ": ",
                      round(.data[[input$metric]], 1), "%")
    )) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      scale_fill_manual(values = c("All" = "#378ADD", "Full-Time" = "#1D9E75",
                                   "Part-Time" = "#BA7517")) +
      labs(x = "Age group", y = if (input$metric == "gpg_median") "Median GPG (%)" else "Mean GPG (%)",
           fill = NULL) +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })
  
  
  # ── Industry tab ─────────────────────────────────────────────────────────────
  
  output$plot_industry <- renderPlotly({
    make_bar(industry_data(), "short_label", input$metric, flip = TRUE)
  })
  
  
  # ── Occupation tab ────────────────────────────────────────────────────────────
  
  output$plot_occupation <- renderPlotly({
    make_bar(occ_data(), "description", input$metric, flip = TRUE)
  })
  
  
  # ── Region tab ────────────────────────────────────────────────────────────────
  
  output$plot_region <- renderPlotly({
    make_bar(region_data(), "description", input$metric, flip = TRUE)
  })
  
  
  # ── Public vs private tab ─────────────────────────────────────────────────────
  
  output$plot_pubpriv <- renderPlotly({
    d <- df_pubpriv %>%
      filter(description %in% c("Public sector", "Private sector",
                                "Non-profit body or mutual association")) %>%
      pivot_longer(cols = c(gpg_median, gpg_mean),
                   names_to = "measure", values_to = "value") %>%
      mutate(measure = recode(measure,
                              gpg_median = "Median GPG",
                              gpg_mean   = "Mean GPG"))
    
    p <- ggplot(d, aes(
      x    = description,
      y    = value,
      fill = interaction(emp_type, measure),
      text = paste0(emp_type, " — ", measure, ": ", round(value, 1), "%")
    )) +
      geom_col(position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "GPG (%)", fill = NULL) +
      facet_wrap(~ measure) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui = ui, server = server)
