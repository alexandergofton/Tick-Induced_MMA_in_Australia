# ============================================================================
# Alpha-Gal Syndrome - OPTIMIZED VERSION for Posit Connect Cloud
# Key optimizations:
# 1. Lazy loading of figure data (only load when viewed)
# 2. Caching loaded data
# 3. Simplified data loading
# ============================================================================

library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(tidyr)
library(plotly)
library(geojsonsf)
library(smoothr)

# ============================================================================
# LOAD ONLY ESSENTIAL DATA ON STARTUP (Map data only)
# ============================================================================

# Read SA3 shapefile (contains spatial geometry + case data)
sa3_sf <- st_read("public_data/sa3_cases_simple.shp", quiet = TRUE)

# Read SA3 cases CSV (non-spatial version)
sa3_cases_csv <- read_csv("public_data/sa3_cases.csv", show_col_types = FALSE)

# Read risk ratio data for Figure 2
rr_sex_by_age <- read_csv(
  "public_data/rr_sex_by_age.csv",
  show_col_types = FALSE
)

rr_age_only <- read_csv("public_data/rr_age_only.csv", show_col_types = FALSE)

combined_rr <- read_csv(
  "public_data/combined_rr_results.csv",
  show_col_types = FALSE
)

# Read cached Ixodes holocyclus distribution or process if needed
holocyclus_cache <- "public_data/holocyclus_line_processed.rds"
if (file.exists(holocyclus_cache)) {
  holocyclus_line <- readRDS(holocyclus_cache)
} else {
  holocyclus_line <- geojson_sf(
    "public_data/holocyclus_distribution_polyline.geojson"
  ) %>%
    filter(st_geometry_type(.) == "LINESTRING") %>%
    smooth(method = "spline")
  saveRDS(holocyclus_line, holocyclus_cache)

# ============================================================================
# COLOR SCHEME
# ============================================================================

MidnightBlue <- "#00313C"
MiddayBlue <- "#00A9CE"

color_mapping_blue <- c(
  ">200 Suspected cases" = "#08306b",
  "100-199 Suspected cases" = "#08519c",
  "50-99 Suspected cases" = "#2171b5",
  "10-49 Suspected cases" = "#9ecae1",
  "<10 Suspected cases" = "#deebf7",
  "0 Suspected cases" = "#f7f7f7",
  "No data" = "grey90"
)

category_order <- c(
  ">200 Suspected cases",
  "100-199 Suspected cases",
  "50-99 Suspected cases",
  "10-49 Suspected cases",
  "<10 Suspected cases",
  "0 Suspected cases",
  "No data"
)

# ============================================================================
# DATA PREPARATION
# ============================================================================

sa3_sf <- st_transform(sa3_sf, crs = 4326)
sa3_sf$cppCat <- factor(sa3_sf$cppCat, levels = category_order)
sa3_sf$row_id <- seq_len(nrow(sa3_sf))

pal <- colorFactor(
  palette = color_mapping_blue,
  domain = category_order,
  ordered = TRUE
)

# ============================================================================
# HELPER FUNCTION: LAZY DATA LOADER
# ============================================================================

# Cache for loaded data
data_cache <- reactiveValues()

load_data_once <- function(cache_name, file_path, ...) {
  # Check if already loaded
  if (!is.null(data_cache[[cache_name]])) {
    return(data_cache[[cache_name]])
  }

  # Load and cache
  data <- read_csv(file_path, show_col_types = FALSE, ...)
  data_cache[[cache_name]] <- data
  return(data)
}

# ============================================================================
# UI (same as before)
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Alpha-Gal Syndrome Surveillance - Australia"),

  tags$head(
    tags$style(HTML(
      "
      .leaflet-container { background-color: #f0f0f0; }
      h3 { color: #00313C; margin-top: 20px; }
    "
    ))
  ),

  h3("Interactive Map"),
  leafletOutput("map", height = "600px"),

  br(),

  h3("Data Table"),
  DTOutput("data_table"),

  br(),

  h3("Figure 1: Age Distribution"),
  fluidRow(
    column(6, plotlyOutput("fig1_panel_a", height = "400px")),
    column(6, plotlyOutput("fig1_panel_b", height = "400px"))
  ),

  br(),

  h3("Figure 2: Risk Ratios"),
  fluidRow(
    column(4, plotlyOutput("fig2_panel_a", height = "400px")),
    column(4, plotlyOutput("fig2_panel_b", height = "400px")),
    column(4, plotlyOutput("fig2_panel_c", height = "400px"))
  ),

  br(),

  h3("Figure 3: Annual Testing Trends"),
  fluidRow(
    column(4, plotlyOutput("fig3_panel_a", height = "400px")),
    column(4, plotlyOutput("fig3_panel_b", height = "400px")),
    column(4, plotlyOutput("fig3_panel_c", height = "400px"))
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Map - already loaded
  output$map <- renderLeaflet({
    leaflet(sa3_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ pal(cppCat),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        opacity = 1,
        layerId = ~row_id,
        popup = ~ paste0(
          "<strong>",
          SA_name,
          "</strong><br>",
          "Cases per 1M PPY: ",
          round(c1MPPY, 1),
          "<br>",
          "Total suspected cases: ",
          totalSus
        )
      ) %>%
      addPolylines(
        data = holocyclus_line,
        color = "red",
        weight = 2,
        opacity = 0.8,
        popup = "Ixodes holocyclus distribution"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~cppCat,
        title = "Cases per 1M PPY",
        opacity = 1
      ) %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  })

  # Data table - already loaded
  output$data_table <- renderDT({
    display_data <- sa3_cases_csv %>%
      select(SA_name, totalSus, c1MPPY, cppCat) %>%
      arrange(desc(c1MPPY))

    datatable(
      display_data,
      colnames = c(
        "SA3 Region" = "SA_name",
        "Total Suspected Cases" = "totalSus",
        "Cases per 1M PPY" = "c1MPPY",
        "Category" = "cppCat"
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(2, "desc"))
      ),
      rownames = FALSE
    ) %>%
      formatRound("c1MPPY", 1)
  })

  # LAZY LOADED DATA - Only loads when figure is viewed

  # Figure 1 data
  fig1_data <- reactive({
    list(
      all_tested = load_data_once(
        "fig1_all",
        "public_data/fig1_all_tested.csv"
      ),
      positive = load_data_once(
        "fig1_pos",
        "public_data/fig1_positive_cases.csv"
      )
    )
  })

  output$fig1_panel_a <- renderPlotly({
    data <- fig1_data()$all_tested

    plot_a <- ggplot(data, aes(x = Age_Group, y = Count)) +
      geom_col(fill = "grey70", alpha = 0.7, color = "black") +
      geom_text(aes(label = scales::comma(Count)), vjust = -0.5, size = 3) +
      labs(x = "Age group (years)", y = "Number of people tested") +
      theme_minimal() +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.1))
      )

    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })

  output$fig1_panel_b <- renderPlotly({
    data <- fig1_data()$positive

    plot_b <- ggplot(data, aes(x = Age_Group, y = Count)) +
      geom_col(fill = "grey70", alpha = 0.7, color = "black") +
      geom_text(aes(label = scales::comma(Count)), vjust = -0.5, size = 3) +
      labs(x = "Age group (years)", y = "Number with positive α-Gal sIgE") +
      theme_minimal() +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.1))
      )

    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })

  # Figure 2 data
  fig2_data <- reactive({
    list(
      sex_by_age = load_data_once(
        "fig2_sex_age",
        "public_data/rr_sex_by_age.csv"
      ),
      age_only = load_data_once("fig2_age", "public_data/rr_age_only.csv"),
      combined = load_data_once(
        "fig2_combined",
        "public_data/combined_rr_results.csv"
      )
    )
  })

  output$fig2_panel_a <- renderPlotly({
    data <- fig2_data()$sex_by_age

    plot <- ggplot(data, aes(x = Age_Group, y = RR, color = Sex, group = Sex)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
      scale_color_manual(
        values = c("Female" = MiddayBlue, "Male" = MidnightBlue)
      ) +
      labs(x = "Age group", y = "Risk ratio (95% CI)") +
      theme_minimal()

    ggplotly(plot, tooltip = c("x", "y", "color"))
  })

  output$fig2_panel_b <- renderPlotly({
    data <- fig2_data()$age_only

    plot <- ggplot(data, aes(x = Age_Group, y = RR)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      geom_line(group = 1, linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
      labs(x = "Age group", y = "Risk ratio (95% CI)") +
      theme_minimal()

    ggplotly(plot, tooltip = c("x", "y"))
  })

  output$fig2_panel_c <- renderPlotly({
    data <- fig2_data()$combined %>%
      mutate(
        Model = factor(Model, levels = c("Age + Sex", "Age only", "Sex only"))
      )

    plot <- ggplot(data, aes(x = Variable, y = RR)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
      facet_wrap(~Model, scales = "free_x") +
      labs(x = "", y = "Risk ratio (95% CI)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(plot, tooltip = c("x", "y"))
  })

  # Figure 3 data
  fig3_data <- reactive({
    list(
      annual = load_data_once(
        "fig3_annual",
        "public_data/fig3_annual_data.csv"
      ),
      fitted_tests = tryCatch(
        load_data_once(
          "fig3_fitted_tests",
          "public_data/fig3_fitted_tests.csv"
        ),
        error = function(e) NULL
      ),
      fitted_pos = tryCatch(
        load_data_once("fig3_fitted_pos", "public_data/fig3_fitted_pos.csv"),
        error = function(e) NULL
      ),
      fitted_PR = tryCatch(
        load_data_once("fig3_fitted_PR", "public_data/fig3_fitted_PR.csv"),
        error = function(e) NULL
      ),
      breakpoints = load_data_once(
        "fig3_breakpoints",
        "public_data/fig3_breakpoints.csv"
      )
    )
  })

  output$fig3_panel_a <- renderPlotly({
    data <- fig3_data()

    plot <- ggplot(data$annual, aes(x = Year)) +
      geom_col(aes(y = Total_Tests), fill = "grey70", alpha = 0.7) +
      labs(x = "Year", y = "Total tests") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)

    if (!is.null(data$fitted_tests)) {
      plot <- plot +
        geom_line(
          data = data$fitted_tests,
          aes(x = Year, y = Fitted),
          color = "red",
          linewidth = 1
        )
    }

    ggplotly(plot, tooltip = c("x", "y"))
  })

  output$fig3_panel_b <- renderPlotly({
    data <- fig3_data()

    plot <- ggplot(data$annual, aes(x = Year)) +
      geom_col(aes(y = Positive_Tests), fill = "grey70", alpha = 0.7) +
      labs(x = "Year", y = "Positive tests") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)

    if (!is.null(data$fitted_pos)) {
      plot <- plot +
        geom_line(
          data = data$fitted_pos,
          aes(x = Year, y = Fitted),
          color = "red",
          linewidth = 1
        )
    }

    ggplotly(plot, tooltip = c("x", "y"))
  })

  output$fig3_panel_c <- renderPlotly({
    data <- fig3_data()

    plot <- ggplot(data$annual, aes(x = Year)) +
      geom_line(aes(y = Positivity_Rate * 100), linewidth = 1) +
      geom_point(aes(y = Positivity_Rate * 100), size = 3) +
      labs(x = "Year", y = "Positivity rate (%)") +
      theme_minimal()

    if (!is.null(data$fitted_PR)) {
      plot <- plot +
        geom_line(
          data = data$fitted_PR,
          aes(x = Year, y = Fitted * 100),
          color = "red",
          linewidth = 1
        )
    }

    ggplotly(plot, tooltip = c("x", "y"))
  })
}

shinyApp(ui = ui, server = server)
