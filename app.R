# ============================================================================
# Alpha-Gal Syndrome (AGS) Interactive Map - Shiny App (PUBLIC VERSION)
# SA3 Regional Analysis of MMA Cases in Australia
#
# This version uses pre-computed aggregated data and does NOT include
# individual-level patient data.
# ============================================================================

#setwd("~/Documents/01_Projects/alpha_gal/aGal_IgE_prev/mapping/SHINY_PUBLIC")

# Load required packages
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
# DATA LOADING - ESSENTIAL DATA ONLY (Lazy loading implemented in server)
# ============================================================================

# ESSENTIAL: Load only data needed for initial map display
# Read SA3 shapefile (contains spatial geometry + case data)
sa3_sf <- st_read("public_data/sa3_cases_simple.shp", quiet = TRUE)

# ESSENTIAL: Read cached Ixodes holocyclus distribution for map overlay
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
}

# NOTE: All other data files are now loaded lazily in the server function
# when the corresponding figure/tab is accessed. This significantly reduces
# initial app load time.

# ============================================================================
# COLOR SCHEME
# ============================================================================

MidnightBlue <- "#00313C"
MiddayBlue <- "#00A9CE"

# Blue gradient color mapping for suspected cases categories
color_mapping_blue <- c(
  ">200 Suspected cases" = "#08306b",
  "100-199 Suspected cases" = "#08519c",
  "50-99 Suspected cases" = "#2171b5",
  "10-49 Suspected cases" = "#9ecae1",
  "<10 Suspected cases" = "#deebf7",
  "0 Suspected cases" = "#f7f7f7",
  "No data" = "grey90"
)

# Define the order of categories for the legend (high to low)
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

# Transform to WGS84 for Leaflet (if not already)
sa3_sf <- st_transform(sa3_sf, crs = 4326)

# Ensure cppCat is a factor with correct order
sa3_sf$cppCat <- factor(sa3_sf$cppCat, levels = category_order)

# Add row ID for tracking clicks
sa3_sf$row_id <- seq_len(nrow(sa3_sf))

# Create color palette function
pal <- colorFactor(
  palette = color_mapping_blue,
  domain = category_order,
  ordered = TRUE
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  titlePanel(
    ""
    #"Suspected Mammalian Meat Allergy (MMA) Cases by SA3 Region (2014-2024)"
  ),

  # Custom CSS for modal overlay and clickable headers
  tags$head(
    tags$style(HTML(
      "
      /* Modal overlay for plots */
      #plot_modal_overlay {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.7);
        z-index: 9999;
        overflow: auto;
      }
      #plot_modal_content {
        position: relative;
        margin: 40px auto;
        padding: 20px;
        width: 90%;
        max-width: 1400px;
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
      }
      #plot_modal_close {
        position: absolute;
        top: 10px;
        right: 15px;
        font-size: 28px;
        font-weight: bold;
        color: #666;
        cursor: pointer;
        z-index: 10000;
      }
      #plot_modal_close:hover {
        color: #000;
      }
      #plot_modal_title {
        margin-top: 0;
        margin-bottom: 15px;
        padding-right: 40px;
        color: #333;
      }
      /* Modal overlay for map */
      #map_modal_overlay {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.9);
        z-index: 9999;
        overflow: hidden;
      }
      #map_modal_content {
        position: relative;
        width: 100%;
        height: 100%;
        padding: 20px;
      }
      #map_modal_close {
        position: absolute;
        top: 20px;
        right: 30px;
        font-size: 36px;
        font-weight: bold;
        color: #fff;
        cursor: pointer;
        z-index: 10001;
        background-color: rgba(0, 0, 0, 0.5);
        padding: 5px 15px;
        border-radius: 5px;
      }
      #map_modal_close:hover {
        background-color: rgba(0, 0, 0, 0.8);
      }
      /* Fullscreen button for map */
      .map-fullscreen-btn {
        position: absolute;
        top: 65px;
        right: 10px;
        z-index: 2000;
        background-color: white;
        border: 2px solid rgba(0,0,0,0.2);
        border-radius: 4px;
        padding: 5px 10px;
        cursor: pointer;
        font-size: 18px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.4);
        pointer-events: auto;
      }
      .map-fullscreen-btn:hover {
        background-color: #f4f4f4;
      }
      /* Clickable plot headers */
      .clickable-header {
        cursor: pointer;
        display: inline-flex;
        align-items: center;
        transition: color 0.2s;
      }
      .clickable-header:hover {
        color: #337ab7;
      }
      .clickable-header::after {
        content: '⛶';
        margin-left: 8px;
        font-size: 14px;
        color: #999;
      }
      .clickable-header:hover::after {
        color: #337ab7;
      }
    "
    ))
  ),

  # Modal overlay for zoomed plots
  div(
    id = "plot_modal_overlay",
    onclick = "Shiny.setInputValue('close_modal', Math.random())",
    div(
      id = "plot_modal_content",
      onclick = "event.stopPropagation();",
      span(
        id = "plot_modal_close",
        onclick = "Shiny.setInputValue('close_modal', Math.random())",
        HTML("&times;")
      ),
      h3(id = "plot_modal_title", textOutput("modal_title", inline = TRUE)),
      plotlyOutput("zoomed_plot", height = "700px")
    )
  ),

  # Modal overlay for zoomed map
  div(
    id = "map_modal_overlay",
    onclick = "Shiny.setInputValue('close_map_modal', Math.random())",
    div(
      id = "map_modal_content",
      onclick = "event.stopPropagation();",
      span(
        id = "map_modal_close",
        onclick = "Shiny.setInputValue('close_map_modal', Math.random())",
        HTML("&times;")
      ),
      leafletOutput("map_zoomed", height = "95vh")
    )
  ),

  tabsetPanel(
    id = "main_tabs",
    type = "tabs",

    # =========================================================================
    # TAB 0: Landing Page
    # =========================================================================
    tabPanel(
      "Home",
      br(),
      div(
        style = "max-width: 1000px; margin: 0 auto; padding: 40px 20px;",

        # Title
        h1(
          "Tick-Induced Mammalian Meat Allergy in Australia: National Prevalence and Geographic Distribution from Laboratory Surveillance, 2014-2024",
          style = "color: #00313C; text-align: center; margin-bottom: 30px;"
        ),

        hr(),

        # Authors
        h3("Authors", style = "color: #00313C; margin-top: 30px;"),
        p(
          "Emily Smith",
          tags$sup("1,2"),
          ", Paul Campbell",
          tags$sup("3"),
          ", Carl Kennedy",
          tags$sup("4"),
          ", Karl Baumgart",
          tags$sup("5"),
          ", Lucinda Williams",
          tags$sup("6"),
          ", Sheryl van Nunen",
          tags$sup("7,8,9,10"),
          ", Andrew Walker",
          tags$sup("1"),
          ", and Alexander W. Gofton",
          tags$sup("2,10,*"),
          style = "font-size: 16px; line-height: 1.8;"
        ),

        # Affiliations
        h3("Affiliations", style = "color: #00313C; margin-top: 30px;"),
        p(
          tags$sup("1"),
          " University of Queensland, Brisbane, Australia",
          br(),
          tags$sup("2"),
          " CSIRO Health and Biosecurity, Brisbane, Australia",
          br(),
          tags$sup("3"),
          " QML Pathology, Brisbane, Australia",
          br(),
          tags$sup("4"),
          " Sullivan Nicolaides Pathology, Brisbane, Australia",
          br(),
          tags$sup("5"),
          " Douglas Hanley Moir Pathology, Sydney, Australia",
          br(),
          tags$sup("6"),
          " Laverty Pathology, Sydney, Australia",
          br(),
          tags$sup("7"),
          " Northern Beaches Hospital, Sydney, Australia",
          br(),
          tags$sup("8"),
          " National Allergy Centre of Excellence, Australia",
          br(),
          tags$sup("9"),
          " The University of Sydney, Sydney, Australia",
          br(),
          tags$sup("10"),
          " TiARA (Tick-induced Allergies Research and Awareness), Australia",
          br(),
          br(),
          tags$sup("*"),
          " Correspondance to Alexander W. Gofton, CSIRO Health and Biosecurity",
          br(),
          " 41 Boggo Rd, Dutton Park, QLD 4102, Brisbane, Australia",
          br(),
          " Email: alexander.gofton@csiro.au",

          style = "font-size: 14px; line-height: 1.8;"
        ),

        # Manuscript link
        h3("Published Manuscript", style = "color: #00313C; margin-top: 30px;"),
        p(
          tags$a(
            href = "https://doi.org/10.XXXX/XXXXXX",
            target = "_blank",
            "Link to published manuscript",
            style = "color: #00A9CE; font-size: 16px;"
          ),
          style = "font-size: 16px;"
        ),

        # Abstract
        h3("Abstract", style = "color: #00313C; margin-top: 30px;"),
        p(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. This is a placeholder abstract.
          Replace this text with the actual abstract from your manuscript. The abstract should
          provide a brief overview of the research objectives, methods, key findings, and conclusions.
          Typically 150-300 words summarizing the main points of the study.",
          style = "font-size: 15px; line-height: 1.8; text-align: justify;"
        ),

        # About this app
        h3(
          "About This Interactive Application",
          style = "color: #00313C; margin-top: 30px;"
        ),
        p(
          "This interactive web application provides access to the data visualizations and
          analyses presented in the manuscript. Users can explore:",
          style = "font-size: 15px; line-height: 1.8;"
        ),
        tags$ul(
          style = "font-size: 15px; line-height: 1.8;",
          tags$li(
            "Interactive Map: Explore geographic distribution of suspected MMA cases across Australian SA3 regions"
          ),
          tags$li(
            "Figures 1-7: Interactive versions of all manuscript figures with zoom and exploration capabilities"
          ),
          tags$li("Data tables and detailed regional statistics")
        ),
        p(
          "Navigate through the tabs above to explore different sections of the analysis.
          All figures can be clicked to view in full-screen mode for detailed examination.",
          style = "font-size: 15px; line-height: 1.8;"
        ),

        # Data sources
        h3("Data Sources", style = "color: #00313C; margin-top: 30px;"),
        p(
          "Data: QML Pathology; Sullivan Nicolaides Pathology; DHM Pathology; Laverty Pathology",
          style = "font-size: 14px; color: #666;"
        ),

        hr(style = "margin-top: 40px;"),

        # Footer
        p(
          "© 2025 | For questions or feedback, please contact: your.email@example.com",
          style = "text-align: center; color: #999; font-size: 13px; margin-top: 20px;"
        )
      )
    ), # End of tabPanel "Home"

    # =========================================================================
    # TAB 1: Interactive Map
    # =========================================================================
    tabPanel(
      "Interactive Map",
      br(),
      sidebarLayout(
        # Sidebar panel with selection controls
        sidebarPanel(
          width = 2,
          h4("Selection Controls"),
          hr(),
          # Dropdown for selecting by state/territory
          selectInput(
            "select_region",
            label = "Select by Region:",
            choices = c(
              "-- Choose --" = "",
              "All Australia" = "ALL",
              "New South Wales" = "1",
              "Victoria" = "2",
              "Queensland" = "3",
              "South Australia" = "4",
              "Western Australia" = "5",
              "Tasmania" = "6",
              "Northern Territory" = "7",
              "Australian Capital Territory" = "8",
              "Other Territories" = "9"
            ),
            selected = ""
          ),
          hr(),
          actionButton(
            "clear_selection",
            "Deselect All",
            class = "btn-warning btn-block"
          ),
          hr(),
          p(
            "Or click individual SA3 regions on the map to select/deselect them.",
            style = "color: #666; font-size: 12px;"
          ),
          hr(),
          # Show count of selected regions
          h5("Selection Summary"),
          textOutput("selection_count"),
          tags$style("#selection_count { font-weight: bold; color: #337ab7; }")
        ),

        # Main panel with map, table, and charts
        mainPanel(
          width = 10,
          fluidRow(
            # Map panel (left side - 8 columns)
            column(
              8,
              div(
                style = "position: relative;",
                # Fullscreen button
                actionButton(
                  "zoom_map",
                  icon = icon("expand"),
                  label = NULL,
                  class = "map-fullscreen-btn",
                  title = "View fullscreen"
                ),
                leafletOutput("map", height = "700px")
              )
            ),

            # Table panel (right side - 4 columns)
            column(
              4,
              h4("Selected SA3 Regions"),
              p(
                "Click on SA3 regions to add/remove from table",
                style = "color: #666; font-size: 12px;"
              ),
              DTOutput("selected_table")
            )
          ),

          # Charts section - Tests/Cases over time only (no individual-level density plot)
          fluidRow(
            column(
              12,
              hr(),
              # Tests and Cases Over Time plot (full width to match map + table above)
              h4("Tests and Cases Over Time"),
              plotlyOutput("tests_cases_plot", height = "400px")
            )
          ),

          # Footer info
          tags$div(
            style = "margin-top: 10px; color: #666; font-size: 12px;",
            "Data: QML Pathology; Sullivan Nicolaides Pathology; DHM Pathology; Laverty Pathology"
          )
        )
      )
    ), # End of tabPanel "Interactive Map"

    # =========================================================================
    # TAB 2: Figure 1
    # =========================================================================
    tabPanel(
      "Figure 1",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 1"),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig1_panel_a', {priority: 'event'})",
          "A. All Tested"
        ),
        plotlyOutput("fig1_panel_a", height = "320px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig1_panel_b', {priority: 'event'})",
          "B. Suspected MMA Cases"
        ),
        plotlyOutput("fig1_panel_b", height = "320px"),
        p(
          tags$b("Figure 1"),
          " Age distribution by sex of all tested people (A) and people with suspected MMA (B).",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ), # End of tabPanel "Figure 1"

    # =========================================================================
    # TAB 3: Figure 2
    # =========================================================================
    tabPanel(
      "Figure 2",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 2"),
        fluidRow(
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig2_panel_a', {priority: 'event'})",
              "A. Male vs Female"
            ),
            plotlyOutput("fig2_panel_a", height = "280px")
          ),
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig2_panel_b', {priority: 'event'})",
              "B. Age vs 25-34 ref"
            ),
            plotlyOutput("fig2_panel_b", height = "280px")
          )
        ),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig2_panel_c', {priority: 'event'})",
          "C. Age-Sex vs Female 25-34"
        ),
        plotlyOutput("fig2_panel_c", height = "400px"),
        p(
          tags$b("Figure 2"),
          " Risk ratios comparing: (A) males to females within each age category, ",
          "(B) each age group to the reference (25–34 years, pooled sexes), and ",
          "(C) all age-sex combinations to females aged 25–34 years (the lowest-risk group, 18.5% positivity). ",
          "Filled circles indicate statistical significance (p < 0.01); open circles indicate non-significant comparisons. ",
          "Error bars represent 95% confidence intervals. Dashed vertical line indicates RR = 1.0 (no difference from reference group).",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ), # End of tabPanel "Figure 2"

    # =========================================================================
    # TAB 4: Figure 3
    # =========================================================================
    tabPanel(
      "Figure 3",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 3"),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig3_panel_a', {priority: 'event'})",
          "A. Total Tests"
        ),
        plotlyOutput("fig3_panel_a", height = "280px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig3_panel_b', {priority: 'event'})",
          "B. Suspected MMA Cases"
        ),
        plotlyOutput("fig3_panel_b", height = "280px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig3_panel_c', {priority: 'event'})",
          "C. Positivity Rate"
        ),
        plotlyOutput("fig3_panel_c", height = "280px"),
        p(
          tags$b("Figure 3"),
          HTML(
            " Annual trends in &alpha;-Gal sIgE testing in Australia, 2014-2024. "
          ),
          "(A) Total tests performed. (B) Number of suspected MMA cases. (C) Testing positivity rate. Linear segmented regression models and SE are indicated by red dashed lines and light red areas, and grey dot-dashed line indicated the structural breakpoints identified by the linear segmented regression analysis.",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ), # End of tabPanel "Figure 3"

    # =========================================================================
    # TAB 5: Figure 4
    # =========================================================================
    tabPanel(
      "Figure 4",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 4"),
        fluidRow(
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig4_panel_a', {priority: 'event'})",
              "A. Geographic Expansion"
            ),
            plotlyOutput("fig4_panel_a", height = "250px")
          ),
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig4_panel_b', {priority: 'event'})",
              "B. Testing Intensity"
            ),
            plotlyOutput("fig4_panel_b", height = "250px")
          )
        ),
        fluidRow(
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig4_panel_c', {priority: 'event'})",
              "C. Contribution to Testing Growth"
            ),
            plotlyOutput("fig4_panel_c", height = "250px")
          ),
          column(
            6,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig4_panel_d', {priority: 'event'})",
              "D. Heterogeneity in Regional Testing"
            ),
            plotlyOutput("fig4_panel_d", height = "250px")
          )
        ),
        fluidRow(
          column(
            12,
            h4(
              class = "clickable-header",
              onclick = "Shiny.setInputValue('zoom_plot', 'fig4_panel_e', {priority: 'event'})",
              "E. Testing Volume Heatmap (Top 50 SA3 Regions)"
            ),
            plotlyOutput("fig4_panel_e", height = "400px")
          )
        ),
        p(
          tags$b("Figure 4"),
          HTML(
            " Dual expansion of &alpha;-Gal sIgE testing infrastructure in Australia, 2014-2024. "
          ),
          "(A) Geographic expansion: number of SA3 regions conducting testing; shaded area shows 95% CI. ",
          "(B) Testing intensity: mean tests per region; error bars show standard error. ",
          "(C) Relative contributions of geographic expansion and testing intensity to overall testing volume growth. ",
          "(D) Heterogeneity in regional testing intensity changes; violin plot shows probability density, boxplot indicates median and interquartile range, and individual points represent SA3 regions. ",
          "(E) Heatmap showing annual testing volume for the 50 SA3 regions with highest total test numbers from 2014-2024 in descending order.",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ), # End of tabPanel "Figure 4"

    # =========================================================================
    # TAB 6: Figure 5
    # =========================================================================
    tabPanel(
      "Figure 5",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 5"),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig5_panel_a', {priority: 'event'})",
          "A. Lorenz Curve"
        ),
        plotlyOutput("fig5_panel_a", height = "424px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig5_panel_b', {priority: 'event'})",
          "B. Pareto Chart (Top 30 SA3 Regions)"
        ),
        plotlyOutput("fig5_panel_b", height = "484px"),
        p(
          tags$b("Figure 5"),
          " Geographic concentration of suspected alpha-gal syndrome cases across Australian SA3 regions. ",
          "(A) Lorenz curve showing cumulative distribution of cases across SA3 regions (ranked by case burden). ",
          "The diagonal dashed line represents perfect equality; deviation from this line indicates concentration. ",
          "(B) Pareto chart showing the proportion of total cases accounted for by cumulative percentages of SA3 regions.",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ), # End of tabPanel "Figure 5"

    # =========================================================================
    # TAB 7: Figure 6 (Interactive Map - placeholder)
    # =========================================================================
    tabPanel(
      "Figure 6",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 6 - Interactive Map"),
        div(
          style = "height: 300px; display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #f9f9f9; border: 1px dashed #ccc; border-radius: 4px;",
          p(
            "The Interactive Map is available in the 'Interactive Map' tab.",
            style = "color: #666; font-size: 18px;"
          ),
          p(
            "Click on the 'Interactive Map' tab above to view it.",
            style = "color: #999; font-size: 14px;"
          )
        )
      )
    ), # End of tabPanel "Figure 6"

    # =========================================================================
    # TAB 8: Figure 7
    # =========================================================================
    tabPanel(
      "Figure 7",
      br(),
      div(
        style = "padding: 20px;",
        h3("Figure 7"),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig7_panel_a', {priority: 'event'})",
          "A. Distribution of Annual Rate of Change"
        ),
        plotlyOutput("fig7_panel_a", height = "280px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig7_panel_b', {priority: 'event'})",
          "B. Distribution of Annual Percentage Change"
        ),
        plotlyOutput("fig7_panel_b", height = "280px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig7_panel_c', {priority: 'event'})",
          "C. First Test vs Rate of Change"
        ),
        plotlyOutput("fig7_panel_c", height = "280px"),
        br(),
        h4(
          class = "clickable-header",
          onclick = "Shiny.setInputValue('zoom_plot', 'fig7_panel_d', {priority: 'event'})",
          "D. First vs Last α-Gal Test Levels"
        ),
        plotlyOutput("fig7_panel_d", height = "280px"),
        p(
          tags$b("Figure 7"),
          HTML(
            " Longitudinal changes in &alpha;-Gal sIgE levels in people who had &gt; 2 tests. "
          ),
          "(A) Distribution of individual people's annual rate of change of α-Gal sIgE/year from mixed-effects models. ",
          "Negative values indicate declining antibody levels over time. Dotted line indicates no change; dashed line indicates the median rate of change across the cohort. ",
          "(B) Distribution of individual people's percentage change in α-Gal sIgE over time compared to their first test. ",
          "Dotted line indicates no change; dashed line indicates the median percentage change across the cohort. ",
          "(C) Patients' first test α-Gal sIgE levels (log-transformed) vs. their individual annual rate of change. ",
          "Blue points (rate of decline < 0) indicates patients whose α-gal sIgE levels decreased over time; teal points above indicate increases. ",
          "The red line shows the linear regression fit with 95% confidence interval (shaded area). ",
          "(D) Scatter plot comparing first and last α-gal sIgE measurements (log-transformed) for each person. ",
          "Blue points below the diagonal line indicate patients whose α-gal sIgE levels decreased between first and last test; teal points above indicate increases.",
          style = "color: #333; font-size: 13px; margin-top: 10px;"
        )
      )
    ) # End of tabPanel "Figure 7"
  ) # End of tabsetPanel
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # ============================================================================
  # LAZY LOADING DATA INFRASTRUCTURE
  # ============================================================================
  # Data is loaded on-demand when first accessed by a figure/tab
  # This significantly reduces initial app load time

  # Use a regular environment for caching to avoid reactive dependency issues
  lazy_data <- new.env()

  # Lazy loading helper functions - load data only when first requested
  get_sa3_cases_csv <- function() {
    if (is.null(lazy_data$sa3_cases_csv)) {
      lazy_data$sa3_cases_csv <- read_csv(
        "public_data/sa3_cases.csv",
        show_col_types = FALSE
      )
    }
    lazy_data$sa3_cases_csv
  }

  get_fig1_data <- function() {
    if (is.null(lazy_data$fig1_all_tested)) {
      lazy_data$fig1_all_tested <- read_csv(
        "public_data/fig1_all_tested.csv",
        show_col_types = FALSE
      )
      lazy_data$fig1_positive_cases <- read_csv(
        "public_data/fig1_positive_cases.csv",
        show_col_types = FALSE
      )
    }
    list(
      all_tested = lazy_data$fig1_all_tested,
      positive_cases = lazy_data$fig1_positive_cases
    )
  }

  get_fig2_data <- function() {
    if (is.null(lazy_data$rr_sex_by_age)) {
      lazy_data$rr_sex_by_age <- read_csv(
        "public_data/rr_sex_by_age.csv",
        show_col_types = FALSE
      )
      lazy_data$rr_age_only <- read_csv(
        "public_data/rr_age_only.csv",
        show_col_types = FALSE
      )
      lazy_data$combined_rr <- read_csv(
        "public_data/combined_rr_results.csv",
        show_col_types = FALSE
      )
    }
    list(
      rr_sex_by_age = lazy_data$rr_sex_by_age,
      rr_age_only = lazy_data$rr_age_only,
      combined_rr = lazy_data$combined_rr
    )
  }

  get_fig3_data <- function() {
    if (is.null(lazy_data$fig3_annual_data)) {
      lazy_data$fig3_annual_data <- read_csv(
        "public_data/fig3_annual_data.csv",
        show_col_types = FALSE
      )
      lazy_data$fig3_fitted_tests <- tryCatch(
        read_csv("public_data/fig3_fitted_tests.csv", show_col_types = FALSE),
        error = function(e) NULL
      )
      lazy_data$fig3_fitted_pos <- tryCatch(
        read_csv("public_data/fig3_fitted_pos.csv", show_col_types = FALSE),
        error = function(e) NULL
      )
      lazy_data$fig3_fitted_PR <- tryCatch(
        read_csv("public_data/fig3_fitted_PR.csv", show_col_types = FALSE),
        error = function(e) NULL
      )
      lazy_data$fig3_breakpoints <- read_csv(
        "public_data/fig3_breakpoints.csv",
        show_col_types = FALSE
      )
    }
    list(
      annual_data = lazy_data$fig3_annual_data,
      fitted_tests = lazy_data$fig3_fitted_tests,
      fitted_pos = lazy_data$fig3_fitted_pos,
      fitted_PR = lazy_data$fig3_fitted_PR,
      breakpoints = lazy_data$fig3_breakpoints
    )
  }

  get_fig4_data <- function() {
    if (is.null(lazy_data$fig4_sa3_expansion)) {
      lazy_data$fig4_sa3_expansion <- read_csv(
        "public_data/fig4_sa3_expansion.csv",
        show_col_types = FALSE
      )
      lazy_data$fig4_decomposition <- read_csv(
        "public_data/fig4_decomposition.csv",
        show_col_types = FALSE
      )
      lazy_data$fig4_sa3_expansion_with_se <- read_csv(
        "public_data/fig4_sa3_expansion_with_se.csv",
        show_col_types = FALSE
      )
      lazy_data$fig4_sa3_intensity_change <- read_csv(
        "public_data/fig4_sa3_intensity_change.csv",
        show_col_types = FALSE
      )
      lazy_data$fig4_regional_intensity_annual <- read_csv(
        "public_data/fig4_regional_intensity_annual.csv",
        show_col_types = FALSE
      )
      lazy_data$fig4_top_regions <- read_csv(
        "public_data/fig4_top_regions.csv",
        show_col_types = FALSE
      )$SA3_code
    }
    list(
      sa3_expansion = lazy_data$fig4_sa3_expansion,
      decomposition = lazy_data$fig4_decomposition,
      sa3_expansion_with_se = lazy_data$fig4_sa3_expansion_with_se,
      sa3_intensity_change = lazy_data$fig4_sa3_intensity_change,
      regional_intensity_annual = lazy_data$fig4_regional_intensity_annual,
      top_regions = lazy_data$fig4_top_regions
    )
  }

  get_fig7_data <- function() {
    if (is.null(lazy_data$fig7_individual_slopes_pct_time)) {
      lazy_data$fig7_individual_slopes_pct_time <- read_csv(
        "public_data/fig7_individual_slopes_pct_time.csv",
        show_col_types = FALSE
      )
      lazy_data$fig7_first_last_comparison <- read_csv(
        "public_data/fig7_first_last_comparison.csv",
        show_col_types = FALSE
      )
      lazy_data$fig7_individual_effects <- read_csv(
        "public_data/fig7_individual_effects.csv",
        show_col_types = FALSE
      )
    }
    list(
      individual_slopes_pct_time = lazy_data$fig7_individual_slopes_pct_time,
      first_last_comparison = lazy_data$fig7_first_last_comparison,
      individual_effects = lazy_data$fig7_individual_effects
    )
  }

  # ============================================================================

  # Reactive value to store selected SA3 codes
  selected_sa3s <- reactiveVal(character(0))

  # Output to track if there's a selection (for conditionalPanel)
  output$has_selection <- reactive({
    length(selected_sa3s()) > 0
  })
  outputOptions(output, "has_selection", suspendWhenHidden = FALSE)

  #############################################
  # MODAL ZOOM FUNCTIONALITY
  #############################################

  # Reactive value to track which plot is being zoomed
  zoomed_plot_id <- reactiveVal(NULL)

  # Plot titles mapping
  plot_titles <- list(
    fig1_panel_a = "Figure 1A: Age Distribution - All Tested",
    fig1_panel_b = "Figure 1B: Age Distribution - Suspected MMA Cases",
    fig2_panel_a = "Figure 2A: Risk Ratios - Male vs Female",
    fig2_panel_b = "Figure 2B: Risk Ratios - Age vs 25-34 ref",
    fig2_panel_c = "Figure 2C: Risk Ratios - Age-Sex vs Female 25-34",
    fig3_panel_a = "Figure 3A: Annual Trends - Total Tests",
    fig3_panel_b = "Figure 3B: Annual Trends - Suspected MMA Cases",
    fig3_panel_c = "Figure 3C: Annual Trends - Positivity Rate",
    fig4_panel_a = "Figure 4A: Geographic Expansion",
    fig4_panel_b = "Figure 4B: Testing Intensity",
    fig4_panel_c = "Figure 4C: Contribution to Testing Growth",
    fig4_panel_d = "Figure 4D: Heterogeneity in Regional Testing",
    fig4_panel_e = "Figure 4E: Testing Volume Heatmap",
    fig5_panel_a = "Figure 5A: Lorenz Curve",
    fig5_panel_b = "Figure 5B: Pareto Chart",
    fig7_panel_a = "Figure 7A: Distribution of Annual Rate of Change",
    fig7_panel_b = "Figure 7B: Distribution of Annual Percentage Change",
    fig7_panel_c = "Figure 7C: First Test vs Rate of Change",
    fig7_panel_d = "Figure 7D: First vs Last α-Gal Test Levels"
  )

  # Observer to open modal when a plot header is clicked
  observeEvent(input$zoom_plot, {
    zoomed_plot_id(input$zoom_plot)
    runjs(
      "document.getElementById('plot_modal_overlay').style.display = 'block';"
    )
  })

  # Observer to close modal
  observeEvent(input$close_modal, {
    zoomed_plot_id(NULL)
    runjs(
      "document.getElementById('plot_modal_overlay').style.display = 'none';"
    )
  })

  # Render modal title
  output$modal_title <- renderText({
    plot_id <- zoomed_plot_id()
    if (is.null(plot_id)) {
      return("")
    }
    plot_titles[[plot_id]] %||% plot_id
  })

  # Render the zoomed plot dynamically based on which plot was clicked
  output$zoomed_plot <- renderPlotly({
    plot_id <- zoomed_plot_id()
    if (is.null(plot_id)) {
      return(NULL)
    }

    # Define colors used across figures
    MALE_COL <- "#00A9CE"
    FEMALE_COL <- "#E4002B"

    # Age category order for Figure 2
    age_order <- c(
      "0-14",
      "15-24",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65-74",
      "75-84",
      "85+"
    )

    # Figure 3 colors
    TOTAL_TESTS_COL <- "#999999"
    POS_TESTS_COL <- "grey10"
    RATE_COL <- "grey10"
    TREND_COL <- "red"

    # Switch based on plot_id
    switch(
      plot_id,

      # Figure 1 Panel A: All Tested
      "fig1_panel_a" = {
        fig1_data <- get_fig1_data()
        plot_a <- ggplot(
          fig1_data$all_tested,
          aes(x = Age_cat, y = count, fill = Sex)
        ) +
          geom_col(
            alpha = 0.7,
            position = position_dodge(width = 0.85),
            width = 0.8
          ) +
          labs(x = "Age Category", y = "Count", fill = "Sex") +
          scale_fill_manual(
            values = c("M" = MALE_COL, "F" = FEMALE_COL),
            labels = c("M" = "Male", "F" = "Female")
          ) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, 1800)) +
          theme_minimal() +
          theme(
            legend.position = "right",
            axis.title = element_text(face = "bold")
          )
        ggplotly(plot_a, tooltip = c("x", "y", "fill")) %>%
          layout(
            legend = list(
              orientation = "v",
              x = 1.02,
              xanchor = "left",
              y = 0.5,
              yanchor = "middle"
            ),
            margin = list(l = 60, r = 80, b = 60, t = 20)
          )
      },

      # Figure 1 Panel B: Suspected MMA Cases
      "fig1_panel_b" = {
        fig1_data <- get_fig1_data()
        plot_b <- ggplot(
          fig1_data$positive_cases,
          aes(x = Age_cat, y = count, fill = Sex)
        ) +
          geom_col(
            alpha = 0.7,
            position = position_dodge(width = 0.85),
            width = 0.8
          ) +
          labs(x = "Age Category", y = "Count", fill = "Sex") +
          scale_fill_manual(
            values = c("M" = MALE_COL, "F" = FEMALE_COL),
            labels = c("M" = "Male", "F" = "Female")
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          theme_minimal() +
          theme(
            legend.position = "right",
            axis.title = element_text(face = "bold")
          )
        ggplotly(plot_b, tooltip = c("x", "y", "fill")) %>%
          layout(
            legend = list(
              orientation = "v",
              x = 1.02,
              xanchor = "left",
              y = 0.5,
              yanchor = "middle"
            ),
            margin = list(l = 60, r = 80, b = 60, t = 20)
          )
      },

      # Figure 2 Panel A: Male vs Female
      "fig2_panel_a" = {
        fig2_data <- get_fig2_data()
        rr_local <- fig2_data$rr_sex_by_age %>%
          mutate(
            Age_cat = factor(Age_cat, levels = age_order),
            significant = ifelse(
              P_value < 0.05,
              "Significant",
              "Not Significant"
            )
          )
        plot_a <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
          geom_vline(
            xintercept = 1,
            linetype = "dashed",
            color = "red",
            linewidth = 0.8
          ) +
          geom_point(aes(shape = significant), color = "grey20", size = 3) +
          geom_errorbarh(
            aes(xmin = Lower_CI, xmax = Upper_CI),
            color = "grey20",
            height = 0.3,
            linewidth = 0.6
          ) +
          scale_shape_manual(
            values = c("Significant" = 16, "Not Significant" = 1)
          ) +
          scale_y_discrete(limits = rev) +
          labs(x = "Risk Ratio", y = "Age Category") +
          theme_minimal() +
          theme(legend.position = "none")
        ggplotly(plot_a, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 2 Panel B: Age vs 25-34 ref
      "fig2_panel_b" = {
        fig2_data <- get_fig2_data()
        age_only_order <- c(
          "0-14",
          "15-24",
          "35-44",
          "45-54",
          "55-64",
          "65-74",
          "75-84",
          "85+"
        )
        rr_local <- fig2_data$rr_age_only %>%
          mutate(
            Age_cat = factor(Age_cat, levels = age_only_order),
            significant = ifelse(
              P_value < 0.05,
              "Significant",
              "Not Significant"
            )
          )
        plot_b <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
          geom_vline(
            xintercept = 1,
            linetype = "dashed",
            color = "red",
            linewidth = 0.8
          ) +
          geom_point(aes(shape = significant), color = "grey20", size = 3) +
          geom_errorbarh(
            aes(xmin = Lower_CI, xmax = Upper_CI),
            color = "grey20",
            height = 0.3,
            linewidth = 0.6
          ) +
          scale_shape_manual(
            values = c("Significant" = 16, "Not Significant" = 1)
          ) +
          scale_y_discrete(limits = rev) +
          labs(x = "Risk Ratio", y = "Age Category") +
          theme_minimal() +
          theme(legend.position = "none")
        ggplotly(plot_b, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 2 Panel C: Age-Sex vs Female 25-34
      "fig2_panel_c" = {
        fig2_data <- get_fig2_data()
        plot_c_data <- fig2_data$combined_rr %>%
          mutate(
            group_label = paste0(Age_cat, " ", Sex),
            significant = ifelse(
              P_value < 0.05,
              "Significant",
              "Not Significant"
            ),
            Age_cat = factor(Age_cat, levels = age_order),
            Sex = factor(Sex, levels = c("M", "F")),
            Sex_label = case_when(
              Sex == "M" ~ "Male",
              Sex == "F" ~ "Female",
              TRUE ~ as.character(Sex)
            )
          ) %>%
          arrange(Age_cat, Sex) %>%
          mutate(
            group_label = factor(group_label, levels = unique(group_label))
          )
        plot_c <- ggplot(plot_c_data, aes(x = RR, y = group_label)) +
          geom_vline(
            xintercept = 1,
            linetype = "dashed",
            color = "red",
            linewidth = 0.8
          ) +
          geom_point(aes(color = Sex_label, shape = significant), size = 3) +
          geom_errorbarh(
            aes(xmin = Lower_CI, xmax = Upper_CI, color = Sex_label),
            height = 0.3,
            linewidth = 0.6
          ) +
          scale_color_manual(
            name = "Sex",
            values = c("Male" = MALE_COL, "Female" = FEMALE_COL)
          ) +
          scale_shape_manual(
            values = c("Significant" = 16, "Not Significant" = 1)
          ) +
          scale_y_discrete(limits = rev) +
          labs(x = "Risk Ratio", y = "Age-Sex Category") +
          theme_minimal() +
          theme(legend.position = "right") +
          guides(shape = "none")
        ggplotly(plot_c, tooltip = c("x", "y", "color")) %>%
          layout(
            legend = list(
              orientation = "v",
              x = 1.02,
              xanchor = "left",
              y = 0.5,
              yanchor = "middle"
            ),
            margin = list(l = 80, r = 80, b = 60, t = 20)
          )
      },

      # Figure 3 Panel A: Total Tests
      "fig3_panel_a" = {
        fig3_data <- get_fig3_data()
        plot_a <- ggplot(fig3_data$annual_data, aes(x = Year)) +
          geom_col(
            aes(y = total_tests),
            alpha = 0.8,
            fill = TOTAL_TESTS_COL,
            width = 0.9
          )
        if (!is.null(fig3_data$fitted_tests)) {
          plot_a <- plot_a +
            geom_ribbon(
              data = fig3_data$fitted_tests,
              aes(y = fitted, ymin = lower, ymax = upper),
              fill = TREND_COL,
              alpha = 0.15
            ) +
            geom_line(
              data = fig3_data$fitted_tests,
              aes(y = fitted),
              color = TREND_COL,
              linewidth = 0.5,
              linetype = "dashed"
            ) +
            geom_vline(
              xintercept = fig3_data$breakpoints$breakpoint_tests,
              linetype = "twodash",
              color = "black",
              alpha = 0.5,
              linewidth = 0.8
            )
        }
        plot_a <- plot_a +
          scale_x_continuous(breaks = seq(2014, 2024, 1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          labs(x = NULL, y = "Total number of α-gal sIgE tests") +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank()
          )
        ggplotly(plot_a, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 80, r = 40, b = 40, t = 20)
          )
      },

      # Figure 3 Panel B: Suspected MMA Cases
      "fig3_panel_b" = {
        fig3_data <- get_fig3_data()
        plot_b <- ggplot(fig3_data$annual_data, aes(x = Year)) +
          geom_line(
            aes(y = positive_tests),
            color = POS_TESTS_COL,
            linewidth = 0.6
          ) +
          geom_point(
            aes(y = positive_tests),
            color = POS_TESTS_COL,
            size = 3,
            shape = 16
          )
        if (!is.null(fig3_data$fitted_pos)) {
          plot_b <- plot_b +
            geom_ribbon(
              data = fig3_data$fitted_pos,
              aes(y = fitted, ymin = lower, ymax = upper),
              fill = TREND_COL,
              alpha = 0.15
            ) +
            geom_line(
              data = fig3_data$fitted_pos,
              aes(y = fitted),
              color = TREND_COL,
              linewidth = 0.5,
              linetype = "dashed"
            ) +
            geom_vline(
              xintercept = fig3_data$breakpoints$breakpoint_pos,
              linetype = "twodash",
              color = "black",
              alpha = 0.5,
              linewidth = 0.8
            )
        }
        plot_b <- plot_b +
          scale_x_continuous(breaks = seq(2014, 2024, 1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          labs(x = NULL, y = "Number of suspected MMA cases") +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank()
          )
        ggplotly(plot_b, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 80, r = 40, b = 40, t = 20)
          )
      },

      # Figure 3 Panel C: Positivity Rate
      "fig3_panel_c" = {
        fig3_data <- get_fig3_data()
        plot_c <- ggplot(fig3_data$annual_data, aes(x = Year)) +
          geom_line(aes(y = pos_prop), color = RATE_COL, linewidth = 0.6) +
          geom_point(
            aes(y = pos_prop),
            color = RATE_COL,
            size = 3.5,
            shape = 18
          )
        if (!is.null(fig3_data$fitted_PR)) {
          plot_c <- plot_c +
            geom_ribbon(
              data = fig3_data$fitted_PR,
              aes(y = fitted, ymin = lower, ymax = upper),
              fill = TREND_COL,
              alpha = 0.15
            ) +
            geom_line(
              data = fig3_data$fitted_PR,
              aes(y = fitted),
              color = TREND_COL,
              linewidth = 0.5,
              linetype = "dashed"
            ) +
            geom_vline(
              xintercept = 2019,
              linetype = "twodash",
              color = "black",
              alpha = 0.5,
              linewidth = 0.8
            )
        }
        plot_c <- plot_c +
          scale_x_continuous(breaks = seq(2014, 2024, 1)) +
          scale_y_continuous(
            expand = c(0, 0),
            limits = c(0, NA),
            labels = function(x) paste0(round(x, 1), "%")
          ) +
          labs(x = "Year", y = "Positivity rate (%)") +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank()
          )
        ggplotly(plot_c, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 80, r = 40, b = 40, t = 20)
          )
      },

      # Figure 4 Panel A: Geographic Expansion
      "fig4_panel_a" = {
        fig4_data <- get_fig4_data()
        geo_model <- lm(n_sa3_regions ~ YOT, data = fig4_data$sa3_expansion)
        geo_predictions <- data.frame(
          YOT = fig4_data$sa3_expansion$YOT,
          fit = predict(geo_model, se.fit = TRUE)$fit,
          se = predict(geo_model, se.fit = TRUE)$se.fit
        ) %>%
          mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
        plot_a <- ggplot(
          fig4_data$sa3_expansion,
          aes(x = YOT, y = n_sa3_regions)
        ) +
          geom_ribbon(
            data = geo_predictions,
            aes(y = fit, ymin = lower, ymax = upper),
            fill = "grey70",
            alpha = 0.4
          ) +
          geom_line(color = "black", linewidth = 0.8) +
          geom_point(color = "black", size = 3) +
          scale_x_continuous(breaks = seq(2014, 2024, 2)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          labs(x = "Year", y = "Number of SA3 regions") +
          theme_classic() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "grey60"),
            axis.ticks = element_line(colour = "grey60")
          )
        ggplotly(plot_a, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 40, t = 20)
          )
      },

      # Figure 4 Panel B: Testing Intensity
      "fig4_panel_b" = {
        fig4_data <- get_fig4_data()
        plot_b <- ggplot(
          fig4_data$sa3_expansion_with_se,
          aes(x = YOT, y = mean_tests_per_region)
        ) +
          geom_col(fill = "grey60", alpha = 0.7, width = 0.9) +
          geom_errorbar(
            aes(
              ymin = mean_tests_per_region - se_tests,
              ymax = mean_tests_per_region + se_tests
            ),
            width = 0.3,
            linewidth = 0.4,
            colour = "black"
          ) +
          scale_x_continuous(breaks = seq(2014, 2024, 2)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          labs(x = "Year", y = "Mean tests per region (± SE)") +
          theme_classic() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "grey60"),
            axis.ticks = element_line(colour = "grey60")
          )
        ggplotly(plot_b, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 40, t = 20)
          )
      },

      # Figure 4 Panel C: Contribution to Testing Growth
      "fig4_panel_c" = {
        fig4_data <- get_fig4_data()
        contrib_data <- fig4_data$decomposition %>%
          dplyr::select(YOT, geographic_effect, intensity_effect) %>%
          pivot_longer(-YOT, names_to = "component", values_to = "percent")
        plot_c <- ggplot(
          contrib_data,
          aes(x = YOT, y = percent, fill = component)
        ) +
          geom_col(position = "dodge", alpha = 0.7, width = 0.9) +
          scale_fill_manual(
            values = c(
              "geographic_effect" = "grey60",
              "intensity_effect" = "black"
            ),
            labels = c(
              "geographic_effect" = "Geographic Expansion",
              "intensity_effect" = "Testing Intensity"
            )
          ) +
          scale_x_continuous(breaks = seq(2014, 2024, 2)) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.05)),
            labels = function(x) paste0(x, "%")
          ) +
          labs(x = "Year", y = "Contribution to testing growth", fill = NULL) +
          theme_classic() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "grey60"),
            axis.ticks = element_line(colour = "grey60"),
            legend.position = "bottom"
          )
        ggplotly(plot_c, tooltip = c("x", "y", "fill")) %>%
          layout(
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15
            ),
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 4 Panel D: Heterogeneity in Regional Testing
      "fig4_panel_d" = {
        fig4_data <- get_fig4_data()
        plot_d <- ggplot(
          fig4_data$sa3_intensity_change,
          aes(x = "", y = intensity_pct_change)
        ) +
          geom_violin(fill = "grey70", alpha = 0.5) +
          geom_boxplot(
            width = 0.2,
            fill = "white",
            alpha = 1,
            linewidth = 0.5,
            colour = "black"
          ) +
          geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
          labs(x = NULL, y = "Testing Intensity Change (%)") +
          theme_classic() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.line = element_line(colour = "grey60"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(colour = "grey60")
          )
        ggplotly(plot_d, tooltip = c("y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 20, t = 20)
          )
      },

      # Figure 4 Panel E: Testing Volume Heatmap
      "fig4_panel_e" = {
        fig4_data <- get_fig4_data()
        heatmap_data <- fig4_data$regional_intensity_annual %>%
          filter(SA3_code %in% fig4_data$top_regions)
        region_order <- heatmap_data %>%
          group_by(SA3_code) %>%
          summarise(total = sum(n_tests), .groups = "drop") %>%
          arrange(desc(total)) %>%
          pull(SA3_code)
        heatmap_data <- heatmap_data %>%
          mutate(SA3_code = factor(SA3_code, levels = rev(region_order)))
        plot_e <- ggplot(
          heatmap_data,
          aes(x = YOT, y = SA3_code, fill = n_tests)
        ) +
          geom_tile(color = "white", linewidth = 0.1) +
          scale_fill_viridis_c(
            option = "C",
            trans = "log10",
            breaks = c(1, 5, 10, 50, 100, 500),
            labels = c("1", "5", "10", "50", "100", "500")
          ) +
          scale_x_continuous(
            breaks = 2014:2024,
            limits = c(2013.5, 2024.5),
            expand = c(0, 0)
          ) +
          labs(x = "Year", y = "SA3 Regions", fill = "Tests\n(log scale)") +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank(),
            axis.title.y = element_text(face = "bold")
          )
        ggplotly(plot_e, tooltip = c("x", "y", "fill")) %>%
          layout(margin = list(l = 60, r = 80, b = 60, t = 20))
      },

      # Figure 5 Panel A: Lorenz Curve
      "fig5_panel_a" = {
        sa3_cases_csv <- get_sa3_cases_csv()
        case_concentration_analysis <- sa3_cases_csv %>%
          filter(SA_name != "Canberra East", SA_name != "Norfolk Island") %>%
          filter(!is.na(c1MPPY)) %>%
          arrange(desc(c1MPPY)) %>%
          mutate(
            rank = row_number(),
            cumulative_cases = cumsum(c1MPPY),
            total_cases = sum(c1MPPY, na.rm = TRUE),
            cumulative_percentage = (cumulative_cases / total_cases) * 100,
            regions_percentage = (rank / n()) * 100
          )
        plot_a <- ggplot(
          case_concentration_analysis,
          aes(x = regions_percentage, y = cumulative_percentage)
        ) +
          geom_line(color = "grey60", linewidth = 1) +
          geom_point(color = "black", size = 2, alpha = 0.5) +
          geom_abline(
            intercept = 0,
            slope = 1,
            linetype = "dashed",
            color = "red"
          ) +
          scale_x_continuous(
            breaks = seq(0, 100, 20),
            limits = c(0, 100),
            labels = function(x) paste0(x, "%")
          ) +
          scale_y_continuous(
            breaks = seq(0, 100, 25),
            limits = c(0, 100),
            labels = function(x) paste0(x, "%")
          ) +
          labs(
            x = "Percentage of SA3 regions (ranked by cases per 1M PPY)",
            y = "Cumulative % of total cases per 1M PPY"
          ) +
          theme_classic() +
          theme(
            panel.grid.major = element_line(colour = "grey70", linewidth = 0.1),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "grey60"),
            axis.ticks = element_line(colour = "grey60")
          )
        ggplotly(plot_a, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 5 Panel B: Pareto Chart
      "fig5_panel_b" = {
        sa3_cases_csv <- get_sa3_cases_csv()
        case_concentration_analysis <- sa3_cases_csv %>%
          filter(SA_name != "Canberra East", SA_name != "Norfolk Island") %>%
          filter(!is.na(c1MPPY)) %>%
          arrange(desc(c1MPPY)) %>%
          mutate(
            rank = row_number(),
            cumulative_cases = cumsum(c1MPPY),
            total_cases = sum(c1MPPY, na.rm = TRUE),
            cumulative_percentage = (cumulative_cases / total_cases) * 100
          )
        top_30 <- head(case_concentration_analysis, 30)
        max_c1mppy <- max(top_30$c1MPPY, na.rm = TRUE)
        plot_b <- ggplot(top_30, aes(x = reorder(SA_name, -c1MPPY))) +
          geom_col(aes(y = c1MPPY), fill = "#999999", alpha = 0.7) +
          geom_line(
            aes(y = cumulative_percentage * max_c1mppy / 100, group = 1),
            color = "black",
            linewidth = 0.8
          ) +
          geom_point(
            aes(y = cumulative_percentage * max_c1mppy / 100),
            color = "black",
            size = 2
          ) +
          scale_y_continuous(
            name = "Cases per 1M PPY",
            sec.axis = sec_axis(
              trans = ~ . * 100 / max_c1mppy,
              name = "Cumulative % of total cases"
            ),
            expand = c(0, 0)
          ) +
          labs(x = "SA3 regions ranked by cases per 1M PPY") +
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(
              colour = "grey70",
              linewidth = 0.1
            )
          )
        ggplotly(plot_b, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 60, b = 120, t = 20)
          )
      },

      # Figure 7 Panel A: Distribution of Individual Slopes
      "fig7_panel_a" = {
        fig7_data <- get_fig7_data()
        median_slope <- median(
          fig7_data$individual_slopes_pct_time$individual_slope
        )
        plot_a <- ggplot(
          fig7_data$individual_slopes_pct_time,
          aes(x = individual_slope)
        ) +
          geom_histogram(
            bins = 30,
            fill = "grey70",
            alpha = 0.7,
            color = "black"
          ) +
          geom_vline(
            xintercept = 0,
            linetype = "dotted",
            color = "black",
            linewidth = 1
          ) +
          geom_vline(
            xintercept = median_slope,
            linetype = "dashed",
            color = "black",
            linewidth = 1
          ) +
          labs(
            x = "Annual rate of change (β, log α-Gal sIgE / year)",
            y = "Number of people"
          ) +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank()
          )
        ggplotly(plot_a, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 7 Panel B: Distribution of Annual Percentage Change
      "fig7_panel_b" = {
        fig7_data <- get_fig7_data()
        median_pct <- median(
          fig7_data$individual_slopes_pct_time$annual_pct_change
        )
        plot_b <- ggplot(
          fig7_data$individual_slopes_pct_time,
          aes(x = annual_pct_change)
        ) +
          geom_histogram(
            bins = 30,
            fill = "grey70",
            alpha = 0.7,
            color = "black"
          ) +
          geom_vline(
            xintercept = 0,
            linetype = "dotted",
            color = "black",
            linewidth = 1
          ) +
          geom_vline(
            xintercept = median_pct,
            linetype = "dashed",
            color = "black",
            linewidth = 1
          ) +
          scale_x_continuous(labels = function(x) paste0(x, "%")) +
          labs(
            x = "Annual percentage change in α-Gal sIgE",
            y = "Number of people"
          ) +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            panel.grid.minor = element_blank()
          )
        ggplotly(plot_b, tooltip = c("x", "y")) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 7 Panel C: First Test vs Rate of Change
      "fig7_panel_c" = {
        fig7_data <- get_fig7_data()
        individual_effects <- fig7_data$individual_effects %>%
          mutate(
            Trajectory = ifelse(actual_slope < 0, "Decreasing", "Increasing")
          )
        plot_c <- ggplot(
          individual_effects,
          aes(x = baseline_log_aGal, y = actual_slope)
        ) +
          geom_hline(
            yintercept = 0,
            linetype = "dashed",
            color = "grey40",
            linewidth = 0.8
          ) +
          geom_point(aes(color = Trajectory), alpha = 0.6, size = 2) +
          geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
          geom_vline(
            xintercept = log(0.1 + 1),
            linetype = "dotted",
            color = "black"
          ) +
          scale_color_manual(
            values = c("Increasing" = MidnightBlue, "Decreasing" = MiddayBlue),
            name = "Trajectory"
          ) +
          labs(
            x = "First test log(α-gal sIgE)",
            y = "Rate of decline (log units/year)"
          ) +
          theme_minimal() +
          theme(
            panel.grid.major.x = element_line(
              colour = "grey60",
              linewidth = 0.1
            ),
            legend.position = c(0.99, 0.99),
            legend.justification = c("right", "top"),
            legend.background = element_rect(fill = "white", colour = "grey50")
          ) +
          guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
        ggplotly(plot_c, tooltip = c("x", "y", "color")) %>%
          layout(
            legend = list(
              orientation = "v",
              x = 0.99,
              xanchor = "right",
              y = 0.99,
              yanchor = "top",
              bgcolor = "rgba(255,255,255,0.9)"
            ),
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      },

      # Figure 7 Panel D: First vs Last α-Gal Test Levels
      "fig7_panel_d" = {
        fig7_data <- get_fig7_data()
        first_last_comparison <- fig7_data$first_last_comparison %>%
          mutate(Change = ifelse(decreased, "Decreased", "Increased"))
        plot_d <- ggplot(
          first_last_comparison,
          aes(x = first_log_aGal, y = last_log_aGal)
        ) +
          geom_abline(
            intercept = 0,
            slope = 1,
            linetype = "dashed",
            color = "grey40",
            linewidth = 0.8
          ) +
          geom_point(aes(color = Change), alpha = 0.6, size = 2) +
          scale_color_manual(
            values = c("Increased" = MidnightBlue, "Decreased" = MiddayBlue),
            name = "Change"
          ) +
          labs(
            x = "First test log(α-gal sIgE)",
            y = "Last test log(α-gal sIgE)"
          ) +
          theme_minimal() +
          theme(
            panel.grid.major = element_line(colour = "grey60", linewidth = 0.1),
            legend.position = c(0.99, 0.01),
            legend.justification = c("right", "bottom"),
            legend.background = element_rect(fill = "white", colour = "grey50")
          ) +
          guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
        ggplotly(plot_d, tooltip = c("x", "y", "color")) %>%
          layout(
            legend = list(
              orientation = "v",
              x = 0.99,
              xanchor = "right",
              y = 0.01,
              yanchor = "bottom",
              bgcolor = "rgba(255,255,255,0.9)"
            ),
            margin = list(l = 60, r = 20, b = 60, t = 20)
          )
      }
    ) # End of switch
  })

  # IMPORTANT: Allow the zoomed plot to render even when hidden
  outputOptions(output, "zoomed_plot", suspendWhenHidden = FALSE)

  #############################################
  # MAP MODAL ZOOM FUNCTIONALITY
  #############################################

  # Observer to open map modal when zoom button is clicked
  observeEvent(input$zoom_map, {
    # Show the modal first
    runjs(
      "document.getElementById('map_modal_overlay').style.display = 'block';"
    )

    # Force Leaflet to recalculate map size after modal is visible
    # Use a small delay to ensure the container has proper dimensions
    shinyjs::delay(200, {
      runjs(
        "
        setTimeout(function() {
          // Find the Leaflet map and invalidate its size
          try {
            var mapWidget = HTMLWidgets.find('#map_zoomed');
            if (mapWidget) {
              var leafletMap = mapWidget.getMap();
              if (leafletMap) {
                leafletMap.invalidateSize(true);
                console.log('Map size invalidated successfully');
              }
            }
          } catch(e) {
            console.error('Error invalidating map size:', e);
          }
        }, 50);
      "
      )
    })
  })

  # Observer to close map modal
  observeEvent(input$close_map_modal, {
    runjs(
      "document.getElementById('map_modal_overlay').style.display = 'none';"
    )
  })

  # Render the zoomed map (full screen version)
  output$map_zoomed <- renderLeaflet({
    leaflet(sa3_sf) %>%
      # Add base map tiles
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%

      # Add layer controls for base maps
      addLayersControl(
        baseGroups = c("Light", "Dark", "OpenStreetMap"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%

      # Set initial view (Australia)
      setView(lng = 133.7751, lat = -25.2744, zoom = 4) %>%

      # Create a custom pane for the holocyclus line with higher z-index
      addMapPane("holocyclus_pane", zIndex = 450) %>%

      # Add SA3 polygons with layerId for click tracking
      addPolygons(
        fillColor = ~ pal(cppCat),
        fillOpacity = 0.7,
        color = ~ ifelse(SA_code %in% selected_sa3s(), "red", "#333333"),
        weight = ~ ifelse(SA_code %in% selected_sa3s(), 2, 1),
        opacity = 1,
        layerId = ~SA_code,
        label = ~ paste0(
          SA_name,
          "<br>Cases per 1M PPY: ",
          round(c1MPPY, 1),
          "<br>Total Cases: ",
          ct24pos
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%

      # Add Ixodes holocyclus distribution boundary line to custom pane
      addPolylines(
        data = holocyclus_line,
        color = "red",
        weight = 2.5,
        opacity = 0.9,
        dashArray = "10, 5",
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "12px"
        ),
        label = "Ixodes holocyclus western distribution limit",
        group = "I. holocyclus distribution",
        options = pathOptions(pane = "holocyclus_pane")
      ) %>%

      # Add legend for case categories
      addLegend(
        position = "bottomright",
        colors = color_mapping_blue[category_order],
        labels = category_order,
        title = "Cases per 1M person-years",
        opacity = 0.9
      ) %>%

      # Add custom legend entry for I. holocyclus distribution line
      addControl(
        html = paste0(
          '<div style="background: white; padding: 6px 10px; ',
          'font-size: 12px; line-height: 18px;">',
          '<span style="display: inline-block; width: 30px; height: 0; ',
          'border-top: 2.5px dashed red; vertical-align: middle; ',
          'margin-right: 8px;"></span>',
          'Approximate western edge<br>of I. holocyclus range<br>',
          '</div>'
        ),
        position = "bottomleft"
      )
  })

  # Allow the zoomed map to render even when hidden
  outputOptions(output, "map_zoomed", suspendWhenHidden = FALSE)

  #############################################
  # FIGURE NAVIGATION
  #############################################

  # Reactive value to track current figure

  #############################################
  # INTERACTIVE MAP - Render initial map
  #############################################

  output$map <- renderLeaflet({
    leaflet(sa3_sf) %>%
      # Add base map tiles
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%

      # Add layer controls for base maps
      addLayersControl(
        baseGroups = c("Light", "Dark", "OpenStreetMap"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%

      # Set initial view (Australia)
      setView(lng = 133.7751, lat = -25.2744, zoom = 4) %>%

      # Create a custom pane for the holocyclus line with higher z-index
      # This ensures the line always appears on top of SA3 polygons
      addMapPane("holocyclus_pane", zIndex = 450) %>%

      # Add SA3 polygons with layerId for click tracking
      addPolygons(
        fillColor = ~ pal(cppCat),
        fillOpacity = 0.7,
        color = "#333333",
        weight = 0.5,
        opacity = 1,
        layerId = ~SA_code,
        label = ~ paste0(
          SA_name,
          "<br>Cases per 1M PPY: ",
          round(c1MPPY, 1),
          "<br>Total Cases: ",
          ct24pos
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%

      # Add Ixodes holocyclus distribution boundary line to custom pane
      # The custom pane ensures it always appears on top of SA3 polygons
      addPolylines(
        data = holocyclus_line,
        color = "red",
        weight = 2.5,
        opacity = 0.9,
        dashArray = "10, 5",
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "12px"
        ),
        label = "Ixodes holocyclus western distribution limit",
        group = "I. holocyclus distribution",
        options = pathOptions(pane = "holocyclus_pane")
      ) %>%

      # Add legend for case categories
      addLegend(
        position = "bottomright",
        colors = color_mapping_blue[category_order],
        labels = category_order,
        title = "Cases per 1M person-years",
        opacity = 0.9
      ) %>%

      # Add custom legend entry for I. holocyclus distribution line
      addControl(
        html = paste0(
          '<div style="background: white; padding: 6px 10px; ',
          'font-size: 12px; line-height: 18px;">',
          '<span style="display: inline-block; width: 30px; height: 0; ',
          'border-top: 2.5px dashed red; vertical-align: middle; ',
          'margin-right: 8px;"></span>',
          'Approximate western edge<br>of I. holocyclus range<br>',
          '</div>'
        ),
        position = "bottomleft"
      )
  })

  #############################################
  # INTERACTIVE MAP - Handle polygon clicks (OPTIMIZED)
  #############################################

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click)) {
      return()
    }

    clicked_sa3 <- click$id
    current_selection <- selected_sa3s()

    # Determine if we're selecting or deselecting
    is_deselecting <- clicked_sa3 %in% current_selection

    if (is_deselecting) {
      # Remove from selection
      new_selection <- setdiff(current_selection, clicked_sa3)
    } else {
      # Add to selection
      new_selection <- c(current_selection, clicked_sa3)
    }

    selected_sa3s(new_selection)

    # OPTIMIZED: Only update the clicked polygon, not the entire map
    clicked_data <- sa3_sf[sa3_sf$SA_code == clicked_sa3, ]

    proxy <- leafletProxy("map")
    proxy %>%
      removeShape(layerId = clicked_sa3) %>%
      addPolygons(
        data = clicked_data,
        layerId = ~SA_code,
        fillColor = ~ pal(cppCat),
        fillOpacity = 0.7,
        color = if (is_deselecting) "#333333" else "red",
        weight = if (is_deselecting) 1 else 2,
        opacity = 1,
        label = ~ paste0(
          SA_name,
          "<br>Cases per 1M PPY: ",
          round(c1MPPY, 1),
          "<br>Total Cases: ",
          ct24pos
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  })

  #############################################
  # INTERACTIVE MAP - Handle region dropdown (OPTIMIZED)
  #############################################

  observeEvent(input$select_region, {
    region_code <- input$select_region

    if (region_code == "") {
      return()
    }

    # Get current selection before changing
    current_selection <- selected_sa3s()

    if (region_code == "ALL") {
      # Select all SA3 regions
      new_selection <- as.character(sa3_sf$SA_code)
    } else {
      # Select SA3 regions in the chosen state
      state_sa3s <- sa3_sf %>%
        filter(substr(SA_code, 1, 1) == region_code) %>%
        pull(SA_code)
      new_selection <- as.character(state_sa3s)
    }

    # OPTIMIZED: Find codes that need to be deselected and selected
    codes_to_deselect <- setdiff(current_selection, new_selection)
    codes_to_add <- setdiff(new_selection, current_selection)

    # Update selected SA3s
    selected_sa3s(new_selection)

    proxy <- leafletProxy("map")

    # Update polygons that need to be deselected (turn gray)
    if (length(codes_to_deselect) > 0) {
      deselect_data <- sa3_sf[sa3_sf$SA_code %in% codes_to_deselect, ]
      proxy %>%
        removeShape(layerId = codes_to_deselect) %>%
        addPolygons(
          data = deselect_data,
          layerId = ~SA_code,
          fillColor = ~ pal(cppCat),
          fillOpacity = 0.7,
          color = "#333333",
          weight = 1,
          opacity = 1,
          label = ~ paste0(
            SA_name,
            "<br>Cases per 1M PPY: ",
            round(c1MPPY, 1),
            "<br>Total Cases: ",
            ct24pos
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    }

    # Update polygons that need to be selected (turn red)
    if (length(codes_to_add) > 0) {
      select_data <- sa3_sf[sa3_sf$SA_code %in% codes_to_add, ]
      proxy %>%
        removeShape(layerId = codes_to_add) %>%
        addPolygons(
          data = select_data,
          layerId = ~SA_code,
          fillColor = ~ pal(cppCat),
          fillOpacity = 0.7,
          color = "red",
          weight = 2,
          opacity = 1,
          label = ~ paste0(
            SA_name,
            "<br>Cases per 1M PPY: ",
            round(c1MPPY, 1),
            "<br>Total Cases: ",
            ct24pos
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    }

    # Reset dropdown
    updateSelectInput(session, "select_region", selected = "")
  })

  #############################################
  # INTERACTIVE MAP - Clear selection button (OPTIMIZED)
  #############################################

  observeEvent(input$clear_selection, {
    current_selection <- selected_sa3s()

    # Only update if there are selections to clear
    if (length(current_selection) > 0) {
      # Reset selected SA3s
      selected_sa3s(character(0))

      # OPTIMIZED: Only update the previously selected polygons (turn them gray)
      deselect_data <- sa3_sf[sa3_sf$SA_code %in% current_selection, ]

      proxy <- leafletProxy("map")
      proxy %>%
        removeShape(layerId = current_selection) %>%
        addPolygons(
          data = deselect_data,
          layerId = ~SA_code,
          fillColor = ~ pal(cppCat),
          fillOpacity = 0.7,
          color = "#333333",
          weight = 1,
          opacity = 1,
          label = ~ paste0(
            SA_name,
            "<br>Cases per 1M PPY: ",
            round(c1MPPY, 1),
            "<br>Total Cases: ",
            ct24pos
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    }
  })

  #############################################
  # INTERACTIVE MAP - Selection count
  #############################################

  output$selection_count <- renderText({
    n <- length(selected_sa3s())
    if (n == 0) {
      "No regions selected"
    } else {
      paste(n, "region(s) selected")
    }
  })

  #############################################
  # INTERACTIVE MAP - Selected regions table
  #############################################

  output$selected_table <- renderDT({
    selected_codes <- selected_sa3s()

    if (length(selected_codes) == 0) {
      return(
        datatable(
          data.frame(
            Message = "Click on SA3 regions to select them"
          ),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        )
      )
    }

    selected_data <- sa3_sf %>%
      st_drop_geometry() %>%
      filter(SA_code %in% selected_codes) %>%
      dplyr::select(SA_name, c1MPPY, ct24pos) %>%
      arrange(desc(c1MPPY))

    colnames(selected_data) <- c(
      "SA3 Name",
      "Cases/1M PPY",
      "Total Cases"
    )

    datatable(
      selected_data,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollY = "550px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound("Cases/1M PPY", digits = 1)
  })

  #############################################
  # INTERACTIVE MAP - Tests/Cases over time plot
  #############################################

  output$tests_cases_plot <- renderPlotly({
    selected_codes <- selected_sa3s()

    if (length(selected_codes) == 0) {
      # Show message when no selection
      p <- ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Select SA3 regions to see tests and cases over time",
          size = 6,
          color = "grey50"
        ) +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)
      return(ggplotly(p) %>% layout(showlegend = FALSE))
    }

    # Filter data for selected regions
    selected_data <- sa3_sf %>%
      st_drop_geometry() %>%
      filter(SA_code %in% selected_codes)

    # Prepare time series data
    years <- 2014:2024
    year_suffixes <- sprintf("%02d", years %% 100)

    # Calculate totals for each year
    time_data <- data.frame(Year = years)
    time_data$Tests <- sapply(year_suffixes, function(y) {
      col <- paste0("t", y)
      if (col %in% names(selected_data)) {
        sum(selected_data[[col]], na.rm = TRUE)
      } else {
        NA
      }
    })
    time_data$Cases <- sapply(year_suffixes, function(y) {
      col <- paste0("c", y)
      if (col %in% names(selected_data)) {
        sum(selected_data[[col]], na.rm = TRUE)
      } else {
        NA
      }
    })

    # Pivot for plotting
    plot_data <- time_data %>%
      pivot_longer(
        cols = c("Tests", "Cases"),
        names_to = "Type",
        values_to = "Count"
      )

    # Create plot
    p <- ggplot(
      plot_data,
      aes(x = Year, y = Count, color = Type, group = Type)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_color_manual(
        values = c("Tests" = MiddayBlue, "Cases" = MidnightBlue)
      ) +
      scale_x_continuous(breaks = years) +
      labs(x = "Year", y = "Count", color = NULL) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
      )
  })

  #############################################
  # FIGURE 1: Age Distribution
  #############################################

  # Define colors for figures
  MALE_COL <- "#00A9CE"
  FEMALE_COL <- "#E4002B"

  # Figure 1 Panel A: All Tested
  output$fig1_panel_a <- renderPlotly({
    fig1_data <- get_fig1_data()
    plot_a <- ggplot(
      fig1_data$all_tested,
      aes(x = Age_cat, y = count, fill = Sex)
    ) +
      geom_col(
        alpha = 0.7,
        position = position_dodge(width = 0.85),
        width = 0.8
      ) +
      labs(x = "Age Category", y = "Count", fill = "Sex") +
      scale_fill_manual(
        values = c("M" = MALE_COL, "F" = FEMALE_COL),
        labels = c("M" = "Male", "F" = "Female")
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1800)) +
      theme_minimal() +
      theme(legend.position = "right", axis.title = element_text(face = "bold"))
    ggplotly(plot_a, tooltip = c("x", "y", "fill")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle"
        ),
        margin = list(l = 60, r = 80, b = 60, t = 20)
      )
  })

  # Figure 1 Panel B: Suspected MMA Cases
  output$fig1_panel_b <- renderPlotly({
    fig1_data <- get_fig1_data()
    plot_b <- ggplot(
      fig1_data$positive_cases,
      aes(x = Age_cat, y = count, fill = Sex)
    ) +
      geom_col(
        alpha = 0.7,
        position = position_dodge(width = 0.85),
        width = 0.8
      ) +
      labs(x = "Age Category", y = "Count", fill = "Sex") +
      scale_fill_manual(
        values = c("M" = MALE_COL, "F" = FEMALE_COL),
        labels = c("M" = "Male", "F" = "Female")
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal() +
      theme(legend.position = "right", axis.title = element_text(face = "bold"))
    ggplotly(plot_b, tooltip = c("x", "y", "fill")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle"
        ),
        margin = list(l = 60, r = 80, b = 60, t = 20)
      )
  })

  #############################################
  # FIGURE 2: Risk Ratios
  #############################################

  age_order <- c(
    "0-14",
    "15-24",
    "25-34",
    "35-44",
    "45-54",
    "55-64",
    "65-74",
    "75-84",
    "85+"
  )

  # Figure 2 Panel A: Male vs Female
  output$fig2_panel_a <- renderPlotly({
    fig2_data <- get_fig2_data()
    rr_local <- fig2_data$rr_sex_by_age %>%
      mutate(
        Age_cat = factor(Age_cat, levels = age_order),
        significant = ifelse(P_value < 0.05, "Significant", "Not Significant")
      )
    plot_a <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8
      ) +
      geom_point(aes(shape = significant), color = "grey20", size = 3) +
      geom_errorbarh(
        aes(xmin = Lower_CI, xmax = Upper_CI),
        color = "grey20",
        height = 0.3,
        linewidth = 0.6
      ) +
      scale_shape_manual(
        values = c("Significant" = 16, "Not Significant" = 1)
      ) +
      scale_y_discrete(limits = rev) +
      labs(x = "Risk Ratio", y = "Age Category") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 60, t = 20))
  })

  # Figure 2 Panel B: Age vs 25-34 ref
  output$fig2_panel_b <- renderPlotly({
    fig2_data <- get_fig2_data()
    age_only_order <- c(
      "0-14",
      "15-24",
      "35-44",
      "45-54",
      "55-64",
      "65-74",
      "75-84",
      "85+"
    )
    rr_local <- fig2_data$rr_age_only %>%
      mutate(
        Age_cat = factor(Age_cat, levels = age_only_order),
        significant = ifelse(P_value < 0.05, "Significant", "Not Significant")
      )
    plot_b <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8
      ) +
      geom_point(aes(shape = significant), color = "grey20", size = 3) +
      geom_errorbarh(
        aes(xmin = Lower_CI, xmax = Upper_CI),
        color = "grey20",
        height = 0.3,
        linewidth = 0.6
      ) +
      scale_shape_manual(
        values = c("Significant" = 16, "Not Significant" = 1)
      ) +
      scale_y_discrete(limits = rev) +
      labs(x = "Risk Ratio", y = "Age Category") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 60, t = 20))
  })

  # Figure 2 Panel C: Age-Sex vs Female 25-34
  output$fig2_panel_c <- renderPlotly({
    fig2_data <- get_fig2_data()
    plot_c_data <- fig2_data$combined_rr %>%
      mutate(
        group_label = paste0(Age_cat, " ", Sex),
        significant = ifelse(P_value < 0.05, "Significant", "Not Significant"),
        Age_cat = factor(Age_cat, levels = age_order),
        Sex = factor(Sex, levels = c("M", "F")),
        Sex_label = case_when(
          Sex == "M" ~ "Male",
          Sex == "F" ~ "Female",
          TRUE ~ as.character(Sex)
        )
      ) %>%
      arrange(Age_cat, Sex) %>%
      mutate(group_label = factor(group_label, levels = unique(group_label)))
    plot_c <- ggplot(plot_c_data, aes(x = RR, y = group_label)) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8
      ) +
      geom_point(aes(color = Sex_label, shape = significant), size = 3) +
      geom_errorbarh(
        aes(xmin = Lower_CI, xmax = Upper_CI, color = Sex_label),
        height = 0.3,
        linewidth = 0.6
      ) +
      scale_color_manual(
        name = "Sex",
        values = c("Male" = MALE_COL, "Female" = FEMALE_COL)
      ) +
      scale_shape_manual(
        values = c("Significant" = 16, "Not Significant" = 1)
      ) +
      scale_y_discrete(limits = rev) +
      labs(x = "Risk Ratio", y = "Age-Sex Category") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(shape = "none")
    ggplotly(plot_c, tooltip = c("x", "y", "color")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle"
        ),
        margin = list(l = 80, r = 80, b = 60, t = 20)
      )
  })

  #############################################
  # FIGURE 3: Annual Testing Trends
  #############################################

  TOTAL_TESTS_COL <- "#999999"
  POS_TESTS_COL <- "grey10"
  RATE_COL <- "grey10"
  TREND_COL <- "red"

  # Figure 3 Panel A: Total Tests
  output$fig3_panel_a <- renderPlotly({
    fig3_data <- get_fig3_data()
    plot_a <- ggplot(fig3_data$annual_data, aes(x = Year)) +
      geom_col(
        aes(y = total_tests),
        alpha = 0.8,
        fill = TOTAL_TESTS_COL,
        width = 0.9
      )
    if (!is.null(fig3_data$fitted_tests)) {
      plot_a <- plot_a +
        geom_ribbon(
          data = fig3_data$fitted_tests,
          aes(y = fitted, ymin = lower, ymax = upper),
          fill = TREND_COL,
          alpha = 0.15
        ) +
        geom_line(
          data = fig3_data$fitted_tests,
          aes(y = fitted),
          color = TREND_COL,
          linewidth = 0.5,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = fig3_data$breakpoints$breakpoint_tests,
          linetype = "twodash",
          color = "black",
          alpha = 0.5,
          linewidth = 0.8
        )
    }
    plot_a <- plot_a +
      scale_x_continuous(breaks = seq(2014, 2024, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      labs(x = NULL, y = "Total number of α-gal sIgE tests") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank()
      )
    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 80, r = 40, b = 40, t = 20))
  })

  # Figure 3 Panel B: Suspected MMA Cases
  output$fig3_panel_b <- renderPlotly({
    fig3_data <- get_fig3_data()
    plot_b <- ggplot(fig3_data$annual_data, aes(x = Year)) +
      geom_line(
        aes(y = positive_tests),
        color = POS_TESTS_COL,
        linewidth = 0.6
      ) +
      geom_point(
        aes(y = positive_tests),
        color = POS_TESTS_COL,
        size = 3,
        shape = 16
      )
    if (!is.null(fig3_data$fitted_pos)) {
      plot_b <- plot_b +
        geom_ribbon(
          data = fig3_data$fitted_pos,
          aes(y = fitted, ymin = lower, ymax = upper),
          fill = TREND_COL,
          alpha = 0.15
        ) +
        geom_line(
          data = fig3_data$fitted_pos,
          aes(y = fitted),
          color = TREND_COL,
          linewidth = 0.5,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = fig3_data$breakpoints$breakpoint_pos,
          linetype = "twodash",
          color = "black",
          alpha = 0.5,
          linewidth = 0.8
        )
    }
    plot_b <- plot_b +
      scale_x_continuous(breaks = seq(2014, 2024, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      labs(x = NULL, y = "Number of suspected MMA cases") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank()
      )
    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 80, r = 40, b = 40, t = 20))
  })

  # Figure 3 Panel C: Positivity Rate
  output$fig3_panel_c <- renderPlotly({
    fig3_data <- get_fig3_data()
    plot_c <- ggplot(fig3_data$annual_data, aes(x = Year)) +
      geom_line(aes(y = pos_prop), color = RATE_COL, linewidth = 0.6) +
      geom_point(aes(y = pos_prop), color = RATE_COL, size = 3.5, shape = 18)
    if (!is.null(fig3_data$fitted_PR)) {
      plot_c <- plot_c +
        geom_ribbon(
          data = fig3_data$fitted_PR,
          aes(y = fitted, ymin = lower, ymax = upper),
          fill = TREND_COL,
          alpha = 0.15
        ) +
        geom_line(
          data = fig3_data$fitted_PR,
          aes(y = fitted),
          color = TREND_COL,
          linewidth = 0.5,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = 2019,
          linetype = "twodash",
          color = "black",
          alpha = 0.5,
          linewidth = 0.8
        )
    }
    plot_c <- plot_c +
      scale_x_continuous(breaks = seq(2014, 2024, 1)) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, NA),
        labels = function(x) paste0(round(x, 1), "%")
      ) +
      labs(x = "Year", y = "Positivity rate (%)") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank()
      )
    ggplotly(plot_c, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 80, r = 40, b = 40, t = 20))
  })

  #############################################
  # FIGURE 4: Testing Infrastructure Expansion
  #############################################

  # Figure 4 Panel A: Geographic Expansion
  output$fig4_panel_a <- renderPlotly({
    fig4_data <- get_fig4_data()
    geo_model <- lm(n_sa3_regions ~ YOT, data = fig4_data$sa3_expansion)
    geo_predictions <- data.frame(
      YOT = fig4_data$sa3_expansion$YOT,
      fit = predict(geo_model, se.fit = TRUE)$fit,
      se = predict(geo_model, se.fit = TRUE)$se.fit
    ) %>%
      mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
    plot_a <- ggplot(fig4_data$sa3_expansion, aes(x = YOT, y = n_sa3_regions)) +
      geom_ribbon(
        data = geo_predictions,
        aes(y = fit, ymin = lower, ymax = upper),
        fill = "grey70",
        alpha = 0.4
      ) +
      geom_line(color = "black", linewidth = 0.8) +
      geom_point(color = "black", size = 3) +
      scale_x_continuous(breaks = seq(2014, 2024, 2)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Year", y = "Number of SA3 regions") +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.ticks = element_line(colour = "grey60")
      )
    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 40, t = 20))
  })

  # Figure 4 Panel B: Testing Intensity
  output$fig4_panel_b <- renderPlotly({
    fig4_data <- get_fig4_data()
    plot_b <- ggplot(
      fig4_data$sa3_expansion_with_se,
      aes(x = YOT, y = mean_tests_per_region)
    ) +
      geom_col(fill = "grey60", alpha = 0.7, width = 0.9) +
      geom_errorbar(
        aes(
          ymin = mean_tests_per_region - se_tests,
          ymax = mean_tests_per_region + se_tests
        ),
        width = 0.3,
        linewidth = 0.4,
        colour = "black"
      ) +
      scale_x_continuous(breaks = seq(2014, 2024, 2)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Year", y = "Mean tests per region (± SE)") +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.ticks = element_line(colour = "grey60")
      )
    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 40, t = 20))
  })

  # Figure 4 Panel C: Contribution to Testing Growth
  output$fig4_panel_c <- renderPlotly({
    fig4_data <- get_fig4_data()
    contrib_data <- fig4_data$decomposition %>%
      dplyr::select(YOT, geographic_effect, intensity_effect) %>%
      pivot_longer(-YOT, names_to = "component", values_to = "percent")
    plot_c <- ggplot(
      contrib_data,
      aes(x = YOT, y = percent, fill = component)
    ) +
      geom_col(position = "dodge", alpha = 0.7, width = 0.9) +
      scale_fill_manual(
        values = c(
          "geographic_effect" = "grey60",
          "intensity_effect" = "black"
        ),
        labels = c(
          "geographic_effect" = "Geographic Expansion",
          "intensity_effect" = "Testing Intensity"
        )
      ) +
      scale_x_continuous(breaks = seq(2014, 2024, 2)) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = function(x) paste0(x, "%")
      ) +
      labs(x = "Year", y = "Contribution to testing growth", fill = NULL) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.ticks = element_line(colour = "grey60"),
        legend.position = "bottom"
      )
    ggplotly(plot_c, tooltip = c("x", "y", "fill")) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15
        ),
        margin = list(l = 60, r = 20, b = 60, t = 20)
      )
  })

  # Figure 4 Panel D: Heterogeneity in Regional Testing
  output$fig4_panel_d <- renderPlotly({
    fig4_data <- get_fig4_data()
    plot_d <- ggplot(
      fig4_data$sa3_intensity_change,
      aes(x = "", y = intensity_pct_change)
    ) +
      geom_violin(fill = "grey70", alpha = 0.5) +
      geom_boxplot(
        width = 0.2,
        fill = "white",
        alpha = 1,
        linewidth = 0.5,
        colour = "black"
      ) +
      geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
      labs(x = NULL, y = "Testing Intensity Change (%)") +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "grey60")
      )
    ggplotly(plot_d, tooltip = c("y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 20, t = 20))
  })

  # Figure 4 Panel E: Testing Volume Heatmap
  output$fig4_panel_e <- renderPlotly({
    fig4_data <- get_fig4_data()
    heatmap_data <- fig4_data$regional_intensity_annual %>%
      filter(SA3_code %in% fig4_data$top_regions)
    region_order <- heatmap_data %>%
      group_by(SA3_code) %>%
      summarise(total = sum(n_tests), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(SA3_code)
    heatmap_data <- heatmap_data %>%
      mutate(SA3_code = factor(SA3_code, levels = rev(region_order)))
    plot_e <- ggplot(heatmap_data, aes(x = YOT, y = SA3_code, fill = n_tests)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(
        option = "C",
        trans = "log10",
        breaks = c(1, 5, 10, 50, 100, 500),
        labels = c("1", "5", "10", "50", "100", "500")
      ) +
      scale_x_continuous(
        breaks = 2014:2024,
        limits = c(2013.5, 2024.5),
        expand = c(0, 0)
      ) +
      labs(x = "Year", y = "SA3 Regions", fill = "Tests\n(log scale)") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.title.y = element_text(face = "bold")
      )
    ggplotly(plot_e, tooltip = c("x", "y", "fill")) %>%
      layout(margin = list(l = 60, r = 80, b = 60, t = 20))
  })

  #############################################
  # FIGURE 5: Geographic Concentration
  #############################################

  # Figure 5 Panel A: Lorenz Curve
  output$fig5_panel_a <- renderPlotly({
    sa3_cases_csv <- get_sa3_cases_csv()
    case_concentration_analysis <- sa3_cases_csv %>%
      filter(SA_name != "Canberra East", SA_name != "Norfolk Island") %>%
      filter(!is.na(c1MPPY)) %>%
      arrange(desc(c1MPPY)) %>%
      mutate(
        rank = row_number(),
        cumulative_cases = cumsum(c1MPPY),
        total_cases = sum(c1MPPY, na.rm = TRUE),
        cumulative_percentage = (cumulative_cases / total_cases) * 100,
        regions_percentage = (rank / n()) * 100
      )
    plot_a <- ggplot(
      case_concentration_analysis,
      aes(x = regions_percentage, y = cumulative_percentage)
    ) +
      geom_line(color = "grey60", linewidth = 1) +
      geom_point(color = "black", size = 2, alpha = 0.5) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "red"
      ) +
      scale_x_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, 25),
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        x = "Percentage of SA3 regions (ranked by cases per 1M PPY)",
        y = "Cumulative % of total cases per 1M PPY"
      ) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(colour = "grey70", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.ticks = element_line(colour = "grey60")
      )
    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 60, t = 20))
  })

  # Figure 5 Panel B: Pareto Chart
  output$fig5_panel_b <- renderPlotly({
    sa3_cases_csv <- get_sa3_cases_csv()
    case_concentration_analysis <- sa3_cases_csv %>%
      filter(SA_name != "Canberra East", SA_name != "Norfolk Island") %>%
      filter(!is.na(c1MPPY)) %>%
      arrange(desc(c1MPPY)) %>%
      mutate(
        rank = row_number(),
        cumulative_cases = cumsum(c1MPPY),
        total_cases = sum(c1MPPY, na.rm = TRUE),
        cumulative_percentage = (cumulative_cases / total_cases) * 100
      )
    top_30 <- head(case_concentration_analysis, 30)
    max_c1mppy <- max(top_30$c1MPPY, na.rm = TRUE)
    plot_b <- ggplot(top_30, aes(x = reorder(SA_name, -c1MPPY))) +
      geom_col(aes(y = c1MPPY), fill = "#999999", alpha = 0.7) +
      geom_line(
        aes(y = cumulative_percentage * max_c1mppy / 100, group = 1),
        color = "black",
        linewidth = 0.8
      ) +
      geom_point(
        aes(y = cumulative_percentage * max_c1mppy / 100),
        color = "black",
        size = 2
      ) +
      scale_y_continuous(
        name = "Cases per 1M PPY",
        sec.axis = sec_axis(
          trans = ~ . * 100 / max_c1mppy,
          name = "Cumulative % of total cases"
        ),
        expand = c(0, 0)
      ) +
      labs(x = "SA3 regions ranked by cases per 1M PPY") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", linewidth = 0.1)
      )
    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 60, b = 120, t = 20))
  })

  #############################################
  # FIGURE 7: Longitudinal Changes
  #############################################

  MidnightBlue <- "#00313C"
  MiddayBlue <- "#00A9CE"

  # Figure 7 Panel A: Distribution of Individual Slopes
  output$fig7_panel_a <- renderPlotly({
    fig7_data <- get_fig7_data()
    median_slope <- median(
      fig7_data$individual_slopes_pct_time$individual_slope
    )
    plot_a <- ggplot(
      fig7_data$individual_slopes_pct_time,
      aes(x = individual_slope)
    ) +
      geom_histogram(bins = 30, fill = "grey70", alpha = 0.7, color = "black") +
      geom_vline(
        xintercept = 0,
        linetype = "dotted",
        color = "black",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = median_slope,
        linetype = "dashed",
        color = "black",
        linewidth = 1
      ) +
      labs(
        x = "Annual rate of change (β, log α-Gal sIgE / year)",
        y = "Number of people"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank()
      )
    ggplotly(plot_a, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 60, t = 20))
  })

  # Figure 7 Panel B: Distribution of Annual Percentage Change
  output$fig7_panel_b <- renderPlotly({
    fig7_data <- get_fig7_data()
    median_pct <- median(fig7_data$individual_slopes_pct_time$annual_pct_change)
    plot_b <- ggplot(
      fig7_data$individual_slopes_pct_time,
      aes(x = annual_pct_change)
    ) +
      geom_histogram(bins = 30, fill = "grey70", alpha = 0.7, color = "black") +
      geom_vline(
        xintercept = 0,
        linetype = "dotted",
        color = "black",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = median_pct,
        linetype = "dashed",
        color = "black",
        linewidth = 1
      ) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        x = "Annual percentage change in α-Gal sIgE",
        y = "Number of people"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.1),
        panel.grid.minor = element_blank()
      )
    ggplotly(plot_b, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 20, b = 60, t = 20))
  })

  # Figure 7 Panel C: First Test vs Rate of Change
  output$fig7_panel_c <- renderPlotly({
    fig7_data <- get_fig7_data()
    individual_effects <- fig7_data$individual_effects %>%
      mutate(Trajectory = ifelse(actual_slope < 0, "Decreasing", "Increasing"))
    plot_c <- ggplot(
      individual_effects,
      aes(x = baseline_log_aGal, y = actual_slope)
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "grey40",
        linewidth = 0.8
      ) +
      geom_point(aes(color = Trajectory), alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
      geom_vline(
        xintercept = log(0.1 + 1),
        linetype = "dotted",
        color = "black"
      ) +
      scale_color_manual(
        values = c("Increasing" = MidnightBlue, "Decreasing" = MiddayBlue),
        name = "Trajectory"
      ) +
      labs(
        x = "First test log(α-gal sIgE)",
        y = "Rate of decline (log units/year)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_line(colour = "grey60", linewidth = 0.1),
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", colour = "grey50")
      ) +
      guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
    ggplotly(plot_c, tooltip = c("x", "y", "color")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 0.99,
          xanchor = "right",
          y = 0.99,
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.9)"
        ),
        margin = list(l = 60, r = 20, b = 60, t = 20)
      )
  })

  # Figure 7 Panel D: First vs Last α-Gal Test Levels
  output$fig7_panel_d <- renderPlotly({
    fig7_data <- get_fig7_data()
    first_last_comparison <- fig7_data$first_last_comparison %>%
      mutate(Change = ifelse(decreased, "Decreased", "Increased"))
    plot_d <- ggplot(
      first_last_comparison,
      aes(x = first_log_aGal, y = last_log_aGal)
    ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "grey40",
        linewidth = 0.8
      ) +
      geom_point(aes(color = Change), alpha = 0.6, size = 2) +
      scale_color_manual(
        values = c("Increased" = MidnightBlue, "Decreased" = MiddayBlue),
        name = "Change"
      ) +
      labs(x = "First test log(α-gal sIgE)", y = "Last test log(α-gal sIgE)") +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(colour = "grey60", linewidth = 0.1),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = "white", colour = "grey50")
      ) +
      guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
    ggplotly(plot_d, tooltip = c("x", "y", "color")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 0.99,
          xanchor = "right",
          y = 0.01,
          yanchor = "bottom",
          bgcolor = "rgba(255,255,255,0.9)"
        ),
        margin = list(l = 60, r = 20, b = 60, t = 20)
      )
  })
} # End of server

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
