# ============================================================================
# ui.R - Shiny User Interface for mixchar GUI
# ============================================================================
#
# Purpose:
#   Defines the user interface layout and styling for the mixchar Shiny workbench.
#   Uses a sidebar layout with workflow step navigation and conditional panels
#   for step-specific controls.
#
# Structure:
#   - Sidebar (width 3): Workflow step selector + conditional input panels
#   - Main panel (width 9): Dynamic content rendered by server.R (output$step_body)
#
# Workflow Steps:
#   1. read: Data upload/example selection
#   2. process: Column mapping and processing parameters
#   3. decon: Deconvolution parameters (peaks, seed)
#   4. viz: Visualization options (bw, quantile bands)
#   5. fractions: Carbon fraction parameters and display options
#
# UI Components:
#   - conditionalPanel: Shows/hides inputs based on selected step
#   - uiOutput("step_body"): Main content area (rendered server-side)
#   - uiOutput("datafile_ui"): Dynamic file input (allows reset)
#
# Styling:
#   - Uses bslib for Bootstrap 5 theming
#   - Custom CSS for sidebar gradient and main panel styling
#   - Google Fonts: Barlow (base), Space Grotesk (headings)
#
# Modification Notes:
#   - To add a new workflow step: add to radioButtons choices and create new conditionalPanel
#   - Input IDs must match server.R input$<id> references
#   - Use conditionalPanel("input.step == '<step_name>'") for step-specific UI
#
# ============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)

# Application theme configuration
# Customizes Bootstrap 5 appearance with specific colors and fonts
app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Barlow"),
  heading_font = font_google("Space Grotesk"),
  primary = "#0f766e",
  secondary = "#f97316",
  bg = "#f4f6fb",
  fg = "#0f172a"
)

# ============================================================================
# Main UI Definition
# ============================================================================

ui <- fluidPage(
  theme = app_theme,
  # Custom CSS for sidebar gradient and main panel styling
  tags$head(tags$style(
    "
    .sidebarPanel { background: linear-gradient(180deg,#0f172a 0%,#0f766e 100%); color:#f8fafc; }
    .main-panel { background:#f8fafc; border-radius:12px; padding:18px; position:relative; }
    .shiny-input-container { margin-bottom:12px; }
    "
  )),
  titlePanel(h1("mixchar Shiny workbench", class = "mb-0")),
  sidebarLayout(
    # ------------------------------------------------------------------------
    # Sidebar: Workflow Navigation and Step-Specific Controls
    # ------------------------------------------------------------------------
    sidebarPanel(
      width = 3,
      h4("Workflow steps"),
      # Step selector: Controls which step is active and which conditionalPanel shows
      radioButtons(
        "step", label = NULL,
        choices = c(
        "1. Load data" = "read",
        "2. Process data" = "process",
        "3. Pyrolysis phase deconvolution" = "decon",
        "4. Pyrolysis phase deconvolution plots" = "viz",
        "5. Carbon fractions" = "fractions"
      ),
      selected = "read"
    ),
    hr(),
      # ----------------------------------------------------------------------
      # Step 1: Data Loading
      # ----------------------------------------------------------------------
      conditionalPanel(
        "input.step == 'read'",
        uiOutput("datafile_ui"),
        div(
          class = "mb-2",
          tags$label("Rows to skip (metadata)", `for` = "skip_rows", class = "form-label mb-1"),
          div(
            class = "d-flex align-items-center gap-2",
            numericInput(
              "skip_rows", label = NULL,
              value = 0, min = 0, step = 1, width = "160px"
            ),
            actionButton("apply_skip", "Apply", class = "btn-secondary btn-sm")
          )
        ),
        actionButton("use_example", "Use example data", class = "btn-secondary")
      ),
      # ----------------------------------------------------------------------
      # Step 2: Data Processing
      # ----------------------------------------------------------------------
      conditionalPanel(
        "input.step == 'process'",
        helpText("Example data fills defaults; provide values for your own data."),
        selectInput("temp_col", "Temperature column", choices = c("Select column" = "")),
        selectInput(
          "temp_units",
          "Temperature units",
          choices = c("Celsius (C)" = "C", "Kelvin (K)" = "K"),
          selected = "C"
        ),
        selectInput("mass_col", "Mass loss column", choices = c("Select column" = "")),
        selectInput("time_col", "Time column", choices = c("Select column" = "")),
        selectInput("stage_col", "Stage column (optional, for plotting)", choices = c("Select column" = "")),
        numericInput("init_mass", "Initial mass (mg)", value = NA, min = 0, step = 0.01),
        numericInput("pyro_start", "Pyrolysis start time", value = NA, step = 0.5),
        numericInput("pyro_end", "Pyrolysis end time", value = NA, step = 0.5),
        actionButton("do_process", "Run process()", class = "btn-primary")
      ),
      # ----------------------------------------------------------------------
      # Step 3: Deconvolution
      # ----------------------------------------------------------------------
      conditionalPanel(
        "input.step == 'decon'",
        selectInput("n_peaks", "Number of peaks", choices = c("Auto" = "auto", "3 peaks" = "3", "4 peaks" = "4")),
        numericInput("decon_seed", "Seed", value = 1, min = 1, step = 1),
        actionButton("run_decon", "Run deconvolve()", class = "btn-primary")
      ),
      # ----------------------------------------------------------------------
      # Step 4: Visualization Options
      # ----------------------------------------------------------------------
      conditionalPanel(
        "input.step == 'viz'",
        checkboxInput("decon_bw", "Black/white plot", value = TRUE),
        checkboxInput("show_band", "Show quantile bands (3-peak only)", value = FALSE),
        textInput("band_probs", "Quantile probs", value = "0.025, 0.5, 0.975"),
        numericInput("band_draws", "Draws for ribbons", value = 500, min = 50, step = 50),
        numericInput("band_seed", "Ribbon seed", value = 42, min = 1, step = 1)
      ),
      # ----------------------------------------------------------------------
      # Step 5: Carbon Fractions
      # ----------------------------------------------------------------------
      conditionalPanel(
        "input.step == 'fractions'",
        checkboxInput(
          "show_sankey",
          "Show Sankey diagram (replaces table)",
          value = FALSE
        ),
        numericInput(
          "fc_hemi",
          "Hemicellulose fixed carbon (% dry basis)",
          value = 13.8, min = 0, step = 0.1
        ),
        numericInput(
          "fc_cell",
          "Cellulose fixed carbon (% dry basis)",
          value = 6.4, min = 0, step = 0.1
        ),
        numericInput(
          "fc_lig",
          "Lignin fixed carbon (% dry basis)",
          value = 34, min = 0, step = 0.1
        )
      )
    ),
    # ------------------------------------------------------------------------
    # Main Panel: Dynamic Content Area
    # ------------------------------------------------------------------------
    # Content is rendered server-side based on selected step (see server.R output$step_body)
    mainPanel(
      width = 9,
      class = "main-panel",
      uiOutput("step_body")
    )
  )
)
