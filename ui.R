library(shiny)
library(bslib)
library(DT)

app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Barlow"),
  heading_font = font_google("Space Grotesk"),
  primary = "#0f766e",
  secondary = "#f97316",
  bg = "#f4f6fb",
  fg = "#0f172a"
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(tags$style(
    "
    .sidebarPanel { background: linear-gradient(180deg,#0f172a 0%,#0f766e 100%); color:#f8fafc; }
    .main-panel { background:#f8fafc; border-radius:12px; padding:18px; }
    .shiny-input-container { margin-bottom:12px; }
    "
  )),
  titlePanel(
    div(
      class = "d-flex align-items-center justify-content-between",
      h1("mixchar Shiny workbench", class = "mb-0"),
      uiOutput("repro_script_btn_header")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Workflow steps"),
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
      conditionalPanel(
        "input.step == 'process'",
        helpText("Example data fills defaults; provide values for your own data."),
        selectInput("temp_col", "Temperature column", choices = c("Select column" = "")),
        selectInput("mass_col", "Mass loss column", choices = c("Select column" = "")),
        selectInput("time_col", "Time column", choices = c("Select column" = "")),
        selectInput("stage_col", "Stage column (optional, for plotting)", choices = c("Select column" = "")),
        numericInput("init_mass", "Initial mass (mg)", value = NA, min = 0, step = 0.01),
        numericInput("pyro_start", "Pyrolysis start time", value = NA, step = 0.5),
        numericInput("pyro_end", "Pyrolysis end time", value = NA, step = 0.5),
        actionButton("do_process", "Run process()", class = "btn-primary")
      ),
      conditionalPanel(
        "input.step == 'decon'",
        selectInput("n_peaks", "Number of peaks", choices = c("Auto" = "auto", "3 peaks" = "3", "4 peaks" = "4")),
        numericInput("decon_seed", "Seed", value = 1, min = 1, step = 1),
        actionButton("run_decon", "Run deconvolve()", class = "btn-primary")
      ),
      conditionalPanel(
        "input.step == 'viz'",
        checkboxInput("decon_bw", "Black/white plot", value = TRUE),
        checkboxInput("show_band", "Show quantile bands (3-peak only)", value = FALSE),
        textInput("band_probs", "Quantile probs", value = "0.025, 0.5, 0.975"),
        numericInput("band_draws", "Draws for ribbons", value = 500, min = 50, step = 50),
        numericInput("band_seed", "Ribbon seed", value = 42, min = 1, step = 1)
      ),
      conditionalPanel(
        "input.step == 'fractions'",
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
    mainPanel(
      width = 9,
      class = "main-panel",
      uiOutput("step_body")
    )
  )
)
