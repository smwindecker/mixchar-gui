# ============================================================================
# server.R - Shiny Server Logic for mixchar GUI
# ============================================================================
#
# Purpose:
#   This file contains the server-side logic for the mixchar Shiny workbench.
#   It handles data processing, deconvolution, visualization, and carbon
#   fraction calculations for thermogravimetric analysis.
#
# Architecture:
#   - Uses reactiveValues (rv) to manage application state
#   - Workflow: Load data -> Process -> Deconvolve -> Visualize -> Fractions
#   - Each step depends on previous step's outputs stored in rv
#
# Key Components:
#   - rv$raw: Raw uploaded/example data
#   - rv$processed: Processed data from mixchar::process()
#   - rv$decon: Deconvolution results from mixchar::deconvolve()
#   - rv$source: Tracks data source ("upload" or "example")
#
# Dependencies:
#   - mixchar package (fixed-carbon branch required)
#   - shiny, DT, plotly packages
#   - sankey.R for Sankey diagram rendering
#
# Modification Notes:
#   - When adding new workflow steps, update rv reactiveValues accordingly
#   - UI inputs are accessed via input$<id>, outputs via output$<id>
#   - Use req() to ensure dependencies before rendering outputs
#   - Use observeEvent() for user-triggered actions
#
# ============================================================================

library(shiny)
library(mixchar)
library(DT)
library(plotly)
source("sankey.R")

# Require the fixed-carbon branch of mixchar so carbon-fraction helpers are present.
# This check ensures the required functions exist before the app runs.
if (!exists("calculate_fixed_carbon_fractions", where = asNamespace("mixchar"), inherits = FALSE)) {
  stop(
    "mixchar fixed-carbon branch required. Install via remotes::install_github('smwindecker/mixchar', ref = 'fixed-carbon')."
  )
}

# ----------------------------------------------------------------------------
# Helper Functions
# ----------------------------------------------------------------------------

#' Parse probability values from text input
#'
#' Converts a comma/semicolon/whitespace-separated string of probabilities
#' into a numeric vector. Used for quantile band probabilities in deconvolution plots.
#'
#' @param txt Character string containing probability values (e.g., "0.025, 0.5, 0.975")
#' @return Numeric vector of probabilities. Defaults to c(0.025, 0.5, 0.975) if parsing fails.
parse_probs <- function(txt) {
  vals <- suppressWarnings(as.numeric(unlist(strsplit(txt, "[,;\\s]+"))))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) c(0.025, 0.5, 0.975) else vals
}

# Default parameter values for the beech example dataset.
# These are automatically populated when user clicks "Use example data".
example_defaults <- list(
  skip_rows = 40,
  init_mass = 18.96,
  pyro_start = 127,
  pyro_end = 191.5,
  temp_units = "C"
)

#' Guess column name from candidates
#'
#' Attempts to match a column name in the dataframe to one of the candidate names
#' (case-insensitive). Used for auto-selecting temperature, mass, time columns
#' when loading example data.
#'
#' @param df Data frame to search
#' @param candidates Character vector of candidate column names to match
#' @return First matching column name, or first column in df if no match
guess_col <- function(df, candidates) {
  nms <- names(df)
  hit <- nms[tolower(nms) %in% tolower(candidates)]
  if (length(hit) > 0) return(hit[1])
  nms[1]
}

# ============================================================================
# Shiny Server Function
# ============================================================================

server <- function(input, output, session) {
  # --------------------------------------------------------------------------
  # Application State Management
  # --------------------------------------------------------------------------
  # Reactive values store the application state across user interactions.
  # These values are shared across all observers and reactive expressions.
  #
  # rv$raw: Raw data from CSV upload or example file
  # rv$processed: Processed data object from mixchar::process()
  # rv$decon: Deconvolution results from mixchar::deconvolve()
  # rv$file_reset: Counter to invalidate fileInput when reset needed
  # rv$source: Data source identifier ("upload", "example", or "none")
  # rv$example_path: Path to example data file if using example
  # --------------------------------------------------------------------------
  rv <- reactiveValues(
    raw = NULL,
    processed = NULL,
    decon = NULL,
    file_reset = 0,
    source = "none",
    example_path = NULL
  )
  
  # --------------------------------------------------------------------------
  # Data Selection Helper Functions
  # --------------------------------------------------------------------------
  
  #' Get data configuration for temperature program plot
  #'
  #' Determines which dataset and columns to use for the temperature program plot.
  #' Prefers original stage-coded raw data (e.g., beech has 6 stages) over processed
  #' data, as it preserves more detailed stage information for visualization.
  #'
  #' @return List with data, time_col, temp_col, and stage_col
  temp_program_data <- function() {
    if (!is.null(rv$raw)) {
      stage_col <- if (!is.null(input$stage_col) && nzchar(input$stage_col) &&
                       input$stage_col %in% names(rv$raw)) {
        input$stage_col
      } else if ("stage" %in% names(rv$raw)) {
        "stage"
      } else {
        NULL
      }
      
      if (!is.null(stage_col)) {
        # Use raw data with user-selected or auto-detected stage column
        return(list(
          data = rv$raw,
          time_col = input$time_col,
          temp_col = input$temp_col,
          stage_col = stage_col
        ))
      }
    }
    
    # Fallback to processed data if no stage column available in raw data
    list(
      data = rv$processed$all_data,
      time_col = "time",
      temp_col = "temp_C",
      stage_col = "stage"
    )
  }
  
  # --------------------------------------------------------------------------
  # UI Input Management Functions
  # --------------------------------------------------------------------------
  
  # Dynamic file input that resets when rv$file_reset changes.
  # This allows programmatic clearing of uploaded files.
  output$datafile_ui <- renderUI({
    rv$file_reset  # invalidate when reset is requested
    fileInput("datafile", "Upload CSV", accept = c(".csv"))
  })
  
  #' Reset all column selection inputs to empty
  #'
  #' Clears the column selection dropdowns when new data is loaded.
  #' Called when user uploads a new file or resets the application.
  reset_column_inputs <- function() {
    updateSelectInput(session, "temp_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "mass_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "time_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "stage_col", choices = c("Select column" = ""), selected = "")
  }
  
  #' Update column selection inputs with dataframe column names
  #'
  #' Populates the column selection dropdowns with available columns from the
  #' loaded dataframe. Optionally pre-selects columns if provided.
  #'
  #' @param df Data frame whose column names will populate the dropdowns
  #' @param selected Named list with optional pre-selected columns (temp, mass, time, stage)
  set_column_inputs <- function(df, selected = list(temp = "", mass = "", time = "", stage = "")) {
    choices <- c("Select column" = "", names(df))
    updateSelectInput(
      session, "temp_col",
      choices = choices,
      selected = if (is.null(selected$temp)) "" else selected$temp
    )
    updateSelectInput(
      session, "mass_col",
      choices = choices,
      selected = if (is.null(selected$mass)) "" else selected$mass
    )
    updateSelectInput(
      session, "time_col",
      choices = choices,
      selected = if (is.null(selected$time)) "" else selected$time
    )
    updateSelectInput(
      session, "stage_col",
      choices = choices,
      selected = if (is.null(selected$stage)) "" else selected$stage
    )
  }
  
  #' Reset processing parameter inputs to defaults
  #'
  #' Clears processing inputs (mass, pyrolysis times) when new data is loaded.
  #' Temperature units are reset to example defaults.
  reset_processing_inputs <- function() {
    updateNumericInput(session, "init_mass", value = NA_real_)
    updateNumericInput(session, "pyro_start", value = NA_real_)
    updateNumericInput(session, "pyro_end", value = NA_real_)
    updateSelectInput(session, "temp_units", selected = example_defaults$temp_units)
  }

  # --------------------------------------------------------------------------
  # Reproducible Script Generation
  # --------------------------------------------------------------------------
  
  #' Build reproducible R script from current app state
  #'
  #' Generates a complete R script that reproduces the current analysis workflow.
  #' The script includes data loading, processing, deconvolution, plotting, and
  #' (if applicable) carbon fraction calculations.
  #'
  #' @return Character vector of R code lines
  #' @note Requires rv$processed to be set. Script includes all parameters
  #'       currently configured in the app (columns, processing params, etc.)
  build_repro_script <- function() {
    req(rv$processed)

    fmt_bool <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"
    skip_val <- if (is.na(input$skip_rows)) 0 else input$skip_rows
    data_path <- if (identical(rv$source, "upload") && !is.null(input$datafile$name)) {
      input$datafile$name
    } else if (identical(rv$source, "example") && !is.null(rv$example_path)) {
      basename(rv$example_path)
    } else {
      "path/to/your-data.csv"
    }
    stage_arg <- if (!is.null(input$stage_col) && nzchar(input$stage_col)) {
      shQuote(input$stage_col)
    } else {
      "NULL"
    }
    colour_segments <- fmt_bool(!isTRUE(input$colour_segments))

    script <- c(
      "# R script exported from the mixchar Shiny workbench",
      "library(mixchar)",
      "",
      sprintf("data_path <- %s  # adjust to your own file path", shQuote(data_path)),
      sprintf("raw <- read.csv(data_path, skip = %s, check.names = FALSE)", skip_val),
      "",
      "# Process data with the parameters used in the app",
      "processed <- process(",
      sprintf("  data = raw,"),
      sprintf("  init_mass = %s,", input$init_mass),
      sprintf("  temp = %s,", shQuote(input$temp_col)),
      sprintf("  mass_loss = %s,", shQuote(input$mass_col)),
      sprintf("  time = %s,", shQuote(input$time_col)),
      sprintf("  pyrolysis_start_time = %s,", input$pyro_start),
      sprintf("  pyrolysis_end_time = %s,", input$pyro_end),
      sprintf("  temp_units = %s", shQuote(input$temp_units)),
      ")",
      "",
      "# Summaries and plots",
      "stage_summary(processed)",
      "png('processed_plot_mass.png', width = 1200, height = 1200, res = 150)",
      sprintf("plot(processed, plot_type = 'mass', colour_segments = %s)", colour_segments),
      "dev.off()",
      "",
      "png('temp_program_plot.png', width = 1200, height = 1200, res = 150)",
      sprintf("plot_temp_program(raw, time_col = %s, temp_col = %s, stage_col = %s)",
              shQuote(input$time_col), shQuote(input$temp_col), stage_arg),
      "dev.off()"
    )

    # Add deconvolution section if deconvolution has been run
    if (!is.null(rv$decon)) {
      n_peaks_val <- if (is.null(rv$decon$n_peaks)) "NULL" else rv$decon$n_peaks
      probs <- parse_probs(input$band_probs)
      prob_line <- paste(format(probs, trim = TRUE), collapse = ", ")

      decon_lines <- c(
        "",
        "# Deconvolution",
        sprintf("decon <- deconvolve(processed, seed = %s, n_peaks = %s)", input$decon_seed, n_peaks_val),
        "weights <- decon$weights",
        "write.csv(weights, 'decon_weights.csv', row.names = FALSE)",
        "",
        sprintf("probs <- c(%s)", prob_line),
        "png('decon_plot.png', width = 1200, height = 1200, res = 150)",
        sprintf("plot(decon, bw = %s, show_quantile_band = %s, quantile_probs = probs, n_draws = %s, seed = %s)",
                fmt_bool(input$decon_bw), fmt_bool(input$show_band), input$band_draws, input$band_seed),
        "dev.off()"
      )

      # Carbon fractions are only calculated for 3-peak deconvolutions
      if (!is.null(rv$decon$n_peaks) && rv$decon$n_peaks == 3) {
        fc_lines <- c(
          "",
          "# Carbon fractions (3-peak only)",
          "moisture <- mixchar:::calculate_moisture_content(processed)",
          "ash <- mixchar:::calculate_ash_content(processed)",
          sprintf("fc <- calculate_fixed_carbon_fractions(decon, dry_basis_fixed_carbon_hemicellulose = %s, dry_basis_fixed_carbon_cellulose = %s, dry_basis_fixed_carbon_lignin = %s)",
                  input$fc_hemi, input$fc_cell, input$fc_lig),
          "totals <- mixchar:::calculate_total_fractions(decon, fc)",
          "vol_H <- if (!is.null(fc$volatile_HC_fraction)) fc$volatile_HC_fraction else fc$Hvp",
          "vol_C <- if (!is.null(fc$volatile_CL_fraction)) fc$volatile_CL_fraction else fc$Cvp",
          "vol_L <- if (!is.null(fc$volatile_LG_fraction)) fc$volatile_LG_fraction else fc$Lvp",
          "fc_H <- if (!is.null(fc$fixed_carbon_HC_fraction)) fc$fixed_carbon_HC_fraction else fc$Hfp",
          "fc_C <- if (!is.null(fc$fixed_carbon_CL_fraction)) fc$fixed_carbon_CL_fraction else fc$Cfp",
          "fc_L <- if (!is.null(fc$fixed_carbon_LG_fraction)) fc$fixed_carbon_LG_fraction else fc$Lfp",
          "fractions <- data.frame(",
          "  Component = c(",
          "    'Moisture',",
          "    'Hemicellulose volatile', 'Cellulose volatile', 'Lignin volatile',",
          "    'Hemicellulose fixed carbon', 'Cellulose fixed carbon', 'Lignin fixed carbon',",
          "    'Hemicellulose total', 'Cellulose total', 'Lignin total',",
          "    'Ash'",
          "  ),",
          "  Percent = round(c(",
          "    moisture,",
          "    vol_H, vol_C, vol_L,",
          "    fc_H, fc_C, fc_L,",
          "    totals$H_total, totals$C_total, totals$L_total,",
          "    ash",
          "  ), 2)",
          ")",
          "write.csv(fractions, 'carbon_fractions.csv', row.names = FALSE)"
        )
        decon_lines <- c(decon_lines, fc_lines)
      }

      script <- c(script, decon_lines)
    }

    script
  }
  
  #' Apply example data default parameters
  #'
  #' Populates all processing inputs with values appropriate for the beech
  #' example dataset. Also increments file_reset to clear any uploaded file.
  apply_example_inputs <- function() {
    updateNumericInput(session, "skip_rows", value = example_defaults$skip_rows)
    updateNumericInput(session, "init_mass", value = example_defaults$init_mass)
    updateNumericInput(session, "pyro_start", value = example_defaults$pyro_start)
    updateNumericInput(session, "pyro_end", value = example_defaults$pyro_end)
    updateSelectInput(session, "temp_units", selected = example_defaults$temp_units)
    rv$file_reset <- rv$file_reset + 1  # clear any previously uploaded file in the UI
  }
  
  # --------------------------------------------------------------------------
  # Data Loading Observers
  # --------------------------------------------------------------------------
  
  # Load example data and auto-configure column selections
  observeEvent(input$use_example, {
    apply_example_inputs()
    example_path <- if (file.exists("beech_example.csv")) {
      "beech_example.csv"
    } else if (file.exists(file.path("..", "beech_example.csv"))) {
      file.path("..", "beech_example.csv")
    } else {
      stop("example data not found; place it in this app folder or its parent.")
    }
    rv$source <- "example"
    rv$example_path <- example_path
    df <- read.csv(example_path, skip = example_defaults$skip_rows, check.names = FALSE)
    rv$raw <- df
    set_column_inputs(df, selected = list(
      temp = guess_col(df, c("temp", "temperature", "temp_c")),
      mass = guess_col(df, c("mass_loss", "massloss", "mass")),
      time = guess_col(df, c("time", "t", "minutes", "min")),
      stage = if ("stage" %in% names(df)) "stage" else ""
    ))
    rv$processed <- NULL
    rv$decon <- NULL
  })
  
  # Handle CSV file upload from user
  observeEvent(input$datafile, {
    req(input$datafile)
    # Reset skip rows to default (0) when user uploads their own file.
    # This prevents using example skip_rows value with user data.
    skip_val <- 0
    updateNumericInput(session, "skip_rows", value = skip_val)
    
    df <- read.csv(input$datafile$datapath, skip = skip_val, check.names = FALSE)
    rv$raw <- df
    rv$processed <- NULL
    rv$decon <- NULL
    rv$source <- "upload"
    rv$example_path <- NULL
    reset_processing_inputs()
    set_column_inputs(df)
  })
  
  # Reload data with new skip_rows value
  # This allows users to adjust metadata rows without re-uploading
  observeEvent(input$apply_skip, {
    skip_val <- if (is.na(input$skip_rows)) 0 else input$skip_rows
    
    # Determine which dataset to reload based on source (upload vs example)
    data_path <- NULL
    if (!is.null(input$datafile) && rv$source == "upload") {
      data_path <- input$datafile$datapath
    } else if (rv$source == "example" && !is.null(rv$example_path) && file.exists(rv$example_path)) {
      data_path <- rv$example_path
    }
    
    if (is.null(data_path)) {
      showNotification("No data to reload. Upload a file or use example data first.", type = "error")
      return()
    }
    
    df <- read.csv(data_path, skip = skip_val, check.names = FALSE)
    rv$raw <- df
    rv$processed <- NULL
    rv$decon <- NULL
    set_column_inputs(df, selected = list(
      temp = input$temp_col,
      mass = input$mass_col,
      time = input$time_col,
      stage = input$stage_col
    ))
  })
  
  # --------------------------------------------------------------------------
  # Data Display Outputs
  # --------------------------------------------------------------------------
  
  # Display raw data preview table
  output$raw_preview <- renderDataTable({
    req(rv$raw)
    datatable(
      rv$raw,
      options = list(
        dom = "t",
        scrollX = TRUE,
        scrollY = "60vh",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        orderClasses = TRUE
      ),
      class = "display nowrap"
    )
  })
  
  # --------------------------------------------------------------------------
  # Data Processing
  # --------------------------------------------------------------------------
  
  # Process raw data using mixchar::process()
  # This is the core data transformation step that identifies pyrolysis stages
  observeEvent(input$do_process, {
    req(rv$raw)
    temp_col <- input$temp_col
    mass_loss_col <- input$mass_col
    time_col <- input$time_col
    temp_units <- input$temp_units
    
    # Validate all required inputs are provided
    validate(
      need(!is.na(input$init_mass), "Initial mass is required."),
      need(!is.na(input$pyro_start), "Pyrolysis start time is required."),
      need(!is.na(input$pyro_end), "Pyrolysis end time is required."),
      need(temp_col != "", "Select the temperature column."),
      need(mass_loss_col != "", "Select the mass loss column."),
      need(time_col != "", "Select the time column."),
      need(!is.null(temp_units) && nzchar(temp_units), "Select the temperature units."),
      need(temp_units %in% c("C", "K"), "Temperature units must be C or K."),
      need(input$pyro_end > input$pyro_start, "Pyrolysis end must be after start."),
      need(all(c(temp_col, mass_loss_col, time_col) %in% names(rv$raw)),
           "Data must include temperature, mass loss, and time columns.")
    )
    
    # Attempt processing with error handling
    # If processing fails, show notification and reset state
    proc <- tryCatch({
      process(
        data = rv$raw,
        init_mass = input$init_mass,
        temp = temp_col,
        mass_loss = mass_loss_col,
        time = time_col,
        pyrolysis_start_time = input$pyro_start,
        pyrolysis_end_time = input$pyro_end,
        temp_units = temp_units
      )
    }, error = function(e) {
      rv$processed <- NULL
      rv$decon <- NULL
      showNotification(
        paste(
          "Processing failed. Please ensure the temperature units (C/K) match the file and the pyrolysis window is appropriate.",
          e$message
        ),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
    
    if (is.null(proc)) return()
    rv$processed <- proc
    rv$decon <- NULL  # Clear deconvolution results when new processing occurs
  })
  
  # --------------------------------------------------------------------------
  # Processing Results Display
  # --------------------------------------------------------------------------
  
  # Display stage summary table from processed data
  output$stage_summary_tbl <- renderDataTable({
    req(rv$processed)
    df <- mixchar::stage_summary(rv$processed)
    numeric_cols <- which(vapply(df, is.numeric, logical(1)))
    dt <- datatable(df, options = list(dom = "t"))
    if (length(numeric_cols)) {
      dt <- formatRound(dt, columns = numeric_cols, digits = 2)
    }
    dt
  })
  
  output$dl_stage_summary <- downloadHandler(
    filename = function() "stage_summary.csv",
    content = function(file) {
      req(rv$processed)
      df <- mixchar::stage_summary(rv$processed)
      numeric_cols <- which(vapply(df, is.numeric, logical(1)))
      if (length(numeric_cols)) {
        df[numeric_cols] <- lapply(df[numeric_cols], round, 2)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Temperature program plot showing stages over time
  # Uses temp_program_data() to select appropriate data source
  output$temp_program_plot <- renderPlot({
    req(rv$processed)
    cfg <- temp_program_data()
    plot_temp_program(cfg$data,
                      time_col = cfg$time_col,
                      temp_col = cfg$temp_col,
                      stage_col = cfg$stage_col)
  }, res = 120)
  
  output$dl_temp_program_plot <- downloadHandler(
    filename = function() "temp_program_plot.png",
    content = function(file) {
      req(rv$processed)
      png(file, width = 1200, height = 1200, res = 150)
      cfg <- temp_program_data()
      plot_temp_program(cfg$data,
                        time_col = cfg$time_col,
                        temp_col = cfg$temp_col,
                        stage_col = cfg$stage_col)
      dev.off()
    }
  )
  
  # Mass loss plot with stage boundaries
  output$process_plot <- renderPlot({
    req(rv$processed)
    plot(rv$processed,
         plot_type = "mass",
         colour_segments = !isTRUE(input$colour_segments))
  }, res = 120)
  
  output$dl_processed_plot <- downloadHandler(
    filename = function() "processed_plot_mass.png",
    content = function(file) {
      req(rv$processed)
      png(file, width = 1200, height = 1200, res = 150)
      plot(rv$processed,
           plot_type = "mass",
           colour_segments = !isTRUE(input$colour_segments))
      dev.off()
    }
  )
  
  # --------------------------------------------------------------------------
  # Deconvolution
  # --------------------------------------------------------------------------
  
  # Run deconvolution analysis on processed data
  # This separates the pyrolysis curve into component peaks (hemicellulose, cellulose, lignin)
  observeEvent(input$run_decon, {
    req(rv$processed)
    # Convert UI selection to numeric or NULL for auto-detection
    np <- switch(input$n_peaks, auto = NULL, `3` = 3, `4` = 4)
    rv$decon <- NULL  # hide outputs/download until fresh results are ready
    decon_res <- NULL
    withProgress(message = "Deconvolving...", value = 0, {
      decon_res <- tryCatch({
        deconvolve(rv$processed, seed = input$decon_seed, n_peaks = np)
      }, error = function(e) {
        rv$decon <- NULL
        showNotification(
          paste(
            "Deconvolution failed. Check temperature units, pyrolysis window, and data quality.",
            e$message
          ),
          type = "error",
          duration = 10
        )
        NULL
      })
    })
    if (is.null(decon_res)) return()
    rv$decon <- decon_res
  })
  
  # --------------------------------------------------------------------------
  # Deconvolution Results Display
  # --------------------------------------------------------------------------
  
  # Display deconvolution weights table
  output$weights_tbl <- renderDataTable({
    req(rv$decon)
    df <- rv$decon$weights
    numeric_cols <- which(vapply(df, is.numeric, logical(1)))
    dt <- datatable(df, options = list(dom = "t"))
    if (length(numeric_cols)) {
      dt <- formatRound(dt, columns = numeric_cols, digits = 2)
    }
    dt
  })
  
  output$dl_weights_tbl <- downloadHandler(
    filename = function() "decon_weights.csv",
    content = function(file) {
      req(rv$decon)
      df <- rv$decon$weights
      numeric_cols <- which(vapply(df, is.numeric, logical(1)))
      if (length(numeric_cols)) {
        df[numeric_cols] <- lapply(df[numeric_cols], round, 2)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Deconvolution plot with optional quantile bands
  output$decon_plot <- renderPlot({
    req(rv$decon)
    probs <- parse_probs(input$band_probs)
    plot(rv$decon,
         bw = input$decon_bw,
         show_quantile_band = input$show_band,
         quantile_probs = probs,
         n_draws = input$band_draws,
         seed = input$band_seed)
  }, res = 120)
  
  output$dl_decon_plot <- downloadHandler(
    filename = function() "decon_plot.png",
    content = function(file) {
      req(rv$decon)
      png(file, width = 1200, height = 1200, res = 150)
      probs <- parse_probs(input$band_probs)
      plot(rv$decon,
           bw = input$decon_bw,
           show_quantile_band = input$show_band,
           quantile_probs = probs,
           n_draws = input$band_draws,
           seed = input$band_seed)
      dev.off()
    }
  )

  # --------------------------------------------------------------------------
  # Carbon Fraction Calculations
  # --------------------------------------------------------------------------
  
  #' Calculate carbon fraction values for display
  #'
  #' Computes moisture, ash, volatile, fixed carbon, and total fractions for
  #' hemicellulose, cellulose, and lignin. Only works with 3-peak deconvolutions.
  #'
  #' @return Named list with all fraction values (moisture, ash, vol_H, vol_C, etc.)
  #' @note This is a reactive expression that recalculates when dependencies change
  fraction_values <- reactive({
    req(rv$processed, rv$decon)
    if (is.null(rv$decon$n_peaks) || rv$decon$n_peaks != 3) {
      stop("Carbon fractions currently supported for 3 peaks. Please deconvolve with 3 peaks (or Auto -> 3).")
    }

    # Helper to safely extract values, returning NA if missing
    val_or_na <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else x

    moisture <- val_or_na(mixchar:::calculate_moisture_content(rv$processed))
    ash <- val_or_na(mixchar:::calculate_ash_content(rv$processed))
    fc <- calculate_fixed_carbon_fractions(
      rv$decon,
      dry_basis_fixed_carbon_hemicellulose = input$fc_hemi,
      dry_basis_fixed_carbon_cellulose = input$fc_cell,
      dry_basis_fixed_carbon_lignin = input$fc_lig
    )
    totals <- mixchar:::calculate_total_fractions(rv$decon, fc)

    # Extract fraction values, handling both new and legacy field names
    # New names: volatile_HC_fraction, fixed_carbon_HC_fraction, etc.
    # Legacy names: Hvp, Hfp, etc. (used as fallback)
    vol_H <- val_or_na(if (!is.null(fc$volatile_HC_fraction)) fc$volatile_HC_fraction else fc$Hvp)
    vol_C <- val_or_na(if (!is.null(fc$volatile_CL_fraction)) fc$volatile_CL_fraction else fc$Cvp)
    vol_L <- val_or_na(if (!is.null(fc$volatile_LG_fraction)) fc$volatile_LG_fraction else fc$Lvp)
    fc_H <- val_or_na(if (!is.null(fc$fixed_carbon_HC_fraction)) fc$fixed_carbon_HC_fraction else fc$Hfp)
    fc_C <- val_or_na(if (!is.null(fc$fixed_carbon_CL_fraction)) fc$fixed_carbon_CL_fraction else fc$Cfp)
    fc_L <- val_or_na(if (!is.null(fc$fixed_carbon_LG_fraction)) fc$fixed_carbon_LG_fraction else fc$Lfp)

    list(
      moisture = moisture,
      ash = ash,
      vol_H = vol_H,
      vol_C = vol_C,
      vol_L = vol_L,
      fc_H = fc_H,
      fc_C = fc_C,
      fc_L = fc_L,
      total_H = val_or_na(totals$H_total),
      total_C = val_or_na(totals$C_total),
      total_L = val_or_na(totals$L_total)
    )
  })
  
  output$fractions_tbl <- renderDataTable({
    vals <- tryCatch(fraction_values(), error = function(e) {
      validate(need(FALSE, e$message))
    })

    tbl <- fraction_table(vals)
    
    datatable(
      tbl,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollY = "60vh"
      )
    )
  })
  
  output$dl_fractions_tbl <- downloadHandler(
    filename = function() "carbon_fractions.csv",
    content = function(file) {
      vals <- fraction_values()
      tbl <- fraction_table(vals)
      write.csv(tbl, file, row.names = FALSE)
    }
  )

  output$fractions_sankey <- renderPlotly({
    vals <- tryCatch(fraction_values(), error = function(e) {
      validate(need(FALSE, e$message))
    })
    render_fractions_sankey(vals)
  })

  output$dl_repro_script <- downloadHandler(
    filename = function() "mixchar_reproducible_analysis.R",
    content = function(file) {
      req(rv$processed, rv$decon)
      writeLines(build_repro_script(), con = file, useBytes = TRUE)
    }
  )
  
  # --------------------------------------------------------------------------
  # Dynamic UI Rendering
  # --------------------------------------------------------------------------
  
  #' Render main content area based on selected workflow step
  #'
  #' Dynamically generates the main panel content based on which step the user
  #' has selected. Each step shows different outputs and controls.
  #'
  #' Steps:
  #'   - read: Data upload and preview
  #'   - process: Processing results, stage summary, plots
  #'   - decon: Deconvolution weights table
  #'   - viz: Deconvolution visualization
  #'   - fractions: Carbon fractions table or Sankey diagram
  output$step_body <- renderUI({
    switch(input$step,
           read = tagList(
             h3("1. Load data"),
             p("Upload your CSV or use the built-in beech_example.csv; set skip rows if metadata is present."),
             DTOutput("raw_preview", height = "60vh")
           ),
           process = {
             base <- tagList(
               h3("2. Process data"),
               p("Run process() and inspect stage boundaries.")
             )
             if (is.null(rv$processed)) return(base)
             tagList(
               base,
               div(
                 class = "d-flex align-items-center justify-content-between mb-2",
                 h4("Stage summary"),
                 downloadButton("dl_stage_summary", "Download table", class = "btn-secondary btn-sm")
               ),
               DTOutput("stage_summary_tbl"),
               br(),
               h4("Visualize data"),
               div(
                 class = "d-flex align-items-center gap-4 mb-2",
                 {
                   temp_checked <- if (is.null(input$show_temp_plot)) TRUE else isTRUE(input$show_temp_plot)
                   checkboxInput("show_temp_plot", "Temperature program", value = temp_checked)
                 },
                 {
                   mass_checked <- if (is.null(input$show_mass_plot)) TRUE else isTRUE(input$show_mass_plot)
                   checkboxInput("show_mass_plot", "Mass loss", value = mass_checked)
                 }
               ),
               {
                 show_temp <- if (is.null(input$show_temp_plot)) TRUE else isTRUE(input$show_temp_plot)
                 show_mass <- if (is.null(input$show_mass_plot)) TRUE else isTRUE(input$show_mass_plot)
                 both <- show_temp && show_mass
                 
                 plots <- list()
                 if (show_temp) {
                   plots[[length(plots) + 1]] <- column(
                     width = if (both) 6 else 12,
                     div(
                       class = "d-flex align-items-center justify-content-between mb-2",
                       span(),
                       downloadButton("dl_temp_program_plot", "Download plot", class = "btn-primary")
                     ),
                     plotOutput("temp_program_plot", height = "520px")
                   )
                 }
                 if (show_mass) {
                   plots[[length(plots) + 1]] <- column(
                     width = if (both) 6 else 12,
                     div(
                       class = "d-flex align-items-center justify-content-between mb-2",
                       checkboxInput("colour_segments", "Black/white only (hide stage annotations)", value = FALSE),
                       downloadButton("dl_processed_plot", "Download plot", class = "btn-primary")
                     ),
                     plotOutput("process_plot", height = "520px")
                   )
                 }
                 
                 if (length(plots) == 0) {
                   helpText("Select at least one plot to display.")
                 } else {
                   do.call(fluidRow, plots)
                 }
               }
             )
           },
           decon = tagList(
             div(
               class = "d-flex align-items-center justify-content-between mb-2",
               h3("3. Pyrolysis phase deconvolution"),
               if (!is.null(rv$decon)) {
                 downloadButton("dl_weights_tbl", "Download table", class = "btn-secondary btn-sm")
               }
             ),
             p("Weights table from deconvolve()."),
             DTOutput("weights_tbl")
           ),
           viz = tagList(
             h3("4. Pyrolysis phase deconvolution plots"),
             fluidRow(
               column(
                 width = 12,
                 align = "right",
                 if (!is.null(rv$decon)) {
                   downloadButton("dl_decon_plot", "Download decon plot", class = "btn-primary")
                 }
               )
             ),
             plotOutput("decon_plot", height = "520px")
           ),
            fractions = tagList(
              div(
                class = "position-relative mb-2",
                h3("5. Carbon fractions"),
                if (!is.null(rv$processed) && !is.null(rv$decon)) {
                  div(
                   class = "d-flex flex-column align-items-end gap-1",
                   style = "position:absolute; top:0; right:-12px;",
                   downloadButton("dl_fractions_tbl", "Download table", class = "btn-secondary btn-sm")
                  )
                }
              ),
              p("Adjust assumed fixed carbon percentages for each pseudo-component in the sidebar if needed."),
              if (isTRUE(input$show_sankey)) {
                plotlyOutput("fractions_sankey", height = "640px")
              } else {
                DTOutput("fractions_tbl")
              },
              if (!is.null(rv$processed) && !is.null(rv$decon)) {
                div(
                  class = "d-flex justify-content-end mt-3",
                  downloadButton("dl_repro_script", "Reproducible R script", class = "btn-outline-primary btn-sm")
                )
              }
            )
    )
  })
}
