library(shiny)
library(mixchar)
library(DT)

# Require the fixed-carbon branch of mixchar so carbon-fraction helpers are present.
if (!exists("calculate_fixed_carbon_fractions", where = asNamespace("mixchar"), inherits = FALSE)) {
  stop(
    "mixchar fixed-carbon branch required. Install via remotes::install_github('smwindecker/mixchar', ref = 'fixed-carbon')."
  )
}

parse_probs <- function(txt) {
  vals <- suppressWarnings(as.numeric(unlist(strsplit(txt, "[,;\\s]+"))))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) c(0.025, 0.5, 0.975) else vals
}

example_defaults <- list(
  skip_rows = 40,
  init_mass = 18.96,
  pyro_start = 127,
  pyro_end = 191.5
)

guess_col <- function(df, candidates) {
  nms <- names(df)
  hit <- nms[tolower(nms) %in% tolower(candidates)]
  if (length(hit) > 0) return(hit[1])
  nms[1]
}

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw = NULL,
    processed = NULL,
    decon = NULL,
    file_reset = 0,
    source = "none",
    example_path = NULL
  )

  output$datafile_ui <- renderUI({
    rv$file_reset  # invalidate when reset is requested
    fileInput("datafile", "Upload CSV", accept = c(".csv"))
  })

  reset_column_inputs <- function() {
    updateSelectInput(session, "temp_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "mass_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "time_col", choices = c("Select column" = ""), selected = "")
  }

  set_column_inputs <- function(df, selected = list(temp = "", mass = "", time = "")) {
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
  }

  reset_processing_inputs <- function() {
    updateNumericInput(session, "init_mass", value = NA_real_)
    updateNumericInput(session, "pyro_start", value = NA_real_)
    updateNumericInput(session, "pyro_end", value = NA_real_)
  }

  apply_example_inputs <- function() {
    updateNumericInput(session, "skip_rows", value = example_defaults$skip_rows)
    updateNumericInput(session, "init_mass", value = example_defaults$init_mass)
    updateNumericInput(session, "pyro_start", value = example_defaults$pyro_start)
    updateNumericInput(session, "pyro_end", value = example_defaults$pyro_end)
    rv$file_reset <- rv$file_reset + 1  # clear any previously uploaded file in the UI
  }

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
      time = guess_col(df, c("time", "t", "minutes", "min"))
    ))
    rv$processed <- NULL
    rv$decon <- NULL
  })

  observeEvent(input$datafile, {
    req(input$datafile)
    # Reset skip rows to default (0) when user uploads their own file, e.g. after using example data.
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

  observeEvent(input$apply_skip, {
    skip_val <- if (is.na(input$skip_rows)) 0 else input$skip_rows

    # Determine which dataset to reload based on source
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
      time = input$time_col
    ))
  })

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

  observeEvent(input$do_process, {
    req(rv$raw)
    temp_col <- input$temp_col
    mass_loss_col <- input$mass_col
    time_col <- input$time_col

    validate(
      need(!is.na(input$init_mass), "Initial mass is required."),
      need(!is.na(input$pyro_start), "Pyrolysis start time is required."),
      need(!is.na(input$pyro_end), "Pyrolysis end time is required."),
      need(temp_col != "", "Select the temperature column."),
      need(mass_loss_col != "", "Select the mass loss column."),
      need(time_col != "", "Select the time column."),
      need(input$pyro_end > input$pyro_start, "Pyrolysis end must be after start."),
      need(all(c(temp_col, mass_loss_col, time_col) %in% names(rv$raw)),
           "Data must include temperature, mass loss, and time columns.")
    )

    rv$processed <- process(
      data = rv$raw,
      init_mass = input$init_mass,
      temp = temp_col,
      mass_loss = mass_loss_col,
      time = time_col,
      pyrolysis_start_time = input$pyro_start,
      pyrolysis_end_time = input$pyro_end,
      temp_units = "C"
    )
    rv$decon <- NULL
  })

  output$stage_summary_tbl <- renderDataTable({
    req(rv$processed)
    df <- stage_summary(rv$processed)
    numeric_cols <- which(vapply(df, is.numeric, logical(1)))
    dt <- datatable(df, options = list(dom = "t"))
    if (length(numeric_cols)) {
      dt <- formatRound(dt, columns = numeric_cols, digits = 2)
    }
    dt
  })

  output$processed_data_tbl <- renderDataTable({
    req(rv$processed)
    datatable(
      rv$processed$all_data,
      options = list(
        dom = "t",
        scrollX = TRUE,
        scrollY = "50vh",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        orderClasses = TRUE
      ),
      class = "display nowrap"
    )
  })

  output$process_plot <- renderPlot({
    req(rv$processed)
    plot(rv$processed,
         plot_type = input$processed_plot_choice,
         colour_segments = !isTRUE(input$colour_segments))
  }, res = 120)

  output$dl_processed_plot <- downloadHandler(
    filename = function() paste0("processed_plot_", input$processed_plot_choice, ".png"),
    content = function(file) {
      req(rv$processed)
      png(file, width = 1200, height = 1200, res = 150)
      plot(rv$processed,
           plot_type = input$processed_plot_choice,
           colour_segments = !isTRUE(input$colour_segments))
      dev.off()
    }
  )

  observeEvent(input$run_decon, {
    req(rv$processed)
    np <- switch(input$n_peaks, auto = NULL, `3` = 3, `4` = 4)
    withProgress(message = "Deconvolving...", value = 0, {
      rv$decon <- deconvolve(rv$processed, seed = input$decon_seed, n_peaks = np)
    })
  })

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

  output$fractions_tbl <- renderDataTable({
    req(rv$processed, rv$decon)
    validate(
      need(rv$decon$n_peaks == 3,
           "Carbon fractions currently supported for 3 peaks. Please deconvolve with 3 peaks (or Auto → 3).")
    )

    tbl <- tryCatch({
      moisture <- mixchar:::calculate_moisture_content(rv$processed)
      ash <- mixchar:::calculate_ash_content(rv$processed)
      fc <- calculate_fixed_carbon_fractions(
        rv$decon,
        dry_basis_fixed_carbon_hemicellulose = input$fc_hemi,
        dry_basis_fixed_carbon_cellulose = input$fc_cell,
        dry_basis_fixed_carbon_lignin = input$fc_lig
      )
      totals <- mixchar:::calculate_total_fractions(rv$decon, fc)

      data.frame(
        Component = c(
          "Moisture", "Ash",
          "Hemicellulose total", "Cellulose total", "Lignin total",
          "Hemicellulose fixed carbon", "Cellulose fixed carbon", "Lignin fixed carbon"
        ),
        Percent = round(
          c(moisture, ash,
            totals$H_total, totals$C_total, totals$L_total,
            fc$Hfp, fc$Cfp, fc$Lfp),
          2
        )
      )
    }, error = function(e) {
      data.frame(
        Component = "Error",
        Percent = NA_real_,
        Note = paste("Unable to compute fractions:", e$message)
      )
    })

    datatable(tbl, options = list(dom = "t"))
  })

  output$step_body <- renderUI({
    switch(input$step,
           read = tagList(
             h3("1. Load data"),
             p("Upload your CSV or use the built-in beech_example.csv; set skip rows if metadata is present."),
             DTOutput("raw_preview", height = "60vh")
           ),
           process = tagList(
             h3("2. Process data"),
             p("Run process() and inspect stage boundaries."),
             h4("Stage summary"),
             DTOutput("stage_summary_tbl"),
             br(),
             h4("Processed data (all_data)"),
             DTOutput("processed_data_tbl", height = "50vh")
           ),
           plots = tagList(
             h3("3. Processed plots"),
             fluidRow(
               column(
                 width = 8,
                 radioButtons(
                   "processed_plot_choice",
                   label = "Plot selector",
                   choices = c("Mass Loss Profiles" = "mass", "DTG Curve" = "rate"),
                   selected = "mass",
                   inline = TRUE
                 )
               ),
               column(
                 width = 4,
                 align = "right",
                 downloadButton("dl_processed_plot", "Download plot", class = "btn-primary")
               )
             ),
             plotOutput("process_plot", height = "520px")
           ),
           decon = tagList(
             h3("4. Pyrolysis phase deconvolution"),
             p("Weights table from deconvolve()."),
             DTOutput("weights_tbl")
           ),
           viz = tagList(
             h3("5. Pyrolysis phase deconvolution plots"),
             fluidRow(
               column(
                 width = 12,
                 align = "right",
                 downloadButton("dl_decon_plot", "Download decon plot", class = "btn-primary")
               )
             ),
             plotOutput("decon_plot", height = "520px")
           ),
           fractions = tagList(
             h3("6. Carbon fractions"),
             p("Adjust assumed fixed carbon percentages for each pseudo-component in the sidebar if needed."),
             DTOutput("fractions_tbl")
           )
    )
  })
}
