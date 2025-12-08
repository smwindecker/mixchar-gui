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
  
  # Prefer original stage-coded data (e.g., beech has 6 stages) for the temp program plot.
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
        return(list(
          data = rv$raw,
          time_col = input$time_col,
          temp_col = input$temp_col,
          stage_col = stage_col
        ))
      }
    }
    
    list(
      data = rv$processed$all_data,
      time_col = "time",
      temp_col = "temp_C",
      stage_col = "stage"
    )
  }
  
  output$datafile_ui <- renderUI({
    rv$file_reset  # invalidate when reset is requested
    fileInput("datafile", "Upload CSV", accept = c(".csv"))
  })
  
  reset_column_inputs <- function() {
    updateSelectInput(session, "temp_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "mass_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "time_col", choices = c("Select column" = ""), selected = "")
    updateSelectInput(session, "stage_col", choices = c("Select column" = ""), selected = "")
  }
  
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
      time = guess_col(df, c("time", "t", "minutes", "min")),
      stage = if ("stage" %in% names(df)) "stage" else ""
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
      time = input$time_col,
      stage = input$stage_col
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
  
  output$dl_stage_summary <- downloadHandler(
    filename = function() "stage_summary.csv",
    content = function(file) {
      req(rv$processed)
      df <- stage_summary(rv$processed)
      numeric_cols <- which(vapply(df, is.numeric, logical(1)))
      if (length(numeric_cols)) {
        df[numeric_cols] <- lapply(df[numeric_cols], round, 2)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
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
      
      # 支持旧字段名和新字段名：优先 verbose 名称，若不存在则退回缩写 Hfp/Cfp/Lfp
      fc_H <- if (!is.null(fc$fixed_carbon_HC_fraction)) fc$fixed_carbon_HC_fraction else fc$Hfp
      fc_C <- if (!is.null(fc$fixed_carbon_CL_fraction)) fc$fixed_carbon_CL_fraction else fc$Cfp
      fc_L <- if (!is.null(fc$fixed_carbon_LG_fraction)) fc$fixed_carbon_LG_fraction else fc$Lfp
      
      data.frame(
        Component = c(
          "Moisture", "Ash",
          "Hemicellulose total", "Cellulose total", "Lignin total",
          "Hemicellulose fixed carbon", "Cellulose fixed carbon", "Lignin fixed carbon"
        ),
        Percent = round(
          c(moisture, ash,
            totals$H_total, totals$C_total, totals$L_total,
            fc_H, fc_C, fc_L),
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
  
  output$dl_fractions_tbl <- downloadHandler(
    filename = function() "carbon_fractions.csv",
    content = function(file) {
      req(rv$processed, rv$decon)
      if (rv$decon$n_peaks != 3) {
        stop("Carbon fractions currently supported for 3 peaks. Please deconvolve with 3 peaks (or Auto -> 3).")
      }
      
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
        
        fc_H <- if (!is.null(fc$fixed_carbon_HC_fraction)) fc$fixed_carbon_HC_fraction else fc$Hfp
        fc_C <- if (!is.null(fc$fixed_carbon_CL_fraction)) fc$fixed_carbon_CL_fraction else fc$Cfp
        fc_L <- if (!is.null(fc$fixed_carbon_LG_fraction)) fc$fixed_carbon_LG_fraction else fc$Lfp
        
        data.frame(
          Component = c(
            "Moisture", "Ash",
            "Hemicellulose total", "Cellulose total", "Lignin total",
            "Hemicellulose fixed carbon", "Cellulose fixed carbon", "Lignin fixed carbon"
          ),
          Percent = round(
            c(moisture, ash,
              totals$H_total, totals$C_total, totals$L_total,
              fc_H, fc_C, fc_L),
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
      
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  
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
               downloadButton("dl_weights_tbl", "Download table", class = "btn-secondary btn-sm")
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
                 downloadButton("dl_decon_plot", "Download decon plot", class = "btn-primary")
               )
             ),
             plotOutput("decon_plot", height = "520px")
           ),
           fractions = tagList(
             div(
               class = "d-flex align-items-center justify-content-between mb-2",
               h3("5. Carbon fractions"),
               downloadButton("dl_fractions_tbl", "Download table", class = "btn-secondary btn-sm")
             ),
             p("Adjust assumed fixed carbon percentages for each pseudo-component in the sidebar if needed."),
             DTOutput("fractions_tbl")
           )
    )
  })
}
