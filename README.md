# mixchar-gui

Shiny GUI for the `mixchar` package (thermogravimetric deconvolution).

## Overview

This Shiny application provides an interactive workbench for analyzing thermogravimetric data through the mixchar package. It guides users through a five-step workflow: data loading, processing, deconvolution, visualization, and carbon fraction calculation.

## Requirements
- R (>= 4.0)
- `mixchar` package — **fixed-carbon branch** (latest carbon-fraction work)  
  ```r
  install.packages("remotes")  # if remotes is not installed
  remotes::install_github("smwindecker/mixchar", ref = "fixed-carbon")
  # or install from your local fixed-carbon checkout:
  # remotes::install_local("/path/to/mixchar")
  ```
- Shiny runtime (`shiny`, `DT`) and `plotly` for the Sankey diagram:  
  ```r
  install.packages(c("shiny", "DT", "plotly"))
  ```

### Quick install & run (copy-paste)
```r
install.packages(c("shiny","DT","bslib","plotly","remotes"), repos = "https://cloud.r-project.org")
remotes::install_github("smwindecker/mixchar", ref = "fixed-carbon", upgrade = "never")
shiny::runApp(".")
```

## Getting the GUI
- Download/clone the GUI repo: <https://github.com/smwindecker/mixchar-gui>
- (Already here) place the example data `beech_example.csv` in the repo root; keep `ui.R` and `server.R` together.

## Run
In the repo root:
```r
shiny::runApp(".")
```

## Notes
- Make sure the `mixchar` fixed-carbon branch is installed before running the GUI.
- You can upload your own CSV in the app and set skip rows/column mapping; or use the bundled `beech_example.csv`.

---

## Architecture & Code Structure

### File Organization

- **`ui.R`**: User interface definition
  - Sidebar layout with workflow step navigation
  - Conditional panels for step-specific inputs
  - Dynamic content area (rendered server-side)

- **`server.R`**: Server-side logic and data processing
  - Reactive state management (`rv` reactiveValues)
  - Data processing, deconvolution, and fraction calculations
  - Output rendering and download handlers

- **`sankey.R`**: Sankey diagram visualization
  - Carbon fraction flow visualization
  - Plotly-based interactive diagrams

### Application State Management

The app uses Shiny's `reactiveValues` (`rv`) to manage state:

- `rv$raw`: Raw uploaded/example data
- `rv$processed`: Processed data from `mixchar::process()`
- `rv$decon`: Deconvolution results from `mixchar::deconvolve()`
- `rv$source`: Data source identifier ("upload", "example", or "none")
- `rv$file_reset`: Counter for file input reset
- `rv$example_path`: Path to example data file

### Workflow Steps

1. **Load data** (`input$step == "read"`)
   - Upload CSV or use example data
   - Configure skip rows for metadata

2. **Process data** (`input$step == "process"`)
   - Map columns (temperature, mass loss, time, stage)
   - Set processing parameters (initial mass, pyrolysis window)
   - View stage summary and plots

3. **Deconvolution** (`input$step == "decon"`)
   - Configure peak count (auto, 3, or 4)
   - Set random seed for reproducibility
   - View weights table

4. **Visualization** (`input$step == "viz"`)
   - Configure plot appearance (bw, quantile bands)
   - View deconvolution plots

5. **Carbon fractions** (`input$step == "fractions"`)
   - Adjust fixed carbon percentages
   - View table or Sankey diagram
   - Export reproducible R script

---

## Development Guide

### Adding a New Workflow Step

1. **Update `ui.R`**:
   - Add step to `radioButtons` choices in sidebar
   - Create new `conditionalPanel` for step-specific inputs
   - Use `conditionalPanel("input.step == '<step_name>'")` syntax

2. **Update `server.R`**:
   - Add new case to `output$step_body` switch statement
   - Create any required observers (`observeEvent`) or outputs
   - Update `rv` reactiveValues if new state is needed

3. **Example**:
   ```r
   # In ui.R sidebarPanel:
   conditionalPanel(
     "input.step == 'newstep'",
     numericInput("new_param", "New Parameter", value = 0)
   )
   
   # In server.R output$step_body:
   newstep = tagList(
     h3("New Step"),
     plotOutput("new_plot")
   )
   ```

### Modifying Data Processing

- Processing logic is in `observeEvent(input$do_process, ...)`
- Validation occurs before processing
- Errors are caught and displayed via `showNotification()`
- Processing results are stored in `rv$processed`

### Adding New Visualizations

1. Create output renderer (e.g., `output$new_plot <- renderPlot({...})`)
2. Add download handler if needed (e.g., `output$dl_new_plot <- downloadHandler({...})`)
3. Include in appropriate step's UI (via `output$step_body`)

### Modifying Sankey Diagram

- Edit `sankey.R` functions:
  - `build_sankey_labels()`: Change node label formatting
  - `render_fractions_sankey()`: Modify layout, colors, or links
- Node positions: Adjust `node_x` and `node_y` arrays
- Colors: Modify `col_hemi`, `col_cell`, etc.
- Links: Update `link_sources`, `link_targets`, `link_values` arrays

### Code Style & Documentation

- Functions are documented with purpose, parameters, and return values
- Complex logic includes inline comments explaining "why"
- File headers describe purpose, architecture, and modification notes
- Use `req()` to ensure dependencies before rendering outputs
- Use `validate(need(...))` for user input validation

### Testing Changes

1. Ensure `mixchar` fixed-carbon branch is installed
2. Run `shiny::runApp(".")` from project root
3. Test each workflow step with example data
4. Verify error handling with invalid inputs
5. Check download handlers produce correct files

### Common Modifications

**Change default values**:
- Edit `example_defaults` list in `server.R`
- Modify `value` parameters in `ui.R` inputs

**Add new input field**:
- Add to appropriate `conditionalPanel` in `ui.R`
- Access via `input$<id>` in `server.R`
- Update validation if required

**Modify color scheme**:
- Edit `app_theme` in `ui.R` for overall theme
- Edit color variables in `sankey.R` for diagram colors
- Modify CSS in `tags$style()` for custom styling

---

## Troubleshooting

- **"mixchar fixed-carbon branch required"**: Install via `remotes::install_github('smwindecker/mixchar', ref = 'fixed-carbon')`
- **Processing fails**: Check temperature units match data (C vs K), verify pyrolysis window is appropriate
- **Deconvolution fails**: Ensure processing completed successfully, check data quality
- **Sankey diagram missing**: Requires 3-peak deconvolution; run deconvolution with 3 peaks first
