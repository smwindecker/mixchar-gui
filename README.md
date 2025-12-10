# mixchar-gui

Shiny GUI for the `mixchar` package (thermogravimetric deconvolution).

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
