library(plotly)

# Build the labels used in the Sankey nodes with inline percentages.
build_sankey_labels <- function(vals) {
  c(
    "Total Biomass (100%)",
    sprintf("Moisture (%.2f%%)", vals$moisture),
    sprintf("Hemicellulose total (%.2f%%)", vals$total_H),
    sprintf("Cellulose total (%.2f%%)", vals$total_C),
    sprintf("Lignin total (%.2f%%)", vals$total_L),
    sprintf("Ash (%.2f%%)", vals$ash),
    sprintf("Hemi. Volatile (%.2f%%)", vals$vol_H),
    sprintf("Hemi. Fixed Carbon (%.2f%%)", vals$fc_H),
    sprintf("Cell. Volatile (%.2f%%)", vals$vol_C),
    sprintf("Cell. Fixed Carbon (%.2f%%)", vals$fc_C),
    sprintf("Lignin Volatile (%.2f%%)", vals$vol_L),
    sprintf("Lignin Fixed Carbon (%.2f%%)", vals$fc_L)
  )
}

# Return a plotly Sankey figure given the computed fraction values.
render_fractions_sankey <- function(vals) {
  all_vals <- unlist(vals, use.names = FALSE)
  if (any(is.na(all_vals))) {
    stop("Carbon fraction values missing; unable to build Sankey.")
  }

  node_labels <- build_sankey_labels(vals)

  # Three-column layout: source (left), totals (middle), vol/fc (right).
  node_x <- c(0, rep(0.48, 5), rep(0.94, 6))
  node_y <- c(
    0.50,  # Total Biomass (left)
    0.05,  # Moisture (top)
    0.25,  # Hemi total
    0.45,  # Cell total
    0.65,  # Lignin total
    0.92,  # Ash (bottom)
    0.22, 0.30,  # Hemi volatile / fixed carbon near Hemi total
    0.42, 0.50,  # Cell volatile / fixed carbon near Cell total
    0.62, 0.70   # Lignin volatile / fixed carbon near Lignin total
  )

  col_hemi <- "#0ea5e9"
  col_cell <- "#22c55e"
  col_lig <- "#f59e0b"
  col_moist <- "#14b8a6"
  col_ash <- "#475569"

  node_colors <- c(
    "#0f172a",
    col_moist,
    col_hemi,
    col_cell,
    col_lig,
    col_ash,
    col_hemi, col_hemi,
    col_cell, col_cell,
    col_lig, col_lig
  )

  link_sources <- c(0, 0, 0, 0, 0, 2, 2, 3, 3, 4, 4)
  link_targets <- c(1, 5, 2, 3, 4, 6, 7, 8, 9, 10, 11)
  link_values <- c(
    vals$moisture, vals$ash,
    vals$total_H, vals$total_C, vals$total_L,
    vals$vol_H, vals$fc_H,
    vals$vol_C, vals$fc_C,
    vals$vol_L, vals$fc_L
  )
  link_colors <- c(
    col_moist, col_ash,
    col_hemi, col_cell, col_lig,
    col_hemi, col_hemi,
    col_cell, col_cell,
    col_lig, col_lig
  )

  plot_ly(
    type = "sankey",
    arrangement = "fixed",
    node = list(
      label = node_labels,
      x = node_x,
      y = node_y,
      color = node_colors,
      pad = 14,
      line = list(color = "rgba(15, 23, 42, 0.3)", width = 1)
    ),
    link = list(
      source = link_sources,
      target = link_targets,
      value = round(link_values, 3),
      color = link_colors,
      hovertemplate = "%{source.label} -> %{target.label}: %{value:.2f}%<extra></extra>"
    )
  ) |>
    layout(
      margin = list(l = 20, r = 80, t = 10, b = 10),
      font = list(family = "Barlow, sans-serif", size = 14, color = "#0f172a")
    )
}
