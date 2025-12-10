# ============================================================================
# sankey.R - Sankey Diagram Visualization for Carbon Fractions
# ============================================================================
#
# Purpose:
#   Creates interactive Sankey diagrams to visualize carbon fraction flows
#   from total biomass through components (moisture, ash, hemicellulose,
#   cellulose, lignin) to their volatile and fixed carbon sub-components.
#
# Architecture:
#   - Three-column layout: source (left), totals (middle), vol/fc (right)
#   - Left: Total Biomass (100%)
#   - Middle: Moisture, Ash, Hemicellulose/Cellulose/Lignin totals
#   - Right: Volatile and Fixed Carbon for each component
#
# Dependencies:
#   - plotly package for interactive visualization
#
# Usage:
#   Called from server.R via render_fractions_sankey() when user selects
#   Sankey diagram view in the fractions step.
#
# Modification Notes:
#   - Node positions (node_x, node_y) control layout - adjust for different arrangements
#   - Colors are defined per component type (hemi, cell, lig, moisture, ash)
#   - Link sources/targets use 0-based indexing matching node order
#   - To add new components, update node_labels, node positions, and link arrays
#
# ============================================================================

library(plotly)

# ----------------------------------------------------------------------------
# Label Generation
# ----------------------------------------------------------------------------

#' Build node labels with inline percentages for Sankey diagram
#'
#' Creates formatted labels for all nodes showing component names and their
#' percentage values. Labels are displayed on the Sankey nodes.
#'
#' @param vals Named list containing fraction values:
#'   - moisture, ash (direct percentages)
#'   - total_H, total_C, total_L (hemicellulose, cellulose, lignin totals)
#'   - vol_H, vol_C, vol_L (volatile fractions)
#'   - fc_H, fc_C, fc_L (fixed carbon fractions)
#' @return Character vector of 12 formatted labels (one per node)
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

# ----------------------------------------------------------------------------
# Sankey Diagram Rendering
# ----------------------------------------------------------------------------

#' Render interactive Sankey diagram for carbon fractions
#'
#' Creates a plotly Sankey diagram visualizing the flow of carbon fractions
#' from total biomass through components to volatile and fixed carbon sub-components.
#'
#' @param vals Named list of fraction values (see build_sankey_labels for structure)
#' @return plotly object (Sankey diagram)
#' @note Requires all values to be non-NA. Called from server.R when user
#'       enables Sankey view in the fractions step.
render_fractions_sankey <- function(vals) {
  # Validate that all required values are present
  all_vals <- unlist(vals, use.names = FALSE)
  if (any(is.na(all_vals))) {
    stop("Carbon fraction values missing; unable to build Sankey.")
  }

  node_labels <- build_sankey_labels(vals)

  # --------------------------------------------------------------------------
  # Node Layout Configuration
  # --------------------------------------------------------------------------
  # Three-column layout: source (left), totals (middle), vol/fc (right)
  # Node indices: 0=Total Biomass, 1=Moisture, 2=Hemi total, 3=Cell total,
  #               4=Lignin total, 5=Ash, 6-7=Hemi vol/fc, 8-9=Cell vol/fc,
  #               10-11=Lignin vol/fc
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

  # --------------------------------------------------------------------------
  # Color Scheme
  # --------------------------------------------------------------------------
  # Component-specific colors for visual consistency across nodes and links
  col_hemi <- "#0ea5e9"    # Blue for hemicellulose
  col_cell <- "#22c55e"    # Green for cellulose
  col_lig <- "#f59e0b"     # Orange for lignin
  col_moist <- "#14b8a6"   # Teal for moisture
  col_ash <- "#475569"     # Gray for ash

  # Node colors matching component types
  # Order: Total Biomass, Moisture, Hemi, Cell, Lignin, Ash, then vol/fc pairs
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

  # --------------------------------------------------------------------------
  # Link Configuration (Flow Connections)
  # --------------------------------------------------------------------------
  # Links define the flow paths between nodes
  # Format: source node index -> target node index with flow value
  # Links from Total Biomass (0) to: Moisture(1), Ash(5), Hemi(2), Cell(3), Lignin(4)
  # Links from totals to vol/fc: Hemi(2)->vol(6)/fc(7), Cell(3)->vol(8)/fc(9), Lignin(4)->vol(10)/fc(11)
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

  # --------------------------------------------------------------------------
  # Create Plotly Sankey Diagram
  # --------------------------------------------------------------------------
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
