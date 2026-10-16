# ICES standard graphs (catches, recruitment, fishing pressure, biomass),
# styled to match the SAG standard graph images.
#
# Data: icesSAG::getStockDownloadData() (x$sag). Fields used:
#   Year, Catches, Landings, Discards, CatchesLandingsUnits,
#   Low_Recruitment, Recruitment, High_Recruitment, UnitOfRecruitment, RecruitmentAge,
#   Low_FishingPressure, FishingPressure, High_FishingPressure,
#   FishingPressureDescription, FAge,
#   Low_StockSize, StockSize, High_StockSize, StockSizeDescription, StockSizeUnits,
#   ConfidenceIntervalDefinition
# Reference points: SAG FishStockReferencePoints (x$refpts), keyed FMSY, Fpa,
# FLim, MSYBtrigger, Bpa, Blim.

ices_cols <- list(
  catches = "#002b5f",
  discards = "#fda500",
  recruitment = "#28b3e8",
  errorbar = "#2f4f4f",
  fishing = "#ed5f26",
  fishing_ci = "#f2a497",
  ssb = "#047c6c",
  ssb_ci = "#94b0a9",
  msy = "#689dff",
  pa = "#000000"
)

# ── Units and scaling ──────────────────────────────────────────────────────

# Multiplier from SAG UnitOfRecruitment to numbers (as in ICES get_scaling_factor()).
# An empty unit means thousands for recruitment, following ICES.
recruitment_multiplier <- function(unit) {
  unit <- trimws(unit[1])
  if (is.na(unit) || unit %in% c("thousands", "Thousands", "empty")) return(1e3)
  if (unit %in% c("N", "Relative Recruitment", "Number of individuals (fisheries)")) return(1)
  if (unit %in% c("Number of individuals (billions)", "UWTV abundance (billions)")) return(1e9)
  stop("Unrecognised SAG UnitOfRecruitment: '", unit, "'")
}

# Axis scaling as in ICES get_scaling(): 1, 1000, millions or billions.
ices_scaling <- function(values, tonnes = TRUE) {
  order <- floor(log10(max(values, na.rm = TRUE)))
  suffix <- if (tonnes) c("billions t", "millions t", "1000 t", "t") else c("billions", "millions", "thousands", "")
  if (order >= 9) return(list(divisor = 1e9, suffix = suffix[1]))
  if (order >= 6) return(list(divisor = 1e6, suffix = suffix[2]))
  if (order >= 3) return(list(divisor = 1e3, suffix = suffix[3]))
  list(divisor = 1, suffix = suffix[4])
}

axis_label <- function(what, suffix) if (nzchar(suffix)) paste(what, "in", suffix) else what

# ── Theme ──────────────────────────────────────────────────────────────────

theme_sag <- function(title_colour, base_size = 7) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = title_colour, size = base_size * 1.5, hjust = 0,
                                         margin = ggplot2::margin(b = 1)),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = "grey55", size = base_size * 0.75, hjust = 1,
                                            margin = ggplot2::margin(b = 2)),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(colour = "black", size = base_size * 1.05),
      axis.text = ggplot2::element_text(colour = "black", size = base_size),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(2.5, "pt"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.4),
      panel.grid.major.y = ggplot2::element_line(colour = "grey80", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size, margin = ggplot2::margin(l = 2, r = 8)),
      legend.key.width = ggplot2::unit(14, "pt"),
      legend.key.height = ggplot2::unit(7, "pt"),
      legend.spacing.x = ggplot2::unit(2, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-4, 0, 0, 0),
      plot.margin = ggplot2::margin(3, 6, 2, 3, "pt")
    )
}

y_zero <- function() ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)))
x_years <- function() ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 8))

# ── Line panel with ribbon and reference lines ─────────────────────────────

# refs: list per reference point (in legend order) with key, colour, linetype
# and plotmath label. Lines are drawn MSY first, so the black PA/lim lines sit
# on top, as in SAG.
sag_line_panel <- function(df, low, est, high, refs, refpts, line_col, ci_col, ci_label,
                           title, ylab, stamp, divisor = 1) {
  d <- data.frame(Year = df$Year, low = df[[low]] / divisor, est = df[[est]] / divisor,
                  high = df[[high]] / divisor)
  d <- d[!is.na(d$est), ]
  ci <- d[!is.na(d$low) & !is.na(d$high), ]

  refs <- Filter(function(r) !is.na(refpts[[r$key]]), refs)
  keys <- vapply(refs, `[[`, character(1), "key")
  ref_df <- data.frame(key = keys, value = unname(refpts[keys]) / divisor)
  draw_order <- order(!grepl("MSY", keys))   # MSY lines first, underneath
  ref_df <- ref_df[draw_order, , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(Year))
  if (nrow(ci)) {
    p <- p + ggplot2::geom_ribbon(data = ci, ggplot2::aes(ymin = low, ymax = high, fill = ci_label))
  }
  p <- p + ggplot2::geom_line(ggplot2::aes(y = est), colour = line_col, linewidth = 0.55)
  if (nrow(ref_df)) {
    p <- p + ggplot2::geom_hline(
      data = ref_df,
      ggplot2::aes(yintercept = value, colour = key, linetype = key),
      linewidth = 0.6
    )
  }
  if (length(refs)) {
    labs_expr <- as.expression(lapply(refs, `[[`, "label"))
    p <- p +
      ggplot2::scale_colour_manual(values = stats::setNames(vapply(refs, `[[`, "", "colour"), keys),
                                   breaks = keys, labels = labs_expr) +
      ggplot2::scale_linetype_manual(values = stats::setNames(vapply(refs, `[[`, "", "linetype"), keys),
                                     breaks = keys, labels = labs_expr)
  }
  p +
    ggplot2::scale_fill_manual(values = stats::setNames(ci_col, ci_label)) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 2)
    ) +
    y_zero() + x_years() + ggplot2::expand_limits(y = 0) +
    ggplot2::labs(title = title, subtitle = stamp, y = ylab) +
    theme_sag(line_col)
}

# ── The four graphs ────────────────────────────────────────────────────────

#' ICES standard graphs
#'
#' Catches (or landings), recruitment, fishing pressure and stock size, 2 x 2,
#' in the style of the SAG standard graphs, with confidence bounds and
#' reference points.
#'
#' @param sag data frame from [icesSAG::getStockDownloadData()] (element
#'   `sag` of [get_stock_data()] output).
#' @param refpts named numeric vector keyed by SAG reference point field
#'   name (`FMSY`, `Fpa`, `FLim`, `MSYBtrigger`, `Bpa`, `Blim`), as in
#'   `advice_fields(x)$refpts`. `NA` values are not drawn.
#' @param stamp text for the grey stamp above each panel (`NULL` for none).
#' @param prelim_year catch year to show as preliminary (`NULL` for none).
#' @return A cowplot grid of four ggplot panels.
#' @family sheet building blocks
#' @export
plot_stock_summary <- function(sag, refpts, stamp = NULL, prelim_year = NULL) {
  sag <- sag[order(sag$Year), ]
  ci_label <- sag_field(sag, "ConfidenceIntervalDefinition")
  if (is.na(ci_label)) ci_label <- "Confidence interval"

  # ── Catches ──────────────────────────────────────────────────────────────
  unit_c <- sag_field(sag, "CatchesLandingsUnits")
  if (!is.na(unit_c) && unit_c != "tonnes") stop("Unrecognised SAG CatchesLandingsUnits: '", unit_c, "'")
  # Components of the catch in SAG. A component counts as present only if it
  # has a value above zero (SAG often holds zeros for unreported discards).
  # Landings alone -> "Landings"; landings plus any other component ->
  # stacked "Catches"; no landings -> total "Catches".
  components <- c(
    Landings = "Landings", Discards = "Discards",
    IBC = "Industrial bycatch", Unallocated_Removals = "Unallocated removals"
  )
  present <- names(components)[vapply(names(components), function(f) {
    f %in% names(sag) && any(sag[[f]] > 0, na.rm = TRUE)
  }, logical(1))]

  if ("Landings" %in% present) {
    catch_long <- do.call(rbind, lapply(present, function(f) {
      data.frame(Year = sag$Year, type = components[[f]], value = sag[[f]])
    }))
  } else {
    catch_long <- data.frame(Year = sag$Year, type = "Catches", value = sag$Catches)
  }
  catch_what <- if (identical(present, "Landings")) "Landings" else "Catches"
  catch_long <- catch_long[!is.na(catch_long$value), ]
  if (!is.null(prelim_year)) catch_long$type[catch_long$Year == prelim_year] <- paste("Preliminary", tolower(catch_what))
  catch_long$type <- factor(catch_long$type, levels = intersect(
    c(paste("Preliminary", tolower(catch_what)), "Unallocated removals", "Industrial bycatch", "Discards", "Landings", "Catches"),
    unique(catch_long$type)
  ))
  sc <- ices_scaling(stats::aggregate(value ~ Year, catch_long, sum)$value)

  p_catch <- ggplot2::ggplot(catch_long, ggplot2::aes(Year, value / sc$divisor, fill = type)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::scale_fill_manual(values = c(
      Landings = ices_cols$catches, Discards = ices_cols$discards,
      `Industrial bycatch` = "#00b29d", `Unallocated removals` = "#6eb200",
      Catches = ices_cols$catches,
      `Preliminary catches` = "#6eb5d2", `Preliminary landings` = "#6eb5d2"
    )) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    y_zero() + x_years() +
    ggplot2::labs(title = catch_what, subtitle = stamp, y = axis_label(catch_what, sc$suffix)) +
    theme_sag(ices_cols$catches)

  # ── Recruitment ──────────────────────────────────────────────────────────
  mult <- recruitment_multiplier(sag$UnitOfRecruitment)
  rec <- data.frame(Year = sag$Year, low = sag$Low_Recruitment * mult,
                    est = sag$Recruitment * mult, high = sag$High_Recruitment * mult)
  rec <- rec[!is.na(rec$est), ]
  sr <- ices_scaling(c(rec$est, rec$high), tonnes = FALSE)
  rec[c("low", "est", "high")] <- rec[c("low", "est", "high")] / sr$divisor
  rec_ci <- rec[!is.na(rec$low) & !is.na(rec$high), ]

  p_rec <- ggplot2::ggplot(rec, ggplot2::aes(Year)) +
    ggplot2::geom_col(ggplot2::aes(y = est, fill = "Recruitment"), width = 0.55)
  if (nrow(rec_ci)) {
    p_rec <- p_rec + ggplot2::geom_errorbar(
      data = rec_ci, ggplot2::aes(ymin = low, ymax = high, colour = ci_label),
      width = 0.5, linewidth = 0.3
    )
  }
  p_rec <- p_rec +
    ggplot2::scale_fill_manual(values = c(Recruitment = ices_cols$recruitment)) +
    ggplot2::scale_colour_manual(values = stats::setNames(ices_cols$errorbar, ci_label)) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2)) +
    y_zero() + x_years() +
    ggplot2::labs(title = sag_rec_label(sag), subtitle = stamp, y = axis_label("Recruitment", sr$suffix)) +
    theme_sag(ices_cols$recruitment)

  # ── Fishing pressure ─────────────────────────────────────────────────────
  p_f <- sag_line_panel(
    sag, "Low_FishingPressure", "FishingPressure", "High_FishingPressure",
    refs = list(
      list(key = "Fpa", colour = ices_cols$pa, linetype = "22", label = quote(F[PA])),
      list(key = "FLim", colour = ices_cols$pa, linetype = "dashed", label = quote(F[lim])),
      list(key = "FMSY", colour = ices_cols$msy, linetype = "solid", label = quote(F[MSY]))
    ),
    refpts = refpts, line_col = ices_cols$fishing, ci_col = ices_cols$fishing_ci, ci_label = ci_label,
    title = "Fishing pressure", ylab = sag_f_label(sag), stamp = stamp
  )

  # ── Biomass ──────────────────────────────────────────────────────────────
  b_desc <- sag_ssb_label(sag)
  b_units <- sag_field(sag, "StockSizeUnits")
  b_tonnes <- !is.na(b_units) && b_units == "tonnes"
  sb <- if (b_tonnes) {
    ices_scaling(c(sag$StockSize, sag$High_StockSize, refpts[c("MSYBtrigger", "Bpa", "Blim")]))
  } else {
    list(divisor = 1, suffix = if (is.na(b_units)) missing_in_sag else b_units)
  }

  p_ssb <- sag_line_panel(
    sag, "Low_StockSize", "StockSize", "High_StockSize",
    refs = list(
      list(key = "Bpa", colour = ices_cols$pa, linetype = "11", label = quote(B[PA])),
      list(key = "Blim", colour = ices_cols$pa, linetype = "22", label = quote(B[lim])),
      list(key = "MSYBtrigger", colour = ices_cols$msy, linetype = "solid", label = quote(MSY ~ B[trigger]))
    ),
    refpts = refpts, line_col = ices_cols$ssb, ci_col = ices_cols$ssb_ci, ci_label = ci_label,
    title = "Biomass", ylab = axis_label(b_desc, sb$suffix), stamp = stamp, divisor = sb$divisor
  )

  cowplot::plot_grid(p_catch, p_rec, p_f, p_ssb, ncol = 2, align = "hv", axis = "tblr")
}
