# Historical assessment results ("Quality of the assessment" figure)

history_cols <- c("AssessmentYear", "Year", "Recruitment", "UnitOfRecruitment",
                  "StockSize", "FishingPressure")

#' Download earlier assessments of a stock from SAG
#'
#' For each of the `n_years - 1` years before `assessment_year`, finds the
#' published SAG assessments of the stock and keeps the one with purpose
#' "Advice". Years with no assessment (e.g. biennial stocks) are skipped. A
#' year with more than one advice assessment is left out and reported in
#' `notes`, rather than guessing which to use.
#'
#' @param stock_code ICES stock code.
#' @param assessment_year current assessment year.
#' @param sag current SAG data (already downloaded).
#' @param n_years number of assessments to show, including the current one.
#' @return list with `data` (one row per assessment year and year) and
#'   `notes` (character).
#' @noRd
get_sag_history <- function(stock_code, assessment_year, sag, n_years = 5) {
  current <- sag
  current$AssessmentYear <- assessment_year
  out <- list(current[, intersect(history_cols, names(current))])
  notes <- character(0)

  for (y in seq(assessment_year - 1, length.out = n_years - 1, by = -1)) {
    keys <- unique(icesSAG::findAssessmentKey(stock_code, year = y, regex = FALSE))
    if (!length(keys)) next
    d <- lapply(keys, icesSAG::getStockDownloadData)
    d <- Filter(function(z) NROW(z) > 0 && any(trimws(z$Purpose) == "Advice"), d)
    if (length(d) > 1) {
      notes <- c(notes, paste0(
        "SAG has ", length(d), " advice assessments of ", stock_code, " in ", y,
        "; that year is left out of the historical assessment plots"
      ))
      next
    }
    if (length(d) == 1) out <- c(out, list(d[[1]][, intersect(history_cols, names(d[[1]]))]))
  }

  # Columns missing from an older download are added as NA
  out <- lapply(out, function(z) {
    for (col in setdiff(history_cols, names(z))) z[[col]] <- NA
    z[, history_cols]
  })
  list(data = do.call(rbind, out), notes = notes)
}

#' Historical assessment results
#'
#' Stock size, fishing pressure and recruitment from the current and earlier
#' assessments, as in the "Quality of the assessment" section of ICES advice
#' sheets. The current assessment is drawn in the panel colour, earlier ones
#' in progressively lighter greys, with the current reference points. The
#' legends show the reference points only.
#'
#' @param history historical SAG data (element `sag_history$data` of
#'   [get_stock_data()] output).
#' @param sag current SAG data (element `sag`), for labels and units.
#' @param refpts named numeric vector of reference points, as in
#'   `advice_fields(x)$refpts`.
#' @param from_assessment first assessment year to include, e.g. the year of
#'   a change in assessment method, so that earlier, non-comparable
#'   assessments are left out. `NULL` (default) includes all.
#' @param first_year first data year shown on the x axis. `NULL` (default)
#'   is 20 years before the latest assessment.
#' @return A cowplot row of three ggplot panels.
#' @family sheet building blocks
#' @export
plot_historical <- function(history, sag, refpts, from_assessment = NULL, first_year = NULL) {
  h <- history
  h$AssessmentYear <- as.integer(h$AssessmentYear)
  if (!is.null(from_assessment)) {
    h <- h[h$AssessmentYear >= from_assessment, ]
    if (!nrow(h)) stop("No assessments from ", from_assessment, " onwards in the historical data.")
  }
  if (is.null(first_year)) first_year <- max(h$AssessmentYear) - 20
  h <- h[h$Year >= first_year, ]
  yrs <- sort(unique(h$AssessmentYear), decreasing = TRUE)   # newest first

  # Recruitment to numbers, per assessment (units can differ between years)
  h$Recruitment <- h$Recruitment * vapply(h$UnitOfRecruitment, recruitment_multiplier, numeric(1))

  year_cols <- function(panel_col) {
    greys <- c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9")
    stats::setNames(c(panel_col, greys)[seq_along(yrs)], as.character(yrs))
  }

  panel <- function(value, divisor, panel_col, title, ylab, refs) {
    d <- data.frame(Year = h$Year, value = h[[value]] / divisor,
                    AssessmentYear = factor(h$AssessmentYear, levels = rev(yrs)))
    d <- d[!is.na(d$value), ]
    p <- ggplot2::ggplot(d, ggplot2::aes(Year, value, colour = AssessmentYear, group = AssessmentYear)) +
      ggplot2::geom_line(ggplot2::aes(linewidth = AssessmentYear == as.character(yrs[1])), show.legend = FALSE) +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 0.7, `FALSE` = 0.45), guide = "none") +
      ggplot2::scale_colour_manual(values = year_cols(panel_col), guide = "none")

    refs <- Filter(function(r) !is.na(refpts[[r$key]]), refs)
    if (length(refs)) {
      ref_df <- data.frame(
        key = vapply(refs, `[[`, "", "key"),
        value = vapply(refs, function(r) refpts[[r$key]] / divisor, numeric(1))
      )
      ref_df$key <- factor(ref_df$key, levels = ref_df$key)
      ref_df <- ref_df[order(!grepl("MSY", ref_df$key)), ]   # MSY lines underneath
      p <- p +
        ggplot2::geom_hline(data = ref_df, ggplot2::aes(yintercept = value, linetype = key),
                            colour = vapply(refs, `[[`, "", "colour")[match(ref_df$key, vapply(refs, `[[`, "", "key"))],
                            linewidth = 0.5) +
        ggplot2::scale_linetype_manual(
          values = stats::setNames(vapply(refs, `[[`, "", "linetype"), vapply(refs, `[[`, "", "key")),
          labels = as.expression(lapply(refs, `[[`, "label")), name = NULL
        ) +
        ggplot2::guides(linetype = ggplot2::guide_legend(
          nrow = if (length(refs) > 2) 2 else 1,
          override.aes = list(colour = vapply(refs, `[[`, "", "colour"))
        ))
    }
    p + y_zero() + ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::labs(title = title, y = ylab) +
      theme_sag(panel_col) +
      ggplot2::theme(
        legend.key.width = ggplot2::unit(14, "pt"),
        legend.text = ggplot2::element_text(size = 6, margin = ggplot2::margin(l = 1, r = 4))
      )
  }

  b_units <- sag_field(sag, "StockSizeUnits")
  sb <- if (!is.na(b_units) && b_units == "tonnes") {
    ices_scaling(c(h$StockSize, refpts[c("MSYBtrigger", "Bpa", "Blim")]))
  } else {
    list(divisor = 1, suffix = if (is.na(b_units)) missing_in_sag else b_units)
  }
  sr <- ices_scaling(h$Recruitment, tonnes = FALSE)

  p_ssb <- panel("StockSize", sb$divisor, ices_cols$ssb, "Biomass",
                 axis_label(sag_ssb_label(sag), sb$suffix),
                 list(list(key = "Bpa", colour = ices_cols$pa, linetype = "11", label = quote(B[PA])),
                      list(key = "Blim", colour = ices_cols$pa, linetype = "22", label = quote(B[lim])),
                      list(key = "MSYBtrigger", colour = ices_cols$msy, linetype = "solid", label = quote(MSY ~ B[trigger]))))
  p_f <- panel("FishingPressure", 1, ices_cols$fishing, "Fishing pressure", sag_f_label(sag),
               list(list(key = "Fpa", colour = ices_cols$pa, linetype = "22", label = quote(F[PA])),
                    list(key = "FLim", colour = ices_cols$pa, linetype = "dashed", label = quote(F[lim])),
                    list(key = "FMSY", colour = ices_cols$msy, linetype = "solid", label = quote(F[MSY]))))
  p_rec <- panel("Recruitment", sr$divisor, ices_cols$recruitment, sag_rec_label(sag),
                 axis_label("Recruitment", sr$suffix), list())

  cowplot::plot_grid(p_ssb, p_f, p_rec, nrow = 1, align = "h", axis = "tb")
}
