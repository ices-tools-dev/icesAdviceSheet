# Helpers for the generic advice sheet. Table styling is carried over from the
# Golden Redfish advice sheet (reg-spict, "2 advice sheet.qmd").

# Table width. The template's A4 text area is 6.69 in, but Quarto places each
# table inside a full-width layout cell with 0.075 in margins each side
# (6.54 in usable). A wider table overhangs the cell and Word clips its
# left and right borders, so keep a little inside that.
page_width_in <- 6.4

# ── flextable styling ──────────────────────────────────────────────────────

fit_ft_page <- function(ft, max_width = page_width_in) {
  col_widths <- ft$body$colwidths
  total <- sum(col_widths)
  if (length(total) == 1 && is.finite(total) && total > 0) {
    ft <- flextable::width(
      ft,
      j = seq_along(col_widths),
      width = col_widths * max_width / total
    )
  }
  flextable::set_table_properties(ft, layout = "fixed", width = 1)
}

ices_ft <- function(ft) {
  ft |>
    flextable::bg(bg = "#E8EAEA", part = "header") |>
    flextable::border_inner(officer::fp_border(color = "black", width = 0.5)) |>
    flextable::border_outer(officer::fp_border(color = "black", width = 0.75)) |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(align = "center", part = "header") |>
    flextable::padding(padding = 2, part = "all") |>
    flextable::autofit() |>
    fit_ft_page()
}

fmt_t <- function(x) {
  ifelse(is.na(x), NA_character_, format(round(x), big.mark = "\u2009", scientific = FALSE, trim = TRUE))
}

# ── SAG labels ─────────────────────────────────────────────────────────────
# From icesSAG::getStockDownloadData() fields; a field that is empty is left out.

sag_field <- function(sag, field) {
  v <- trimws(as.character(sag[[field]][1]))
  if (!length(v) || is.na(v) || !nzchar(v)) NA_character_ else v
}

# "F (ages 3–7)"; when SAG FAge is empty the gap is shown in the label
sag_f_label <- function(sag) {
  desc <- sag_field(sag, "FishingPressureDescription")
  if (is.na(desc)) desc <- "F"
  age <- sag_field(sag, "FAge")
  paste0(desc, " (ages ", if (is.na(age)) "[missing in SAG]" else age, ")")
}

missing_in_sag <- "[units missing in SAG]"

#' Inconsistencies in the ASD advice record
#'
#' Checks the current ASD advice record for problems the Advice Drafting
#' Group should follow up, e.g. advice said to apply from a date before its
#' release. The sheet shows each one as `[Pending: ...]`.
#'
#' @param asd_record current ASD advice record (element `asd_record`).
#' @return Character vector, one message per problem (empty if none).
#' @family sheet building blocks
#' @export
asd_gaps <- function(asd_record) {
  from <- as.Date(substr(trimws(asd_record$adviceApplicableFrom), 1, 10))
  released <- as.Date(substr(trimws(asd_record$adviceReleasedDate), 1, 10))
  c(
    if (!is.na(from) && !is.na(released) && from < released) {
      paste0(
        "ASD says this advice applies from ", format_ices_date(asd_record$adviceApplicableFrom),
        ", before its release date (", format_ices_date(asd_record$adviceReleasedDate),
        "); check adviceApplicableFrom and adviceApplicableUntil in ASD"
      )
    }
  )
}

#' SAG fields that are missing
#'
#' Checks for SAG fields that should be filled in but are empty (currently
#' `FAge` and `StockSizeUnits`). The sheet shows each one as
#' `[Pending: ...]` so the Advice Drafting Group can follow up.
#'
#' @param sag SAG data (element `sag`).
#' @return Character vector, one message per gap (empty if none).
#' @family sheet building blocks
#' @export
sag_gaps <- function(sag) {
  c(
    if (is.na(sag_field(sag, "FAge"))) {
      "the F age range (FAge) is missing from SAG; add it in SAG so it appears on the fishing pressure axis and in the assessment summary"
    },
    if (is.na(sag_field(sag, "StockSizeUnits"))) {
      "the stock size units (StockSizeUnits) are missing from SAG; add them in SAG so the biomass axis is scaled and labelled, and the assessment summary shows units"
    }
  )
}

# "Recruitment (age 1)"
sag_rec_label <- function(sag) {
  age <- sag_field(sag, "RecruitmentAge")
  if (is.na(age)) "Recruitment" else paste0("Recruitment (age ", age, ")")
}

sag_ssb_label <- function(sag) {
  desc <- sag_field(sag, "StockSizeDescription")
  if (is.na(desc)) "Stock size" else desc
}

# ── Summary of the assessment table ────────────────────────────────────────

#' Assessment summary table
#'
#' Estimate, high and low for recruitment, stock size and fishing pressure,
#' plus catches, landings and discards, from SAG. Values are shown in SAG's
#' own units, as stored.
#'
#' @param sag data frame from [icesSAG::getStockDownloadData()] (element
#'   `sag` of [get_stock_data()] output).
#' @return A flextable.
#' @family sheet building blocks
#' @export
stock_summary_ft <- function(sag) {
  ci_label <- sag_field(sag, "ConfidenceIntervalDefinition")
  num <- function(v) ifelse(is.na(v), NA_character_, fmt_t(v))
  rat <- function(v) ifelse(is.na(v), NA_character_, as.character(v))

  tbl <- data.frame(
    Year = as.character(sag$Year),
    R = num(sag$Recruitment), R_hi = num(sag$High_Recruitment), R_lo = num(sag$Low_Recruitment),
    B = num(sag$StockSize), B_hi = num(sag$High_StockSize), B_lo = num(sag$Low_StockSize),
    C = num(sag$Catches), L = num(sag$Landings), D = num(sag$Discards),
    F = rat(sag$FishingPressure), F_hi = rat(sag$High_FishingPressure), F_lo = rat(sag$Low_FishingPressure)
  )

  unit_line <- function(u) if (is.na(u)) "" else paste0("\n", u)
  r_unit <- unit_line(sag_field(sag, "UnitOfRecruitment"))
  b_unit <- unit_line(sag_field(sag, "StockSizeUnits"))
  if (!nzchar(b_unit)) b_unit <- paste0("\n", missing_in_sag)
  c_unit <- unit_line(sag_field(sag, "CatchesLandingsUnits"))
  hi <- if (is.na(ci_label)) "High" else paste0("High\n", ci_label)
  lo <- if (is.na(ci_label)) "Low" else paste0("Low\n", ci_label)

  top <- c("Year",
           rep(paste0(sag_rec_label(sag), r_unit), 3),
           rep(paste0(sag_ssb_label(sag), b_unit), 3),
           paste0("Catches", c_unit), paste0("Landings", c_unit), paste0("Discards", c_unit),
           rep(sag_f_label(sag), 3))
  sub <- c("Year", "Estimate", hi, lo, "Estimate", hi, lo,
           paste0("Catches", c_unit), paste0("Landings", c_unit), paste0("Discards", c_unit),
           "Estimate", hi, lo)

  flextable::flextable(tbl) |>
    flextable::set_header_labels(values = stats::setNames(as.list(sub), names(tbl))) |>
    flextable::add_header_row(values = top, top = TRUE) |>
    flextable::merge_h(part = "header") |>
    flextable::merge_v(part = "header") |>
    flextable::colformat_char(na_str = "\u2013") |>
    ices_ft() |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::valign(valign = "center", part = "header")
}

# ── ASD catch scenarios table ──────────────────────────────────────────────

asd_purpose_labels <- c(
  BasisAdvice = "ICES advice basis",
  OtherScenarios = "Other scenarios"
)

#' Catch scenario table
#'
#' Builds the catch scenario table from ASD. Uses the fields returned by
#' [icesASD::getCatchScenariosTable()]: `aR_ID`, `cS_Label`, `cS_Purpose`,
#' `aK_ID`, `aK_Label`, `yearLabel`, `unit`, `value`. Notes
#' ([icesASD::getCatchScenariosNotes()]: `symbol`, `notes`) are added as
#' footer lines, in the order returned.
#'
#' @param asd_table catch scenario table (element `asd_table`).
#' @param asd_notes catch scenario notes (element `asd_notes`), or `NULL`.
#' @return A flextable.
#' @family sheet building blocks
#' @export
asd_scenarios_ft <- function(asd_table, asd_notes = NULL) {
  unknown <- setdiff(unique(asd_table$cS_Purpose), names(asd_purpose_labels))
  if (length(unknown)) {
    stop("Unrecognised ASD cS_Purpose value(s): ", paste(unknown, collapse = ", "))
  }

  cols <- unique(asd_table[, c("aK_ID", "aK_Label", "yearLabel", "unit")])
  cols <- cols[order(cols$aK_ID), ]
  rows <- unique(asd_table[, c("aR_ID", "cS_Label", "cS_Purpose")])
  rows <- rows[order(match(rows$cS_Purpose, names(asd_purpose_labels)), rows$aR_ID), ]

  fmt_value <- function(v, unit) {
    if (is.na(v)) return(NA_character_)
    switch(unit,
      t = fmt_t(v),
      `%` = as.character(v),
      ratio = as.character(v),
      stop("Unrecognised ASD unit: ", unit)
    )
  }

  body <- t(vapply(seq_len(nrow(rows)), function(i) {
    vals <- vapply(seq_len(nrow(cols)), function(j) {
      hit <- asd_table$aR_ID == rows$aR_ID[i] & asd_table$aK_ID == cols$aK_ID[j]
      if (!any(hit)) return(NA_character_)
      fmt_value(asd_table$value[hit][1], cols$unit[j])
    }, character(1))
    c(rows$cS_Label[i], vals)
  }, character(nrow(cols) + 1)))

  # Insert a group header row before each purpose block
  out <- NULL
  grp_rows <- integer(0)
  for (p in unique(rows$cS_Purpose)) {
    out <- rbind(out, c(asd_purpose_labels[[p]], rep(NA_character_, nrow(cols))))
    grp_rows <- c(grp_rows, nrow(out))
    out <- rbind(out, body[rows$cS_Purpose == p, , drop = FALSE])
  }

  # Positional column keys; ASD labels (which can contain non-ASCII
  # characters) go in the header only.
  keys <- c("Basis", paste0("col", seq_len(nrow(cols))))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- keys
  headers <- c("Basis", paste0(cols$aK_Label, "\n(", cols$yearLabel, ")"))

  ft <- flextable::flextable(out) |>
    flextable::set_header_labels(values = stats::setNames(as.list(headers), keys)) |>
    flextable::colformat_char(na_str = "") |>
    flextable::bold(i = grp_rows, part = "body") |>
    flextable::bg(i = grp_rows, bg = "#F5F5F5", part = "body") |>
    ices_ft() |>
    flextable::align(j = -1, align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::fontsize(size = 7.5, part = "header") |>
    flextable::width(j = 1, width = 1.2) |>
    flextable::width(j = -1, width = (page_width_in - 1.2) / nrow(cols))
  for (g in grp_rows) {
    ft <- flextable::merge_at(ft, i = g, j = seq_len(ncol(out)), part = "body")
  }
  ft <- flextable::align(ft, i = grp_rows, align = "left", part = "body")

  if (NROW(asd_notes)) {
    ft <- ft |>
      flextable::add_footer_lines(paste(trimws(asd_notes$symbol), trimws(asd_notes$notes))) |>
      flextable::fontsize(size = 8, part = "footer") |>
      flextable::align(align = "left", part = "footer")
  }
  ft
}

# ── Advice basis and category ──────────────────────────────────────────────

advice_category_label <- function(x) {
  labels <- c(MSY = "MSY approach", PA = "Precautionary approach", MP = "Management plan")
  ifelse(x %in% names(labels), labels[x], x)
}

# ── Two-column Item / Description tables ───────────────────────────────────

#' Two-column item and description table
#'
#' The ICES style for "Basis of the advice" and "Basis of the assessment":
#' no header, shaded bold first column.
#'
#' @param df data frame with two columns (item, description).
#' @return A flextable.
#' @family sheet building blocks
#' @export
item_table_ft <- function(df) {
  flextable::flextable(df) |>
    flextable::delete_part(part = "header") |>
    flextable::bg(j = 1, bg = "#E8EAEA", part = "body") |>
    flextable::border_inner(officer::fp_border(color = "black", width = 0.5)) |>
    flextable::border_outer(officer::fp_border(color = "black", width = 0.75)) |>
    flextable::bold(j = 1, part = "body") |>
    flextable::width(j = 1, width = 1.84) |>
    flextable::width(j = 2, width = 4.66) |>
    fit_ft_page()
}

# ── Stock status sentence ──────────────────────────────────────────────────

# "A", "A and B", "A, B, and C"
list_text <- function(x) {
  n <- length(x)
  if (n <= 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

# Position of a value relative to named reference points (NA ones dropped),
# e.g. "above F~MSY~ and below F~pa~"
status_phrase <- function(value, refs) {
  refs <- refs[!is.na(refs)]
  if (!length(refs) || is.na(value)) return(NA_character_)
  parts <- c(
    if (any(value > refs)) paste("above", list_text(names(refs)[value > refs])),
    if (any(value == refs)) paste("at", list_text(names(refs)[value == refs])),
    if (any(value < refs)) paste("below", list_text(names(refs)[value < refs]))
  )
  paste(parts, collapse = " and ")
}

#' Stock status sentence
#'
#' States fishing pressure and stock size relative to the reference points.
#' Fishing pressure is taken in the last year with an estimate up to
#' `assessment_year - 1`; stock size in `assessment_year` if SAG has it,
#' otherwise the last year with an estimate. The years used are stated.
#'
#' @param sag SAG data (element `sag`).
#' @param refpts named numeric vector of reference points, as in
#'   `advice_fields(x)$refpts`.
#' @param assessment_year assessment year.
#' @return Markdown text, or `NA` if there are no estimates or reference
#'   points to compare.
#' @family sheet building blocks
#' @export
status_sentence <- function(sag, refpts, assessment_year) {
  f_row <- sag[!is.na(sag$FishingPressure) & sag$Year <= assessment_year - 1, ]
  b_row <- sag[!is.na(sag$StockSize) & sag$Year <= assessment_year, ]
  if (!nrow(f_row) || !nrow(b_row)) return(NA_character_)
  f_row <- f_row[nrow(f_row), ]
  b_row <- b_row[nrow(b_row), ]

  f_txt <- status_phrase(f_row$FishingPressure, c(
    "F~MSY~" = refpts[["FMSY"]], "F~pa~" = refpts[["Fpa"]], "F~lim~" = refpts[["FLim"]]
  ))
  b_txt <- status_phrase(b_row$StockSize, c(
    "MSY B~trigger~" = refpts[["MSYBtrigger"]], "B~pa~" = refpts[["Bpa"]], "B~lim~" = refpts[["Blim"]]
  ))
  if (is.na(f_txt) || is.na(b_txt)) return(NA_character_)

  b_desc <- sag_ssb_label(sag)
  b_what <- if (grepl("^SSB", b_desc)) "spawning-stock size" else "stock size"
  paste0(
    "Fishing pressure on the stock (F~", f_row$Year, "~) is ", f_txt,
    "; ", b_what, " (", b_desc, "~", b_row$Year, "~) is ", b_txt, "."
  )
}

# ── Reference points table ─────────────────────────────────────────────────

refpts_defs <- data.frame(
  key = c("MSYBtrigger", "FMSY", "Blim", "Bpa", "FLim", "Fpa", "FMGT_lower", "FMGT_upper"),
  framework = c(
    "MSY approach", "MSY approach",
    rep("Precautionary approach", 4),
    "Management plan", "Management plan"
  ),
  main = c("MSY B", "F", "B", "B", "F", "F", "MAP target range F", "MAP target range F"),
  sub = c("trigger", "MSY", "lim", "pa", "lim", "pa", "lower", "upper"),
  unit = c("t", "f", "t", "t", "f", "f", "f", "f")
)

#' Reference points table
#'
#' From the SAG reference point values; reference points with no value are
#' left out. The technical basis is left for the expert group.
#'
#' @param refpts named numeric vector of reference points, as in
#'   `advice_fields(x)$refpts`.
#' @return A flextable.
#' @family sheet building blocks
#' @export
refpts_ft <- function(refpts) {
  d <- refpts_defs[!is.na(refpts[refpts_defs$key]), ]
  vals <- refpts[d$key]
  tbl <- data.frame(
    Framework = d$framework,
    `Reference point` = "",
    Value = ifelse(d$unit == "t", paste(fmt_t(vals), "t"), format(vals, drop0trailing = TRUE, trim = TRUE)),
    `Technical basis` = "[To be supplied]",
    check.names = FALSE
  )
  ft <- flextable::flextable(tbl)
  for (i in seq_len(nrow(d))) {
    ft <- flextable::compose(
      ft,
      i = i, j = "Reference point",
      value = flextable::as_paragraph(d$main[i], flextable::as_sub(d$sub[i]))
    )
  }
  ft |>
    flextable::merge_v(j = "Framework") |>
    ices_ft() |>
    flextable::align(j = 1:3, align = "center", part = "all") |>
    flextable::valign(j = 1, valign = "center", part = "body") |>
    flextable::width(j = 1, width = 1.3) |>
    flextable::width(j = 2, width = 1.6) |>
    flextable::width(j = 3, width = 0.9) |>
    flextable::width(j = 4, width = 2.9) |>
    fit_ft_page()
}

# ── History of advice table ────────────────────────────────────────────────

#' History of advice table
#'
#' One row per assessment year from the current (not replaced) ASD advice
#' records, labelled with the period the advice applies to: `"2022"` for a
#' calendar year, `"2022/2023"` for a split year. ICES catches come from SAG,
#' matched on the first year of the period. Agreed TAC has no source and is
#' shown as a dash.
#'
#' @param asd_history all years' ASD advice records (element `asd_history`).
#' @param sag SAG data (element `sag`).
#' @return A flextable.
#' @family sheet building blocks
#' @export
advice_history_ft <- function(asd_history, sag) {
  # One current ("Advice", not "Replaced") record per assessment year
  h <- asd_history[trimws(asd_history$adviceStatus) == "Advice", ]
  dup <- unique(h$assessmentYear[duplicated(h$assessmentYear)])
  if (length(dup)) {
    stop("More than one current ASD advice record for assessment year(s) ", paste(dup, collapse = ", "),
         ". Inspect x$asd_history.")
  }

  # Period the advice applies to: "2022" for a calendar year, "2022/2023"
  # for a split (fishing) year
  from <- as.Date(substr(trimws(h$adviceApplicableFrom), 1, 10))
  until <- as.Date(substr(trimws(h$adviceApplicableUntil), 1, 10))
  y1 <- format(from, "%Y")
  y2 <- format(until, "%Y")
  h$period <- ifelse(is.na(y2) | y1 == y2, y1, paste0(y1, "/", y2))
  h$start_year <- as.integer(y1)
  h <- h[order(from, h$assessmentYear), ]

  # SAG catches matched on the first year of the period
  catches <- sag[, c("Year", "Catches")]
  value <- suppressWarnings(as.numeric(h$adviceValue))

  tbl <- data.frame(
    Year = h$period,
    `ICES advice` = advice_category_label(trimws(h$adviceCategory)),
    `Catch corresponding\nto advice` = ifelse(is.na(value), trimws(h$adviceValue), fmt_t(value)),
    `Agreed TAC` = NA_character_,
    `ICES catches` = fmt_t(catches$Catches[match(h$start_year, catches$Year)]),
    check.names = FALSE
  )

  flextable::flextable(tbl) |>
    flextable::colformat_char(na_str = "\u2013") |>
    ices_ft() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 2, align = "left", part = "body") |>
    flextable::add_footer_lines("Agreed TAC: source to be agreed.") |>
    flextable::fontsize(size = 8, part = "footer")
}
