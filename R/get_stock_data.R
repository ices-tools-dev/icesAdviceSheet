#' Download the data for a draft advice sheet
#'
#' Pulls, for one stock and assessment year:
#' * the Stock Information Database (SID) record ([icesSD::getSD()]), which
#'   also gives the SAG assessment key (`AssessmentKey`);
#' * the SAG assessment results with confidence bounds
#'   ([icesSAG::getStockDownloadData()]) and reference points
#'   ([icesSAG::getFishStockReferencePoints()]);
#' * the stock's advice assessments in SAG for the four previous years, for
#'   the historical assessment plots;
#' * the Advice and Scenarios Database (ASD) advice record, catch scenario
#'   table and notes, and all years' advice records (icesASD);
#'
#' checks they are consistent, and saves them as one list in
#' `<out_dir>/<stock_code>_<assessment_year>.rds`.
#'
#' @param stock_code ICES stock code, e.g. `"sol.27.7d"`.
#' @param assessment_year assessment year, e.g. `2026`.
#' @param out_dir folder for the data file (created if needed).
#'
#' @return The list, invisibly, with elements `stock_code`,
#'   `assessment_year`, `assessment_key`, `sid`, `refpts`, `sag`,
#'   `sag_history` (list of `data` and `notes`), `asd_record`, `asd_table`,
#'   `asd_notes` and `asd_history`.
#'
#' @examples
#' \dontrun{
#' x <- get_stock_data("sol.27.7d", 2026)
#' inspect_stock_data(x)
#' }
#' @export
get_stock_data <- function(stock_code, assessment_year, out_dir = "data") {
  # ── SID ───────────────────────────────────────────────────────────────────
  sid <- icesSD::getSD(stock = stock_code, year = assessment_year)
  check_one_row(sid, "SID record", stock_code, assessment_year)

  assessment_key <- sid$AssessmentKey
  if (is.na(assessment_key)) {
    stop(stock_code, " ", assessment_year, " has no SAG assessment key in SID (not published in SAG?).")
  }

  # ── ASD ───────────────────────────────────────────────────────────────────
  asd_record <- icesASD::getAdviceViewRecord(stockcode = stock_code, year = assessment_year)
  check_one_row(asd_record, "ASD advice record", stock_code, assessment_year)
  if (!identical(as.integer(asd_record$assessmentKey), as.integer(assessment_key))) {
    stop(
      "SID and ASD point to different SAG assessments for ", stock_code, " ", assessment_year,
      ": SID AssessmentKey = ", assessment_key, ", ASD assessmentKey = ", asd_record$assessmentKey, "."
    )
  }
  asd_table <- icesASD::getCatchScenariosTable(asd_record$adviceKey)
  asd_notes <- icesASD::getCatchScenariosNotes(asd_record$adviceKey)

  # All years' advice records, for the history of advice table
  asd_history <- icesASD::getAdviceViewRecord(stockcode = stock_code)

  # ── SAG ───────────────────────────────────────────────────────────────────
  refpts <- icesSAG::getFishStockReferencePoints(assessment_key)
  check_one_row(refpts, "SAG reference point record", stock_code, assessment_year)

  # Assessment results by year, with confidence bounds and units
  sag <- icesSAG::getStockDownloadData(assessment_key)
  if (anyDuplicated(sag$Year)) {
    stop(
      "SAG data for ", stock_code, " ", assessment_year,
      " has more than one row per year (assessment components?). Inspect x$sag."
    )
  }
  sag <- sag[order(sag$Year), ]

  # Earlier assessments, for the historical assessment plots
  sag_history <- get_sag_history(stock_code, assessment_year, sag)

  out <- list(
    stock_code = stock_code,
    assessment_year = assessment_year,
    assessment_key = assessment_key,
    sid = sid,
    refpts = refpts,
    sag = sag,
    sag_history = sag_history,
    asd_record = asd_record,
    asd_table = asd_table,
    asd_notes = asd_notes,
    asd_history = asd_history
  )

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, file.path(out_dir, paste0(stock_code, "_", assessment_year, ".rds")))
  invisible(out)
}

check_one_row <- function(x, what, stock_code, assessment_year) {
  if (NROW(x) != 1) {
    stop(
      "Expected one ", what, " for ", stock_code, " ", assessment_year,
      ", found ", NROW(x), "."
    )
  }
}

#' Show the fields downloaded from each source
#'
#' Prints the structure of the SID, SAG and ASD data, to check field names
#' and values.
#'
#' @param x list returned by [get_stock_data()], or read from its data file
#'   with [readRDS()].
#' @return `NULL`, invisibly.
#' @export
inspect_stock_data <- function(x) {
  for (nm in c("sid", "refpts", "sag", "asd_record", "asd_notes")) {
    cat("\n====", nm, "====\n")
    utils::str(x[[nm]], give.attr = FALSE, vec.len = 2, nchar.max = 120)
  }
  invisible(NULL)
}
