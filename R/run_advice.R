#' Draft an ICES advice sheet for one stock
#'
#' Fetches the stock's data from SID, SAG and ASD ([get_stock_data()]) and
#' renders a draft advice sheet as a Word document ([render_advice()]).
#'
#' @param stock_code ICES stock code, e.g. `"sol.27.7d"`.
#' @param assessment_year assessment year, e.g. `2026`.
#' @param data_dir folder for the downloaded data (created if needed).
#' @param out_dir folder for the draft (created if needed).
#' @param template Word template for styles, headers and footers. The
#'   default is the ICES advice template included in the package.
#' @param historical_from first assessment year in the historical assessment
#'   plots, e.g. the year of a change in assessment method. `NULL` (default)
#'   includes all assessments downloaded (up to five).
#' @param historical_first_year first data year on the x axis of the
#'   historical assessment plots. `NULL` (default) is 20 years before the
#'   latest assessment.
#'
#' @return Path to the draft `.docx`, invisibly.
#'
#' @seealso [render_advice()] to re-render from saved data,
#'   [advice_todo()] to list what a draft still needs.
#'
#' @examples
#' \dontrun{
#' run_advice("sol.27.7d", 2026)
#' }
#' @export
run_advice <- function(stock_code, assessment_year, data_dir = "data", out_dir = "output",
                       template = default_template(),
                       historical_from = NULL, historical_first_year = NULL) {
  get_stock_data(stock_code, assessment_year, out_dir = data_dir)
  render_advice(stock_code, assessment_year, data_dir = data_dir, out_dir = out_dir,
                template = template, historical_from = historical_from,
                historical_first_year = historical_first_year)
}

#' Render a draft advice sheet from saved data
#'
#' Renders the draft from the data file saved by [get_stock_data()], without
#' contacting ICES. Use it after changing the template, or to re-render a
#' stock downloaded earlier.
#'
#' The sheet is rendered in a temporary folder, so nothing is written to the
#' working directory except the draft in `out_dir`. Rendering needs the Quarto
#' command-line tool (<https://quarto.org>).
#'
#' @inheritParams run_advice
#' @return Path to the draft `.docx`, invisibly.
#'
#' @examples
#' \dontrun{
#' get_stock_data("sol.27.7d", 2026)
#' render_advice("sol.27.7d", 2026)
#'
#' # Method changed at the 2024 benchmark: show only assessments since then
#' render_advice("sol.27.7d", 2026, historical_from = 2024)
#' }
#' @export
render_advice <- function(stock_code, assessment_year, data_dir = "data", out_dir = "output",
                          template = default_template(),
                          historical_from = NULL, historical_first_year = NULL) {
  data_file <- file.path(data_dir, paste0(stock_code, "_", assessment_year, ".rds"))
  if (!file.exists(data_file)) {
    stop("No data for ", stock_code, " ", assessment_year, " in '", data_dir,
         "'. Run get_stock_data() or run_advice() first.")
  }
  x <- readRDS(data_file)

  work <- tempfile("advice")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(system.file("quarto", "advice_sheet.qmd", package = "icesAdviceDraft"), work)
  make_reference_doc(header_values(x), template = template, out = file.path(work, "_reference.docx"))

  out_file <- paste0(stock_code, "_", assessment_year, "_advice_draft.docx")
  quarto::quarto_render(
    input = file.path(work, "advice_sheet.qmd"),
    output_file = out_file,
    execute_params = c(
      list(
        stock_code = stock_code,
        assessment_year = assessment_year,
        data_dir = normalizePath(data_dir, winslash = "/")
      ),
      # left out when NULL, so the sheet's defaults apply
      if (!is.null(historical_from)) list(historical_from = historical_from),
      if (!is.null(historical_first_year)) list(historical_first_year = historical_first_year)
    ),
    quiet = TRUE
  )

  # Quarto writes caption paragraphs that Word rejects; repair them
  fix_caption_paragraphs(file.path(work, out_file))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(out_dir, out_file)
  if (!file.copy(file.path(work, out_file), dest, overwrite = TRUE)) {
    stop("Could not write '", dest, "'. Is it open in Word?")
  }
  invisible(dest)
}

default_template <- function() {
  system.file("templates", "advice_template.docx", package = "icesAdviceDraft")
}
