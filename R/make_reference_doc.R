#' Make a stock-specific copy of the ICES advice template
#'
#' The template (inst/templates/advice_template.docx) is the reg-spict advice
#' template with the stock-specific header and footer text replaced by
#' placeholders: {{STOCK_CODE}}, {{YEAR}}, {{ECOREGION}}, {{PUB_DATE}} and
#' {{DOI}}. This fills them in and writes a new .docx for Quarto to use as
#' its reference-doc. Styles (including the green Heading 1 bar) are untouched.
#'
#' @param values named character vector, names matching the placeholders
#'   without braces, e.g. c(STOCK_CODE = "sol.27.7d", YEAR = "2026", ...).
#' @param template path to the placeholder template.
#' @param out path for the filled-in copy.
#' @noRd
make_reference_doc <- function(values,
                               template = default_template(),
                               out = tempfile(fileext = ".docx")) {
  tmp <- tempfile("refdoc")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(template, exdir = tmp)

  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  parts <- list.files(file.path(tmp, "word"), pattern = "^(header|footer)[0-9]*\\.xml$", full.names = TRUE)
  for (p in parts) {
    xml <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (nm in names(values)) {
      xml <- gsub(paste0("{{", nm, "}}"), xml_escape(values[[nm]]), xml, fixed = TRUE)
    }
    left <- regmatches(xml, gregexpr("\\{\\{[A-Z_]+\\}\\}", xml))[[1]]
    if (length(left)) stop("Unfilled placeholder(s) in ", basename(p), ": ", paste(unique(left), collapse = ", "))
    writeLines(xml, p, useBytes = TRUE)
  }

  # Absolute path: zip::zip() works from inside `root`
  out <- file.path(normalizePath(dirname(out)), basename(out))
  if (file.exists(out)) file.remove(out)
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE)
  zip::zip(out, files = files, root = tmp)
  out
}
