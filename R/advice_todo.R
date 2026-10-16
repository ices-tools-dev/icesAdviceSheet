#' List what a draft advice sheet still needs
#'
#' Finds the placeholders left in a draft:
#' * `[Pending: ...]`: data missing or inconsistent in SID, SAG or ASD; fix it
#'   at source and re-run.
#' * `[To be supplied ...]`: text for the expert group to write in the draft.
#'
#' @param docx path(s) to draft `.docx` files, e.g. as returned by
#'   [run_advice()].
#'
#' @return A data frame with columns `file`, `item` and `n` (number of
#'   times the item appears), invisibly. It is also printed.
#'
#' @examples
#' \dontrun{
#' draft <- run_advice("sol.27.7d", 2026)
#' advice_todo(draft)
#' }
#' @export
advice_todo <- function(docx) {
  out <- do.call(rbind, lapply(docx, function(f) {
    # Quarto nests each table inside a caption table, so a whole table can
    # come back as one string: find every match, not just the first.
    txt <- officer::docx_summary(officer::read_docx(f))$text
    hits <- unlist(regmatches(txt, gregexpr("\\[(Pending:|(Text )?[Tt]o be supplied)[^]]*\\]", txt)))
    if (!length(hits)) return(data.frame(file = f, item = character(0), n = integer(0)))
    tab <- table(hits)
    data.frame(file = f, item = names(tab), n = as.integer(tab), row.names = NULL)
  }))

  for (f in docx) {
    d <- out[out$file == f, ]
    message("\n", f, ": ", sum(d$n), " item(s) to complete")
    for (i in seq_len(nrow(d))) message("  ", d$n[i], " x ", d$item[i])
  }
  invisible(out)
}
