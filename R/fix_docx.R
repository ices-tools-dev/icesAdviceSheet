#' Repair Quarto's figure and table captions so Word will open the document
#'
#' Quarto (Word output, cross-referenced figures and tables) puts each figure
#' or table in a one-cell layout table with its caption, and produces two
#' things Word rejects as "unreadable content" (LibreOffice ignores both):
#'
#' 1. Each caption paragraph has two `<w:pPr>` blocks: one from pandoc (the
#'    centred cell) and one of Quarto's own (caption style, spacing,
#'    alignment). Word allows one. They are merged, keeping Quarto's
#'    settings, with the children in schema order.
#' 2. For tables the caption comes first, so the cell ends with the table.
#'    Word requires every cell to end with a paragraph. A minimal empty
#'    paragraph (no spacing, tiny line height) is added.
#'
#' [render_advice()] calls this on every draft. It is safe to run more than
#' once: a repaired file is left as it is.
#'
#' @param docx path to a `.docx` rendered by Quarto. It is rewritten in place.
#' @return Number of captions merged plus cells closed, invisibly.
#' @export
fix_caption_paragraphs <- function(docx) {
  tmp <- tempfile("docx")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(docx, exdir = tmp)
  p <- file.path(tmp, "word", "document.xml")

  # 1. Merge duplicate <w:pPr> blocks (text-level, so nothing else is touched)
  xml <- readChar(p, file.size(p), useBytes = TRUE)
  pat <- "(?s)<w:pPr>((?:(?!</w:pPr>).)*)</w:pPr>\\s*<w:pPr>((?:(?!</w:pPr>).)*)</w:pPr>"
  m <- gregexpr(pat, xml, perl = TRUE, useBytes = TRUE)
  n_ppr <- sum(m[[1]] > 0)
  if (n_ppr > 0) {
    regmatches(xml, m) <- list(vapply(regmatches(xml, m)[[1]], merge_ppr, character(1), USE.NAMES = FALSE))
    writeBin(charToRaw(xml), p)
  }

  # 2. Close table cells whose last block is a nested table
  doc <- xml2::read_xml(p)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  cells <- xml2::xml_find_all(doc, "//w:tc[(w:p|w:tbl|w:sdt)[last()][self::w:tbl]]", ns)
  for (cell in cells) {
    para <- xml2::xml_add_child(cell, "w:p")
    ppr <- xml2::xml_add_child(para, "w:pPr")
    xml2::xml_add_child(ppr, "w:spacing", `w:before` = "0", `w:after` = "0", `w:line` = "20", `w:lineRule` = "exact")
    rpr <- xml2::xml_add_child(ppr, "w:rPr")
    xml2::xml_add_child(rpr, "w:sz", `w:val` = "2")
  }
  if (length(cells)) xml2::write_xml(doc, p, options = "as_xml")   # no reformatting

  n <- n_ppr + length(cells)
  if (n == 0) return(invisible(0L))
  docx <- file.path(normalizePath(dirname(docx)), basename(docx))
  file.remove(docx)
  zip::zip(docx, files = list.files(tmp, recursive = TRUE, all.files = TRUE), root = tmp)
  invisible(n)
}

# Two adjacent <w:pPr> blocks -> one. The later setting of each property wins.
merge_ppr <- function(s) {
  last_tag <- function(tag) {
    r <- regmatches(s, gregexpr(paste0("<w:", tag, "[ /][^>]*>"), s, perl = TRUE))[[1]]
    if (length(r)) r[length(r)] else ""
  }
  jc <- last_tag("jc")
  jc <- sub('w:val="start"', 'w:val="left"', jc, fixed = TRUE)   # transitional schema values
  jc <- sub('w:val="end"', 'w:val="right"', jc, fixed = TRUE)
  # schema order: pStyle, keepNext, ..., spacing, ..., jc
  paste0("<w:pPr>", last_tag("pStyle"), last_tag("keepNext"), last_tag("spacing"), jc, "</w:pPr>")
}
