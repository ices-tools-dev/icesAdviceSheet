#' Advice sheet fields
#'
#' Extracts and cleans the values the sheet uses (trimmed text, dates as
#' "30 June 2026", DOIs without the `https://doi.org/` prefix, invalid DOIs
#' as `NA`). Sources:
#' * SID ([icesSD::getSD()]): `StockKeyDescription`, `SpeciesScientificName`,
#'   `SpeciesCommonName`, `EcoRegion`, `DataCategory`, `AssessmentType`,
#'   `ExpertGroup`;
#' * SAG ([icesSAG::getFishStockReferencePoints()]): `FMSY`, `Fpa`, `FLim`,
#'   `MSYBtrigger`, `Bpa`, `Blim`, `FMGT_lower`, `FMGT_upper`;
#' * ASD ([icesASD::getAdviceViewRecord()]): `adviceSentence`, `adviceValue`,
#'   `adviceValueUnit`, `adviceCategory`, `adviceReleasedDate`, `adviceDOI`,
#'   `managementAlignment`, `mpwebLink`.
#'
#' @family sheet building blocks
#' @param x list returned by [get_stock_data()].
#' @return A named list of cleaned values. Missing values are `NA`.
#' @export
advice_fields <- function(x) {
  sid <- x$sid
  asd <- x$asd_record
  rp <- x$refpts

  description <- trimws(sid$StockKeyDescription)
  species <- trimws(sid$SpeciesScientificName)

  list(
    # Stock title, with the scientific name in italics (markdown)
    title_md = if (nzchar(species)) {
      sub(species, paste0("*", species, "*"), description, fixed = TRUE)
    } else {
      description
    },
    species_scientific = species,
    species_common = trimws(sid$SpeciesCommonName),
    ecoregion = format_ecoregion(sid$EcoRegion),
    data_category = trimws(sid$DataCategory),
    assessment_type = trimws(sid$AssessmentType),
    expert_group = trimws(sid$ExpertGroup),

    advice_sentence = trimws(asd$adviceSentence),
    advice_value = suppressWarnings(as.numeric(asd$adviceValue)),
    advice_unit = trimws(asd$adviceValueUnit),
    advice_category = trimws(asd$adviceCategory),
    pub_date = format_ices_date(asd$adviceReleasedDate),
    doi = clean_doi(asd$adviceDOI),
    management_alignment = trimws(asd$managementAlignment),
    management_plan_link = trimws(asd$mpwebLink),

    # Reference points, keyed by their SAG field names
    refpts = vapply(
      c("FMSY", "Fpa", "FLim", "MSYBtrigger", "Bpa", "Blim", "FMGT_lower", "FMGT_upper"),
      function(nm) suppressWarnings(as.numeric(rp[[nm]][1])),
      numeric(1)
    )
  )
}

# "Greater North Sea" -> "Greater North Sea ecoregion";
# "Celtic Seas, Greater North Sea" -> "Celtic Seas and Greater North Sea ecoregions"
format_ecoregion <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  parts <- trimws(strsplit(x, ",")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 1) return(paste(parts, "ecoregion"))
  paste(
    paste(parts[-length(parts)], collapse = ", "),
    "and", parts[length(parts)], "ecoregions"
  )
}

# "2026-06-30T00:00:00" -> "30 June 2026" (English month names, whatever the locale)
format_ices_date <- function(x) {
  d <- as.Date(substr(trimws(x), 1, 10))
  if (is.na(d)) return(NA_character_)
  paste(
    as.integer(format(d, "%d")),
    month.name[as.integer(format(d, "%m"))],
    format(d, "%Y")
  )
}

# " https://doi.org/10.17895/ices.advice.4874" -> "10.17895/ices.advice.4874".
# Anything that is not a DOI (e.g. "Figshare Api request failed!") -> NA.
clean_doi <- function(x) {
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", trimws(x))
  ifelse(grepl("^10\\.[0-9]{4,9}/\\S+$", x), x, NA_character_)
}

# Text for the page header and footer placeholders
header_values <- function(x, f = advice_fields(x)) {
  or_pending <- function(v, what) if (is.na(v)) paste0("[Pending: ", what, "]") else v
  c(
    STOCK_CODE = x$stock_code,
    YEAR = as.character(x$assessment_year),
    ECOREGION = or_pending(f$ecoregion, "ecoregion"),
    PUB_DATE = or_pending(f$pub_date, "date"),
    DOI = or_pending(f$doi, "DOI")
  )
}
