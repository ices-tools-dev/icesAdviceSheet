# icesAdviceDraft

Draft ICES advice sheets, as Word documents, for any stock published in the
ICES Stock Assessment Graphs (SAG) database. For a stock and assessment year
it pulls:

- the stock description, ecoregion, data category and expert group from the
  Stock Information Database (SID);
- assessment results with confidence bounds, and reference points, from SAG;
- the stock's advice assessments in SAG for the four previous years, for the
  historical assessment plots;
- the advice, catch scenarios, notes and advice history from the Advice and
  Scenarios Database (ASD);

and renders a draft in the ICES advice template, with the ICES standard
graphs and tables.

The draft marks what it could not fill in:

- **[Pending: ...]**: data missing or inconsistent in SID, SAG or ASD (for
  example no F age range in SAG). Fix it at source and re-run.
- **[To be supplied ...]**: text for the expert group to write (quality of
  the assessment, technical basis of reference points, and so on).

## Installation

```r
install.packages("remotes")
remotes::install_github("ices-tools-dev/icesAdviceDraft",
  repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))
```

The ICES packages (icesSD, icesSAG, icesASD) are installed from the ICES
r-universe. Rendering also needs the [Quarto](https://quarto.org)
command-line tool (version 1.4 or later); RStudio includes it.

## Usage

```r
library(icesAdviceDraft)

# Download the data and render the draft
draft <- run_advice("sol.27.7d", 2026)
#> output/sol.27.7d_2026_advice_draft.docx

# What the draft still needs
advice_todo(draft)
```

Several stocks:

```r
stocks <- c("sol.27.7d", "bli.27.5a14", "her.27.5a")
drafts <- lapply(stocks, function(s) {
  tryCatch(run_advice(s, 2026), error = function(e) {
    message(s, " FAILED: ", conditionMessage(e))
    NULL
  })
})
advice_todo(unlist(drafts))
```

`run_advice()` is `get_stock_data()` followed by `render_advice()`. The
data are saved in `data/`, so a draft can be re-rendered without
downloading again:

```r
render_advice("sol.27.7d", 2026)
```

To check the downloaded fields for a stock:

```r
x <- readRDS("data/sol.27.7d_2026.rds")
inspect_stock_data(x)
```

## Checks

`get_stock_data()` stops with a message, rather than guessing, when:

- SID, SAG or ASD return no record, or more than one, for the stock and year;
- SID and ASD point to different SAG assessments;
- SAG has more than one row per year.

The draft also flags, as [Pending: ...], missing SAG fields (F age range,
stock size units), ASD advice said to apply before its release date, and
years left out of the historical assessment plots because SAG has more than
one advice assessment for them.

## Word compatibility

Quarto writes figure and table captions in a way Word reports as
"unreadable content". `render_advice()` repairs every draft with
`fix_caption_paragraphs()`, which can also be used on other Quarto Word
output.

## Data sources

- SID: <https://sd.ices.dk> (icesSD)
- SAG: <https://sag.ices.dk> (icesSAG)
- ASD: <https://asd.ices.dk> (icesASD)
