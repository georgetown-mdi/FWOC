# Author: Mark Richardson and Claude Code
# Purpose: Download and format the BLS macro series — CPI-U, national and state
#          unemployment — into metadata/bls/formatted/, consumed by 13_bls_macro.R.

library(httr2)
library(dplyr)
library(readr)
library(stringr)

# BLS requires a descriptive User-Agent on its download endpoints. Set it in
# your environment instead of hardcoding a contact here — e.g. add to ~/.Renviron:
#   BLS_USER_AGENT="Your Name your@email.org"
ua <- Sys.getenv("BLS_USER_AGENT")
if (!nzchar(ua)) {
  stop(
    "Set BLS_USER_AGENT to your contact (e.g. in ~/.Renviron:\n",
    '  BLS_USER_AGENT="Jane Doe jane@example.org"\n',
    "BLS requires a descriptive User-Agent on its download endpoints."
  )
}

# BLS rejects httr2's default User-Agent with a 403, and req_perform() writes
# that error page to path= before it raises — so stage each download in a .part
# sidecar and rename on success, keeping a good raw file safe until it lands.
download_bls <- function(url, destfile) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(destfile, ".part")
  on.exit(unlink(part), add = TRUE)
  request(url) |>
    req_user_agent(ua) |>
    req_retry(max_tries = 3) |>
    req_perform(path = part)
  if (!file.rename(part, destfile)) {
    stop("Downloaded ", part, " but could not move it to ", destfile)
  }
  invisible(destfile)
}

# Helper: the three BLS flat files share one layout. readr's default trim_ws
# strips the padding BLS puts around its header names and field values.
read_bls <- function(path) {
  read_tsv(
    path,
    col_types = cols(
      series_id      = col_character(),
      year           = col_integer(),
      period         = col_character(),
      value          = col_double(),
      footnote_codes = col_character(),
      .default       = col_character()
    ),
    na = c("", "-"), # BLS marks "not available" with "-" (e.g. CPI Oct 2025)
    show_col_types = FALSE
  )
}

# BLS codes months M01-M12 and the annual average M13; every series below drops M13.

# download_bls() creates raw/ as it goes; formatted/ needs one call up front.
dir.create("metadata/bls/formatted", recursive = TRUE, showWarnings = FALSE)

#### CPI: CUUR0000SA0 ####

download_bls(
  "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",
  "metadata/bls/raw/cu.data.1.AllItems.txt"
)

cpi <- read_bls("metadata/bls/raw/cu.data.1.AllItems.txt") |>
  filter(
    series_id == "CUUR0000SA0",
    period != "M13"
  ) |>
  transmute(
    year,
    month = as.integer(str_remove(period, "M")),
    cpi_u = value
  )

write_csv(cpi, "metadata/bls/formatted/cpi.csv", na = "")

#### National unemployment: LNS14000000 (SA monthly) ####

download_bls(
  "https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData",
  "metadata/bls/raw/ln.data.1.AllData.txt"
)

unrate_national <- read_bls("metadata/bls/raw/ln.data.1.AllData.txt") |>
  filter(
    series_id == "LNS14000000",
    period != "M13"
  ) |>
  transmute(
    year,
    month = as.integer(str_remove(period, "M")),
    unrate_us = value
  )

write_csv(unrate_national, "metadata/bls/formatted/unrate_national.csv", na = "")

#### State unemployment: LASST{FIPS}0000000000003 (SA monthly) ####

download_bls(
  "https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS",
  "metadata/bls/raw/la.data.3.AllStatesS.txt"
)

# State unemployment rate series follow the pattern:
#   LA + S (seasonal) + ST (statewide) + 2-digit FIPS + 11 zeros + 03 (rate)
# That gives 20-char IDs like LASST060000000000003 (California).
unrate_state <- read_bls("metadata/bls/raw/la.data.3.AllStatesS.txt") |>
  filter(
    str_detect(series_id, "^LASST[0-9]{2}0{11}03$"),
    period != "M13"
  ) |>
  transmute(
    state_fips = str_sub(series_id, 6, 7),
    year,
    month = as.integer(str_remove(period, "M")),
    unrate_state = value
  )

write_csv(unrate_state, "metadata/bls/formatted/unrate_state.csv", na = "")
