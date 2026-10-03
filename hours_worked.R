# Average annual hours worked per person in employment ------------------------
# A replacement for PWT's avh, built the way the growth literature does it:
#
# - OECD members: OECD average annual hours actually worked per worker
#   (national accounts based, DSD_HW@DF_AVG_ANN_HRS_WKD). For most of them this
#   is identical to PWT avh, but the current OECD vintage differs for some
#   (Mexico about +37 %, USA about +1.5 %).
# - Everyone else: ILO mean weekly hours actually worked per employed person
#   from household surveys (DF_HOW_TEMP_SEX_ECO_NB), scaled to annual hours
#   with the median OECD-annual / ILO-weekly ratio of the countries in both
#   (about 45.5 weeks; PWT effectively uses about 51 and so ignores holidays).
#   Labour force surveys are preferred over household income surveys, main-job
#   hours over main-and-second-job hours.
#
# `series` marks comparable stretches: each OECD country is one series, an
# ILO country starts a new one whenever the survey type or job coverage
# changes or the ILO flags a break. Growth rates should not be computed
# across a change of `series`, and level regressions should use country x
# series fixed effects.
#
# pop1564: World Bank working-age population (SP.POP.1564.TO), millions, for
# hours per adult.
#
# Usage: source("hours_worked.R"); hrs <- load_hours()
# Returns iso, year, hours, source, series, pop1564. The raw downloads and the
# finished table are kept in D:/projects/MacroData. The sources are revised
# regularly, so everything is downloaded and rebuilt again once the table is
# older than `max_age` days or when refresh = TRUE.

library(dplyr)

load_hours <- function(refresh = FALSE, max_age = 30) {
  data_dir <- "D:/projects/MacroData"
  cache <- file.path(data_dir, "hours_worked.rds")
  if (!refresh && file.exists(cache) &&
      difftime(Sys.time(), file.mtime(cache), units = "days") < max_age) {
    return(readRDS(cache))
  }

  get <- function(url, dest, accept = NULL) {
    # Downloaded to a temporary file first, so a failed request never
    # overwrites the copy kept in the data folder
    tmp <- tempfile()
    dest <- file.path(data_dir, dest)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    for (attempt in 1:5) {
      ok <- tryCatch({
        download.file(url, tmp, mode = "wb", quiet = TRUE,
                      headers = if (!is.null(accept)) c(Accept = accept))
        file.size(tmp) > 0
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) {
        args <- c("-sfL", if (!is.null(accept)) c("-H", shQuote(paste("Accept:", accept))),
                  "-o", shQuote(tmp), shQuote(url))
        ok <- system2("curl", args) == 0 && file.size(tmp) > 0
      }
      if (ok) {
        file.copy(tmp, dest, overwrite = TRUE)
        return(dest)
      }
      Sys.sleep(2^attempt)
    }
    stop("download failed: ", url)
  }

  oecd <- read.csv(get(paste0("https://sdmx.oecd.org/public/rest/data/",
                              "OECD.ELS.SAE,DSD_HW@DF_AVG_ANN_HRS_WKD,/all?startPeriod=1950"),
                       "oecd/oecd_hrs.csv",
                       accept = "application/vnd.sdmx.data+csv; charset=utf-8")) %>%
    filter(WORKER_STATUS == "_T", !is.na(OBS_VALUE)) %>%
    transmute(iso = REF_AREA, year = as.integer(TIME_PERIOD), hours = OBS_VALUE)

  ilo <- read.csv(get(paste0("https://sdmx.ilo.org/rest/data/ILO,DF_HOW_TEMP_SEX_ECO_NB/",
                             ".A..SEX_T.ECO_AGGREGATE_TOTAL?format=csv"),
                      "ilo/ilo_how.csv")) %>%
    # Monthly or hourly time units and implausible weekly values are left out
    filter(!grepl("Time unit", NOTE_INDICATOR), OBS_VALUE > 15, OBS_VALUE < 70,
           grepl("^(LFS|HS|HIES)", SOURCE)) %>%
    transmute(iso = REF_AREA, year = as.integer(TIME_PERIOD), weekly = OBS_VALUE,
              survey = sub(" -.*", "", SOURCE),
              job = ifelse(grepl("second job", NOTE_INDICATOR), "all", "main"),
              brk = grepl("Break in series", paste(NOTE_INDICATOR, NOTE_SOURCE))) %>%
    mutate(prio = match(survey, c("LFS", "HS", "HIES")) + (job == "all") * 3) %>%
    group_by(iso, year) %>%
    slice_min(prio, n = 1, with_ties = FALSE) %>%
    group_by(iso) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(series = paste0("ILO", cumsum(c(TRUE, survey[-1] != survey[-n()] |
                                               job[-1] != job[-n()] | brk[-1])))) %>%
    ungroup()

  weeks <- inner_join(oecd, ilo, by = c("iso", "year")) %>%
    with(median(hours / weekly))

  wb <- jsonlite::fromJSON(get(paste0("https://api.worldbank.org/v2/country/all/indicator/",
                                      "SP.POP.1564.TO?format=json&per_page=20000&date=1950:2030"),
                                  "worldbank/wb_pop1564.json"))[[2]]
  wa <- tibble(iso = wb$countryiso3code, year = as.integer(wb$date),
               pop1564 = wb$value / 1e6) %>%
    filter(iso != "", !is.na(pop1564))

  hrs <- bind_rows(
    mutate(oecd, source = "OECD", series = "OECD"),
    ilo %>%
      filter(!iso %in% oecd$iso) %>%
      transmute(iso, year, hours = weekly * weeks, source = "ILO", series)
  ) %>%
    left_join(wa, by = c("iso", "year")) %>%
    arrange(iso, year)
  attr(hrs, "weeks_per_year") <- weeks

  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(hrs, cache)
  hrs
}
