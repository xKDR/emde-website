# ---------------------------------------------------------------------------
# fetch.R : one bulk BIS download, cached, turned into a wide panel of
#           local-currency-units per USD (one column per BIS reference area).
# ---------------------------------------------------------------------------

# Downloads one small CSV per BIS reference area (the countries in config.yml
# plus the anchor currencies) instead of one huge file, which BIS cuts off.
# Each area is cached in data/raw/bis/<AREA>.csv, so a re-run only downloads
# what is missing. A "no data" area (HTTP 404) is cached as an empty marker.
fetch_bis_panel <- function(cfg, areas, cache_dir = "data/raw/bis", refresh = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  areas <- sort(unique(areas))
  failed <- character(0)

  for (a in areas) {
    f <- file.path(cache_dir, paste0(a, ".csv"))
    if (file.exists(f) && !refresh) next
    url <- sprintf(
      "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/D.%s..A?format=csv&startPeriod=%s",
      a, cfg$bis$from
    )
    ok <- FALSE
    for (attempt in 1:4) {
      tmp <- tempfile(fileext = ".csv")
      h <- curl::new_handle(connecttimeout = 30, timeout = 300)
      res <- tryCatch({ curl::curl_download(url, tmp, handle = h, quiet = TRUE); "ok" },
                      error = function(e) conditionMessage(e))
      if (identical(res, "ok") && file.exists(tmp) && file.size(tmp) > 0) {
        file.copy(tmp, f, overwrite = TRUE); ok <- TRUE; break
      }
      if (grepl("404", res)) { writeLines("", f); ok <- TRUE; message("  ", a, ": no BIS data"); break }
      message("  ", a, ": attempt ", attempt, " failed (", res, ")")
      Sys.sleep(2 * attempt)
    }
    if (ok) message("  fetched ", a) else failed <- c(failed, a)
  }
  if (length(failed) > 0)
    warning("Could not download: ", paste(failed, collapse = ", "), ". Re-run to retry.", call. = FALSE)

  files <- file.path(cache_dir, paste0(areas, ".csv"))
  files <- files[file.exists(files) & file.size(files) > 1]
  if (length(files) == 0) stop("No BIS data available")

  raw <- dplyr::bind_rows(lapply(files, function(f)
    readr::read_csv(f, col_select = c(TIME_PERIOD, REF_AREA, OBS_VALUE),
                    col_types = readr::cols(.default = "c"), progress = FALSE)))
  raw |>
    dplyr::mutate(date = as.Date(TIME_PERIOD), OBS_VALUE = as.numeric(OBS_VALUE)) |>
    dplyr::filter(!is.na(OBS_VALUE), !is.na(date)) |>
    dplyr::group_by(date, REF_AREA) |>
    dplyr::summarise(v = dplyr::first(OBS_VALUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = REF_AREA, values_from = v) |>
    dplyr::arrange(date) |>
    as.data.frame()
}

# Build the input data.frame for one country: everything expressed in numeraire
# units, as in the original korea.qmd (KRW = KR / NZ, USD = 1 / NZ, ...).
make_fx_data <- function(panel, area, ccy, settings, anchors) {
  num_area <- anchors[[settings$numeraire]]
  if (is.null(num_area) || !num_area %in% names(panel))
    stop("numeraire ", settings$numeraire, " not available in BIS data")
  num <- panel[[num_area]]

  out <- data.frame(date = panel$date)
  out[[ccy]] <- panel[[area]] / num
  out$USD    <- 1 / num
  for (r in setdiff(settings$regressors, c("USD", ccy))) {
    a <- anchors[[r]]
    if (is.null(a) || !a %in% names(panel)) stop("regressor ", r, " has no BIS area in anchor_areas")
    out[[r]] <- panel[[a]] / num
  }

  out <- out[out$date >= as.Date(settings$start), , drop = FALSE]
  if (!is.null(settings$end)) out <- out[out$date <= as.Date(settings$end), , drop = FALSE]
  cols <- unique(c(ccy, "USD", setdiff(settings$regressors, ccy)))
  out <- out[stats::complete.cases(out[, cols]), c("date", cols), drop = FALSE]
  rownames(out) <- NULL
  out
}
