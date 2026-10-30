#!/usr/bin/env Rscript
# =============================================================================
# build.R : the single entry point.
#
#   Rscript build.R                  fetch (cached) -> fit all -> generate pages
#   Rscript build.R KR IN            only (re)process these ISO2C codes
#   Rscript build.R --refresh-data   re-download BIS data first
#   Rscript build.R --force          ignore the per-country cache and refit
#   Rscript build.R --render         run `quarto render` at the end
#
# Edit config.yml, never this file, to add or change countries.
# =============================================================================

args   <- commandArgs(trailingOnly = TRUE)
flags  <- args[startsWith(args, "--")]
subset <- toupper(args[!startsWith(args, "--")])

if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(yaml, countrycode, dplyr, tidyr, readr, curl, digest, zoo, fxregime)

`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("R/fit.R", "R/fetch.R", "R/emit.R")) source(f)

dir.create("results",   showWarnings = FALSE)
dir.create("countries", showWarnings = FALSE)

# ---- 1. config --------------------------------------------------------------
cfg <- yaml::read_yaml("config.yml")

entries <- lapply(cfg$countries, function(x) if (is.character(x)) list(code = x) else x)
meta <- do.call(rbind, lapply(entries, function(e) {
  iso2 <- toupper(e$code)
  iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
  if (is.na(iso3)) stop("config.yml: '", iso2, "' is not a valid ISO2C code")
  s <- utils::modifyList(cfg$defaults, e[setdiff(names(e), "code")])
  s$start <- as.Date(as.character(s$start))
  if (!is.null(s$end)) s$end <- as.Date(as.character(s$end))
  data.frame(
    iso2 = iso2, iso3 = iso3,
    name = countrycode::countrycode(iso2, "iso2c", "country.name", warn = FALSE),
    region = dplyr::coalesce(countrycode::countrycode(iso2, "iso2c", "region", warn = FALSE), "Other"),
    ccy = countrycode::countrycode(iso2, "iso2c", "iso4217c", warn = FALSE),
    stringsAsFactors = FALSE
  ) |> (\(d) { d$settings <- list(s); d })()
}))
if (anyDuplicated(meta$iso2)) stop("config.yml: duplicate country codes")

# ---- 2. data (one download) -------------------------------------------------
anchors <- cfg$anchor_areas
panel   <- fetch_bis_panel(cfg, areas = c(meta$iso2, unlist(anchors)),
                           refresh = "--refresh-data" %in% flags)
fit_sig <- digest::digest(readLines("R/fit.R", warn = FALSE))

# ---- 3. fit each country (parallel, cached) ---------------------------------
process_one <- function(i) {
  m <- meta[i, ]; s <- m$settings[[1]]
  out <- function(status, msg = "") data.frame(iso2 = m$iso2, iso3 = m$iso3, status = status, msg = msg)
  rds <- file.path("results", paste0(m$iso3, ".rds"))

  if (length(subset) > 0 && !m$iso2 %in% subset)
    return(out(if (file.exists(rds)) "cached" else "not built"))

  tryCatch({
    if (is.na(m$ccy)) stop("no currency code for this country")
    if (m$ccy == s$numeraire) stop("country currency is the numeraire")
    if (m$ccy == "USD") stop("USD is the anchor, nothing to estimate")

    area <- if (m$iso2 %in% names(panel)) m$iso2 else if (!is.null(anchors[[m$ccy]])) anchors[[m$ccy]] else NA
    if (is.na(area) || !area %in% names(panel)) stop("no BIS series found (ref area ", m$iso2, ")")

    fx   <- make_fx_data(panel, area, m$ccy, s, anchors)
    if (nrow(fx) < 3 * s$h) stop("only ", nrow(fx), " usable daily observations")
    hash <- digest::digest(list(s, area, m$ccy, fx, fit_sig))

    if (!"--force" %in% flags && file.exists(rds) && identical(readRDS(rds)$hash, hash))
      return(out("cached"))

    fit <- fit_country(fx, m$ccy, s$regressors, s$h, s$breaks, s$ic, s$frequency)
    saveRDS(list(iso2 = m$iso2, iso3 = m$iso3, name = m$name, region = m$region,
                 settings = s, fx_data = fx, fit = fit, hash = hash), rds)
    out(if (nzchar(fit$note)) "fitted (fallback)" else "fitted", fit$note)
  }, error = function(e) out("failed", conditionMessage(e)))
}

cores <- if (.Platform$OS.type == "windows") 1L else as.integer(cfg$cores %||% 1L)
report <- dplyr::bind_rows(parallel::mclapply(seq_len(nrow(meta)), process_one, mc.cores = cores))

# ---- 4. emit pages, summary, sidebar ---------------------------------------
built <- list()
for (i in seq_len(nrow(meta))) {
  rds <- file.path("results", paste0(meta$iso3[i], ".rds"))
  if (file.exists(rds) && !report$status[i] %in% "failed") built[[meta$iso3[i]]] <- readRDS(rds)
}
for (r in built) emit_country(r)

# remove pages for countries no longer in the config
for (d in setdiff(list.files("countries"), names(built))) unlink(file.path("countries", d), recursive = TRUE)

regimes <- dplyr::bind_rows(lapply(built, function(r)
  cbind(iso3 = r$iso3, country = r$name, r$fit$regimes, stringsAsFactors = FALSE)))
events <- if (length(cfg$events) > 0)
  do.call(rbind, lapply(cfg$events, function(e)
    data.frame(label = e$label, date = as.Date(as.character(e$date))))) else NULL

saveRDS(list(regimes = regimes, order = names(built), events = events, built = Sys.time()),
        "results/summary.rds")
utils::write.csv(regimes, "emde_regimes_all.csv", row.names = FALSE)
emit_sidebar(do.call(rbind, lapply(built, function(r)
  data.frame(iso3 = r$iso3, name = r$name, region = r$region))))

utils::write.csv(report, "results/build_report.csv", row.names = FALSE)
cat("\n--- Build report ---\n"); print(report, row.names = FALSE)
cat(sprintf("\n%d of %d countries have pages.\n", length(built), nrow(meta)))

if ("--render" %in% flags) system2("quarto", "render")
