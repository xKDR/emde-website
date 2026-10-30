# ---------------------------------------------------------------------------
# plots.R : shared theme, branding and plot builders used by every page.
# `logo` is a path to the XKDR logo *relative to the page being rendered*.
# ---------------------------------------------------------------------------

emde_font <- "Latin Modern Roman"
# Paul Tol "bright" qualitative palette (same colours khroma::scale_*_bright() gives)
tol_bright <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")

xkdr_theme <- function(base_size = 10) {
  ggplot2::theme_bw(base_family = emde_font, base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_blank(),
      axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks.length  = ggplot2::unit(0.15, "cm"),
      axis.ticks         = ggplot2::element_line(color = "black", linewidth = 0.5),
      axis.text.x        = ggplot2::element_text(size = base_size, color = "black", margin = ggplot2::margin(t = 7)),
      axis.text.y        = ggplot2::element_text(size = base_size, color = "black", margin = ggplot2::margin(r = 7)),
      axis.title.y       = ggplot2::element_text(size = base_size, color = "black", margin = ggplot2::margin(r = 6)),
      plot.margin        = ggplot2::margin(t = 6, r = 8, b = 6, l = 6, unit = "pt"),
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = base_size - 2, color = "black")
    )
}

add_xkdr_branding <- function(ply_obj, logo) {
  logo_uri <- knitr::image_uri(logo)
  ply_obj |>
    plotly::layout(
      images = list(list(
        source = logo_uri, xref = "paper", yref = "paper",
        x = 0.98, y = -0.03, sizex = 0.075, sizey = 0.075,
        xanchor = "right", yanchor = "top", opacity = 1
      )),
      annotations = list(list(
        x = 1, y = -0.12, text = "Source: XKDR Forum", showarrow = FALSE,
        xref = "paper", yref = "paper", xanchor = "right", yanchor = "top",
        font = list(size = 14, color = "#6c757d", family = emde_font)
      ))
    ) |>
    plotly::config(displaylogo = FALSE)
}

# --- Country page: level of the exchange rate with break dates ---------------
plot_level <- function(res, label, logo) {
  df <- res$level
  bd <- res$break_dates
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = rate)) +
    ggplot2::geom_line(linewidth = 2 / ggplot2::.pt, color = "#EE6677") +
    ggplot2::geom_vline(xintercept = as.numeric(bd), color = "grey50",
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::labs(x = NULL, y = label) +
    xkdr_theme(16) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(mult = c(0.04, 0.04)))
  ply <- plotly::ggplotly(p, tooltip = c("x", "y"))

  if (length(bd) > 0 && !any(is.na(bd))) {
    med <- stats::median(df$rate, na.rm = TRUE)
    for (b in bd) {
      idx  <- which.min(abs(as.numeric(df$date) - b))
      high <- df$rate[idx] > med
      ply <- plotly::add_annotations(
        ply, x = b, y = if (high) 0.02 else 0.98, xref = "x", yref = "paper",
        text = format(as.Date(b, origin = "1970-01-01"), "%Y-%m"), showarrow = FALSE,
        textangle = -90, xanchor = "right", yanchor = if (high) "bottom" else "top",
        font = list(color = "grey50", size = 16, family = emde_font)
      )
    }
  }
  add_xkdr_branding(ply, logo)
}

# --- Country page: evolution of weights --------------------------------------
plot_weights <- function(res, logo) {
  p <- ggplot2::ggplot(res$weights, ggplot2::aes(x = date, y = Weight, color = Currency)) +
    ggplot2::geom_line(linewidth = 2 / ggplot2::.pt) +
    ggplot2::scale_color_manual(values = tol_bright) +
    ggplot2::labs(x = NULL, y = "Coefficients", color = NULL) +
    xkdr_theme(16) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(mult = c(0.04, 0.04)))
  plotly::ggplotly(p, tooltip = c("x", "y", "Currency")) |> add_xkdr_branding(logo)
}

# --- Landing page: R-squared timeline, one row per country (ISO3) ------------
# regimes : the stacked summary table (iso3, country, start, end, r2, ...)
# order   : ISO3 codes in display order (top to bottom)
# events  : data.frame(label, date)
plot_timeline <- function(regimes, order, events, logo) {
  n <- length(order)
  d <- regimes |>
    dplyr::filter(iso3 %in% order) |>
    dplyr::arrange(iso3, start) |>
    dplyr::group_by(iso3) |>
    dplyr::mutate(xmax = dplyr::lead(start, default = dplyr::last(end))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row  = match(iso3, order),
      ymin = n - row + 0.06,
      ymax = n - row + 0.94,
      text = sprintf("%s (%s)<br>%s to %s<br>R\u00b2 = %s",
                     country, iso3, format(start), format(end),
                     ifelse(is.na(r2), "n/a", sprintf("%.2f", r2)))
    )

  pal <- c("white", "#d9d6f7", "#9a9be3", "#3a6fb5", "#1d2f66", "#150b34")
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = start, xmax = xmax, ymin = ymin, ymax = ymax,
                                    fill = r2, text = text)) +
    ggplot2::scale_fill_gradientn(colours = pal, limits = c(0, 1), name = "R\u00b2",
                                  na.value = "grey85") +
    ggplot2::scale_y_continuous(breaks = n - seq_len(n) + 0.5, labels = order,
                                expand = c(0, 0)) +
    ggplot2::scale_x_date(expand = c(0, 0), date_breaks = "5 years", date_labels = "%Y") +
    ggplot2::labs(x = NULL, y = NULL, title = "Emerging Markets: R\u00b2 Timeline") +
    xkdr_theme(10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.ticks.length.y = ggplot2::unit(0, "cm"))

  if (!is.null(events) && nrow(events) > 0) {
    events$date <- as.Date(events$date)
    p <- p +
      ggplot2::geom_vline(data = events, ggplot2::aes(xintercept = as.numeric(date), colour = label),
                          linetype = "dashed", linewidth = 0.5) +
      ggplot2::scale_colour_manual(values = rep(tol_bright, length.out = nrow(events)), name = NULL)
  }

  plotly::ggplotly(p, tooltip = "text", height = max(600, 16 * n + 160)) |>
    plotly::layout(
      margin = list(b = 190),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.17, yanchor = "top")
    ) |>
    add_xkdr_branding(logo)
}
