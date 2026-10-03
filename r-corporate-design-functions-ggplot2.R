# ---------------------------------------------------------------------------
# r-corporate-design-functions-ggplot2.R
#
# Corporate design for ggplot2 charts. Everything brand-specific lives in the
# `corporate_design` list below; to re-brand, edit that list and nothing else.
# The functions only read from it.
#
# Usage
#   source("r-corporate-design-functions-ggplot2.R")
#   options(corporate_design.mode = "dark")   # optional, default "light"
#   g <- ggplot(...) + scale_fill_corporate_design() + theme_corporate_design()
#   save_chart(g, "chart-name", lang = "de", format = "portrait")
#
# Provides
#   corporate_design                 the configuration (colours, font, sizes)
#   colours_corporate_design         categorical colours, light mode (vector)
#   font_corporate_design            the font family in use ("" = default)
#   linewidth_corporate_design       default line width for line charts
#   formats_corporate_design         export sizes in pixels
#   design_mode()                    "light" or "dark"
#   design_colours()                 named colours of the active mode
#   palette_corporate_design()       "cat", "diverging" or "seq" palettes
#   theme_corporate_design()         the chart theme; `...` overrides elements
#   scale_{colour,fill}_corporate_design()   categorical scales
#   scale_{colour,fill}_highlight()  colour a few series, grey out the rest
#   geom_zeroline()                  baseline at zero (horizontal or vertical)
#   label_last(), wrap_labels()      labelling helpers
#   caption_corporate_design()       standard source line
#   save_chart()                     export at exact pixel sizes
# ---------------------------------------------------------------------------

library(ggplot2)

# -- Configuration: the only part to change when re-branding ----------------
corporate_design <- list(

  # First installed family wins; "" falls back to the device default.
  fonts = c("Roboto Condensed", "Source Sans 3", "Source Sans Pro",
            "Fira Sans Condensed", "Segoe UI", "Corbel"),

  # Series colours (in palette order) plus the roles the theme uses:
  # ink = primary text, ink_soft = secondary text, grid = gridlines,
  # rest = residual "Other" category, mute = background series,
  # paper = canvas, neutral = midpoint of diverging palettes,
  # seq_low = light end of sequential palettes.
  colours = list(
    light = c(
      red      = "#C10016",
      blue     = "#0F4C81",
      amber    = "#E8A33D",
      teal     = "#17807A",
      violet   = "#7A3B9B",
      rose     = "#D4587E",
      olive    = "#6E8C21",
      rust     = "#B4591F",
      slate    = "#5B6D7E",
      ink      = "#22282E",
      ink_soft = "#5C666F",
      grid     = "#DCE1E5",
      rest     = "#9AA5AE",
      mute     = "#C9D2D8",
      paper    = "#FFFFFF",
      neutral  = "#E9D9C8",
      seq_low  = "#EDF2F6"
    ),
    # Hues are lifted so they keep their contrast on the dark canvas.
    dark = c(
      red      = "#F2445A",
      blue     = "#5AA2E6",
      amber    = "#F2B54F",
      teal     = "#2EB8A8",
      violet   = "#AE7AD6",
      rose     = "#EE84A6",
      olive    = "#A0C247",
      rust     = "#E5813F",
      slate    = "#8FA1B3",
      ink      = "#ECEFF2",
      ink_soft = "#9AA6B1",
      grid     = "#2C353D",
      rest     = "#56626D",
      mute     = "#3A444D",
      paper    = "#14191E",
      neutral  = "#4A4540",
      seq_low  = "#3A444D"
    )
  ),

  # Order of the categorical palette. The first four are the most
  # distinguishable, including under red-green colour blindness.
  series = c("red", "blue", "amber", "teal", "violet",
             "rose", "olive", "rust", "slate"),

  base_size = 13,
  linewidth = .9,

  # Export sizes in pixels; dpi keeps point sizes predictable.
  formats = list(
    portrait  = c(1080, 1350),  # 4:5, the default
    square    = c(1080, 1080),
    story     = c(1080, 1920),  # 9:16
    landscape = c(1200,  675)   # 16:9
  ),
  dpi = 150,

  # Caption wording per language. `handle`, if set, is appended to every
  # caption on export, e.g. c(de = "@account_de", en = "@account_en").
  caption = list(
    de = list(src = "Quelle: ", own = "Eigene Berechnungen.",
              last = "Letzter Wert: "),
    en = list(src = "Source: ", own = "Own calculations.",
              last = "Latest observation: ")
  ),
  handle = NULL
)

# -- Derived values ----------------------------------------------------------
font_corporate_design <- local({
  have <- tryCatch(unique(systemfonts::system_fonts()$family),
                   error = function(e) character())
  hit <- corporate_design$fonts[corporate_design$fonts %in% have]
  if (length(hit)) hit[1] else ""
})

# The standard Windows screen device (RStudio's default backend) only knows
# fonts registered with windowsFonts(); register ours so previews do not warn
# "Zeichensatzfamilie in der Windows Zeichensatzdatenbank nicht gefunden".
if (.Platform$OS.type == "windows" && nzchar(font_corporate_design) &&
    !font_corporate_design %in% names(grDevices::windowsFonts())) {
  do.call(grDevices::windowsFonts,
          stats::setNames(list(grDevices::windowsFont(font_corporate_design)),
                          font_corporate_design))
}

colours_corporate_design   <- unname(corporate_design$colours$light[corporate_design$series])
linewidth_corporate_design <- corporate_design$linewidth
formats_corporate_design   <- corporate_design$formats

# -- Colours and palettes ----------------------------------------------------

# Active mode. Read at call time, so set the option before building the plot.
design_mode <- function() {
  mode <- getOption("corporate_design.mode", "light")
  if (!mode %in% names(corporate_design$colours)) {
    stop("corporate_design.mode must be one of: ",
         paste(names(corporate_design$colours), collapse = ", "))
  }
  mode
}

design_colours <- function(mode = design_mode()) corporate_design$colours[[mode]]

#   "cat"       categorical, in the order of corporate_design$series
#   "diverging" negative -> neutral -> positive
#   "seq"       single-hue ramp, light to dark
palette_corporate_design <- function(type = "cat", n = NULL, hue = "blue",
                                     mode = design_mode()) {
  cols <- design_colours(mode)
  out <- switch(
    type,
    cat       = cols[corporate_design$series],
    diverging = cols[c("red", "neutral", "blue")],
    seq       = grDevices::colorRampPalette(
      c(cols[["seq_low"]], cols[[hue]]))(if (is.null(n)) 6 else n),
    stop("unknown palette type: ", type)
  )
  out <- unname(out)
  if (!is.null(n) && type == "cat") {
    out <- if (n > length(out)) grDevices::colorRampPalette(out)(n) else out[seq_len(n)]
  }
  out
}

scale_colour_corporate_design <- function(...) {
  ggplot2::scale_colour_manual(..., values = palette_corporate_design("cat"))
}
scale_fill_corporate_design <- function(...) {
  ggplot2::scale_fill_manual(..., values = palette_corporate_design("cat"))
}
scale_color_corporate_design <- scale_colour_corporate_design

# Real colours for the series that carry the story, a neutral grey for the
# residual level so it stops competing for attention.
highlight_values <- function(highlight, rest = "Other", rest_colour = NULL) {
  vals <- stats::setNames(palette_corporate_design("cat", length(highlight)), highlight)
  if (is.null(rest_colour)) rest_colour <- unname(design_colours()[["rest"]])
  c(vals, stats::setNames(rep(rest_colour, length(rest)), rest))
}

scale_colour_highlight <- function(highlight, rest = "Other", rest_colour = NULL,
                                   breaks = NULL, ...) {
  vals <- highlight_values(highlight, rest, rest_colour)
  ggplot2::scale_colour_manual(values = vals,
                               breaks = if (is.null(breaks)) names(vals) else breaks, ...)
}

scale_fill_highlight <- function(highlight, rest = "Other", rest_colour = NULL,
                                 breaks = NULL, ...) {
  vals <- highlight_values(highlight, rest, rest_colour)
  ggplot2::scale_fill_manual(values = vals,
                             breaks = if (is.null(breaks)) names(vals) else breaks, ...)
}

scale_color_highlight <- scale_colour_highlight

# -- Theme -------------------------------------------------------------------
# base_size is the point size of body text; every other size derives from it.
# grid is "y" (default), "x", "both" or "none". Further arguments are passed
# to ggplot2::theme() and override the defaults.
theme_corporate_design <- function(base_size = corporate_design$base_size,
                                   grid = "y", ticks = FALSE,
                                   mode = design_mode(), ...) {

  cols  <- design_colours(mode)
  ink   <- unname(cols[["ink"]])
  soft  <- unname(cols[["ink_soft"]])
  line  <- unname(cols[["grid"]])
  paper <- unname(cols[["paper"]])
  ff    <- font_corporate_design

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = ff) +
    ggplot2::theme(
      # canvas: generous, even margins so the chart breathes in a feed
      plot.background  = ggplot2::element_rect(fill = paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = paper, colour = NA),
      plot.margin      = ggplot2::margin(base_size * 1.4, base_size * 1.4,
                                         base_size, base_size * 1.4),

      # title block aligns to the image edge, not to the panel edge
      plot.title.position = "plot",
      plot.title    = ggplot2::element_text(size = base_size * 1.65, face = "bold",
                                            colour = ink, family = ff,
                                            margin = ggplot2::margin(b = base_size * .35),
                                            lineheight = 1.1),
      plot.subtitle = ggplot2::element_text(size = base_size * 1.1, colour = soft,
                                            family = ff,
                                            margin = ggplot2::margin(b = base_size * 1.1),
                                            lineheight = 1.15),
      plot.caption.position = "plot",
      plot.caption  = ggplot2::element_text(size = base_size * .72, colour = soft,
                                            family = ff, hjust = 0, lineheight = 1.2,
                                            margin = ggplot2::margin(t = base_size)),

      # grid: one direction only, light enough to sit behind the data
      panel.grid.major = ggplot2::element_line(colour = line, linewidth = .4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),

      # axes: no frame, the labels do the work
      axis.line   = ggplot2::element_blank(),
      axis.ticks  = if (ticks) ggplot2::element_line(colour = line, linewidth = .4) else ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(base_size * .25, "pt"),
      axis.text   = ggplot2::element_text(size = base_size * .85, colour = soft, family = ff),
      axis.title  = ggplot2::element_text(size = base_size * .85, colour = soft, family = ff),

      # facets read as small headings, not as boxed labels
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = base_size * .95, face = "bold",
                                         colour = ink, family = ff, hjust = 0,
                                         margin = ggplot2::margin(b = base_size * .3)),
      panel.spacing = ggplot2::unit(base_size * 1.2, "pt"),

      # legend: top-left and compact, so it reads as part of the header
      legend.position      = "top",
      legend.location      = "plot",   # align with the title, not the panel
      legend.justification = "left",
      legend.direction     = "horizontal",
      legend.title         = ggplot2::element_blank(),
      legend.background    = ggplot2::element_blank(),
      legend.key           = ggplot2::element_blank(),
      legend.text          = ggplot2::element_text(size = base_size * .8, colour = ink,
                                                   family = ff),
      legend.key.size      = ggplot2::unit(base_size * .8, "pt"),
      legend.margin        = ggplot2::margin(b = base_size * .6),
      legend.box.spacing   = ggplot2::unit(0, "pt")
    )

  th <- th + switch(
    grid,
    y    = ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()),
    x    = ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()),
    both = ggplot2::theme(),
    none = ggplot2::theme(panel.grid.major = ggplot2::element_blank()),
    ggplot2::theme()
  )

  th + ggplot2::theme(...)
}

# -- Building blocks ---------------------------------------------------------

# A zero line that reads as a baseline rather than as another gridline.
# Use x = 0 for horizontal bar charts, where the baseline is vertical.
geom_zeroline <- function(y = 0, x = NULL, ...) {
  ink <- unname(design_colours()[["ink"]])
  if (!is.null(x)) {
    ggplot2::geom_vline(xintercept = x, colour = ink, linewidth = .5, ...)
  } else {
    ggplot2::geom_hline(yintercept = y, colour = ink, linewidth = .5, ...)
  }
}

# Last observation per group -- feed this to geom_text() to label lines
# directly instead of spending vertical space on a legend.
label_last <- function(data, x, group) {
  x <- rlang::ensym(x)
  group <- rlang::ensym(group)
  dplyr::ungroup(dplyr::filter(dplyr::group_by(data, !!group), !!x == max(!!x)))
}

# Wrap long category names so axes and multi-column legends cannot clip them.
# Official COICOP and COFOG titles run to 50+ characters.
wrap_labels <- function(x, width = 30) {
  vapply(as.character(x),
         function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

# Source line in the house wording, so attribution is identical everywhere.
caption_corporate_design <- function(source, last = NULL, note = NULL, lang = "de") {
  lab <- corporate_design$caption[[lang]]
  if (is.null(lab)) stop("no caption wording for lang = \"", lang, "\"")
  txt <- paste0(lab$src, source, " ", lab$own)
  if (!is.null(last)) txt <- paste0(txt, " ", lab$last, last, ".")
  if (!is.null(note)) txt <- paste0(txt, "\n", note)
  txt
}

# -- Export ------------------------------------------------------------------
# save_chart(g, "government-debt-to-income", lang = "de")
# writes <dir>/<date>-<name>[-<lang>][-dark].jpeg at the exact pixel size of
# `format`, on the canvas colour of the active mode.
save_chart <- function(plot, name, lang = NULL, format = "portrait",
                       dir = "pics", dpi = corporate_design$dpi,
                       date = Sys.Date()) {
  dims <- corporate_design$formats[[format]]
  if (is.null(dims)) {
    stop("format must be one of: ", paste(names(corporate_design$formats), collapse = ", "))
  }
  handle <- corporate_design$handle
  if (!is.null(handle)) {
    tag <- if (!is.null(lang) && !is.null(names(handle))) handle[[lang]] else handle[[1]]
    cap <- plot$labels$caption
    if (is.null(cap) || !grepl(tag, cap, fixed = TRUE)) {
      plot <- plot + ggplot2::labs(caption = if (is.null(cap)) tag else paste0(cap, "\n", tag))
    }
  }
  mode <- design_mode()
  file <- paste0(format(date, "%Y%m%d"), "-", name,
                 if (!is.null(lang)) paste0("-", lang) else "",
                 if (mode != "light") paste0("-", mode) else "", ".jpeg")
  path <- file.path(dir, file)
  ggplot2::ggsave(path, plot = plot,
                  width = dims[1] / dpi, height = dims[2] / dpi, dpi = dpi,
                  units = "in", device = ragg::agg_jpeg, quality = 92,
                  bg = unname(design_colours(mode)[["paper"]]))
  message("wrote ", path, " (", dims[1], "x", dims[2], ")")
  invisible(path)
}

# -- Legacy ------------------------------------------------------------------
# Objects the older scripts add to their plots directly.
theme_instagram    <- theme_corporate_design(base_size = 12, grid = "y")
scale_colour_insta <- ggplot2::scale_colour_manual(values = colours_corporate_design)
scale_fill_insta   <- ggplot2::scale_fill_manual(values = colours_corporate_design)
