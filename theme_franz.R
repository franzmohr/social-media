# ---------------------------------------------------------------------------
# theme_franz.R -- design system for the @franzmohr_de / @franzmohr_en charts
#
# Provides
#   franz_colours   named brand colours
#   franz_pal()     palette generator ("cat", "diverging", "seq")
#   scale_*_franz() ggplot2 scales built on those palettes
#   scale_*_highlight()  highlight a few series, grey out the rest
#   theme_franz()   the chart theme (base_size scales everything)
#   theme_instagram, scale_colour_insta, scale_fill_insta
#                   backwards-compatible aliases used by the older scripts
#   save_post()     export at exact social-media pixel sizes
#   franz_caption() build a source line with the account handle appended
#   label_last()    helper for direct labelling at the end of lines
# ---------------------------------------------------------------------------

library(ggplot2)

# -- Typography -------------------------------------------------------------
# Preference order. Install "Roboto Condensed" or "Source Sans 3" for a look
# that is not the Windows default; the chain falls back gracefully.
franz_font <- local({
  wanted <- c("Roboto Condensed", "Source Sans 3", "Source Sans Pro",
              "Fira Sans Condensed", "Segoe UI", "Corbel")
  have <- tryCatch(unique(systemfonts::system_fonts()$family),
                   error = function(e) character())
  hit <- wanted[wanted %in% have]
  if (length(hit)) hit[1] else ""
})

# -- Colours ----------------------------------------------------------------
franz_colours <- c(
  red      = "#C10016",  # brand anchor
  blue     = "#0F4C81",  # brand anchor
  amber    = "#E8A33D",
  teal     = "#17807A",
  violet   = "#7A3B9B",
  rose     = "#D4587E",
  olive    = "#6E8C21",
  rust     = "#B4591F",
  slate    = "#5B6D7E",
  ink      = "#22282E",  # primary text
  ink_soft = "#5C666F",  # secondary text
  grid     = "#DCE1E5",  # gridlines
  rest     = "#9AA5AE",  # the "Other" / residual category
  mute     = "#C9D2D8",  # de-emphasised background series
  paper    = "#FFFFFF"   # canvas
)

# Palettes.
#   "cat"       categorical; the first four are the most distinguishable,
#               including under red-green colour blindness
#   "diverging" negative -> neutral -> positive
#   "seq"       single-hue ramp, light to dark
franz_pal <- function(type = "cat", n = NULL, hue = "blue") {
  cat_pal <- unname(franz_colours[c("red", "blue", "amber", "teal", "violet",
                                    "rose", "olive", "rust", "slate")])
  out <- switch(
    type,
    cat       = cat_pal,
    diverging = c(franz_colours[["red"]], "#E9D9C8", franz_colours[["blue"]]),
    seq       = grDevices::colorRampPalette(
      c("#EDF2F6", franz_colours[[hue]]))(if (is.null(n)) 6 else n),
    stop("unknown palette type: ", type)
  )
  if (!is.null(n) && type == "cat") {
    out <- if (n > length(out)) grDevices::colorRampPalette(out)(n) else out[seq_len(n)]
  }
  unname(out)
}

scale_colour_franz <- function(...) scale_colour_manual(..., values = franz_pal("cat"))
scale_fill_franz   <- function(...) scale_fill_manual(..., values = franz_pal("cat"))
scale_color_franz  <- scale_colour_franz

# Highlight scales: real colours for the series that carry the story, neutral
# grey for the residual level so it stops competing for attention.
franz_highlight_values <- function(highlight, rest = "Other", rest_colour = NULL) {
  vals <- stats::setNames(franz_pal("cat", length(highlight)), highlight)
  if (is.null(rest_colour)) rest_colour <- unname(franz_colours[["rest"]])
  c(vals, stats::setNames(rep(rest_colour, length(rest)), rest))
}

scale_colour_highlight <- function(highlight, rest = "Other", rest_colour = NULL,
                                   breaks = NULL, ...) {
  vals <- franz_highlight_values(highlight, rest, rest_colour)
  scale_colour_manual(values = vals,
                      breaks = if (is.null(breaks)) names(vals) else breaks, ...)
}

scale_fill_highlight <- function(highlight, rest = "Other", rest_colour = NULL,
                                 breaks = NULL, ...) {
  vals <- franz_highlight_values(highlight, rest, rest_colour)
  scale_fill_manual(values = vals,
                    breaks = if (is.null(breaks)) names(vals) else breaks, ...)
}

# -- Theme ------------------------------------------------------------------
# base_size is the point size of body text; every other size derives from it.
# grid is "y" (default), "x", "both" or "none".
theme_franz <- function(base_size = 13, grid = "y", ticks = FALSE) {

  ink   <- unname(franz_colours[["ink"]])
  soft  <- unname(franz_colours[["ink_soft"]])
  line  <- unname(franz_colours[["grid"]])
  paper <- unname(franz_colours[["paper"]])
  ff    <- franz_font

  th <- theme_minimal(base_size = base_size, base_family = ff) +
    theme(
      # canvas: generous, even margins so the chart breathes in a feed
      plot.background  = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      plot.margin      = margin(base_size * 1.4, base_size * 1.4,
                                base_size, base_size * 1.4),

      # title block aligns to the image edge, not to the panel edge
      plot.title.position = "plot",
      plot.title    = element_text(size = base_size * 1.65, face = "bold",
                                   colour = ink, family = ff,
                                   margin = margin(b = base_size * .35),
                                   lineheight = 1.1),
      plot.subtitle = element_text(size = base_size * 1.1, colour = soft,
                                   family = ff,
                                   margin = margin(b = base_size * 1.1),
                                   lineheight = 1.15),
      plot.caption.position = "plot",
      plot.caption  = element_text(size = base_size * .72, colour = soft,
                                   family = ff, hjust = 0, lineheight = 1.2,
                                   margin = margin(t = base_size)),

      # grid: one direction only, light enough to sit behind the data
      panel.grid.major = element_line(colour = line, linewidth = .4),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),

      # axes: no frame, the labels do the work
      axis.line   = element_blank(),
      axis.ticks  = if (ticks) element_line(colour = line, linewidth = .4) else element_blank(),
      axis.ticks.length = unit(base_size * .25, "pt"),
      axis.text   = element_text(size = base_size * .85, colour = soft, family = ff),
      axis.title  = element_text(size = base_size * .85, colour = soft, family = ff),

      # facets read as small headings, not as boxed labels
      strip.background = element_blank(),
      strip.text = element_text(size = base_size * .95, face = "bold",
                                colour = ink, family = ff, hjust = 0,
                                margin = margin(b = base_size * .3)),
      panel.spacing = unit(base_size * 1.2, "pt"),

      # legend: top-left and compact, so it reads as part of the header
      legend.position      = "top",
      legend.location      = "plot",   # align with the title, not the panel
      legend.justification = "left",
      legend.direction     = "horizontal",
      legend.title         = element_blank(),
      legend.background    = element_blank(),
      legend.key           = element_blank(),
      legend.text          = element_text(size = base_size * .8, colour = ink,
                                          family = ff),
      legend.key.size      = unit(base_size * .8, "pt"),
      legend.margin        = margin(b = base_size * .6),
      legend.box.spacing   = unit(0, "pt")
    )

  th + switch(
    grid,
    y    = theme(panel.grid.major.x = element_blank()),
    x    = theme(panel.grid.major.y = element_blank()),
    both = theme(),
    none = theme(panel.grid.major = element_blank()),
    theme()
  )
}

# -- Building blocks --------------------------------------------------------

# A zero line that reads as a baseline rather than as another gridline.
geom_zeroline <- function(y = 0, ...) {
  geom_hline(yintercept = y, colour = unname(franz_colours[["ink"]]),
             linewidth = .5, ...)
}

# Last observation per group -- feed this to geom_text() to label lines
# directly instead of spending vertical space on a legend.
label_last <- function(data, x, group) {
  x <- rlang::ensym(x)
  group <- rlang::ensym(group)
  dplyr::ungroup(dplyr::filter(dplyr::group_by(data, !!group), !!x == max(!!x)))
}

# Wrap long category names so a multi-column legend cannot clip them. Official
# COICOP and COFOG titles run to 50+ characters and will otherwise overflow.
wrap_labels <- function(x, width = 30) {
  vapply(as.character(x),
         function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

# Source line plus handle. Keeps attribution identical across every post.
franz_caption <- function(source, last = NULL, note = NULL, lang = "de") {
  lab <- if (lang == "de") {
    list(src = "Quelle: ", own = "Eigene Berechnungen.", last = "Letzter Wert: ")
  } else {
    list(src = "Source: ", own = "Own calculations.", last = "Latest observation: ")
  }
  handle <- if (lang == "de") "@franzmohr_de" else "@franzmohr_en"
  txt <- paste0(lab$src, source, " ", lab$own)
  if (!is.null(last)) txt <- paste0(txt, " ", lab$last, last, ".")
  if (!is.null(note)) txt <- paste0(txt, "\n", note)
  paste0(txt, "\n", handle)
}

# -- Export -----------------------------------------------------------------
# Exact pixel sizes. 150 dpi keeps point sizes predictable (1 pt ~ 2.08 px).
franz_formats <- list(
  portrait  = c(1080, 1350),  # 4:5 -- most feed real estate, the default
  square    = c(1080, 1080),
  story     = c(1080, 1920),  # 9:16 stories / reels
  landscape = c(1200,  675)   # 16:9 for LinkedIn / X
)

# save_post(g, "government-debt-to-income", lang = "de")
#
# The handle is appended to the caption here rather than in each script, so
# every exported image carries it whether or not the caption came from
# franz_caption(). Screenshots travel without the post around them.
save_post <- function(plot, name, lang = NULL, format = "portrait",
                      dir = "pics", dpi = 150, date = Sys.Date(),
                      handle = TRUE) {
  dims <- franz_formats[[format]]
  if (is.null(dims)) {
    stop("format must be one of: ", paste(names(franz_formats), collapse = ", "))
  }
  if (handle) {
    cap <- tryCatch(plot$labels$caption, error = function(e) NULL)
    if (is.null(cap) || !grepl("@franzmohr", cap, fixed = TRUE)) {
      tag <- if (identical(lang, "en")) "@franzmohr_en" else "@franzmohr_de"
      plot <- plot + labs(caption = if (is.null(cap)) tag else paste0(cap, "\n", tag))
    }
  }
  file <- paste0(format(date, "%Y%m%d"), "-", name,
                 if (!is.null(lang)) paste0("-", lang) else "", ".jpeg")
  path <- file.path(dir, file)
  ggsave(path, plot = plot,
         width = dims[1] / dpi, height = dims[2] / dpi, dpi = dpi,
         units = "in", device = ragg::agg_jpeg, quality = 92, bg = "white")
  message("wrote ", path, " (", dims[1], "x", dims[2], ")")
  invisible(path)
}

# -- Backwards compatibility ------------------------------------------------
# The ~110 existing scripts source this file and expect these three objects.
theme_instagram    <- theme_franz(base_size = 12, grid = "y")
scale_colour_insta <- scale_colour_manual(values = franz_pal("cat"))
scale_fill_insta   <- scale_fill_manual(values = franz_pal("cat"))
