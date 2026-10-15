# Regenerates man/figures/logo.png.
#
# The hex sticker is drawn from these coordinates rather than kept as an
# opaque binary: the palette lives in one place and the wordmark, the URL and
# the bar heights can be changed without a drawing program. Geometry is in
# pixels of the 518 x 600 sticker, y measured from the top, so the numbers
# below can be read straight off the image.

library(grid)

W <- 518
H <- 600
OUT <- file.path("man", "figures", "logo.png")

# Palette: the badge colours used throughout the package (see vcs_palette()).
NAVY  <- "#0B3D63"   # sticker background
TEAL  <- "#128585"   # border and the tallest bar's accent
LIGHT <- "#9FD6D6"   # pale teal
RULE  <- "#7FB3D5"   # the baseline under the bars
URLC  <- "#8FB8D4"   # the small url line
BARS  <- c("#5A8FC7", "#2E6FB0", "#11619E", LIGHT)

# Bars: left edge, right edge and top, with a common base. Measured from the
# original sticker so the redraw lines up with it.
bar_x0 <- c(151, 210, 270, 331)
bar_x1 <- c(189, 249, 310, 369)
bar_y0 <- c(273, 233, 193, 154)
BASE   <- 340

hex_xy <- function(cx, cy, rx, ry) {
  # Pointy-top hexagon: a vertex at the top, flat left and right sides.
  list(x = c(cx, cx + rx, cx + rx, cx, cx - rx, cx - rx),
       y = c(cy - ry, cy - ry / 2, cy + ry / 2, cy + ry, cy + ry / 2, cy - ry / 2))
}

draw_logo <- function(scale = 1) {
  s <- function(v) v * scale
  pushViewport(viewport(xscale = c(0, W), yscale = c(H, 0)))

  h <- hex_xy(W / 2, H / 2, W / 2 - 2, H / 2 - 3)
  grid.polygon(h$x, h$y, default.units = "native",
               gp = gpar(fill = NAVY, col = TEAL, lwd = s(9), linejoin = "mitre"))

  # Checkmark, sitting over the tallest bar.
  grid.lines(x = c(300, 324, 386), y = c(114, 140, 86), default.units = "native",
             gp = gpar(col = "white", lwd = s(17), lineend = "butt", linejoin = "mitre"))

  for (i in seq_along(bar_x0)) {
    # The viewport's y scale runs downwards, so a rectangle is positioned by
    # its centre rather than by an edge whose meaning would then flip.
    grid.rect(x = (bar_x0[i] + bar_x1[i]) / 2, y = (bar_y0[i] + BASE) / 2,
              width = bar_x1[i] - bar_x0[i], height = BASE - bar_y0[i],
              default.units = "native", gp = gpar(fill = BARS[i], col = NA))
  }

  grid.segments(131, 345, 386, 345, default.units = "native",
                gp = gpar(col = RULE, lwd = s(5), lineend = "butt"))

  # grid centres text on its bounding box, so these are the centres of the
  # measured cap heights, not baselines.
  grid.text("vaxsurvR", x = W / 2, y = 434, default.units = "native",
            gp = gpar(col = "white", fontfamily = "Arial", fontface = "bold",
                      fontsize = s(78)))
  grid.text("github.com/bosangir", x = W / 2, y = 504, default.units = "native",
            gp = gpar(col = URLC, fontfamily = "Arial", fontsize = s(20)))
  popViewport()
}

# Drawn at 3x and reduced, which anti-aliases the hex border and the
# checkmark more cleanly than drawing once at final size.
SCALE <- 3
tmp <- tempfile(fileext = ".png")
ragg::agg_png(tmp, width = W * SCALE, height = H * SCALE, background = "transparent")
draw_logo(SCALE)
dev.off()

img <- magick::image_read(tmp)
img <- magick::image_resize(img, paste0(W, "x", H), filter = "Lanczos")
magick::image_write(img, OUT, format = "png")
cat("wrote", OUT, "\n")
