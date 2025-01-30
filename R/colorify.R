# TODO recreate
RColorBrewer::brewer.pal.info
# TODO note all RColorBrewer palettes are also in, some are named slightly different, aka 'Dark2' vs 'Dark 2'
## TODO create option to display RColorBrewer palettes
brewer_palettes <- rownames(RColorBrewer::brewer.pal.info)
# TODO add options for distinct color palettes, or simply add to infoverview
# TODO mention pakcage at https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
# TODO link documentations
# TODO Shiny color picker/palette creator
# TODO check niches of other color packages and see if you can merge their functionality (RColorBrewer, Viridis)
# TODO be able to return a function? as some plots seem to require that 
# TODO be able to return named vector ?
# TODO exportable: hexcodes, RGB values, HSL values to .csv, .png of pie and palettes 
# TODO finish docs after finishing TODOs
# TODO check scale_continuous need for colour output (need of function as output?)

#' Create and/or modify color/gradient palettes
#'
#' Either generate theoretically maximally different colors, select an available R grDevices palette and/or modify the colors of the given gradient/palette
#'
#' @param n default: NULL, else integer, amount of colors to create, if palette selected and more colors requested they will be generated
#' @param colors if integer, generate that many theoretically maximally different colors, select palette by name (options: see grDevices::palette.pals(), "maus"), or vector of R color names or color hexcodes
#' @param n_gradient default: n, else integer, amount of colors to output as gradient, after completing palette for n colors
#' @param h hue factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param s saturation factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param l lightness/brightness factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param r red factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param g green factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param b blue factor, default: 1, multiply values by factor, proportional scaling from base value of 1
#' @param alpha numeric, sets color alpha values
#' @param seed integer, set seed for generation of colors
#' @param rev default: FALSE, if TRUE, reverse order of colors
#' @param plot default: FALSE, if TRUE plot pie chart of color palette
#'
#' @returns vector of color hexcodes
#'
#' @export
#'
#' @description
#' Note for colorblind use: "Okabe-Ito"
#' TODO add description
#'
#'
#' @examples
#' ## if parameters identical, change seed to change generation
#' colorify(10, plot = TRUE, seed = 1)
#' colorify(10, plot = TRUE, seed = 42)
#' ## set colors, generate additional up to n
#' colorify(colors = c("red", "white", "blue"), n = 5, plot = TRUE)
#' ## create gradients
#' colorify(colors = c("orange", "red", "white", "blue", "orange"), n_gradient = 100, plot = TRUE)
#' 
#' ## viridis gradient, lighten and saturate, darken
#' colorify(colors = "viridis", n = 100, plot = TRUE)
#' colorify(colors = "viridis", n = 10, plot = TRUE, l = 1.2, s = 10)
#' colorify(colors = "viridis", n = 10, plot = TRUE, l = .9)
#' 
#' TODO rgb, hsl examples
#' TODO all parameter examples
colorify <- function(n = NULL, colors = character(0), n_gradient = n, h = 1, s = 1, l = 1, r = 1, g = 1, b = 1, alpha = 1, seed = 1L, rev = FALSE, plot = FALSE) {
  stopifnot(
    is.character(colors),
    is.numeric(alpha),
    is.numeric(h),
    is.numeric(s),
    is.numeric(l),
    is.numeric(r),
    is.numeric(g),
    is.numeric(b),
    is.numeric(seed),
    is.null(n) | is.numeric(n_gradient),
    is.null(n) | is.numeric(n),
    is.logical(plot)
  )
  
  set.seed(round(seed))
  alpha <- max(0, min(1, alpha))
  n <- ifelse(is.null(n), length(colors), max(0, round(n)))
  n_gradient <- ifelse(is.null(n_gradient), length(colors), max(0, round(n_gradient)))
  
  if (tolower(colors[1]) %in% tolower(grDevices::palette.pals())) {
    colors <- grDevices::palette.colors(n = NULL, palette = colors[1])
  } else if (tolower(colors[1]) %in% tolower(grDevices::hcl.pals())) {
    colors <- grDevices::hcl.colors(n, palette = colors[1])
  } else if (colors[1] %in% "maus") {
    colors <- character(0) # TODO set palette(s)
  }
  
  if (length(colors) < n) {
    message(n-length(colors), " colors generated")
    ## generate theoretically distinct RGB values and convert to hexcodes
    rgb_matrix <- matrix(runif((n-length(colors)) * 3, min = 0, max = 255), ncol = 3)
    colors <- c(colors, apply(rgb_matrix, 1, function(rgbv) {
      rgb(rgbv[1], rgbv[2], rgbv[3], maxColorValue = 255)
    }))
  }
  
  ## create gradient of set and/or generated colors
  if (n_gradient > n) {
    colors <- grDevices::colorRampPalette(colors)(n_gradient)
  }
  
  if (length(colors) == 0) stop("Input starting colors, palette name, or n colors to generate.")
  
  ## convert hex or R color names to RGB to HSV
  rgb_values <- col2rgb(colors)
  ## adjust values within range
  rgb_values[1, ] <- pmax(0, pmin(255, rgb_values[1, ] * r))
  rgb_values[2, ] <- pmax(0, pmin(255, rgb_values[2, ] * g))
  rgb_values[3, ] <- pmax(0, pmin(255, rgb_values[3, ] * b))
  ## RGB to HSV
  hsv_values <- rgb2hsv(rgb_values[1,], rgb_values[2,], rgb_values[3,])
  ## adjust values within range
  hsv_values["h", ] <- pmax(0, pmin(1, hsv_values["h", ] * h))
  hsv_values["s", ] <- pmax(0, pmin(1, hsv_values["s", ] * s))
  hsv_values["v", ] <- pmax(0, pmin(1, hsv_values["v", ] * l))
  ## convert back to hex
  colors <- hsv(hsv_values["h",], hsv_values["s",], hsv_values["v",], alpha = alpha)
  
  ## reverse order of colors
  if (rev) {
    colors <- rev(colors)
  }
  ## plot pie to visualize colors
  if (plot) {
    pie(rep(1, length(colors)), col = colors, border = NA)
  }
  return(colors)
}

#' Display R grDevices palettes
#'
#' @param n integer, amount of colors to display
#' @param i_palettes index range as numerical vector for choosing palettes, any i over maximum amount of palettes are removed
#' @param border default: FALSE, if TRUE show color rectangle borders
#'
#' @export
#'
#' @return named vector with source and name of palettes, 'hcl' for grDevices::hcl.pals() and 'pal' for grDevices::palette.pals()
#'
#' @description
#' Use colorify() to select and modify the palettes, see its documentation.
#' Note that discrete palettes with maximum n colors will be repeated in plotting.
#'
#' @examples
#' display_palettes()
#' display_palettes(i_palettes = 50:75)
#' display_palettes(i_palettes = c(1,5,10,20,40,100,119)
#' display_palettes(n = 100, i_palettes = 1:10)
#' display_palettes(n = 10, i_palettes = 1:10, border = TRUE)
display_palettes <- function(n = 10, i_palettes = 1:131, border = FALSE) {
  stopifnot(
    is.numeric(n),
    is.numeric(i_palettes),
    is.logical(border)
  )
  
  ## get base R grDevices palettes
  hcl_palettes <- grDevices::hcl.pals()
  grd_palettes <- grDevices::palette.pals()
  hcl_palettes <- setNames(hcl_palettes, rep("hcl", length(hcl_palettes)))
  grd_palettes <- setNames(grd_palettes, rep("pal", length(grd_palettes)))
  all_palettes <- c(hcl_palettes, grd_palettes)
  i_palettes <- i_palettes[i_palettes <= length(all_palettes)]
  all_palettes <- all_palettes[i_palettes]
  
  ## save and set par plot margins
  old_par <- par(no.readonly = TRUE)
  par(mar = c(0, 0, 0, 0))
  
  ## initialize empty plot
  plot(NULL, xlim = c(-3, n), ylim = c(0, length(all_palettes)), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", main = "")
  for (i in seq_along(all_palettes)) {
    ## get grDevices colors by palette name
    if (names(all_palettes[i]) == "hcl") {
      colors <- grDevices::hcl.colors(n, palette = all_palettes[i])
    } else if (names(all_palettes[i]) == "pal") {
      colors <- grDevices::palette.colors(palette = all_palettes[i])
    }
    
    ## draw rectangular palettes
    y_bottom <- i - 1
    y_top <- i - 0.1
    rect(xleft = 0:(n - 1), ybottom = y_bottom, xright = 1:n, ytop = y_top, col = colors, border = ifelse(border, TRUE, NA))
    
    ## center text in palette, no falling off side of plot
    text_x <- n / 2
    text_y <- (y_bottom + y_top) / 2
    text(x = text_x, y = text_y, labels = all_palettes[i], col = "black", cex = 0.9, font = 2)
  }
  
  ## reset plot margins to default
  par(old_par)
  
  return(all_palettes)
}
