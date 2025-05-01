#' Create and/or modify color/gradient palettes
#'
#' Either generate theoretically maximally different colors, select an available R grDevices palette and/or modify the colors of the given gradient/palette
#'
#' @param n default: NULL, else integer, amount of colors to create, if palette selected and more colors requested they will be generated
#' @param colors character (vector), combination of selecting palette(s) by name (options: see display_palettes()), and/or vector of R color names and/or color hexcodes
#' @param colors_lock default: rep(FALSE, length(colors), numerical or logical index of colors (not) to be modified, if logical length != colors it will be cut or filled with TRUE/FALSE, prefix with '!' for logical vectors and '-' for numerical vectors to get inverse, see examples. If gradient_n %% length(colors) == 0, i.e. if gradient_n divisive by amount of colors without rest, set repeat given locking pattern
#' @param colors_names default: character(0), else character vector of color names
#' @param colors_breakpoints default: numeric(0), else numeric vector of breakpoints to colorRamp in between
#'
#' @param gradient_n default: n, else integer, amount of colors to output as gradient, after completing palette for n colors
#' @param gradient_space default: "rgb", else "Lab", see ?grDevices::colorRamp()
#' @param gradient_interpolate default: "linear", else "spline", see ? grDevices::colorRamp()
#' 
#' @param hf hue factor, default: 1, multiply values by factor, proportional to base value of 1
#' @param sf saturation factor, default: 1, multiply values by factor, proportional to base value of 1
#' @param lf lightness/brightness factor, default: 1, multiply values by factor, proportional to base value of 1
#' @param rf red factor, default: 1, multiply values by factor, proportional to base value of 1
#' @param gf green factor, default: 1, multiply values by factor, proportional to base value of 1
#' @param bf blue factor, default: 1, multiply values by factor, proportional to base value of 1
#' 
#' @param hv hue value, default: 0, add value to values, linear from base value of 0
#' @param sv saturation value, default: 0, add value to values, linear from base value of 0
#' @param lv lightness/brightness value, default: 0, add value to values, linear from base value of 0
#' @param rv red value, default: 0, add value to values, linear from base value of 0
#' @param gv green value, default: 0, add value to values, linear from base value of 0
#' @param bv blue value, default: 0, add value to values, linear from base value of 0
#' 
#' @param alpha numeric, sets color alpha values
#' @param rev default: FALSE, if TRUE, reverse order of colors
#' @param plot default: FALSE, if TRUE plot pie chart of color palette
#' @param export default: FALSE, if TRUE: export = getwd(), if export = "string/", save hexcodes, rgb, and hsl values to export/colorify.csv
#' @param verbose default: TRUE, else FALSE - to log status messages 
#' @param ... additional arguments to pass on 
#' @returns vector of color hexcodes
#'
#' @export
#'
#' @description
#' Note for colorblind use: "Okabe-Ito"
#' 
#' Addition of values happens before multiplication with factors. 
#' 
#' Palette names are stripped of whitespace and lowered for name matching. 
#' All RColorBrewer and Viridis palettes are included.
#' 
#' All grDevices plotting functions are provided as palettes, simply use colors = "rainbow", "heat", "terrain", "topo" or "cm".
#'
#' @examples
#' if(interactive()) {
#'   ## if parameters identical, change seed to change generation
#'   colorify(10, plot = TRUE, seed = 1)
#'   colorify(10, plot = TRUE, seed = 42)
#'   ## set colors, generate additional up to n
#'   colorify(colors = c("red", "white", "blue"), n = 5, plot = TRUE)
#'   ## create gradients
#'   colorify(colors = c("orange", "red", "white", "blue", "orange"), gradient_n = 100, plot = TRUE)
#'   
#'   ## viridis gradient, lighten and saturate, darken
#'   colorify(colors = "viridis", n = 100, plot = TRUE)
#'   colorify(colors = "viridis", n = 10, plot = TRUE, l = 1.2, s = 10)
#'   colorify(colors = "viridis", n = 10, plot = TRUE, l = .9)
#'   
#'   ## palette selected by name in colors[1], can add colors to selected palette, if n < length, remove colors , if greater generate 
#'   colorify(colors = c("Okabe-Ito", "red", "blue", "yellow"), plot = TRUE, n = 10)
#'   
#'   ## no adjustments to locked indices 
#'   colorify(colors = "Okabe-Ito", colors_lock = c(FALSE,FALSE,TRUE,TRUE), plot = TRUE, rv = -300)
#'   colorify(colors = "Okabe-Ito", colors_lock = c(FALSE,FALSE,TRUE,TRUE), plot = TRUE, rv = 300)
#'   
#'   ## colors_lock and inversing
#'   colors <- colorify(5)
#'   colorify(colors_lock = c(TRUE,TRUE), colors=colors)
#'   colorify(colors_lock = ! c(TRUE,FALSE,TRUE), colors=colors)
#'   colorify(colors_lock = c(3,4), colors=colors)
#'   colorify(colors_lock = - c(3,4), colors=colors)
#'   
#'   ## rainbow
#'   colorify(colors=grDevices::rainbow(100, s = .5), plot = TRUE)
#'   colorify(colors="rainbow", n = 100, sf = .5, plot = TRUE)
#'   colorify(colors=grDevices::rainbow(100, v = .5), plot = TRUE)
#'   colorify(colors="rainbow", n = 100, lf = .5, plot = TRUE,)
#'   colorify(colors=grDevices::rainbow(100, start = .25, end = .75), plot = TRUE)
#'   colorify(colors=grDevices::rainbow(100)[25:75], plot = TRUE)
#' }
colorify <- function(
    n = NULL, colors = character(0), colors_lock = NULL, colors_names = character(0), colors_breakpoints = numeric(0),
    gradient_n = n, gradient_space = c("rgb", "Lab"), gradient_interpolate = c("linear", "spline"),
    hf = 1, sf = 1, lf = 1, rf = 1, gf = 1, bf = 1,
    hv = 0, sv = 0, lv = 0, rv = 0L, gv = 0L, bv = 0L,
    alpha = 1, rev = FALSE, plot = FALSE, export = FALSE, verbose = TRUE, ...) {
  
  stopifnot(
    is.character(c(colors, colors_names)),
    is.numeric(c(colors_breakpoints, hf, sf, lf, rf, gf, bf, hv, sv, lv, rv, gv, bv, alpha, seed)),
    is.null(n) | is.numeric(n),
    is.null(gradient_n) | is.numeric(gradient_n),
    is.logical(plot),
    is.logical(export) | is.character(plot),
    is.null(colors_lock) | is.logical(colors_lock) | is.numeric(colors_lock)
  )
  gradient_space <- match.arg(gradient_space, choices = c("rgb", "Lab"))
  gradient_interpolate <- match.arg(gradient_interpolate, choices = c("linear", "spline"))
  
  alpha <- max(0, min(1, alpha))
  gradient_n <- ifelse(is.null(gradient_n), length(colors), max(0, round(gradient_n)))
  
  ## add named palette(s) to colors
  colors <- unname(unlist(sapply(colors, function(color) {
    original_palette <- palette_name_mapping(color)
    if (original_palette %in% grDevices::palette.pals()) {
      grDevices::palette.colors(n = NULL, palette = original_palette)
    } else if (original_palette %in% grDevices::hcl.pals()) {
      if (is.null(n)) stop("To select hcl palette, pass n colors.")
      grDevices::hcl.colors(n, palette = original_palette)
    } else if (original_palette == "Rainbow") {
      grDevices::rainbow(n)
    } else if (original_palette == "Heat") {
      grDevices::heat.colors(n)
    } else if (original_palette == "Terrain") {
      grDevices::terrain.colors(n)
    } else if (original_palette == "Topo") {
      grDevices::topo.colors(n)
    } else if (original_palette == "Cm") {
      grDevices::cm.colors(n)
    } else {
      color
    }
  })))
  
  n <- ifelse(is.null(n), length(colors), max(0, round(n)))
  if (length(colors) > n) colors <- colors[seq_len(n)]
  if (length(colors) < n) {
    if (verbose) message(n-length(colors), " colors generated")
    ## generate theoretically distinct RGB values and convert to hexcodes
    rgb_matrix <- matrix(runif((n-length(colors)) * 3, min = 0, max = 255), ncol = 3)
    colors <- c(colors, apply(rgb_matrix, 1, function(rgbv) {
      rgb(rgbv[1], rgbv[2], rgbv[3], maxColorValue = 255)
    }))
  }
  
  ## create gradient of set and/or generated colors
  if (gradient_n > n & ! length(colors_breakpoints) > 0) colors <- grDevices::colorRampPalette(colors, space = gradient_space, interpolate = gradient_interpolate, ...)(gradient_n)
  
  if (length(colors) == 0) stop("Input starting colors, palette name, or n colors to generate.")
  
  ## convert hex or R color names to RGB to HSV
  rgb_values <- col2rgb(colors)
  ## set colors to be modified
  if (is.null(colors_lock)) {
    colors_lock = rep(FALSE, length(colors))
  }
  colors_lock_bool <- identical(substitute(colors_lock)[[1]], as.symbol("!")) | identical(substitute(colors_lock)[[1]], as.symbol("-"))
  if (is.numeric(colors_lock)) {
    colors_i <- seq_len(length(colors))
    colors_lock_i <- replace(rep(FALSE, length(colors)), colors_i[colors_lock], TRUE)
  } else { ## if logical
    if (gradient_n > n & gradient_n %% length(colors_lock) == 0) {
      colors_lock_i <- rep(colors_lock, gradient_n / length(colors_lock))
    }
    else if (length(colors_lock) >= length(colors)) {
      colors_lock_i <- colors_lock[seq_len(length(colors))]
    } else {
      colors_lock_i <- c(colors_lock, rep(colors_lock_bool, length(colors) - length(colors_lock)))
    }
  }
  colors_lock_i <- ! colors_lock_i
  
  ## adjust values within RGB range
  if (rv != 0) rgb_values[1, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[1, ][colors_lock_i] + rv))
  if (gv != 0) rgb_values[2, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[2, ][colors_lock_i] + gv))
  if (bv != 0) rgb_values[3, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[3, ][colors_lock_i] + bv))
  if (rf != 1) rgb_values[1, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[1, ][colors_lock_i] * rf))
  if (gf != 1) rgb_values[2, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[2, ][colors_lock_i] * gf))
  if (bf != 1) rgb_values[3, ][colors_lock_i] <- pmax(0, pmin(255, rgb_values[3, ][colors_lock_i] * bf))
  ## RGB to HSV
  hsv_values <- rgb2hsv(rgb_values[1,], rgb_values[2,], rgb_values[3,])
  ## adjust values within HSV range
  if (hv != 0) hsv_values["h", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["h", ][colors_lock_i] + hv))
  if (sv != 0) hsv_values["s", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["s", ][colors_lock_i] + sv))
  if (lv != 0) hsv_values["v", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["v", ][colors_lock_i] + lv))
  if (hf != 1) hsv_values["h", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["h", ][colors_lock_i] * hf))
  if (sf != 1) hsv_values["s", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["s", ][colors_lock_i] * sf))
  if (lf != 1) hsv_values["v", ][colors_lock_i] <- pmax(0, pmin(1, hsv_values["v", ][colors_lock_i] * lf))
  ## convert back to hex
  colors <- hsv(hsv_values["h",], hsv_values["s",], hsv_values["v",], alpha = alpha)
  
  ## set names
  if (length(colors_names) == length(colors)) {
    names(colors) <- colors_names
  } else if (length(colors_names) != 0) {
    warning("colors_names given: need same length as amount of requested colors")
  }
  
  ## reverse order of colors
  # TODO give more ordering options: start/end from
  if (rev) colors <- rev(colors)
  
  ## plot pie to visualize colors
  if (plot) pie(rep(1, length(colors)), col = colors, border = NA)
  
  if (is.character(export) | isTRUE(export)) {
    df <- setNames(cbind(as.data.frame(colors), t(rgb_values), t(hsv_values)), c("hexcode", "r", "g", "b", "h", "s", "l"))
    ifelse(isTRUE(export), write.csv2(df, file = file.path(getwd(), "colorify.csv")), write.csv2(df, file = file.path(export, "colorify.csv")))
  }
  
  ## return color map function if breakpoints per color given
  if (length(colors_breakpoints) > 0) {
    return(colorify_map(colors = colors, breakpoints = colors_breakpoints, ...)) # TODO test , n_breakpoints = gradient_n
  }
  return(colors)
}

#' Colorify colorRamp between colors mapping to breakpoint values
#'
#' @param colors hexcolor character vector
#' @param breakpoints numeric vector matching colors per value
#' @param ... to pass arguments to grDevices::colorRamp
#'
#' @description
#' Note that breakpoints and colors will be ordered ascendingly by breakpoints values
#'
#' @returns function with colors and breaks attributes, can be called as function(c(values)) to return hexcolorcodes
colorify_map <- function(colors, breakpoints, ...) {
  if (length(colors) != length(breakpoints)) stop("for color mapping: 'colors' and 'breakpoints' must be the same length.")
  if (length(colors) < 2 | length(breakpoints) < 2) stop("You need at least two colors and two breakpoints.")
  
  ## order breakpoints and colors ascendingly, respectively
  ord <- order(breakpoints)
  breakpoints <- breakpoints[ord]
  colors <- colors[ord]
  
  ## colorRamp per sequential color pair: map colors to interval [0, 1]
  ramp_list <- lapply(seq_len(length(colors) - 1), function(i) colorRamp(c(colors[i], colors[i+1]), ...))
  
  colorify_mapped <- function(values) {
    ## initialize mapped hexcolors
    mapped_colors <- character(length(values))
    ## for each value index, figure out where it belongs:
    for (vi in seq_along(values)) {
      value <- values[vi]
      ## if below/above the first/last breakpoint, clamp to first/last color
      if (value <= breakpoints[1]) {
        mapped_colors[vi] <- colors[1]
      } else if (value >= breakpoints[length(breakpoints)]) {
        mapped_colors[vi] <- colors[length(colors)]
      } else {
        ## find ramp index of value between breakpoints
        ri <- findInterval(value, breakpoints, left.open = TRUE)
        ## scale value between interval [0, 1] for specific color ramp pair
        scaled <- (value - breakpoints[ri]) / (breakpoints[ri + 1] - breakpoints[ri])
        ## get rgb values of pecific color ramp pair by scaled value
        # message(4, " ", ri)
        rgb_val <- ramp_list[[ri]](scaled)
        ## convert rgb to hexcolor
        mapped_colors[vi] <- rgb(rgb_val[1], rgb_val[2], rgb_val[3], maxColorValue = 255)
      }
    }
    return(mapped_colors)
  }
  
  ## attach function attributes
  attr(colorify_mapped, "breaks") <- breakpoints
  attr(colorify_mapped, "colors") <- colors
  return(colorify_mapped)
}

#' Display R grDevices palettes
#'
#' @param n integer, amount of colors to display
#' @param i_palettes default: numeric vector as index/range for choosing palettes, or a combination of 'rcolorbrewer', 'viridis', 'rainbow' (grDevices Palettes) to show specific palettes
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
#' Any numeric i_palettes over maximum amount of palettes are not displayed.
#'
#' Contains all Viridis palettes, excluding Turbo.
#'
#' @examples
#' if (interactive()) {
#'   display_palettes()
#'   display_palettes(i_palettes = 50:75)
#'  
#'   display_palettes(i_palettes = 'RColorBrewer')
#'   display_palettes(i_palettes = 'Viridis')
#'   display_palettes(i_palettes = c("rainbow", "viridis"))
#'  
#'   display_palettes(i_palettes = c(1,5,10,20,40,100,119))
#'   display_palettes(n = 100, i_palettes = seq_len(10))
#'   display_palettes(n = 10, i_palettes = seq_len(10), border = TRUE)
#' }
display_palettes <- function(n = 10, i_palettes = seq_len(1000), border = FALSE) {
  stopifnot(
    is.numeric(n),
    is.numeric(i_palettes) | is.character(i_palettes),
    is.logical(border)
  )
  
  ## get base R grDevices palettes
  viridis_palette_names <- c("Viridis", "Plasma", "Inferno", "Cividis", "Rocket", "Mako")
  grDevices_palettes <- c("Rainbow", "Heat", "Terrain", "Topo", "Cm")
  hcl_palettes <- grDevices::hcl.pals()
  base_palettes <- grDevices::palette.pals()
  grd_palettes <- setNames(grDevices_palettes, rep("grDevices", length(grDevices_palettes)))
  hcl_palettes <- setNames(hcl_palettes, rep("hcl", length(hcl_palettes)))
  base_palettes <- setNames(base_palettes, rep("pal", length(base_palettes)))
  all_palettes <- c(grd_palettes, hcl_palettes, base_palettes)
  
  if (is.character(i_palettes)) {
    i_palettes <- unlist(sapply(i_palettes, function(pal) {
      if (tolower(pal) == 'rcolorbrewer') {
        brewer_palettes <- c(
          "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral",
          "Accent", "Dark 2", "Paired", "Pastel 1", "Pastel 2", "Set 1", "Set 2", "Set 3",
          "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Grays", "Oranges", "OrRd",
          "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
        )
        match(brewer_palettes, all_palettes)
      } else if (tolower(pal) %in% tolower(viridis_palette_names)) {
        match(viridis_palette_names, all_palettes)
      } else if (tolower(pal) %in% tolower(grd_palettes)) {
        match(grd_palettes, all_palettes)
      } else {
        message("pass i_palettes = 'rcolorbrewer', 'viridis'(-palettes), see grDevices Palettes or numeric index/range e.g. = seq_len(30)")
      }
    }))
    
  }
  ## set index between 1 and maximum amount of palettes
  i_palettes <- i_palettes[i_palettes > 0 & i_palettes <= length(all_palettes)]
  ## select palettes by index
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
    } else if (all_palettes[i] == "Rainbow") {
      colors <- grDevices::rainbow(n)
    } else if (all_palettes[i] == "Heat") {
      colors <- grDevices::heat.colors(n)
    } else if (all_palettes[i] == "Terrain") {
      colors <- grDevices::terrain.colors(n)
    } else if (all_palettes[i] == "Topo") {
      colors <- grDevices::topo.colors(n)
    } else if (all_palettes[i] == "Cm") {
      colors <- grDevices::cm.colors(n)
    }
    
    ## draw rectangular palettes
    y_bottom <- i - 1
    y_top <- i - 0.1
    rect(xleft = 0:(n - 1), ybottom = y_bottom, xright = seq_len(n), ytop = y_top, col = colors, border = ifelse(border, TRUE, NA))
    
    ## center text in palette, no falling off side of plot
    text_x <- n / 2
    text_y <- (y_bottom + y_top) / 2
    text(x = text_x, y = text_y, labels = all_palettes[i], col = "black", cex = 0.9, font = 2)
  }
  
  ## reset plot margins to default
  par(old_par)
  
  return(all_palettes)
}

#' Palette original name mapping
#'
#' @param palette string: name of palette, will be lower()ed and stripped of whitespace
#'
#' @return original palette name
#'
#' @description
#' All ColorBrewer palettes overlap with grDevices palettes
#' Viridis palettes, except "Magma", overlap with grDevices palettes
#'
#' @examples
#' if(interactive()) {
#'   palette_name_mapping("dark2") # "Dark 2"
#' }
palette_name_mapping <- function(palette) {
  palette_mapping <- list(
    ## custom palettes
    "rainbow" = "Rainbow", "heat" = "Heat",
    "terrain" = "Terrain", "topo" = "Topo", "cm" = "Cm",
    ## all RColorBrewer palettes
    ## grDevices::palette.pals()
    "r3" = "R3", "r 3" = "R3", "r4" = "R4", "r 4" = "R4", "ggplot2" = "ggplot2", "okabe-ito" = "Okabe-Ito",
    "accent" = "Accent", "dark2" = "Dark 2", "dark 2" = "Dark 2", "paired" = "Paired",
    "pastel1" = "Pastel 1", "pastel 1" = "Pastel 1", "pastel2" = "Pastel 2", "pastel 2" = "Pastel 2",
    "set1" = "Set 1", "set 1" = "Set 1", "set2" = "Set 2", "set 2" = "Set 2", "set3" = "Set 3", "set 3" = "Set 3",
    "tableau10" = "Tableau 10", "tableau 10" = "Tableau 10",
    "classictableau" = "Classic Tableau", "classic tableau" = "Classic Tableau",
    "polychrome36" = "Polychrome 36", "polychrome 36" = "Polychrome 36",
    "alphabet" = "Alphabet",
    ## grDevices::hcl.pals()
    "dark3" = "Dark 3", "dark 3" = "Dark 3",
    "warm" = "Warm", "cold" = "Cold", "harmonic" = "Harmonic", "dynamic" = "Dynamic",
    "grays" = "Grays", "greys", "Grays", "lightgrays" = "Light Grays", "light grays" = "Light Grays",
    "blues2" = "Blues 2", "blues 2" = "Blues 2", "blues3" = "Blues 3", "blues 3" = "Blues 3",
    "purples2" = "Purples 2", "purples 2" = "Purples 2", "purples3" = "Purples 3", "purples 3" = "Purples 3",
    "reds2" = "Reds 2", "reds 2" = "Reds 2", "reds3" = "Reds 3", "reds 3" = "Reds 3",
    "greens2" = "Greens 2", "greens 2" = "Greens 2", "greens3" = "Greens 3", "greens 3" = "Greens 3",
    "oslo" = "Oslo", "purple-blue" = "Purple-Blue", "red-purple" = "Red-Purple",
    "red-blue" = "Red-Blue", "purple-orange" = "Purple-Orange", "purple-yellow" = "Purple-Yellow",
    "blue-yellow" = "Blue-Yellow", "green-yellow" = "Green-Yellow", "red-yellow" = "Red-Yellow",
    "heat" = "Heat", "heat2" = "Heat 2", "heat 2" = "Heat 2",
    "terrain" = "Terrain", "terrain2" = "Terrain 2", "terrain 2" = "Terrain 2",
    "viridis" = "Viridis", "plasma" = "Plasma", "inferno" = "Inferno", "rocket" = "Rocket", "mako" = "Mako",
    "darkmint" = "Dark Mint", "dark mint" = "Dark Mint", "mint" = "Mint",
    "blugrn" = "BluGrn", "teal" = "Teal", "tealgrn" = "TealGrn", "emrld" = "Emrld",
    "bluyl" = "BluYl", "ag_grnyl" = "ag_GrnYl", "peach" = "Peach", "pinkyl" = "PinkYl",
    "burg" = "Burg", "burgyl" = "BurgYl", "redor" = "RedOr", "oryel" = "OrYel",
    "purp" = "Purp", "purpor" = "PurpOr", "sunset" = "Sunset", "magenta" = "Magenta",
    "sunsetdark" = "SunsetDark", "ag_sunset" = "ag_Sunset", "brwnyl" = "BrwnYl",
    "ylorrd" = "YlOrRd", "ylorbr" = "YlOrBr", "oranges" = "Oranges", "reds" = "Reds",
    "rdpu" = "RdPu", "purd" = "PuRd", "purples" = "Purples", "pubugn" = "PuBuGn",
    "pubu" = "PuBu", "greens" = "Greens", "bugn" = "BuGn", "gnbu" = "GnBu", "bupu" = "BuPu",
    "blues" = "Blues",
    "lajolla" = "Lajolla", "turku" = "Turku", "hawaii" = "Hawaii", "batlow" = "Batlow",
    "blue-red" = "Blue-Red", "blue-red2" = "Blue-Red 2", "blue-red 2" = "Blue-Red 2",
    "blue-red3" = "Blue-Red 3", "blue-red 3" = "Blue-Red 3",
    "red-green" = "Red-Green", "purple-green" = "Purple-Green", "purple-brown" = "Purple-Brown",
    "green-brown" = "Green-Brown", "blue-yellow2" = "Blue-Yellow 2", "blue-yellow 2" = "Blue-Yellow 2",
    "blue-yellow3" = "Blue-Yellow 3", "blue-yellow 3" = "Blue-Yellow 3",
    "green-orange" = "Green-Orange", "cyan-magenta" = "Cyan-Magenta", "tropic" = "Tropic",
    "broc" = "Broc", "cork" = "Cork", "vik" = "Vik", "berlin" = "Berlin", "lisbon" = "Lisbon",
    "tofino" = "Tofino", "armyrose" = "ArmyRose", "earth" = "Earth", "fall" = "Fall",
    "geyser" = "Geyser", "tealrose" = "TealRose", "temps" = "Temps",
    "puor" = "PuOr", "rdbu" = "RdBu", "rdgy" = "RdGy", "piyg" = "PiYG",
    "prgn" = "PRGn", "brbg" = "BrBG", "rdylbu" = "RdYlBu", "rdylgn" = "RdYlGn", "spectral" = "Spectral",
    "zissou1" = "Zissou 1", "zissou 1" = "Zissou 1",
    "cividis" = "Cividis", "roma" = "Roma"
  )
  original_palette <- palette_mapping[[tolower(gsub(" ", "", palette))]]
  ifelse(is.null(original_palette), return(""), return(original_palette))
}
