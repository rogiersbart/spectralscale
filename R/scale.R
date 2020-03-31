# TODO ----

  #' Expose guide to user
  #' Evaluate whether guide_bins is better or not for binned scales

# scale creation functions ----

  create_discrete_fill_scale <- function(palette) {
    function (...) ggplot2::discrete_scale(
      "fill",
      "spectralscale_discrete_fill_scale",
      palette = palette,
      guide = "legend",
      ...
    )
  }
  create_continuous_fill_scale <- function(palette, div = FALSE) {
    if (div) {
      return(
        function(mid = 0, ...) ggplot2::continuous_scale(
          "fill",
          "spectralscale_continuous_fill_scale",
          palette = scales::gradient_n_pal(palette(7)),
          guide = "colourbar",
          rescaler = colorspace:::mid_rescaler(mid),
          ...
        )
      )
    }
    function(...) ggplot2::continuous_scale(
      "fill",
      "spectralscale_continuous_fill_scale",
      palette = scales::gradient_n_pal(palette(7)),
      guide = "colourbar",
      ...
    )
  }
  create_binned_fill_scale <- function(palette, div = FALSE) {
    if (div) {
      return(
        function(mid = 0, ...) ggplot2:::binned_scale(
          "fill",
          "spectralscale_binned_fill_scale",
          palette = scales::gradient_n_pal(palette(7)),
          guide = "colourbar",
          rescaler = colorspace:::mid_rescaler(mid),
          ...
        )
      )
    }
    function(...) ggplot2:::binned_scale(
      "fill",
      "spectralscale_binned_fill_scale",
      palette = scales::gradient_n_pal(palette(7)),
      guide = "colourbar",
      ...
    )
  }
  create_discrete_colour_scale <- function(palette) {
    function(...) ggplot2::discrete_scale(
      "colour",
      "spectralscale_discrete_colour_scale",
      palette = palette,
      guide = "legend",
      ...
    )
  }
  create_continuous_colour_scale <- function(palette, div = FALSE) {
    if (div) {
      return(
        function(mid = mid, ...) ggplot2::continuous_scale(
          "colour",
          "spectralscale_continuous_colour_scale",
          palette = scales::gradient_n_pal(palette(7)),
          guide = "colourbar",
          na.value = "grey50",
          rescaler = colorspace:::mid_rescaler(mid),
          ...
        )
      )
    }
    function(...) ggplot2::continuous_scale(
      "colour",
      "spectralscale_continuous_colour_scale",
      palette = scales::gradient_n_pal(palette(7)),
      guide = "colourbar",
      na.value = "grey50",
      ...
    )
  }
  create_binned_colour_scale <- function(palette, div = FALSE) {
    if (div) {
      return(
        function(mid = mid, ...) ggplot2:::binned_scale(
          "colour",
          "spectralscale_binned_colour_scale",
          palette = scales::gradient_n_pal(palette(7)),
          guide = "colourbar",
          na.value = "grey50",
          rescaler = colorspace:::mid_rescaler(mid),
          ...
        )
      )
    }
    function(...) ggplot2:::binned_scale(
      "colour",
      "spectralscale_binned_colour_scale",
      palette = scales::gradient_n_pal(palette(7)),
      guide = "colourbar",
      na.value = "grey50",
      ...
    )
  }

# create scales ----

  fill_discrete_sequential_warm <- create_discrete_fill_scale(palette_discrete_sequential_warm)
  fill_discrete_sequential_cold <- create_discrete_fill_scale(palette_discrete_sequential_cold)
  colour_discrete_sequential_warm <- create_discrete_colour_scale(palette_discrete_sequential_warm)
  colour_discrete_sequential_cold <- create_discrete_colour_scale(palette_discrete_sequential_cold)
  fill_discrete_diverging <- create_discrete_fill_scale(palette_discrete_diverging)
  colour_discrete_diverging <- create_discrete_colour_scale(palette_discrete_diverging)
  fill_discrete_qualitative <- create_discrete_fill_scale(palette_discrete_qualitative)
  colour_discrete_qualitative <- create_discrete_colour_scale(palette_discrete_qualitative)
  fill_discrete_paired <- create_discrete_fill_scale(palette_discrete_paired)
  colour_discrete_paired <- create_discrete_colour_scale(palette_discrete_paired)
  fill_continuous_sequential_warm <- create_continuous_fill_scale(palette_continuous_sequential_warm)
  fill_continuous_sequential_cold <- create_continuous_fill_scale(palette_continuous_sequential_cold)
  colour_continuous_sequential_warm <- create_continuous_colour_scale(palette_continuous_sequential_warm)
  colour_continuous_sequential_cold <- create_continuous_colour_scale(palette_continuous_sequential_cold)
  fill_continuous_diverging <- create_continuous_fill_scale(palette_continuous_diverging, TRUE)
  colour_continuous_diverging <- create_continuous_colour_scale(palette_continuous_diverging, TRUE)
  fill_binned_sequential_warm <- create_binned_fill_scale(palette_continuous_sequential_warm)
  fill_binned_sequential_cold <- create_binned_fill_scale(palette_continuous_sequential_cold)
  colour_binned_sequential_warm <- create_binned_colour_scale(palette_continuous_sequential_warm)
  colour_binned_sequential_cold <- create_binned_colour_scale(palette_continuous_sequential_cold)
  fill_binned_diverging <- create_binned_fill_scale(palette_continuous_diverging, TRUE)
  colour_binned_diverging <- create_binned_colour_scale(palette_continuous_diverging, TRUE)

# remove scale creation functions ----

  rm(
    create_discrete_fill_scale,
    create_continuous_fill_scale,
    create_discrete_colour_scale,
    create_continuous_colour_scale
  )

# scale wrappers ----

  fill_continuous_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(fill_continuous_sequential_warm(...))
    if (type == "cold") return(fill_continuous_sequential_cold(...))
  }
  colour_continuous_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(colour_continuous_sequential_warm(...))
    if (type == "cold") return(colour_continuous_sequential_cold(...))
  }
  fill_binned_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(fill_binned_sequential_warm(...))
    if (type == "cold") return(fill_binned_sequential_cold(...))
  }
  colour_binned_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(colour_binned_sequential_warm(...))
    if (type == "cold") return(colour_binned_sequential_cold(...))
  }
  fill_discrete_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(fill_discrete_sequential_warm(...))
    if (type == "cold") return(fill_discrete_sequential_cold(...))
  }
  colour_discrete_sequential <- function(type = "warm", ...) {
    if (type == "warm") return(colour_discrete_sequential_warm(...))
    if (type == "cold") return(colour_discrete_sequential_cold(...))
  }
  fill_continuous <- fill_continuous_sequential
  colour_continuous <- colour_continuous_sequential
  color_continuous_sequential <- colour_continuous_sequential
  color_continuous <- colour_continuous
  color_continuous_diverging <- colour_continuous_diverging
  color_continuous_sequential_warm <- colour_continuous_sequential_warm
  color_continuous_sequential_cold <- colour_continuous_sequential_cold

  fill_binned <- fill_binned_sequential
  colour_binned <- colour_binned_sequential
  color_binned_sequential <- colour_binned_sequential
  color_binned <- colour_binned
  color_binned_diverging <- colour_binned_diverging
  color_binned_sequential_warm <- colour_binned_sequential_warm
  color_binned_sequential_cold <- colour_binned_sequential_cold


  fill_discrete <- fill_discrete_qualitative
  colour_discrete <- colour_discrete_qualitative
  color_discrete_sequential <- colour_discrete_sequential
  color_discrete <- colour_discrete
  color_discrete_paired <- colour_discrete_paired
  color_discrete_qualitative <- colour_discrete_qualitative
  color_discrete_diverging <- colour_discrete_diverging
  color_discrete_sequential_warm <- colour_discrete_sequential_warm
  color_discrete_sequential_cold <- colour_discrete_sequential_cold
  fill_c <- function(type = "warm", ...) {
    switch(
      type,
      warm = fill_continuous_sequential_warm(...),
      cold = fill_continuous_sequential_cold(...),
      div = fill_continuous_diverging(...)
    )
  }
  colour_c <- function(type = "warm", ...) {
    switch(
      type,
      warm = colour_continuous_sequential_warm(...),
      cold = colour_continuous_sequential_cold(...),
      div = colour_continuous_diverging(...)
    )
  }
  color_c <- colour_c
  fill_b <- function(type = "warm", ...) {
    switch(
      type,
      warm = fill_binned_sequential_warm(...),
      cold = fill_binned_sequential_cold(...),
      div = fill_binned_diverging(...)
    )
  }
  colour_b <- function(type = "warm", ...) {
    switch(
      type,
      warm = colour_binned_sequential_warm(...),
      cold = colour_binned_sequential_cold(...),
      div = colour_binned_diverging(...)
    )
  }
  color_b <- colour_b
  fill_d <- function(type = "qual", ...) {
    switch(
      type,
      warm = fill_discrete_sequential_warm(...),
      cold = fill_discrete_sequential_cold(...),
      div = fill_discrete_diverging(...),
      pair = fill_discrete_paired(...),
      qual = fill_discrete_qualitative(...)
    )
  }
  colour_d <- function(type = "div", ...) {
    switch(
      type,
      warm = colour_discrete_sequential_warm(...),
      cold = colour_discrete_sequential_cold(...),
      div = colour_discrete_diverging(...),
      pair = colour_discrete_paired(...),
      qual = colour_discrete_qualitative(...)
    )
  }
  color_d <- colour_d