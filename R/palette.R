# TODO ----

  #' - add lighten, darken and desaturate arguments to all palettes?
  #' - then also to scales?

# palettes ----

  # continuous
    palette_continuous_sequential_warm <- function(n, ...) {
      if ("rev" %in% names(list(...))) rev <- list(...)$rev
      else rev <- FALSE
      rev <- !rev
      colorspace::sequential_hcl(n = n,
                                 ...,
                                 h1 =  0, h2 =  85,
                                 c1 = 90, c2 =  45,
                                 l1 = 37, l2 =  98,
                                 p1 =  1, p2 = 1.2,
                                 cmax = 120,
                                 rev = rev)
    }
    palette_continuous_sequential_cold <- function(n, ...) {
      if ("rev" %in% names(list(...))) rev <- list(...)$rev
      else rev <- FALSE
      rev <- !rev
      colorspace::sequential_hcl(n = n,
                                 ...,
                                 h1 = 270, h2 =  85,
                                 c1 =  65, c2 =  45,
                                 l1 =  37, l2 =  98,
                                 p1 =   1, p2 = 1.2,
                                 cmax = 65,
                                 rev = rev)
    }
    palette_continuous_diverging <- function(n, ...) {
      colorspace::divergingx_hcl(n + 2, ..., palette = "Spectral")[-c(1, n + 2)]
    }

  # discrete

    palette_discrete_sequential_warm <- function(n, ...) {
      if ("rev" %in% names(list(...))) rev <- list(...)$rev
      else rev <- FALSE
      rev <- !rev
      colorspace::sequential_hcl(n = n + 1,
                                 ...,
                                 h1 =  0, h2 =  85,
                                 c1 = 90, c2 =  45,
                                 l1 = 37 + 5, l2 =  98 - 5,
                                 p1 =  1 - 0.3, p2 = 1.2,
                                 cmax = 120 + 40,
                                 rev = rev)[-1]
    }
    palette_discrete_sequential_cold <- function(n, ...) {
      if ("rev" %in% names(list(...))) rev <- list(...)$rev
      else rev <- FALSE
      rev <- !rev
      colorspace::sequential_hcl(n = n + 1,
                                 ...,
                                 h1 = 270, h2 =  85,
                                 c1 =  65, c2 =  45,
                                 l1 =  37 + 5, l2 =  98 - 5,
                                 p1 =   0, p2 = 1.2,
                                 cmax = 65,
                                 rev = rev)[-1]
    }
    palette_discrete_diverging <- function(n, ...) {
      colorspace::divergingx_hcl(n = n + 2,
                                 ...,
                                 h1 = 0, h2 = 85, h3 = 270,
                                 c1 = 90, c2 = 45, c3 = 65,
                                 l1 = 37 + 5, l2 = 98 - 5, l3 = 37 + 5,
                                 p1 = 1 - 0.3, p2 = 1.2, p3 = 1 - 1, p4 = 1.2,
                                 cmax1 = 120 + 40, cmax2 = NA)[-c(1, n + 2)]
    }
    palette_discrete_qualitative <- function(n) {
      cols <- palette_discrete_diverging(n)
      cols2 <- cols
      for (i in 1:n) {
        if (i %% 2 == 0) cols2[i] <- rev(cols)[i %/% 2]
        else cols2[i] <- cols[i %/% 2 + 1]
      }
      cols2
    }
    palette_discrete_paired <- function(n) {
      nr <- ifelse(n %% 2 == 0, n, n + 1)
      cols <- palette_discrete_qualitative(nr/2)
      cols_light <- colorspace::lighten(cols, 0)
      cols_dark <- colorspace::darken(cols, amount = .2)
      cols <- rep(cols_light, each = 2)
      cols[seq(2, nr, 2)] <- cols_dark
      if (n %% 2 != 0) cols <- cols[-(n+1)]
      cols
    }

# palette wrappers ----

  palette_continuous_sequential <- function(n, type = "warm", ...) {
    if (type == "warm") return(palette_continuous_sequential_warm(n, ...))
    if (type == "cold") return(palette_continuous_sequential_cold(n, ...))
  }
  palette_discrete_sequential <- function(n, type = "warm", ...) {
    if (type == "warm") return(palette_discrete_sequential_warm(n, ...))
    if (type == "cold") return(palette_discrete_sequential_cold(n, ...))
  }
  palette_continuous <- palette_continuous_sequential
  palette_discrete <- palette_discrete_qualitative

# helpers for designing and inspecting palettes ----

  palette_overview <- function(fn, n = 7) {
    pal_list <- list(
      "Continuous sequential warm" = palette_continuous_sequential(n, "warm"),
      "Continuous sequential cold" = palette_continuous_sequential(n, "cold"),
      "Continuous diverging" = palette_continuous_diverging(n),
      "Discrete sequential warm" = palette_discrete_sequential(n, "warm"),
      "Discrete sequential cold" = palette_discrete_sequential(n, "cold"),
      "Discrete diverging" = palette_discrete_diverging(n),
      "Discrete qualitative" = palette_discrete_qualitative(n),
      "Discrete paired" = palette_discrete_paired(n)
    )
    if (!missing(fn)) pal_list <- lapply(pal_list, fn)
    colorspace::swatchplot(pal_list)
  }
  palette_check <- function(cols) {
    colorspace::swatchplot(
      "Original" = cols,
      "Deutan CVD" = colorspace::deutan(cols),
      "Tritan CVD" = colorspace::tritan(cols),
      "Protan CVD" = colorspace::protan(cols),
      "Desaturated" = colorspace::desaturate(cols)
    )
  }
