#' Generate GeoJSON boundary files for outline maps
#'
#' Downloads vector boundary data from \pkg{rnaturalearth} and writes
#' simplified GeoJSON files to a target directory.
#' These files can be used as overlay layers in plotly mapbox maps
#' to create NHM-themed outline-only maps without external tile
#' services.
#'
#' Three layers are produced:
#' \describe{
#'   \item{world_110m.geojson}{World country boundaries at 110 m
#'     Natural Earth scale — lightweight, suitable for global zoom.}
#'   \item{world_50m.geojson}{World country boundaries at 50 m
#'     Natural Earth scale — more detail for continental zoom.}
#'   \item{uk_states.geojson}{UK county / unitary-authority boundaries
#'     from Natural Earth 10 m states data (requires
#'     \pkg{rnaturalearthhires}).}
#' }
#'
#' @param output_dir Directory to write the GeoJSON files into. Created
#'   if it does not exist.
#' @param tolerance_110m Simplification tolerance for the 110 m layer
#'   (degrees). Default 0.5.
#' @param tolerance_50m Simplification tolerance for the 50 m layer
#'   (degrees). Default 0.1.
#' @param tolerance_states Simplification tolerance for the UK states
#'   layer (degrees). Default 0.01.
#' @param countries Character vector of country names for the states
#'   layer. Defaults to \code{"united kingdom"}.
#' @param verbose Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly, a named character vector of the file paths written.
#' @export
nhm_generate_boundaries <- function(
    output_dir,
    tolerance_110m  = 0.5,
    tolerance_50m   = 0.1,
    tolerance_states = 0.01,
    countries        = "united kingdom",
    verbose          = TRUE) {

  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Package 'rnaturalearth' is required.", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Disable s2 to avoid topology errors from simplification
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)

  # Helper: write GeoJSON + save as pre-parsed .rds for fast loading
  write_boundary <- function(sf_obj, geojson_path) {
    sf::st_write(sf_obj, geojson_path, driver = "GeoJSON",
                 delete_dsn = TRUE, quiet = TRUE)
    parsed <- jsonlite::fromJSON(geojson_path, simplifyVector = FALSE)
    rds_path <- sub("\\.geojson$", ".rds", geojson_path)
    saveRDS(parsed, rds_path)
  }

  paths <- character()

  # ── World 110 m ──────────────────────────────────────────────
  if (verbose) message("Generating world_110m.geojson ...")
  world110 <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  world110 <- world110[, "geometry"]
  world110 <- sf::st_simplify(world110, dTolerance = tolerance_110m,
                              preserveTopology = TRUE)
  p <- file.path(output_dir, "world_110m.geojson")
  write_boundary(world110, p)
  paths["world_110m"] <- p
  if (verbose) message("  -> ", nrow(world110), " features, ",
                       file.size(p), " bytes")

  # ── World 50 m ───────────────────────────────────────────────
  if (verbose) message("Generating world_50m.geojson ...")
  world50 <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  world50 <- world50[, "geometry"]
  world50 <- sf::st_simplify(world50, dTolerance = tolerance_50m,
                             preserveTopology = TRUE)
  p <- file.path(output_dir, "world_50m.geojson")
  write_boundary(world50, p)
  paths["world_50m"] <- p
  if (verbose) message("  -> ", nrow(world50), " features, ",
                       file.size(p), " bytes")

  # ── States / provinces ───────────────────────────────────────
  if (length(countries) > 0) {
    if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
      warning("Package 'rnaturalearthhires' is needed for states data. ",
              "Install with: install.packages('rnaturalearthhires', ",
              "repos = 'https://ropensci.r-universe.dev')",
              call. = FALSE)
    } else {
      for (cty in countries) {
        tag <- gsub("[^a-z0-9]+", "_", tolower(cty))
        fname <- paste0(tag, "_states.geojson")
        if (verbose) message("Generating ", fname, " ...")
        states <- rnaturalearth::ne_states(country = cty,
                                           returnclass = "sf")
        states <- states[, "geometry"]
        states <- sf::st_simplify(states, dTolerance = tolerance_states,
                                  preserveTopology = TRUE)
        p <- file.path(output_dir, fname)
        write_boundary(states, p)
        paths[paste0(tag, "_states")] <- p
        if (verbose) message("  -> ", nrow(states), " features, ",
                             file.size(p), " bytes")
      }
    }
  }

  if (verbose) message("Done. ", length(paths), " file(s) written to ",
                       normalizePath(output_dir))
  invisible(paths)
}
