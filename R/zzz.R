`%||%` <- function(a, b) if (is.null(a)) b else a

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "shinynhm",
    directoryPath = system.file("www", package = "shinynhm")
  )
}

.onUnload <- function(libname) {
  shiny::removeResourcePath("shinynhm")
}
