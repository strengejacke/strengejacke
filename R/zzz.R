load <- c("sjlabelled", "sjmisc", "sjstats", "sjPlot")

.onAttach <- function(...) {
  needed <- load[!is_attached(load)]

  if (length(needed) == 0)
    return()

  packageStartupMessage(paste0("Loading sj!-packages: ", needed, collapse = "\n"))
  lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}


#' @importFrom dplyr filter
#' @importFrom utils menu install.packages
sj_update <- function() {

  deps <- sj_deps()
  behind <- dplyr::filter(deps, behind)

  if (nrow(behind) == 0) return(invisible())

  message("The following packages are out of date:")
  message(paste0(" * ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")"), collapse = "\n")

  message("Update now?")
  do_it <- utils::menu(c("Yes", "No")) == 1

  if (!do_it) {
    return(invisible())
  }

  utils::install.packages(
    behind$package,
    quiet = TRUE,
    dependencies = FALSE
  )

  invisible()
}


#' @importFrom utils available.packages packageVersion
#' @importFrom tools package_dependencies
#' @importFrom purrr map2_lgl map_chr
#' @importFrom tibble tibble
sj_deps <- function() {
  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies("tidyverse", pkgs, recursive = F)

  pkg_deps <- unique(sort(unlist(deps)))

  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"], package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)

  behind <- purrr::map2_lgl(cran_version, local_version, `>`)

  tibble::tibble(
    package = pkg_deps,
    cran = purrr::map_chr(cran_version, as.character),
    local = purrr::map_chr(local_version, as.character),
    behind = behind
  )
}