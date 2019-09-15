.onAttach <- function(...) {
  pkgs <- utils::available.packages(contriburl = contrib.url("https://cran.r-project.org", type = getOption("pkgType")))
  sj_versions <- sj_version(pkgs)
  sj_pkgs <- c("sjlabelled", "sjmisc", "sjstats", "sjPlot", "ggeffects", "esc")
  needed <- sj_pkgs[!is_attached(sj_pkgs)]

  if (length(needed) == 0)
    return()

  easystats_versions <- .easystats_version(pkgs)
  easystats_versions <- easystats_versions[easystats_versions$behind, ]

  sj_versions <- sj_versions[sj_versions$package %in% needed, ]
  suppressPackageStartupMessages(suppressWarnings(lapply(sj_versions$package, library, character.only = TRUE, warn.conflicts = FALSE)))

  needs_update <- sj_versions$behind
  sj_versions <- sj_versions[, c("package", "local")]

  max_len_pkg <- max(nchar(sj_versions$package))
  max_len_ver <- max(nchar(sj_versions$local))

  insight::print_color("# Attaching packages (red = needs update)\n", "blue")

  for (i in 1:nrow(sj_versions)) {
    cat(paste0("* ", format(sj_versions$package[i], width = max_len_pkg)))
    cat(" ")
    insight::print_color(format(sj_versions$local[i], width = max_len_ver), ifelse(needs_update[i], "red", "green"))
    cat("\n")
  }

  if (length(easystats_versions)) {
    insight::print_color(sprintf("\nFollowing packages should be updated: %s\n", paste0(easystats_versions$package, collapse = ", ")), color = "red")
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}


#' Update sj!-packages from CRAN, if necessary.
#'
#' @importFrom insight print_color
#' @importFrom utils menu install.packages
#' @export
sj_update <- function() {
  pkgs <- utils::available.packages(contriburl = contrib.url("https://cran.r-project.org", type = getOption("pkgType")))
  sj_pkgs <- sj_version(pkgs)
  behind <- sj_pkgs[sj_pkgs$behind, ]

  if (nrow(behind) == 0) {
    insight::print_color("All sj!-packages are up to date!\n", "green")
    return(invisible())
  }

  message("The following packages are out of date:")
  message(paste0("* ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")"), collapse = "\n")

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
sj_version <- function(pkgs) {
  sj_pkgs <- c("sjlabelled", "sjmisc", "sjstats", "sjPlot", "ggeffects", "esc")
  sj_on_cran <- intersect(sj_pkgs, rownames(pkgs))
  # sj_not_on_cran <- setdiff(sj_pkgs, sj_on_cran)

  cran_version <- lapply(pkgs[sj_on_cran, "Version"], package_version)
  local_version <- lapply(sj_on_cran, utils::packageVersion)
  # local_version_dev <- lapply(sj_not_on_cran, utils::packageVersion)

  behind <- mapply('>', cran_version, local_version)

  data.frame(
    package = sj_on_cran,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
    # data.frame(
    #   package = sj_not_on_cran,
    #   cran = NA,
    #   local = sapply(local_version_dev, as.character),
    #   behind = FALSE,
    #   stringsAsFactors = FALSE,
    #   row.names = NULL
    # )
}


#' @importFrom utils available.packages packageVersion
#' @importFrom tools package_dependencies
.easystats_version <- function(pkgs) {
  easystats_pkgs <- c("insight", "bayestestR", "performance", "parameters", "see", "correlation", "estimate", "report")
  easystats_on_cran <- intersect(easystats_pkgs, rownames(pkgs))

  cran_version <- lapply(pkgs[easystats_on_cran, "Version"], package_version)
  local_version <- lapply(easystats_on_cran, utils::packageVersion)

  behind <- mapply('>', cran_version, local_version)

  data.frame(
    package = easystats_on_cran,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
