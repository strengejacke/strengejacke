.onAttach <- function(...) {
  sj_versions <- .sj_version()
  sj_pkgs <- c("ggeffects", "sjlabelled", "sjmisc", "sjstats", "sjPlot", "esc")
  needed <- sj_pkgs[!is_attached(sj_pkgs)]

  if (length(needed) == 0)
    return()

  sj_versions <- sj_versions[sj_versions$package %in% needed, ]
  suppressPackageStartupMessages(suppressWarnings(lapply(sj_versions$package, library, character.only = TRUE, warn.conflicts = FALSE)))

  needs_update <- sj_versions$behind
  sj_versions <- sj_versions[, c("package", "local")]

  max_len_pkg <- max(nchar(sj_versions$package))
  max_len_ver <- max(nchar(sj_versions$local))

  insight::print_color("# Attaching packages", "blue")

  if (any(needs_update)) {
    insight::print_color(" (", "blue")
    insight::print_color("red", "red")
    insight::print_color(" = needs update)", "blue")
  }

  cat("\n")

  symbol_tick <- "\u2714 "
  symbol_warning <- "\u26A0 "

  for (i in 1:nrow(sj_versions)) {
    if (needs_update[i])
      insight::print_color(symbol_warning, "red")
    else
      insight::print_color(symbol_tick, "green")

    cat(format(sj_versions$package[i], width = max_len_pkg))
    cat(" ")
    insight::print_color(format(sj_versions$local[i], width = max_len_ver), ifelse(needs_update[i], "red", "green"))

    if (i %% 2 == 0)
      cat("\n")
    else
      cat("   ")
  }

  cat("\n")
  if (.cran_checks()) cat("\n")

  if (any(needs_update)) {
    insight::print_color("Update packages in red with 'sj_update()'.\n", "yellow")
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}



#' Update sj-packages and its dependencies from CRAN, if necessary.
#'
#' @param which String, indicates whether sj-packages (\code{which = "core"}), dependencies (\code{which = "deps"}) or both (\code{which = "all"}) should be checked for available updates.
#' @importFrom utils menu install.packages
#' @export
sj_update <- function(which = c("all", "core", "deps")) {
  which <- match.arg(which)

  if (which %in% c("all", "core")) {
    core <- .sj_version()
    behind <- core[core$behind, ]

    if (nrow(behind) == 0) {
      insight::print_color("All sj-packages are up to date!\n", "green")
      return(invisible())
    }

    message("The following packages are out of date:")
    message(paste0(" * ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")"), collapse = "\n")

    message("Update now?")
    do_it <- utils::menu(c("Yes", "No")) == 1

    if (!do_it) {
      return(invisible())
    }

    # detach packages before installing
    lapply(behind$package, unloadNamespace)

    utils::install.packages(
      behind$package,
      quiet = TRUE,
      dependencies = FALSE
    )
  }

  if (which %in% c("all", "deps")) {
    deps <- .sj_deps()
    behind <- deps[deps$behind, ]

    if (nrow(behind) == 0) {
      insight::print_color("All sj-dependencies are up to date!\n", "green")
      return(invisible())
    }

    message("The following packages are out of date:")
    message(paste0(" * ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")"), collapse = "\n")

    message("Update now?")
    do_it <- utils::menu(c("Yes", "No")) == 1

    if (!do_it) {
      return(invisible())
    }

    # detach packages before installing
    lapply(behind$package, unloadNamespace)

    utils::install.packages(
      behind$package,
      quiet = TRUE,
      dependencies = FALSE
    )
  }

  invisible()
}


#' @importFrom utils available.packages packageVersion
#' @importFrom tools package_dependencies
.sj_deps <- function() {
  pkgs <- utils::available.packages()

  deps <-
    tools::package_dependencies(
      c("ggeffects", "sjlabelled", "sjmisc", "sjstats", "sjPlot", "esc"),
      pkgs,
      recursive = F
    )

  pkg_deps <- unique(sort(unlist(deps)))

  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"], package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)

  behind <- mapply('>', cran_version, local_version)

  data.frame(
    package = pkg_deps,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    stringsAsFactors = FALSE
  )
}



#' @importFrom utils available.packages packageVersion
#' @importFrom tools package_dependencies
.sj_version <- function() {
  pkgs <- tryCatch(
    {
      utils::available.packages(contriburl = contrib.url("https://cran.r-project.org", type = getOption("pkgType")))
    },
    warning = function(w) { NULL },
    error = function(e) { NULL}
  )

  sj_on_cran <- c("ggeffects", "sjlabelled", "sjmisc", "sjstats", "sjPlot", "esc")

  if (!is.null(pkgs)) {
    cran_version <- lapply(pkgs[sj_on_cran, "Version"], package_version)
    local_version <- lapply(sj_on_cran, utils::packageVersion)

    behind <- mapply('>', cran_version, local_version)

    out <- data.frame(
      package = sj_on_cran,
      cran = sapply(cran_version, as.character),
      local = sapply(local_version, as.character),
      behind = behind,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    local_version <- lapply(sj_on_cran, utils::packageVersion)

    out <- data.frame(
      package = sj_on_cran,
      cran = NA,
      local = sapply(local_version, as.character),
      behind = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
}



#' Install the strengejacke suite from github
#'
#' @importFrom devtools install_github
#' @export
install_sj_latest <- function() {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install_github(
      c("strengejacke/sjlabelled",
        "strengejacke/sjstats",
        "strengejacke/sjmisc",
        "strengejacke/ggeffects",
        "strengejacke/sjPlot",
        "strengejacke/esc"
      ), upgrade = "never"
    )
  } else {
    message("Package \"devools\" required.")
  }

}


#' @importFrom xml2 read_html
#' @importFrom rvest html_table
.cran_checks <- function(full = FALSE) {
  on_cran <- c("ggeffects", "sjlabelled", "sjmisc", "sjstats", "sjPlot", "esc")
  error <- FALSE
  tryCatch(
    {
      for (i in on_cran) {
        url <- sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", i)
        html_page <- xml2::read_html(url)
        html_table <- rvest::html_table(html_page)
        check_status <- html_table[[1]]$Status

        if (isTRUE(full)) {
          all_ok <- TRUE
          max_len <- max(nchar(on_cran))
          i <- format(i, width = max_len)
          cat(sprintf("%s ", i))

          if (any("error" %in% tolower(check_status))) {
            n <- sum("error" == tolower(check_status))
            if (n == 1)
              insight::print_color("1 Error", "red")
            else
              insight::print_color(sprintf("%g Errors", n), "red")
            error <- TRUE
            all_ok <- FALSE
          }
          if (any(c("warn", "warning") %in% tolower(check_status))) {
            if (!all_ok) cat(", ")
            n <- sum("warning" == tolower(check_status))
            if (n == 1)
              insight::print_color("1 Warning", "red")
            else
              insight::print_color(sprintf("%g Warnings", n), "red")
            error <- TRUE
            all_ok <- FALSE
          }
          if (any("note" %in% tolower(check_status))) {
            if (!all_ok) cat(", ")
            n <- sum("note" == tolower(check_status))
            if (n == 1)
              insight::print_color("1 Note", "blue")
            else
              insight::print_color(sprintf("%g Notes", n), "blue")
            all_ok <- FALSE
          }
          if (isTRUE(all_ok)) {
            insight::print_color("Ok", "green")
          }
          cat("\n")
        } else {
          if (any(c("warning", "error") %in% tolower(check_status))) {
            insight::print_color(sprintf("Warnings or errors in CRAN checks for package '%s'.\n", i), "red")
            error <- TRUE
          }
        }
      }

      invisible(error)
    },
    warning = function(w) { invisible(FALSE) },
    error = function(e) { invisible(FALSE) }
  )
}



#' Show CRAN check status for strengejacke-packages
#'
#' @export
CRAN_checks <- function() {
  .cran_checks(full = TRUE)
}
