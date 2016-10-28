load <- c("sjmisc", "sjstats", "sjPlot")

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