# Build Website uses this generator through index.Rmd's site setting.
# Unlike _site.yml's resource-copy exclude field, render_exclude skips knitting.
presentation_site <- function(input, ...) {
  config <- rmarkdown::site_config(input)
  excluded <- config$render_exclude
  generator <- rmarkdown::default_site_generator(input, ...)
  render_default <- generator$render

  generator$render <- function(input_file, output_format, envir, quiet, ...) {
    if (is.null(input_file)) {
      input_file <- list.files(input, pattern = "^[^_].*\\.[Rr]?md$", full.names = TRUE)
      input_file <- input_file[
        !grepl("^README\\.R?md$", basename(input_file)) &
          !basename(input_file) %in% excluded]
      if (!quiet && length(excluded))
        message("Skipping automatic rendering: ", paste(excluded, collapse = ", "))
      if (!length(input_file)) return(invisible(NULL))
    }
    # An explicit file list uses the default generator's incremental mode,
    # preserving the existing proposal HTML and its assets under docs/.
    # A manually requested single-file render is still allowed.
    render_default(input_file = input_file, output_format = output_format,
                   envir = envir, quiet = quiet, ...)
  }
  generator
}
