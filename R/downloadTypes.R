downloadTypes <- function() {

  download_names <-
    c(
      "CSV Import File",
      "Validations"
    )
  download_types <-
    c("csv_import",
      "vr_rules")

  names(download_types) <- download_names

  download_types
}
