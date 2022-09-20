getVersionInfo <- function() {

  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]

  paste0("Version: ", currVersion) %>%
    paste('<div style="font-size:small;text-align: center;"><p>', .) %>%
    paste(., "</p></div>")
}
