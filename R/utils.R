getVersionInfo <- function() {

  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]

  paste0("Version: ", currVersion) %>%
    paste('<div style="font-size:small;text-align: center;"><p>', .) %>%
    paste(., "</p></div>")
}




getUserOperatingUnits <- function(uid,
                                  d2session = d2_default_session) {
  #SIMS should always be validated in the context of a global user
    #Global user, then get all operating units
    if (uid == "ybg3MO3hcf4") {
      datimvalidation::getValidOperatingUnits(d2session = d2session) %>%
        dplyr::bind_rows(.,data.frame(name="Global",id="ybg3MO3hcf4")) %>%
        dplyr::arrange(name)

    } else {
      getValidOperatingUnits(d2session = d2session) %>%
        dplyr::filter(id == uid)
    }
}


getDataStreamsInput <- function(d2session) {
  if (d2session$user_orgunit == "ybg3MO3hcf4") {
    c("Results" = "RESULTS", "Targets" = "TARGETS", "Narratives" = "NARRATIVES", "SIMS" = "SIMS")
  } else
  {
    c("Results" = "RESULTS", "Targets" = "TARGETS", "Narratives" = "NARRATIVES")
  }
}
