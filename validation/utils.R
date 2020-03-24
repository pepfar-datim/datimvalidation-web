  require(datimvalidation)
  options(shiny.maxRequestSize=20*1024^2)
  options("baseurl" = "http://127.0.0.1:8080/")
  
  DHISLogin<-function(baseurl, username, password) {
    httr::set_config(httr::config(http_version = 0))
    url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
    #Logging in here will give us a cookie to reuse
    r <- httr::GET(url ,
                   httr::authenticate(username, password),
                   httr::timeout(60))
    if(r$status != 200L){
      return(FALSE)
    } else {
      me <- jsonlite::fromJSON(httr::content(r,as = "text"))
      options("organisationUnit" = me$organisationUnits$id)
      return(TRUE)
    }
  }
  
  getUserOperatingUnits<-function(uid) {
    
    #Global user
    if ( uid == "ybg3MO3hcf4" ) {
    getValidOperatingUnits()
    } else {

      getValidOperatingUnits() %>% 
        dplyr::filter(id == uid)
    }
  }