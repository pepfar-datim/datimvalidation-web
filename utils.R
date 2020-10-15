  require(datimvalidation)
  require(config)
  require(futile.logger)

  
  options(shiny.maxRequestSize=20*1024^2)
  config <- config::get()
  options("baseurl" = config$baseurl)

  
  config <- config::get()
  options("baseurl" = config$baseurl)
  
  #Try and create the log directory if it does not exist
  if (!dir.exists(dirname(config$log_path))) {
    tryCatch({
      dir.create(dirname(config$log_path))
    } ,
    error = function(e) {
      warning("Could not create log directory")
    })
    
  }
  #Try and create the log file if it does not exist
  if (!file.exists(config$log_path)) {
    
    tryCatch({
      file.create(config$log_path)
    } ,
    error = function(e) {
      warning("Could not create log file")
    })
  }
  
  #Finally, test for write permission to the file. 
  #Fallback to the console logger if all else fails
  if ( file.access(config$log_path,mode = 2) == -1L ) {
    warning("Cannot write to log file. Falling back to console")
    flog.appender(futile.logger::appender.console())
  } else
  {
    print(paste("About to use a file appender log at",config$log_path))
    flog.appender(futile.logger::appender.file(config$log_path))
  }
  
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