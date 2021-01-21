  require(datimvalidation)
  require(config)
  require(futile.logger)
  options(shiny.maxRequestSize=20*1024^2)
  config <- config::get()


  
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
  

  getUserOperatingUnits<-function(uid,d2session = d2_default_session) {
    
    #Global user
    if ( uid == "ybg3MO3hcf4" ) {
      datimvalidation::getValidOperatingUnits(d2session = d2session)

    } else {

      getValidOperatingUnits(d2session = d2session) %>% 
        dplyr::filter(id == uid)
    }
  }