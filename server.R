library(shiny)
library(shinyjs)
library(openxlsx)
library(magrittr)
require(futile.logger)
require(shinyWidgets)


################ OAuth Client information #####################################
if (interactive()) {
  # testing url
  options(shiny.port = 3123)
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

{

  oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                               key = Sys.getenv("OAUTH_KEYNAME"),        # dhis2 = Client ID
                               secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                               redirect_uri = APP_URL
  )

  oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"),"uaa/oauth"),
                                    request=NULL,# Documentation says to leave this NULL for OAuth2
                                    authorize = "authorize",
                                    access="token"
  )

  oauth_scope <- "ALL"
}

has_auth_code <- function(params) {

  return(!is.null(params$code))
}



shinyServer(function(input, output, session) {

  ready <- reactiveValues(ok = FALSE)

  observeEvent(input$file1, {
    shinyjs::enable("validate")
    ready$ok <- FALSE
  })

  observeEvent(input$validate, {
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })

observeEvent(input$datastream,{
  if(input$datastream == "SIMS") {
    updateSelectInput(session = session, inputId = "ou",
                      choices = (c("Global" = "ybg3MO3hcf4")))
  } else {
    foo <- getUserOperatingUnits(user_input$d2_session$user_orgunit,user_input$d2_session)
    setNames(foo$id,foo$name)
    updateSelectInput(session = session, inputId = "ou",choices = setNames(foo$id,foo$name))
  }

})
  observeEvent(input$reset_input, {
    enableUI()
    ready$ok<-FALSE
    shinyjs::hide("downloadData")
  })

  observeEvent(input$logout,{
    req(input$logout)
    # Gets you back to the login without the authorization code at top
    updateQueryString("?",mode="replace",session=session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated <- FALSE
    user_input$user_name <- ""
    user_input$authorized <- FALSE
    user_input$d2_session <- NULL
    d2_default_session <- NULL
    gc()
    session$reload()

  } )

  disableUI<-function(){
    shinyjs::disable("datastream")
    shinyjs::disable("type")
    shinyjs::disable("ou")
    shinyjs::disable("ou_scheme")
    shinyjs::disable("de_scheme")
    shinyjs::disable("id_scheme")
    shinyjs::disable("ds_type")
    shinyjs::disable("file1")
    shinyjs::disable("header")
  }

  enableUI<-function(){
    shinyjs::enable("datastream")
    shinyjs::enable("type")
    shinyjs::enable("ou")
    shinyjs::enable("ou_scheme")
    shinyjs::enable("de_scheme")
    shinyjs::enable("id_scheme")
    shinyjs::enable("ds_type")
    shinyjs::enable("file1")
    shinyjs::enable("header")
  }

  output$ui <- renderUI({

    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("server"),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            fileInput(
              "file1",
              "Choose data file:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                "application/json",
                "application/xml",
                "application/zip",
                ".csv",
                ".json",
                ".xml",
                ".zip"
              )
            ),
            selectInput(
              "datastream",
              "Dataset type:",
              getDataStreamsInput(user_input$d2_session)
            ),
            selectInput("type", "Type:",
                        c(
                          "CSV" = "csv",
                          "JSON" = "json",
                          "XML" = "xml"
                        )),
            selectInput("ou", "Operating Unit", user_input$user_ous),
            selectInput(
              "de_scheme",
              "Data element ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            selectInput(
              "ou_scheme",
              "Orgunit ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            selectInput(
              "id_scheme",
              "ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            checkboxInput("header", "CSV Header", TRUE),
            tags$hr(),
            actionButton("validate","Validate"),
            tags$hr(),
            selectInput("downloadType", "Download type", downloadTypes()),
            downloadButton("downloadOutputs", "Download"),
            tags$hr(),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("reset_input", "Reset inputs")),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("logout", "Logout"))
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages'))),
            tabPanel("Validation rules", DT::DTOutput("contents"))
          ))
        ))
  }
})

  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               d2_session = NULL,
                               user_ous = NA)

  # password entry UI components:
  # Login components
  output$uiLogin <- renderUI({
    wellPanel(
      fluidRow(img(src='pepfar.png', align = "center")),
        fluidRow(h4("Welcome to the DATIM Validation tool. Please login with your DATIM credentials:")
      ),
      fluidRow(
        actionButton("login_button_oauth","Log in with DATIM"),
        uiOutput("ui_hasauth"),
        uiOutput("ui_redirect")
    ),
    tags$hr(),
    fluidRow(HTML(getVersionInfo()))
    )
  })

  output$ui_redirect = renderUI({
    #print(input$login_button_oauth) useful for debugging
    if(!is.null(input$login_button_oauth)){
      if(input$login_button_oauth>0){
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })

  ### Login Button oauth Checks
  observeEvent(input$login_button_oauth > 0,{

    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    #Wait until the auth code actually exists
    req(has_auth_code(params))

    #Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )

    loginAttempt <- tryCatch({

      datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri= APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment())
      ) },
      # This function throws an error if the login is not successful
      error = function(e) {
        flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datapack")
      }
    )

    if (exists("d2_default_session")) {

      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL

      foo<-getUserOperatingUnits(user_input$d2_session$user_orgunit, d2session = user_input$d2_session )
      user_input$user_ous<-setNames(foo$id,foo$name)

      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "datapack"
      )
    }

  })

  validate<-function() {

    shinyjs::disable("downloadType")
    shinyjs::disable("downloadOutputs")
    if (!ready$ok) {return(NULL)}

    #Lock the UI and hide download button
    disableUI()
    inFile <- input$file1

    if (is.null(inFile)) return(NULL)

    messages<-list()
    vr_results<-list()
    has_error<-FALSE
    step_size <- 1/14

    withProgress(message = 'Validating file', value = 0,{

    incProgress(step_size, detail = ("Loading metadata"))
    ds <- getCurrentDataSets(datastream = input$datastream, d2session = user_input$d2_session)
    incProgress(step_size, detail = ("Parsing data"))
    validation<-list()

    if  ( inFile$type == "application/zip" )  {
      temp_dir<-tempdir()
      input_file<-unzip(inFile$datapath, exdir = temp_dir)
    } else {
      input_file<-inFile$datapath
    }

    # d <-  tryCatch({
      d <- datimvalidation::d2Parser(
        filename = input_file,
        type = input$type,
        datastream = input$datastream,
        organisationUnit = input$ou,
        dataElementIdScheme = input$de_scheme,
        orgUnitIdScheme = input$ou_scheme,
        idScheme = input$id_scheme,
        hasHeader = input$header,
        d2session = user_input$d2_session
      )
    # },
    # error = function(e) {
    #   return(e)
    # },
    # warning = function(w) {
    #   list(paste("Escalated warning to error: ", conditionMessage(w)))
    # })

      #Reset the button to force upload again
      shinyjs::reset("file1")
      disableUI()

      # if (inherits(d, "list")) {
      #   messages <- append( "ERROR! : There were errors while parsing the file. Please check that you have provided the correct paramaters!", messages)
      #   messages <- append( d, messages)
      #   return(NULL)
      #
      # } else {
      #
      #   messages<-append("No problems found during file parsing.",messages)
      # }

      #Common checks for all data
      #Period check
      incProgress(step_size, detail = ("Checking periods."))

      periods <- unique(d$data$import$period)
      msg <- paste(paste( "Periods found: ", periods,sep="",collapse=","))

        d$info$messages <-  datimvalidation::appendMessage( d$info$messages, msg, "INFO")

        #Duplicates check
        incProgress(step_size, detail = ("Checking for duplicate records."))

        d <- getExactDuplicates(d)

        #Check that orgunits are in the provided user hierarchy
        incProgress(step_size, detail = ("Checking sites are within the operating unit"))
        d <-  checkOrgunitsInHierarchy(d = d,
                                       userOrgUnit = input$ou,
                                       d2session = user_input$d2_session)

        incProgress(step_size, detail = ("Checking data element/mechanism associations"))
        d <-
          checkDataElementMechValidity(
            d = d,
            datasets = ds,
            d2session = user_input$d2_session
          )

        #Check data element and organisation unit associations
        incProgress(step_size, detail = ("Checking data element/orgunit associations"))
        d <-
          checkDataElementOrgunitValidity(
            d = d,
            datasets = ds,
            d2session = user_input$d2_session
          )


        #Check data element / category option combinations
        incProgress(step_size, detail = ("Checking data element/disagg associations"))

        d <-
          checkDataElementDisaggValidity(d, datasets = ds,
                                         d2session = user_input$d2_session)
        #Mechanism check
        incProgress(step_size, detail = ("Checking mechanisms."))

        d <-
          checkMechanismValidity(
            d = d,
            organisationUnit = input$ou,
            d2session = user_input$d2_session
          )


        #Only for narratives

        if (input$datastream %in% c("NARRATIVES")) {
          shinyjs::hide("contents")
          d <- checkNarrativeLength(d)
        }

        #Only for MER Results and Targets
        if (input$datastream %in% c("RESULTS","TARGETS")) {
          #Zeros
          incProgress(step_size, detail = ("Checking zeros: "))

          zero_check<-sprintf("%1.2f%%",  ( sum(as.character(d$data$import$value) == "0") / NROW(d$data$import) )  * 100 )
          msg <- paste(paste(
              "Records found: ",
              NROW(d$data$import),
              " records found of which ",
              zero_check,
              " were zeros."
            ))
          d$info$messages <-  datimvalidation::appendMessage(d$info$messages,
                                                             msg,"INFO" )

        #Data element cadence check
        incProgress(step_size, detail = ("Checking data elemement cadence"))
        d <- checkDataElementCadence(d, d2session = user_input$d2_session)

        #Value type compliance check
        incProgress(step_size, detail = ("Checking value type compliance."))

        d <- checkValueTypeCompliance(d, d2session = user_input$d2_session)

        #Negative value check
        incProgress(step_size, detail = ("Checking negative numbers."))

        d <- checkNegativeValues(d, d2session = user_input$d2_session)

        incProgress(step_size, detail = ("Checking validation rules..."))

        if ( Sys.info()['sysname'] == "Linux") {

          ncores <- parallel::detectCores() - 1
          doMC::registerDoMC(cores=ncores)
          is_parallel<-TRUE

        } else {
          is_parallel<-FALSE
        }

        d$tests$validation_rules <-validateData(d$data$import,
                         organisationUnit = input$ou,
                          parallel = is_parallel,
                          d2session = user_input$d2_session)
        if (NROW(d$tests$validation_rules) > 0) {
          msg <- paste("WARNING!", NROW(d$tests$validation_rules),
                       "validation rule violations found.")
          d$info$messages <- datimvalidation::appendMessage(d$info$messages,
                                                            msg,
                                                            "WARNING")
        } else {
          msg <- paste("No validation rule violations found.")
          d$info$messages <- datimvalidation::appendMessage(d$info$messages,
                                                            msg,
                                                            "INFO")
        }
        } #end RESULTS/TARGETS Checks


    }) #End progress bar
    shinyjs::enable("downloadType")
    shinyjs::enable("downloadOutputs")

    d

  } #End validate function

  validation_results <- reactive({ validate() })

  output$downloadOutputs <- downloadHandler(
    filename = function() {
      d <- validation_results()
      prefix <- input$downloadType
      date <- date <- format(Sys.time(), "%Y%m%d_%H%M%S")

      suffix <- if (input$downloadType %in% c("csv_import")) {
        ".csv"
      } else {
        ".xlsx"
      }

      paste0(prefix, "_", date, suffix)
    },
    content = function(file) {

      d <- validation_results()

      if (input$downloadType == "csv_import") {
        write.table(d$data$import,
                    file = file,
                    row.names = FALSE,
                    quote = TRUE,
                    na = "",
                    sep = ",")
      }

      if (input$downloadType == "vr_rules") {
        #The exact structure of the tests is unknown, but filter out anything
        #which is not a data frame.

        is_data_frame <- unlist(lapply(lapply(d$tests, class), function(x) "data.frame" %in% x))

        d$tests <- d$tests[is_data_frame]

        sheets_with_data <- d$tests[lapply(d$tests, NROW) > 0] %>%
          #Limit the number of rows to the maximum in Excel
          purrr::map(., ~ dplyr::slice(.x, 1:1048575)) %>%
          #Collapses nested lists to a string which will fit inside of excel
          purrr::map(., ~ .x %>%
                       dplyr::mutate_if(is.list, \(x) paste(as.character(x[[1]]), sep = "", collapse = ","))) %>%
          #Convert everything to characters and apply Excel limits
          purrr::map(., ~ .x %>%
                       dplyr::mutate_if(is.character, \(x) substring(as.character(x), 0, 36766)))


        if (length(sheets_with_data) > 0) {
          openxlsx::write.xlsx(sheets_with_data, file = file)
        } else {
          showModal(modalDialog(
            title = "Perfect score!",
            "No validation issues, so nothing to download!"
          ))
        }
      }
    }
  )

  output$contents <- DT::renderDT({

    results<-validation_results() %>%
      purrr::pluck(., "tests") %>%
      purrr::pluck(., "validation_rules")

    if ( NROW(results) > 0 ) { results }
    else { NULL }
  })

  output$messages <- renderUI({

    vr<-validation_results()

    messages<-NULL

    if ( is.null(vr)) {
      return(NULL)
    }

    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )

    } else {

      messages <- validation_results() %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "messages")

      if (length(messages$message) > 0) {

        class(messages) <- "data.frame"

        messages %<>%
          dplyr::mutate(level = factor(level, levels = c("ERROR", "WARNING", "INFO"))) %>%
          dplyr::arrange(level) %>%
          dplyr::mutate(msg_html =
                          dplyr::case_when(
                            level == "ERROR" ~ paste('<li><p style = "color:red"><b>', message, "</b></p></li>"),
                            TRUE ~ paste("<li><p>", message, "</p></li>")
                          ))

        messages_sorted <-
          paste0("<ul>", paste(messages$msg_html, sep = "", collapse = ""), "</ul>")

        shiny::HTML(messages_sorted)
      } else {
        tags$li("No Issues with Integrity Checks: Congratulations!")
      } }
  })
})
