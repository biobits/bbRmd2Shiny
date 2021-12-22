

#' A shiny runtime for parametrized r markdown reports.
#'
#' @param file Path to the R Markdown document with configurable parameters.
#' @param params A named list of optional parameter overrides used in place of the document defaults.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#' @param save_caption Caption to use use for button that saves/confirms parameters.
#' @param encoding The encoding of the input file; see \code{\link{file}}.
#' @param run_caption Caption of run button
#' 
#' @return named list with overridden parameter names and value.
#' 
#' @description  
#' The knit_rmd_to_shiny function starts a shiny app that reads the YAML header of a given 
#' rmd document and shows a corresponding input form for the parameters defined in the yaml
#' header. knit_rmd_to_shiny is a modified version of the knit_params_ask function from the rmarkdown
#' package that is used fpor parametrized rmd reports in RStudio. 
#'
#' In addition to that special YAML tags can be used to modify the appearance of the shiny app
#' 
#' @export
#' 
knit_rmd_to_shiny <- function(file = NULL,
                            params = NULL,
                            shiny_args = NULL,
                            save_caption = "Save",
                            run_caption ="Run Report",
                            encoding = "UTF-8",
                            defaultlogo="UCCH_Front.png") {
  

  if (is.null(file)) {
    stop("knit_params_ask must have a non-NULL file or input_lines parameter")
  }
  yamlData<-rmarkdown::yaml_front_matter(file)
  #knit_params <- bbAddParamNames(yamlData$params)
  
  #We need to load the yaml header twice because kit_params from knitr package is able to evaluate embedded R code
  knit_params<- bbKnitrGetParams(file)
  #Custom YAML Item for notifications in Shiny App
  report_note<-yamlData$reportnotification
  
  # List of possible outputformats as defined in rmd file
  outputformats<- bbAddOutputShortNames(yamlData$output)
  ## Input validation on params (checks shared with render)
  if (!is.null(params)) {
    ## Must be a named list
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }
    ## We do not validate names(params) because the document may have changed
    ## but we're loading parameters that were configured with an older
    ## version.
  }
  
  ## If we happen to not have any knit_params, just return an empty named list
  ## and don't fire up the Shiny app.
  # if (length(knit_params) == 0) {
  #   return(params_namedList()) 
  # }

  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)
  
  ## This set of published values is the raw set that came from the user.
  ## It does not include those values that cannot be configured or are
  ## left to use the default.
  values <- params_namedList()
  
  server <- function(input, output, session) {
    
     if(!is.null(report_note)){
      display_report_note(report_note)
     }
    
    # Initial Load of html if present otherwise display_Report shows logo
    #ToD:o If a pdf is present display this if there is no html report
    htmlfile<-paste0(tools::file_path_sans_ext(file),".html")
    
    output$inc<-renderUI({
     
      display_Report(reportfile =  htmlfile) 
     
      })
      
    output$reportTitle<-renderText({ 
                    ret<-ifelse(!is.null(yamlData$reporttitle),yamlData$reporttitle,"Report")
                  #  browser()
                  ret
              })
    
    
    param.ui <- function(param) {
      inputControlFn <- params_get_control(param)
      inputControlFnFormals <- names(formals(inputControlFn))
      
      inputId <- param$name
      label <- params_label(inputControlFn, param)
      
      arguments = list(
        inputId = inputId,
        label = label
      )
      
      # We MUST process the "value" name even if it is not present (due to
      # NULL values).
      attrib_names <- unique(c(names(param), "value"))
      lapply(attrib_names, function(name) {
        if (name %in% c("name", "input", "expr")) {
        } else if (name == "label") {
          arguments$label <<- label
        } else if (name == "value") {
          
          ## The current value for this parameter is either `params` when
          ## overridden by our caller or `param$value` otherwise.
          current_value <- param$value
          if (!is.null(params)) {
            override <- params[[param$name]]
            if (!is.null(override)) {
              current_value <- override
            }
          }
          # Now, transform into something that the input control can handle.
          current_value <- params_value_to_ui(inputControlFn, current_value,
                                              param$show_default)
          
          # value maps to either "value" or "selected" depending on the control.
          if ("value" %in% inputControlFnFormals) {
            arguments$value <<- current_value
          } else if ("selected" %in% inputControlFnFormals) {
            arguments$selected <<- current_value
          }
        } else if (name == "show_default") {
          # No-op
        } else {
          ## Not a special field. Blindly promote to the input control.
          arguments[[name]] <<- if (inherits(param[[name]], 'knit_param_expr')) {
            param[[name]][['value']]
          } else param[[name]]
        }
      })
      
      ## This is based on param$value not current_value because we want to
      ## understand deviation from the report default, not any (optional)
      ## call-time override.
      uidefault <- params_value_to_ui(inputControlFn, param$value, param$show_default)
      hasDefaultValue <- function(value) {
        identical(uidefault, value)
      }
      
      inputControl <- NULL
      unsupported <- setdiff(names(arguments), inputControlFnFormals)
      if (length(unsupported) > 0) {
        inputControl <- shiny::div(class = "form-group",
                                   tags$label(class = "control-label",param$name),
                                   shiny::div(paste('Cannot customize the parameter "', param$name, '" ',
                                                    'because the "', params_get_input(param), '" ',
                                                    'Shiny control does not support: ',
                                                    paste(unsupported, collapse = ', '), sep = '')))
      } else {
        inputControl <- do.call(inputControlFn, arguments)
      }
      
      showSelectControl <- NULL
      selectControl <- NULL
      selectInputId <- paste0("select_", param$name)
      
      ## Helper to materialize a "default/customize" control.
      makeSelectControl <- function(default_name, custom_name) {
        showSelectControl <<- function(current) {
          (is.null(current) || identical(current, "default"))
        }
        hasDefaultValue <<- function(value) { FALSE }
        choices <- list()
        choices[[default_name]] <- "default"
        choices[[custom_name]] <- "custom"
        selectControl <<- shiny::selectInput(inputId = selectInputId,
                                             label = label,
                                             choices = choices)
        
      }
      
      if (is.null(params[[param$name]])) { # prior value; implicit customization
        ## Dates and times with expressions that mean "now" or "today" are first
        ## materialized as selects. If the user chooses to customize the field,
        ## we then show the type-specific picker.
        if (identical("Sys.time()", param$expr)) {
          makeSelectControl(paste0("now (", param$value, ")"),
                            "Use a custom time")
        } else if (identical("Sys.Date()", param$expr)) {
          makeSelectControl(paste0("today (", param$value, ")"),
                            "Use a custom date")
        } else if (is.null(param$value)) {
          # fileInput defaults to null, but for other null values, ask the
          # user to explicitly choose to override (ie. we cannot use value
          # comparison).
          if (!identical(inputControlFn, shiny::fileInput)) {
            makeSelectControl("Unspecified (NULL)",
                              "Use a custom value")
          }
        }
      }
      
      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        # For most parameters, the selectInputId input will be NULL.
        if (!is.null(showSelectControl) && showSelectControl(input[[selectInputId]])) {
          selectControl
        } else {
          inputControl
        }
      })
      
      shiny::observe({
        # A little reactive magic to keep in mind. If you're in one of the
        # "default/custom" selector scenarios, this will never fire until the
        # user selects "custom" because the value-producing input control is
        # not rendered until that point.
        uivalue <- input[[param$name]]
        if (is.null(uivalue) || hasDefaultValue(uivalue)) {
          values[[param$name]] <<- NULL
        } else {
          values[[param$name]] <<- params_value_from_ui(inputControlFn, param$value, uivalue)
        }
      })
    }
    
    lapply(configurable, function(param) {
      param.ui(param)
    })
    
    output$ui_reportformat <-shiny::renderUI({reportformats.ui(outputformats,"ui_reporformat","Select output format")})

    shiny::observeEvent(input$run, {
      
      ##Modal for long running process
            showModal(modalDialog(shiny::tags$div(style="text-align:center;",tagList(shiny::tags$div(id="loading"),
                                                                   tags$h3("calculating...")
                                                                   )), footer=NULL, size="m"))
        outformat<-input$ui_reporformat
       # browser()
       htmlfile<-rmarkdown::render(file,output_format = outformat # "bbtemplates::bb_pdfreport"
                        ,params = values
                        ,envir = new.env(parent = globalenv())
                        ,output_dir = "www")
       finalreport<-display_Report(reportfile = htmlfile,defaultlogo=defaultlogo)
       
      removeModal()
      output$inc<-renderUI({finalreport})
      
      
      
    })
    
    #Cancel Button Event
    # shiny::observeEvent(input$cancel, {
    #   session$onFlushed(function() {
    #     session$close()
    #     shiny::stopApp(NULL)
    #   })
    # })
  }
  
  # paramInput <-shiny::fluidRow(lapply(configurable, function(param) {
  #     shiny::uiOutput(paste0("ui_", param$name))
  #   }))
  paramInput <-lapply(configurable, function(param) {
    shiny::uiOutput(paste0("ui_", param$name))
  })
  reportInput<-shiny::uiOutput("ui_reportformat")
  
  footerButtons <- shiny::fluidRow(
        shiny::actionButton("run", run_caption)
        #,shiny::actionButton("cancel","Cancel")
      )

  style <- tags$style(
   " #loading {
    display: inline-block;
    width: 50px;
    height: 50px;
    border: 3px solid rgba(239, 123, 5,.3);
    border-radius: 50%;
    border-top-color: rgba(239, 123, 5);
      animation: spin 1s ease-in-out infinite;
    -webkit-animation: spin 1s ease-in-out infinite;}

@keyframes spin {
  to { transform: rotate(360deg); }
}
@-webkit-keyframes spin {
  to { -webkit-transform: rotate(360deg); }
}
@-ms-keyframes spin {
  to { -ms-transform: rotate(360deg); }
}"
  )

  #########################################
  # ui <- shinymaterial::material_page(
  #########################################
  ui <- fluidPage(
    tags$head(style),
    
    titlePanel(textOutput("reportTitle")),
    fluidRow(
    
      mainPanel(width = 8,
            htmlOutput("inc")
            )
        ,sidebarPanel(width=4
                        ,paramInput
                        ,reportInput
                        ,footerButtons
                        
        )
    )
  )
  shiny::shinyApp(ui = ui, server = server)
  # shiny_app <- shiny::shinyApp(ui = ui, server = server)
  # shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  # do.call(shiny::runApp, shiny_args)
}