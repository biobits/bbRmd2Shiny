##Helper Functions
##has to be moved to package when Project is ready for deployment

read_utf8 <- function(file, encoding = 'UTF-8') {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = encoding); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

bbKnitrGetParams<-function(file,encoding = 'UTF-8'){
  params<-knitr::knit_params(read_utf8(file,encoding),evaluate = T)
  params
}

bbAddParamNames<-function(paramList){
  namelist<-names(paramList)
  for(i in seq_along(paramList)){
   
    paramList[[i]][["name"]]<-namelist[i]
  }
  paramList
  }


merge_lists <- function(base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- merge_lists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
  
}
  
  knit_params_get <- function(input_lines, params) {
    
    # read the default parameters and extract them into a named list
    knit_params <- knitr::knit_params(input_lines)
    default_params <- list()
    for (param in knit_params) {
      default_params[param$name] <- list(param$value)
    }
    
    # validate params passed to render
    if (!is.null(params)) {
      
      if (identical(params, "ask")) {
        params <- knit_params_ask(
          input_lines = input_lines, shiny_args = list(launch.browser = TRUE)
        )
        if (is.null(params)) {
          stop("render parameter configuration canceled")
        }
      }
      
      # verify they are a list
      if (!is.list(params) || (length(names(params)) != length(params))) {
        stop("render params argument must be a named list")
      }
      
      # verify that all parameters passed are also in the yaml
      invalid_params <- setdiff(names(params), names(default_params))
      if (length(invalid_params) > 0) {
        stop("render params not declared in YAML: ",
             paste(invalid_params, collapse = ", "))
      }
    }
    
    # merge explicitly provided params with defaults
    merge_lists(default_params, params, recursive = FALSE)
  }
  
  params_label <- function(inputControlFn, param) {
    label <- ifelse(is.null(param$label), param$name, param$label)
    if (identical(inputControlFn, shiny::fileInput)) {
      if (is.character(param$value)) {
        label <- paste0(label, " (default: ", param$value, ")")
      }
    }
    label
  }
  
  params_value_to_ui <- function(inputControlFn, value, showDefault) {
    if (is.null(showDefault)) {
      showDefault <- TRUE
    }
    
    isNumericInput <- identical(inputControlFn, shiny::numericInput) ||
      identical(inputControlFn, shiny::sliderInput)
    
    if (identical(inputControlFn, shiny::fileInput)) {
      NULL
    } else if (identical(inputControlFn, shiny::textInput)) {
      ## TODO: if long input, maybe truncate textInput values for display
      
      if (showDefault) {
        classes <- class(value)
        if ("POSIXct" %in% classes) {
          as.character(value)
        } else {
          value
        }
      } else {
        NULL
      }
    } else if (is.null(value)) {
      # The numerics can't deal with a NULL value, but everything else is fine.
      if (isNumericInput) {
        0
      } else {
        value
      }
    } else {
      if (showDefault) {
        ## A type/control that doesn't need special handling; just emit the value.
        value
      } else {
        if (isNumericInput) {
          0
        } else if (identical(inputControlFn, shiny::dateInput)) {
          # Use NA to clear date inputs:
          # https://github.com/rstudio/shiny/pull/1299
          NA
        } else if (identical(inputControlFn, shiny::radioButtons)) {
          # As suggested in ?radioButtons
          character(0)
        } else {
          NULL
        }
      }
    }
  }
  
  params_value_from_ui <- function(inputControlFn, value, uivalue) {
    if (identical(inputControlFn, shiny::fileInput)) {
      backup_file_input(uivalue$datapath)
    } else if (identical(inputControlFn, shiny::textInput)) {
      classes <- class(value)
      if ("POSIXct" %in% classes) {
        if (identical(uivalue, "")) {
          # show_default: false produces this situation
          # Empty POSIXct
          Sys.time()[-1]
        } else {
          tryCatch({
            as.POSIXct(uivalue)
          }, error = function(e) {
            # Unparseable time values produce NULL and float to the default.
            # This happens most frequently when actively editing a date/time.
            NULL
          })
        }
      } else {
        uivalue
      }
    } else {
      ## A type/control that doesn't need special handling; just emit the value.
      uivalue
    }
  }
  
  # Uploaded files will be deleted when the shiny UI is closed, so we need to back
  # them up to new temp files: https://github.com/rstudio/rmarkdown/issues/919
  backup_file_input <- function(files) {
    files2 <- files
    for (i in seq_along(files)) {
      dir.create(d <- tempfile())
      files2[i] <- file.path(d, basename(files[i]))
    }
    file.copy(files, files2)
    files2
  }
  
  params_get_input <- function(param) {
    # Maps between value types and input: XXX
    default_inputs <- list(
      logical = "checkbox",
      Date = "date",
      ## BUG: shiny does not support datetime selectors
      ##     https://github.com/rstudio/shiny/issues/897
      ##     we ask for string input for now.
      POSIXct = "datetime",
      character = "text"
    )
    default_inputs$integer <- default_inputs$numeric <-  {
      ## If min/max are specified, use a slider.
      if (is.null(param$min) || is.null(param$max)) {
        "numeric"
      } else {
        "slider"
      }
    }
    
    input <- param$input
    if (is.null(input)) {
      if (!is.null(param$choices)) {
        ## radio buttons for a small number of choices, select otherwise.
        if (length(param$choices) <= 4) {
          input <- "radio"
        } else {
          input <- "select"
        }
      } else {
        ## Not choices. Look at the value type to find what input control we
        ## should use.
        
        ## A value might have multiple classes. Try: class(Sys.time())
        ## Try to find first class listed with a named control.
        for (c in class(param$value)) {
          default_input <- default_inputs[[c]]
          if (!is.null(default_input)) {
            input <- default_input
            break
          }
        }
      }
    }
    input
  }
  
  params_get_control <- function(param) {
    input <- params_get_input(param)
    if (is.null(input)) {
      return(NULL)
    }
    
    # Maps between input: XXX and the various Shiny input controls
    input_controls <- list(
      checkbox = shiny::checkboxInput,
      numeric  = shiny::numericInput,
      slider   = shiny::sliderInput,
      date     = shiny::dateInput,
      datetime = shiny::textInput, # placeholder for future datetime picker
      text     = shiny::textInput,
      password = shiny::passwordInput,
      file     = shiny::fileInput,
      radio    = shiny::radioButtons,
      select   = shiny::selectInput
      
    )
    control <- input_controls[[input]]
    if (is.null(control)) {
      stop(paste("could not determine what control to use for parameter", param$name, "with input:", input))
    }
    control
  }
  
  # Returns true if the parameter can be configurable with Shiny UI elements.
  params_configurable <- function(param) {
    inputControlFn <- params_get_control(param)
    if (is.null(inputControlFn)) {
      return(FALSE)                       # no Shiny control
    }
    # Some inputs (like selectInput) support the selection of
    # multiple entries through a "multiple" argument.
    multiple_ok <- (!is.null(param$multiple) && param$multiple)
    if (multiple_ok) {
      return(TRUE)
    }
    # sliderInput supports either one or two-value inputs.
    if (identical(inputControlFn, shiny::sliderInput)) {
      return(length(param$value) <= 2)
    }
    # Other inputs only support singular values.
    return(length(param$value) <= 1)     # multiple values only when multi-input controls
  }
  
  # Returns a new empty named list.
  params_namedList <- function() {
    empty <- list()
    names(empty) <- character()
    empty
  }
  
  # Retuns the iFrame for displaying final reports
  display_Report<-function(reportfile,style="height:100vh;width:100%; scrolling=yes"
                           ,defaultlogo="UCCH_Front.png"){
    webpath<-paste0("www/",reportfile)
    if(file.exists(reportfile)|file.exists(webpath)){
      if(file.exists(webpath)){
        shiny::tags$iframe(style=style,src=basename(reportfile),frameborder=0,height="100%") # 
      }
      else if(file.exists(reportfile)){
        shiny::tags$iframe(style=style,src=basename(reportfile),frameborder=0,height="100%")
      }
    }else{shiny::tags$img(src=defaultlogo)}
    
  }
  
  get_outputformat <- function(name) {
    input <- name # In case we pass a listobject as input we have to do more here
    if (is.null(input)) {
      return(NULL)
    }
    
    # Maps between input: XXX and the various outputformats
    input_formats <- list(
      html_document = "html",
      `bbtemplates::bb_pdfreport`  = "pdf",
      pdf_document   = "pdf",
      word_document     = "Word",
      odt_document = "odt"
      
    )
    control <- input_formats[[input]]
    if (is.null(control)) {
      stop(paste("could not determine what format to map for outputformat ", input))
    }
    control
  }
  

  
  bbAddOutputShortNames<-function(formatlist){
    namelist<-names(formatlist)
    for(i in seq_along(formatlist)){
      shtnm<-get_outputformat(namelist[i])
      
      formatlist[[i]][["shortformat"]]<-shtnm
    }
    formatlist
  }
  
  reportformats.ui<-function(formatlist,selectInputId,label){
    # now we switch names an values in the list
    outlist<-purrr::map(formatlist,"shortformat")
    paramlist<-as.list(names(outlist))
    names(paramlist)<-as.vector(outlist)
    #choices<-unlist(map(formatlist,"shortformat"))
    selectControl <<- shiny::selectInput(inputId = selectInputId,
                                         label = label,
                                         choices = paramlist)
    
    selectControl
    
  }
  
  display_report_note<-function(report_note){
  
    if(is.null(report_note)){return}
    message<-NULL
    type<-"default"
    if(exists('message', where=report_note)){
      message<-report_note$message
    }
    if(length(report_note)==1 & !exists('message', where=report_note) & !is.null(report_note)){
      
      message<-report_note$reportnotification
      
    }
    if(exists('type', where=report_note)){
      type<-report_note$type
    }
    
    if(!is.null(message)){
      
      notification<-shiny::showNotification(paste(message), duration = 0,type=type)
      notification
    }else {return}
    
  }