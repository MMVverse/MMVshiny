#' Read input specification file and add columns with reactive expressions
#' for MIN, MAX, DEFAULT, MINNOTE, MAXNOTE and DEFNOTE.
#' 
#' The function looks for expressions of the form Get(something) and replaces 
#' them with state%>%Get('something'). 
#' The function also checks if all the IDs in the Get expressions are present in the 
#' ID column of the spec data table. If not, it stops execution and throws an error.
#'
#' Depending on the type of the ID, it then creates reactive expression columns in 
#' the spec data table. These reactive expressions are created by wrapping the expressions
#' in reactive({}) and assigning them to new columns in the spec data table. The names 
#' of these new columns are ExprVAL, ExprDEFNOTE, ExprMIN, ExprMINNOTE, ExprMAX, 
#' and ExprMAXNOTE.
#'
#' @param filename a character string indicating an xlsx file specifying input parameter
#' specification. It is expected that the file contains at least the following columns:
#'  
#'   - ID: This column is used to identify each input parameter in the spec. 
#'   - TYPE: a character string column defining the type of the input. 
#'   - VALEXPR: This column is used in the creation of the ExprVAL reactive 
#' expression column. 
#'   - DEFAULTVALUENOTE: This column is used in the creation of the ExprDEFNOTE 
#' reactive expression column. It is passed to the ParseValExpr function.
#'   - MIN: This column is used in the creation of the ExprMIN reactive expression 
#' column, but only for rows where the type is "numeric input". 
#'   - MINNOTE: This column is used in the creation of the ExprMINNOTE reactive 
#' expression column, but only for rows where the type is "numeric input". 
#'   - MAX: This column is used in the creation of the ExprMAX reactive expression 
#' column, but only for rows where the type is "numeric input". 
#'   - MAXNOTE: This column is used in the creation of the ExprMAXNOTE reactive 
#' expression column, but only for rows where the type is "numeric input". 
#'  
#' @return a data.table representing the input specification data with added columns
#' ExprVAL, ExprDEFNOTE, ExprMIN, ExprMINNOTE, ExprMAX, and ExprMAXNOTE. An
#' error is thrown if some of the IDs are duplicated. 
#' @importFrom xlsx read.xlsx
#' @importFrom data.table as.data.table 
#' @importFrom data.table setkey
#' @importFrom data.table is.data.table
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny reactiveValues
#' @export  
LoadStateSpecification <- function(filename) {
  spec <- as.data.table(read.xlsx(filename, sheetIndex = 1, stringsAsFactors = FALSE))
  
  # Replace 'Get(' with 'state%>%Get(' in R-code columns of spec
  # @param x is a character string representing the column text value to be parsed 
  # (if it is interpreted as a character string it must be valid R literal or NA_character_ or NA or empty string '').
  # @param idType is a character string representing the type of the ID column in the spec data table.
  # @param colType is a character string representing the type of the column in the spec data table - can be one of note, value, or action.
  # @return a character string representing a valid R expression that can be evaluated using \code{eval(parse(text = value))}.
  ParseValExpr <- function(x, idType, colType) {
    
    id <- gregexpr("Get\\(([^\\)]+)\\)", x)
    id <- unlist(regmatches(x, id))
    id <- gsub("Get\\(([^\\)]+)\\)", "\\1", id)
    id <- unique(id)
    if(!isTRUE(all(id %in% spec$ID))) {
      stop("ParseValExpr: Problem in state-specification - some of the id's in Get-expressions were not matched against ID column: ", 
           id[!id %in% spec$ID] )
    }
    x2 <- gsub("Get\\(([^\\)]+)\\)", "state%>%Get('\\1')", x)
    if(is.na(x2) || x2 == "") {
      if(colType == "value") {
        if(idType %in% c("numeric input", "numeric constant")) {
          "NA_real_"
        } else {
          "NA_character_"
        }
      } else if(colType == "action") {
        "NULL"
      } else if(colType == "note") {
        "NA_character_"
      } else {
        stop("ParseValExpr: colType should be one of 'value', 'note', or 'action'.")
      }
    } else {
      x2
    }
  }
  
  # Create reactive expression columns for R-code columns in spec
  for(id in spec$ID) {
    type <- GetType(id = id, spec = spec)
    
    spec[ID == id, ExprVAL     := ParseValExpr(VALEXPR, idType = type, colType = "value")]
    spec[ID == id, ExprDEFNOTE := ParseValExpr(DEFAULTVALUENOTE, idType = type, colType = "note")]
    spec[ID == id, ExprMIN     := ParseValExpr(MIN, idType = type, colType = "value")]
    spec[ID == id, ExprMINNOTE := ParseValExpr(MINNOTE, idType = type, colType = "note")]
    spec[ID == id, ExprMAX     := ParseValExpr(MAX, idType = type, colType = "value")]
    spec[ID == id, ExprMAXNOTE := ParseValExpr(MAXNOTE, idType = type, colType = "note")]
    
    if("ACTIONEXPR" %in% names(spec)) {
      spec[ID == id, ExprACTION := ParseValExpr(ACTIONEXPR, idType = type, colType = "action")]
    } else {
      spec[ID == id, ExprACTION := "NULL"]
    }
  }
  
  spec
}

# Create an environment to store the state id counter used to generate unique prefixGuiIds
prefixEnv <- new.env()
prefixEnv$counter <- 0

#' Generate next unique prefix for GUI parameter inputs
#' @return a character string representing the next unique prefix for GUI parameter inputs 
#' in the formate stateX where X is a number.
#' @export
NextStateId <- function() {
  prefixEnv$counter <- prefixEnv$counter + 1
  pref <- paste0("state", prefixEnv$counter)
  cat2("== NextStateId ==: ", pref, "\n")
  pref
}

#' Initialize state object based on a parameter specification
#' @param spec a data.table denoting the parameter specification.
#' @param stateId a character string to be used as an identifier of this state object in lists of states.
#' If this is set to "<auto>" (default), the id will be set to "stateX" where X is a unique number 
#' for the duration of the session. Unless listObjects defines a prefixGuiId element, the stateId will
#' be used as a prefix of GUI inputs for all parameters in the state (see also \code{\link{GetGuiId}}).
#' @param listObjects a named list of objects to be added to the state environment. No checks are done for conflicts
#' with the names created in the state during initialization (see the full list in the Value section).
#' @param scriptsToSource a character vector of script file paths to source in the state environment.
#' 
#' @return an initialized state environment containing the following objects (created in this order):
#' * MYSTATE: A reference to the state object itself
#' * stateId: The stateId argument or automatically generated unique identifier if "<auto>" was passed as the stateId.
#' * spec: The spec argument
#' * status: A reactiveValues object containing the status of each input parameter
#' * statusIconClass: A reactiveValues object containing the icon class for the status of each input parameter
#' * statusIconColor: A reactiveValues object containing the icon color for the status of each input parameter
#' * statusTitle: A reactiveValues object containing the title for the status of each input parameter
#' * statusText: A reactiveValues object containing the text for the status of each input parameter
#' * event: A reactiveValues object containing the latest event for each input parameter
#' * validated: A reactiveValues object containing the validated value for each input parameter
#' * displayed: A reactiveValues object containing the displayed value for each input parameter
#' * resetCount: A reactiveValues object containing the reset count for each input parameter - this 
#' is incremented when the Reset button is pressed to trigger observers.
#' * guiInput: A reactiveValues object containing the value inputted by the user in the gui input field 
#' for each input parameter
#' * reportInput: A reactiveValues object containing the value read from the Basic Inputs or Advanced Inputs 
#' tab of a MMVSola report for each input parameter
#' * scInput: A reactiveValues object containing the average value from the included records matching the SCFILTER column
#' of each input parameter
#' * scRawData: A reactiveVal object containing an inputted Science Cloud extract set by SetSCRawData
#' * scRawDataChanged: A reactiveVal object containing a flag to indicate that the SC data has changed
#' * default: A list object containing the default value or parsed expression for each input parameter read from the VALEXPR column
#' of spec
#' * defaultNote: A list object containing the default value note or parsed expression for each input parameter read from the 
#' DEFAULTVALUENOTE column of spec
#' * min: A list object containing the min value or parsed expression for each numeric input parameter read from the MIN column of spec
#' * minNote: A list object containing the min value note or parsed expression for each numeric input parameter read from the MINNOTE 
#' column of spec
#' * max: A list object containing the max value or parsed expression for each numeric input parameter read from the MAX column of spec
#' * maxNote: A list object containing the max value note or parsed expression for each numeric input parameter read from the MAXNOTE 
#' column of spec
#' * objects created from the listObjects argument
#' * objects created from sourcing the R scripts in the scriptsToSource argument
#' 
#' @export
#' @md
InitState <- function(spec, stateId = "<auto>", listObjects = NULL, FLAG_ignoreNameConflicts = FALSE, scriptsToSource = NULL) {
  spec <- as.data.table(spec) 
  setkey(spec, ID)
  
  tStart1 <- R.utils::System$currentTimeMillis()
  state <- new.env()
  
  # Like 'this' in java and C++
  state$MYSTATE <- state
  
  state$stateId <- as.character(stateId)
  if(state$stateId == "<auto>") {
    state$stateId <- NextStateId()
  }
  
  with(state, {
    # Input specification
    spec = spec 
    
    # status object for each input, containing information about source, value within limits, value replaced during validation, etc.
    status = reactiveValues() 
    
    statusIconClass = reactiveValues()
    statusIconColor = reactiveValues()
    statusTitle = reactiveValues()
    statusText = reactiveValues()
    
    # latest input event triggering input processing: one of "INIT","USER","REPORT"
    event = reactiveValues()
    
    # value used for all calculations
    validated = reactiveValues()
    
    # value displayed in the gui input field - can be a rounded version of validated
    displayed = reactiveValues()
    
    # integer incremented for each input field, when the Reset button is pressed
    resetCount = reactiveValues()
    
    # value inputted by the user in the gui input field
    guiInput = reactiveValues()
    
    # value read from the Basic Inputs or Advanced Inputs tab of a MMVSola report
    reportInput = reactiveValues()
    
    # average value from the included records corresponding to a parameter in a Science Cloud input file
    scInput = reactiveValues()
    
    # action handlers for action button inputs (plain list of NULLs or functions, not reactive)
    actionHandler = list()
    
    # Science Cloud input ile
    # - NULL: no Science Cloud file uploaded
    # - data.table: Uploaded Science Cloud file limited to the data for the first compound in it and 
    # augmented with columns INCLUDE (TRUE/FALSE), `REASON TO EXCLUDE` (text), ID (ID of affected parameters), 
    # and `PARAMETERS AFFECTED`
    scRawData = reactiveVal()
    scRawDataChanged = reactiveVal(1)
    
    # lists of numeric or character values or reactive expressions as per specification
    default = list()
    defaultNote = list()
    min = list()
    minNote = list()
    max = list()
    maxNote = list()
  })
  
  # Add objects from the listObjects argument
  if(!is.null(listObjects)) {
    for(name in names(listObjects)) {
      state[[name]] <- listObjects[[name]]
    }
  }
  
  
  tEnd1 <- R.utils::System$currentTimeMillis()
  
  cat("Creating state list object: ", tEnd1-tStart1, " ms\n")
  
  # Create state objects
  tStart3 <- R.utils::System$currentTimeMillis()
  for(id in spec$ID) {
    if(GetType(id = id, spec = spec) %in% c("numeric input", "text input", "radio input", "select input")) {
      state$event[[id]] <- "INIT"
      
      state$default[[id]] <- eval(parse(text = spec[ID == id, ExprVAL]))
      state$defaultNote[[id]] <- eval(parse(text = spec[ID == id, ExprDEFNOTE]))
      state$min[[id]] <- eval(parse(text = spec[ID == id, ExprMIN]))
      state$minNote[[id]] <- eval(parse(text = spec[ID == id, ExprMINNOTE]))
      state$max[[id]] <- eval(parse(text = spec[ID == id, ExprMAX]))
      state$maxNote[[id]] <- eval(parse(text = spec[ID == id, ExprMAXNOTE]))
      
      state$validated[[id]] <- NAVal(id = id, spec = spec)
      state$displayed[[id]] <- NAVal(id = id, spec = spec)
      
      state$resetCount[[id]] <- 0
      
      state$guiInput[[id]] <- NAVal(id = id, spec = spec)
      state$reportInput[[id]] <- NAVal(id = id, spec = spec)
      state$scInput[[id]] <- NAVal(id = id, spec = spec)
      
      
      state$status[[id]] <- list(
        source = "Default value",
        valid = "OK", 
        note = GetValidationNote(id = id, spec = spec)
      )
      
      state$statusIconClass[[id]] <- "glyphicon glyphicon-info-sign"
      state$statusIconColor[[id]] <- "#337ab7"
      state$statusTitle[[id]] <- "Info"
      state$statusText[[id]] <- GetValidationNote(id = id, spec = spec)
    } else if(GetType(id = id, spec = spec) %in% c("numeric constant")) {
      defVal <- try(as.numeric(eval(parse(text=as.character(spec[ID == id, VALEXPR])))), silent = TRUE)
      if(is.null(defVal) || !is.numeric(defVal) || is.na(defVal)) {
        stop("InitState: VALEXPR could not be evaluated as a numeric for numeric constant ",id)
      } else {
        state$validated[[id]] <- defVal
      }
    } else if(GetType(id = id, spec = spec) == "action button input") {
      state$actionHandler[[id]] <- eval(parse(text = spec[ID == id, ExprACTION]))
    } else if(GetType(id = id, spec = spec) == "reactive") {
      state$default[[id]] <- eval(parse(text = spec[ID == id, ExprVAL]))
    }
  }
  tEnd3 <- R.utils::System$currentTimeMillis()
  cat("Create state objects: ", tEnd3-tStart3, " ms\n")
  
  # Source the scripts in the state environment
  if(is.character(scriptsToSource) && length(scriptsToSource) > 0) {
    for(script in scriptsToSource) {
      if(file.exists(script)) {
        source(script, local = state)
      } else {
        stop("InitState: Script file ", script, " does not exist.")
      }
    }
  }
  
  state
}

#' Get the state identifier for a state object
#' @param state a state object.
#' @return a character string representing the state identifier - this is the stateId argument passed to InitState, 
#' or a unique identifier generated by NextStateId if "<auto>" was passed as the stateId argument.
#' @export
GetStateId <- function(state) {
  state$stateId
}

#' Update the state when the user changes the value in a GUI input field
#' @param s the state object
#' @param input the shiny input object.
#' @param id the ID of the input field
#' @param inputValue the value of the input field. If this parameter is left missing, 
#' the value of the input field is read from the shiny input object. Otherwise, the
#' value of the input field from the shiny input object is ignored and this value is
#' used instead.
#' @param output the shiny output object - this is passed to the handler of action button 
#' inputs, but is not used for other input types.
#' @param session the shiny session object - this is passed to the handler of action button 
#' inputs, but is not used for other input types.
#' @return the validated input value based on the user input
#' @export
ProcessGuiInputEvent <- function(s, input, id, inputValue, output = NULL, session = getDefaultReactiveDomain()) {
  if(missing(inputValue)) {
    inputValue <- input[[GetGuiId(s,id)]]
  }
  
  # Action buttons are handled via a countID trigger; they do not participate in validation/displayed.
  if(s %>% GetType(id) == "action button input") {
    handler <- s$actionHandler[[id]]
    if(is.null(handler)) {
      return(s)
    }
    if(!is.function(handler)) {
      stop("ACTIONEXPR for id=", id, " must evaluate to a function(state, input, output, session).")
    }
    handler(state = s, input = input, output = output, session = session)
    return(s)
  }

  if( (s%>%GetType(id) == "numeric input" && length(inputValue) > 0 && !identical(as.numeric(s%>%GetDisplayed(id)), as.numeric(inputValue)) ) ||
      (s%>%GetType(id) == "text input" && length(inputValue) > 0 && !identical(as.character(s%>%GetDisplayed(id)), as.character(inputValue)) ) ||
      (s%>%GetType(id) %in% c("radio input", "select input") && length(inputValue) > 0 && !identical(as.character(s%>%GetDisplayed(id)), as.character(inputValue)) ) ) {
    
    # The user has changed the value in the GUI input field
    cat2('3.')
    
    s%>%SetEvent(id, "USER")
    
    s%>%SetGuiInput(id, inputValue)
    validated <- s%>%Validate(id, "USER")
    s%>%SetDisplayed(id, validated)
    s%>%SetInfoIcon(id)
    s%>%SetEvent(id, NA_character_)
    
    
    cat2("_",id, sep = "")
    
    validated
  } 
}

#' Re-initialize an input after the user has invoked reset for the state object to which it belongs
#' @param s the state object
#' @param id the ID of the input field
#' @return the validated re-initialized value for the input
#' 
#' @details
#' This function is supposed to be called from an observer of GetResetCount(s, id). 
#' Calling this function will re-initialize the input field to its default value and update its 
#' displayed value, color and info icon.
#' 
#' @export
ProcessResetEvent <- function(s, id) {
  if( isTRUE(s%>%GetEvent(id) == "INIT") ) {
    # Initialization or the user has pressed the Reset button and SetGuiInput and SetReportInput have been called with NA vales
    cat2('5.')
    # Re-initialize everything
    validated <- s%>%Validate(id, "INIT")
    s%>%SetDisplayed(id, validated)
    s%>%SetInfoIcon(id)
    s%>%SetEvent(id, NA_character_)
    
    cat2("_",id, sep = "")
    
    validated
  }
}

#' Validate a new SC input value, update the displayed value and the info icon
#' 
#' @param s the state object.
#' @param id the ID of the input parameter.
#' 
#' @details
#' This function is supposed to be called from an observer of GetSCInput(s, id). 
#' The call to this function will not do anything if not \code{isTRUE(GetEvent(s, id) == "SCDATA")}.
#' This function is not supposed to be called by users, but by automatically generated reactive observers.
#' 
#' @seealso \code{\link{SetSCInput}}, \code{\link{GenerateScriptCreatingObservers}}
#' 
#' @return the validated value for the input. 
#' @export
ProcessSCInputEvent <- function(s, id) {
  if( isTRUE(s%>%GetEvent(id) == "SCDATA") ) {
    # The user has uploaded a Science Cloud file and SetSCInput(id, ...) has been called
    cat2('6.', id, sep = "")
    
    validated <- s%>%Validate(id, "SCDATA")
    s%>%SetDisplayed(id, validated)
    s%>%SetInfoIcon(id)
    s%>%SetEvent(id, NA_character_)
    
    cat2("_",validated, sep = "")
    
    validated
  } 
}

#' Validate an input value read from a report, update the displayed value and the 
#' info icon
#' 
#' @param s the state object.
#' @param id the ID of the input parameter.
#' 
#' @details
#' Calling this function will not do anything if not \code{isTRUE(GetEvent(s, id) == "REPORT")}.
#' This function is not supposed to be called by users, but by automatically generated reactive observers.
#' 
#' @seealso \code{\link{GenerateScriptCreatingObservers}}
#' @return the validated input value or nothing if not 
#' \code{isTRUE(GetEvent(s, id) == "REPORT")}.
#' @export
ProcessReportInputEvent <- function(s, id) {
  
  if( isTRUE(s%>%GetEvent(id) == "REPORT") ) {
    
    # The user has uploaded a report and SetReportInput(id, ...) has been called
    cat2('7.')
    validated <- s%>%Validate(id, "REPORT")
    s%>%SetDisplayed(id, validated)
    s%>%SetInfoIcon(id)
    s%>%SetEvent(id, NA_character_)
    
    cat2("_",id, sep = "")
    
    validated
  } 
}

#' Validate default input value after it has changed
#' 
#' @param s the state object.
#' @param id the ID of the input parameter.
#' 
#' @details
#' This function is supposed to be called from an observer of GetDefault(s, id).
#' The call to this function will not do anything if not 
#' \code{is.na(s%>%GetEvent(id)) && s%>%GetSource(id) == "Default value"}.
#' 
#' @return the validated value for the input or nothing if not
#'  \code{is.na(s%>%GetEvent(id)) && s%>%GetSource(id) == "Default value"}.
#' @export
ProcessDefaultChangedEvent <- function(s, id) {
  if( is.na(s%>%GetEvent(id)) ) {
    
    # Reactive expression probably triggerred by gui or report input to another input field, on which this one's 
    # default value depends.
    cat2('4.',id, sep = "")
    
    source <- s%>%GetSource(id)
    cat2('source:', source, '; ')
    
    if(source == "Default value") {
      
      cat2('1.')
      
      s%>%SetEvent(id, "DEFAULT")
      validated <- s%>%Validate(id, "DEFAULT")
      s%>%SetDisplayed(id, validated)
      s%>%SetInfoIcon(id)
      s%>%SetEvent(id, NA_character_)
      
      cat2(', validated:', validated, sep = "")
      
      validated
    }
  } 
}

#' Validate the current value of an input parameter after its MIN has changed and
#' update the info icon
#' 
#' @param s the state object.
#' @param id the ID of the input parameter.
#' 
#' @details
#' This function is supposed to be called from an observer of GetMin(s, id). The 
#' MIN value for an input parameter can changed if it is dependent on another input.
#' 
#' @return the validated value for the input.
#' @export
ProcessMinChangedEvent <- function(s, id) {
  # Reactive expression probably triggerred by gui or report input to another input field, on which this one's 
  # default value depends.
  cat2('8.')
  
  currentEvent <- s%>%GetEvent(id) 
  
  s%>%SetEvent(id, "MIN")
  validated <- s%>%Validate(id, "MIN")
  s%>%SetInfoIcon(id)
  s%>%SetEvent(id, currentEvent)
  
  cat2("_",id, sep = "")
  
  validated
}
#' Validate the current value of an input parameter after its MAX has changed and
#' update the info icon
#' 
#' @param s the state object.
#' @param id the ID of the input parameter.
#' 
#' @details
#' This function is supposed to be called from an observer of GetMax(s, id). The 
#' MAX value for an input parameter can changed if it is dependent on another input.
#' 
#' @return the validated value for the input.
#' @export
ProcessMaxChangedEvent <- function(s, id) {
  # Reactive expression probably triggerred by gui or report input to another input field, on which this one's 
  # default value depends.
  cat2('9.')
  
  # Keep memo of the current event (it has happened to be REPORT and we don't want to lose it).
  currentEvent <- s%>%GetEvent(id)
  s%>%SetEvent(id, "MAX")
  validated <- s%>%Validate(id, "MAX")
  s%>%SetInfoIcon(id)
  # Restore the current event
  s%>%SetEvent(id, currentEvent)
  
  cat2("_",id, sep = "")
  
  validated
}

#' Update the value of a GUI input field following a change in the corresponding displayed reactive value in a state object
#' @param s the state object.
#' @param id the ID of the input field and input parameter.
#' @return nothing - this function only has side effects.
#' @export
ProcessDisplayedChangedEvent <- function(s, id) {
  cat2('10.')
  
  cat2("_",id, sep = "")
  
  # Keep memo of the current event.
  currentEvent <- s%>%GetEvent(id)
  s%>%SetEvent(id, "DISPLAYED")
  
  source2FontColour <- getOption("MMVshiny.source2FontColour", 
                                 default = c(`Default value` = "black",
                                             `User input` = "black",
                                             `Science Cloud` = "black"))
  
  source2BackgroundColour <- getOption("MMVshiny.source2BackgroundColour", 
                                       default = c(`Default value` = "azure",
                                                   `User input` = "white",
                                                   `Science Cloud` = "ivory"))
                                       
  source <- GetSource(s, id)
  
  js$backgroundCol(GetGuiId(s, id), unname(source2BackgroundColour[source]))
  js$fontCol(GetGuiId(s, id), unname(source2FontColour[source]))
  
  if(s%>%GetType(id) == "numeric input") {
    UpdateNumericInput(inputId = GetGuiId(s, id), value = GetDisplayed(s, id))
  } else if(s%>%GetType(id) == "text input") {
    UpdateTextInput(inputId = GetGuiId(s, id), value = GetDisplayed(s, id))
  } else if(s%>%GetType(id) == "radio input") {
    UpdateRadioButtons(inputId = GetGuiId(s, id), selected = GetDisplayed(s, id))
  } else if(s%>%GetType(id) == "select input") {
    UpdateSelectInput(inputId = GetGuiId(s, id), selected = GetDisplayed(s, id))
  }
  # Restore the current event
  s%>%SetEvent(id, currentEvent)
}



#' Reinitialize a state object
#' @param state the state object.
#' @return nothing - calling this function only has side effects.
#' @export
Reset <- function(state) {
  cat("\n\n==================================\n==================RESET=================\n\n")
  
  shinyjs::enable("file1")
  
  for(id in state$spec$ID) {
    
    if(GetType(state, id) %in% c("numeric input", "text input", "radio input", "select input")) {
      SetEvent(state, id, "INIT")
      SetSCInput(state, id, NAVal(state, id = id))
      SetReportInput(state, id, NAVal(state, id = id))
      SetGuiInput(state, id, NAVal(state, id = id))
    } 
    
  }
  
  for(id in state$spec$ID) {
    
    if(GetType(state, id) %in% c("numeric input", "text input", "radio input", "select input")) {
      IncrementResetCount(state, id)
    }
  }
  
  SetSCRawData(state, NULL)
}

#' Get validated value stripped of attributes for id in a state object
#' @param state a state object.
#' @param id a character string id.
#' @return the currently validated value for id in state.
#' @export
Get <- function(state, id) {
  #cat("Get(",id,")")
  type <- GetType(state, id)
  if(type %in% c("numeric input", "numeric constant", "text input", "radio input", "select input")) {
    validated <- state%>%GetValidated(id)
    attributes(validated) <- NULL
    validated
  } else if(type == "reactive") {
    state%>%GetDefault(id)
  }
}

#' Get validation note for validated value for id in a state object
#' @param state a state object.
#' @param id a character string id.
#' @return a character vector.
#' @export
GetValidationNote <- function(state, id, spec = state$spec) {
  if(!missing(state)) {
    status <- state%>%GetStatus(id)
    note <- status$validationNote
  } else {
    note <- NULL
  }
  
  note
}

#' Get source for validated value for id in a state object
#' @param state a state object.
#' @param id a character string id.
#' @return a character string: For inputs, one of "User input", "Report input", "Default value"; 
#' For numeric constants the value for id in the SOURCE column of state$spec.
#' @export
GetSource <- function(state, id) {
  if(GetType(state, id) %in% c("numeric input", "text input", "radio input", "select input")) {
    status <- GetStatus(state, id)
    status$source
  } else {
    state$spec[ID == id, SOURCE]
  }
}

#' Get the type of a parameter in a state object
#' @param state the state object
#' @param id the id of the parameter
#' @param spec the parameter specification table. Default: `state$spec`. This argument
#' only needs to be specified if calling outside of a reactive context without 
#' initialized state.
#'
#' Supported parameter types are "text input", "numeric input", "radio input", "numeric constant"
#'
#' @return the TYPE column value for id in the parameter spec
#' @export
GetType <- function(state, id, spec=state$spec) {
  spec[ID == id, TYPE]
}

#' Get the unit of a parameter in a state object
#' @param state the state object
#' @param id the id of the parameter
#' @param spec the parameter specification table. Default: `state$spec`. This argument
#' only needs to be specified if calling outside of a reactive context without 
#' initialized state.
#'
#' @return the UNIT column value for id in the parameter spec
#' @export
GetUnit <- function(state, id, spec = state$spec) {
  spec[ID == id, UNIT]
}

#' Get the out-of-bound policy for a numeric input id in a state object
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @return a character string, one of "nearest", or "na", or NA_character_ if id is not a 
#' numeric input.
#' @export
GetOutOfBounds <- function(state, id) {
  if(GetType(state, id) %in% c("numeric input")) {
    res <- tolower(state$spec[ID == id, OUTOFBOUNDS])
    if(!res %in% c("na", "nearest", "default or nearest", "keep with error")) {
      stop("In parameter spec OUTOFBOUNDS for numeric input ",id," should be either 'na', 'nearest', 'default or nearest', or 'keep with error'.")
    } 
    res
  } else {
    NA_character_
  } 
}

#' Get the min value for a numeric input id in a state
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @param spec the parameter specification table. Default: `state$spec`. 
#' @return if id is a numeric input, the value of the MIN expression for it, otherwise an error is thrown.
#' @export
GetMin <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) %in% c("numeric input")) {
    if(!missing(state)) {
      if(is.function(state$min[[id]])) {
        as.numeric(state$min[[id]]())
      } else {
        as.numeric(state$min[[id]])
      }
    } else {
      res <- try(as.numeric(spec[ID == id, MIN]), silent = TRUE)
      if(is.numeric(res)) {
        res
      } else {
        NA_real_
      }
    }
  } else {
    stop("GetMin called on non-numeric input ",id)
  }
}

#' Get the reactive note associated with the min value of a numeric input in a state
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @param spec the parameter specification table. Default: `state$spec`.
#' @return if id is a numeric input, the value of the MINNOTE expression for it, otherwise NA.
#' @export
GetMinNote <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) == "numeric input") {
    if(!missing(state)) {
      if(is.function(state$minNote[[id]])) {
        as.character(state$minNote[[id]]())
      } else {
        as.character(state$minNote[[id]])
      }
    } else {
      res <- try(as.character(spec[ID == id, MIN]), silent = TRUE)
      if(is.character(res)) {
        res
      } else {
        NA_real_
      }
    }
  } else {
    NA_character_
  }
}


#' Get the max value for a numeric input id in a state
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @param spec the parameter specification table. Default: `state$spec`.
#' @return if id is a numeric input, the value of the MAX expression for it, otherwise an error is thrown.
#' @export
GetMax <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) %in% c("numeric input")) {
    if(!missing(state)) {
      if(is.function(state$max[[id]])) {
        as.numeric(state$max[[id]]())
      } else {
        as.numeric(state$max[[id]])
      }
    } else {
      res <- try(as.numeric(spec[ID == id, MAX]), silent = TRUE)
      if(is.numeric(res)) {
        res
      } else {
        NA_real_
      }
    }
  } else {
    stop("GetMax called on non-numeric input ",id)
  }
}

#' Get the reactive note associated with the max value of a numeric input in a state
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @param spec the parameter specification table. Default: `state$spec`.
#' @return if id is a numeric input, the value of the MAXNOTE expression for it, otherwise NA.
#' @export
GetMaxNote <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) == "numeric input") {
    if(!missing(state)) {
      if(is.function(state$maxNote[[id]])) {
        as.character(state$maxNote[[id]]())
      } else {
        as.character(state$maxNote[[id]])
      }
    } else {
      res <- try(as.character(spec[ID == id, MAXNOTE]), silent = TRUE)
      if(is.character(res)) {
        res
      } else {
        NA_character_
      }
    }
  } else {
    NA_character_
  }
}

#' For a numeric input, get the DECDIGITS value to use for rounding
#' @param state a state object
#' @param id the id of a numeric input parameter.
#' @param spec the parameter specification table. Default: `state$spec`.
#' @return if id is a numeric input, the value of the DECDIGITS column for it, otherwise NA.
#' @export
GetDecDigits <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) == "numeric input") {
    as.integer(spec[ID == id, DECDIGITS])
  } else {
    NA_integer_
  }
}

#' Set the validated state for input parameter id, based on the available report input, gui input, 
#' dependancy input values and default value
#' 
#' @param state the state object.
#' @param id id of the input parameter.
#' @param logMessage a character string used for debugging purpose only.
#' 
#' @return the validated value. This function has side effects on the state object - it updates
#' the validated value and the status for id.
#' @export
Validate <- function(state, id, logMessage) {
  cat2(" validate ")
  
  event <- state%>%GetEvent(id)
  status <- state%>%GetStatus(id)
  
  if(event == "USER") {
    if( is.na(state%>%GetGuiInput(id)) ) {
      cat2('1.')
      # The user has deleted the value in the gui input field => use default value
      validated <- ValidateValue(state, id, state%>%GetDefault(id))
      state%>%SetValidated(id, validated)
      state%>%SetStatus(id, source="Default value")
      validated
    } else {
      cat2('2.')
      # The user has changed the input value to a non-NA value
      validated <- ValidateValue(state, id, state%>%GetGuiInput(id))
      state%>%SetValidated(id, validated)
      state%>%SetStatus(id, source="User input")
      validated
    }
  } else if(event == "SCDATA") {
    cat2('3.')
    if(is.na(state%>%GetSCInput(id))) {
      cat2('1.')
      # The user has deleted unchecked all records in the Science Cloud data affecting this parameter => use default value
      validated <- ValidateValue(state, id, state%>%GetDefault(id))
      state%>%SetValidated(id, validated)
      state%>%SetStatus(id, source="Default value")
      validated
    } else {
      cat2('2.')
      # The user has uploaded a report in which the value for id is not NA
      validated <- ValidateValue(state, id, state%>%GetSCInput(id))
      state%>%SetValidated(id, validated)
      state%>%SetStatus(id, source="Science Cloud")
      validated
    }
  } else if(event == "REPORT") {
    cat2('4.')
    # The user has uploaded a report in which the value for id is not NA
    validated <- ValidateValue(state, id, state%>%GetReportInput(id))
    state%>%SetValidated(id, validated)
    state%>%SetStatus(id, source=status[["reportSource"]])
    validated
  } else if(event == "INIT") {
    cat2('5.')
    # Initialization with default value
    validated <- ValidateValue(state, id, state%>%GetDefault(id))
    state%>%SetValidated(id, validated)
    state%>%SetStatus(id, source="Default value")
    validated
  } else if(event == "DEFAULT") {
    cat2('6.')
    # Re-initialization after change in a dependancy
    validated <- ValidateValue(state, id, state%>%GetDefault(id))
    state%>%SetValidated(id, validated)
    state%>%SetStatus(id, source="Default value")
    validated
  } else if(event == "MIN") {
    cat2('7.')
    # Re-evaluate value within boundaries after MIN has changed as a result of a change to another parameter.
    validated <- ValidateValue(state, id, state%>%Get(id))
    state%>%SetValidated(id, validated)
    validated
  } else if(event == "MAX") {
    cat2('8.')
    # Re-evaluate value within boundaries after MAX has changed as a result of a change to another parameter.
    validated <- ValidateValue(state, id, state%>%Get(id))
    state%>%SetValidated(id, validated)
    validated
  } else {
    stop("Validate: unknown event", event, " for id=", id)
  }
  
}

#' Get the validated value for id given a suggested value, without updating 
#' the validated state for id
#' @return the validated value corresponding to value
ValidateValue <- function(state, id, value) {
  if(GetType(state,id) %in% c("numeric input")) {
    validated <- state%>%ValidateBoundaries(id, value)
  } else if(GetType(state,id) == "text input") {
    validated <- state%>%ValidateText(id, value)
  } else if(GetType(state,id) %in% c("radio input", "select input")) {
    validated <- state%>%ValidateRadio(id, value)
  }
  
  validated
}

#' Validate that a numeric input value is within `[min,max]` interval
#' @param state a state object
#' @param id the id of a numeric input parameter.  
#' @param value a numeric vector containing the input values to 
#' validate. Each value of this vector is checked to be within the interval 
#' `[GetMin(state, id),GetMax(state,id)]` and if it is outside the interval, 
#' this value is replaced depending on `GetOutOfBounds(state,id)`: if 'nearest', 
#' the nearest boudnary is used; if 'na', NA_real_ is used. 
#' The vector can be of length bigger than one, in particular, 
#' if the same numeric input is informed from a Science Cloud file. 
#' 
#' @return a numeric vector of the same length as `value`. As a side effect, SetStatus is called on id
#' with setting validationNote to a text string.
ValidateBoundaries <- function(state, id, value) {
  if(GetType(state,id) %in% c("numeric input")) {
    
    # a character vector of notes to be added to the status of id as validationNote
    note <- character(0)
    
    min <- as.numeric(GetMin(state, id))
    max <- as.numeric(GetMax(state, id))
    
    # function for rounding
    digs <- GetDecDigits(state, id)
    r <- function(x) {
      round(x, digs)
    }
    
    if(is.na(min)) {
      # MIN not available, so assume -Inf
      min <- -Inf
    }
    if(is.na(max)) {
      # MAX not available, so assume Inf
      max <- Inf
    }
    
    # Check out-of-bounds policy
    outofbounds <- GetOutOfBounds(state, id)
    outofb <- tolower(outofbounds)
    if(! outofb %in% c("nearest", "default or nearest", "na", "keep with error")) {
      stop("ValidateBoundaries: id=", id,
           " outofbounds should be one of 'nearest', 'default or nearest', 'na', or 'keep with error' but is ", 
           outofbounds)
    }
    
    # validated value to be updated
    validated <- NA_real_
    attributes(validated) <- attributes(value)
    
    if(outofb == "default or nearest") {
      defaultValue <- state%>%GetDefault(id)
    }
    
    if( !is.na(value) && value < (min-1e-12) ) {
      # use isTRUE below to cover the case of NA defaultValue
      if(outofb == "default or nearest" && isTRUE(defaultValue >= min && defaultValue <= max) ) {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(defaultValue), ": replaced by default value, because too small."))
        validated <- defaultValue
      } else if(outofb == "default or nearest") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(min), ": replaced by the min limit (", r(min), 
                         "), because too small and the default (", r(defaultValue), ") is not in the valid range."))
        validated <- min
      } else if(outofb == "nearest") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(min), ": replaced by the min limit (", r(min), "), because too small."))
        validated <- min
      } else if(outofb == "na") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", NA_real_, ": replaced by NA, because too small."))
        validated <- NA_real_
      } else if(outofb == "keep with error") {
        note <- c(note, 
                  paste0("ERROR: ", r(value), " is below the lower limit for this parameter."))
        validated <- value
      }
    } else if(!is.na(value) && value > (max + 1e-12)) {
      if(outofb == "default or nearest" && isTRUE(defaultValue >= min && defaultValue <= max) ) {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(defaultValue), ": replaced by default value, because too big."))
        validated <- defaultValue
      } else if(outofb == "default or nearest") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(max), ": replaced by the max limit (", r(max), 
                         "), because too big and the default (", defaultValue, ") is not in the valid range."))
        validated <- max
      } else if(outofb == "nearest") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", r(max), ": replaced by the max limit (", r(max), "), because too big."))
        validated <- max
      } else if(outofb == "na") {
        note <- c(note, 
                  paste0("WARN: ", r(value), " => ", NA_real_, ": replaced by NA, because too big."))
        validated <- NA_real_
      } else if(outofb == "keep with error") {
        note <- c(note, 
                  paste0("ERROR: ", r(value), " is above the upper limit for this parameter."))
        validated <- value
      }
    } else if(!is.na(value)) {
      note <- c(note, 
                paste0("OK: ", r(value), ": in valid range."))
      validated <- value
    }
    
    
    state%>%SetStatus(id, validationNote = note)
    
    validated
  } else {
    stop("ValidateBoundaries: id=",id," is not a numeric input.")
  }
  
}

#' Get the valid range note for a numeric input
#' @param state the state
#' @param id the id of the numeric input
#' @param spec the spec. Default is state$spec.
#' @return a character string with the valid range note.
#' @export
GetValidRangeNote <- function(state, id, spec = state$spec) {
  if(GetType(state,id) == "numeric input") {
    min <- as.numeric(GetMin(state, id))
    max <- as.numeric(GetMax(state, id))
    
    # function for rounding
    digs <- GetDecDigits(state, id)
    r <- function(x) {
      round(x, digs)
    }
    
    if(is.na(min)) {
      # MIN not available, so assume -Inf
      min <- -Inf
    }
    if(is.na(max)) {
      # MAX not available, so assume Inf
      max <- Inf
    }
    
    if( min <= max ) {
      note <- paste0("OK: ", "The valid range is [", r(min), ", ", r(max), "].") 
    } else {
      note <- paste0("ERROR: ", "An error occured in the calculation of the valid range [", r(min), ", ", r(max), "]. ", 
                     "Please, check the parameters used to define the upper and lower limits.")
    }
    note
  } else {
    NA_character_
  }
}
# #' Get the out-of-bounds note for a numeric input
# #' 
# #' @param state the state object.
# #' @param id the id of the numeric input.
# #' @param spec the spec. Default is state$spec.
# #' @return a character string with the out-of-bounds note.
# GetOutOfBoundsNote <- function(state, id, spec = state$spec) {
#   # Check out-of-bounds strategy
#   outofbounds <- GetOutOfBounds(state, id)
#   outofb <- tolower(outofbounds)
#   if(! outofb %in% c("nearest", "default or nearest", "na", "keep with error")) {
#     stop("ValidateBoundaries: id=", id, " outofbounds should be one of 'nearest', 'default or nearest', 'na', or 'keep with error' but is ", outofbounds)
#   }
#   if(outofb == "keep with erorr") {
#     paste0("Out of range values are kept with an error message.")
#   } else {
#     paste0("Out of range values are replaced by ", outofbounds, ".")
#   }
# }

#' Validate a text input value
#' @param state a state object
#' @param id the id of a text input parameter.  
#' @param value a character string or a vector of such.
#' @return a character string. 
ValidateText <- function(state, id, value) {
  if(GetType(state,id) == "text input") {
    v1 <- as.character(value[1])
    attributes(v1) <- attributes(value)
    
    # Enable green status icon.
    note <- paste0("OK:",toString(unname(v1)))
    state%>%SetStatus(id, validationNote = note)
    
    v1
  } else {
    stop("ValidateText called on id=",id,", which is not a text input.")
  }
}

#' Validate a radio input or select input value
#' @param state a state object
#' @param id the id of a radio or select input parameter.  
#' @param value a character vector of length 1.
#' @return if value is within the radio choices for id, the same value is returned, otherwise an error is raised.
#' @export 
ValidateRadio <- function(state, id, value) {
  if(GetType(state,id) %in% c("radio input", "select input")) {
    if(!is.character(value) || length(value) != 1) {
      stop("ValidateRadio: the value to validate must be a character string. ")
    } else if(!value %in% GetRadioChoices(state, id)) {
      stop("ValidateRadio: the value ", value, " is not from the radio choices for id=", id, ". ")
    } else {
      
      # Enable green status icon.
      note <- paste0("OK:",toString(unname(value)))
      state%>%SetStatus(id, validationNote = note)
      
      value
    }
  } else {
    stop("ValidateRadio called on id=",id,", which is not a radio or a select input.")
  }
}

#' Get the validated value of an input parameter id in a state object
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return the validate value for id in state.
#' @export
GetValidated <- function(state, id) {
  state$validated[[id]]
}

SetValidated <- function(state, id, value) {
  state$validated[[id]] <- value
}

#' Get the status for an input parameter id in a state object
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return a list with the status for id in state.
#' @export
GetStatus <- function(state, id) {
  state$status[[id]]
}

#' Set the status for an input parameter id in a state object
#' @param state the state object.
#' @param id the id of the input parameter.
#' @param ... a name-value pairs to update the status for id in state.
#' 
#' @details Existing name value-pairs in the status for id in state are updated with the new values or
#' kept unchanged if not specified in the ... argument.
#' 
#' @return nothing - this function only has side effects: updating the status for id in state.
#' @export
SetStatus <- function(state, id, ...) {
  nameValueList <- list(...)
  for(name in names(nameValueList)) {
    state$status[[id]][[name]] <- nameValueList[[name]]
  }
}

#' Get the current event for an input parameter id in a state object
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return a character string, one of "USER", "SCDATA", "REPORT", "INIT", "DEFAULT", "MIN", "MAX".
#' 
#' @export
#' 
#' @details
#' This function is called from Process...Event functions.
GetEvent <- function(state, id) {
  state$event[[id]]
}

#' Set event for input parameter id in state
#' @param state the state object.
#' @param id the id of the input parameter.
#' @param value character string, one of "USER", "SCDATA", "REPORT", "INIT", "DEFAULT", "MIN", "MAX".
#' @return nothing - this function only has side effects: setting the \code{state$event[[id]]}.
#' 
#' @details
#' This function is supposed to be called before calling an observer code would call Process...Event functions.
#' 
#' @export
SetEvent <- function(state, id, value) {
  cat2("SetEvent(",id, ", ", value, ")\n")
  state$event[[id]] <- value
}

#' Get user input value for input parameter id in state.
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return the guiInput value for id in state.
#' 
#' @export
GetGuiInput <- function(state, id) {
  state$guiInput[[id]]
}

SetGuiInput <- function(state, id, value) {
  state$guiInput[[id]] <- value
}

#' Get the id of the GUI input element associated with parameter id. 
#' 
#' The GUI input element id is the concatenation of state$stateId, '_', and id, 
#' unless a prefixGuiId is specified in state, in which case the GUI input element id is:
#' * id if state$prefixGuiId == ''
#' * the concatenation of state$prefixGuiId, '_', and id, otherwise.
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return a character string.
#' @export
#' @md
GetGuiId <- function(state, id) {
  if(is.null(state$prefixGuiId)) {
    paste0(state$stateId, "_", id)
  } else if(state$prefixGuiId == "") {
    id
  } else {
    paste0(state$prefixGuiId, "_", id)
  }
}

#' Get the value for an id inputted from a report
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return the reportInput value for id in state. 
#' 
#' @details This function is not supposed to be called by end users. Given that 
#' the reportInput is a reactiveValue, this function can only be called from
#' within a reactive context.
#' 
#' @export
GetReportInput <- function(state, id) {
  state$reportInput[[id]]
}

#' Set the value for an input parameter id read from a report
#' @param state the state object.
#' @param id the id of the input parameter.
#' @param value the value to set for id in state.
#' @param flagSynchronous a logical indicating whether the processing of the input, including validation
#' and setting the status, should be synchronous. Default is \code{getOption("MMVshiny.synchSetReportInput", FALSE)}.
#' Beaware that setting this to TRUE can lead to unexpected behavior if you have an observer on 
#' GetReportInput() for the same id.
#' @return nothing - this function only has side effects: setting the \code{state$reportInput[[id]]}. 
#' @details
#' This function should be called within a reactive context. 
#' 
#' @export
SetReportInput <- function(state, id, value, flagSynchronous = getOption("MMVshiny.synchSetReportInput", FALSE)) {
  cat2('Report input for id=',id,', value=', value,'\n')
  state$reportInput[[id]] <- value
  if(flagSynchronous) {
    ProcessReportInputEvent(state, id)
  }
}

#' Get the reset count for an input parameter id in state
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' 
#' @return the reset count for id in state.
#' 
#' @details This function is not supposed to be called by end users but from 
#' automatically generated observers. Since the resetCount is a reactiveValue,
#' this function can only be called from within a reactive context.
#' 
#' @export
GetResetCount <- function(state, id) {
  state$resetCount[[id]]
}
IncrementResetCount <- function(state, id) {
  state$resetCount[[id]] <- state$resetCount[[id]] + 1
}

#' Get the GUILABEL for one or several ID's in the spec
#' @param ids a character vector indicating ids for which the GUILABEL should be returned
#' @return a character vector
#'
#' @export
GetGuiLabel <- function(state, ids, spec = state$spec) {
  sapply(ids, function(id) spec[ID == id, GUILABEL])
}

#' Get the REPORTLABEL for one or several ID's in the spec
#' @param ids a character vector indicating ids for which the REPORTLABEL should be returned
#' @return a character vector
#' @export
GetReportLabel <- function(state, ids, spec = state$spec) {
  sapply(ids, function(id) spec[ID == id, REPORTLABEL])
}

#' Get the possible choices for a radio input from the state specification
#' 
#' @param state the state object.
#' @param id the id of the radio input parameter.
#' @param spec the parameter specification table. Default: `state$spec`.
#' 
#' @return a character vector of the possible choices for the radio input.
#' @export
GetRadioChoices <- function(state, id, spec = state$spec) {
  if(GetType(id = id, spec = spec) %in% c("radio input", "select input")) {
    as.character(strsplit(spec[ID == id, RADIOVALUES], split = ":", fixed = TRUE)[[1]])
  } else {
    stop("GetRadioChoices called for a non-radio/select input ", id)
  }
}

#' Evaluate the default expression for an input parameter in a state
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' 
#' @details This function is not supposed to be called by end users. It is used by
#' automatically generated observer code. Since this function evaluates a reactive
#' expression it can only be called within a reactive context.
#' 
#' @return the result of evaluating the default expression for id in state. If the
#' default expression evaluates to NULL \code{NAVal(state, id)} is returned.
#' @export
GetDefault <- function(state, id) {
  if(is.function(state$default[[id]])) {
    value <- state$default[[id]]()
  } else {
    value <- state$default[[id]]
  }
  
  if(is.null(value)) {
    value <- NAVal(state, id = id)
  }
  
  value
}

#' Evaluate the default note expression for an input parameter in a state
#' 
#' @param state the state object.
#' @param id the id of the input parameter.
#' 
#' @details This function is not supposed to be called by end users. It is used by
#' automatically generated observer code. Since this function evaluates a reactive
#' expression it can only be called within a reactive context.
#' 
#' @return the result of evaluating the default note expression for id in state. 
#' @export
GetDefaultNote <- function(state, id) {
  if(is.function(state$defaultNote[[id]])) {
    as.character(state$defaultNote[[id]]())
  } else {
    as.character(state$defaultNote[[id]])
  }
}

#' Get the displayed value in a GUI input field for an input parameter id in a state object.
#' @param state the state object.
#' @param id the id of the input parameter.
#' @return the displayed for id. 
#' @export
GetDisplayed <- function(state, id) {
  state$displayed[[id]]
}

SetDisplayed <- function(state, id, value) {
  attributes(value) <- NULL
  
  if(state%>%GetType(id) == "numeric input") {
    # function for rounding
    digs <- GetDecDigits(state, id)
    rounded <- round(value, digs)
    state$displayed[[id]] <- rounded
    
  } else if(state%>%GetType(id) %in% c("text input", "radio input", "select input")) {
    state$displayed[[id]] <- value
  } else {
    stop("SetDisplayed: id=",id," is not a numeric input, text input, radio input, or select input.")
  }
}


#' Create an input field and, optionally, a status icon according to the type of id in a spec
#' @param state the state object. Can be missing, in which case, the function assumes that the 
#' create UI element is global for all states, so that the id of the element is equal to the argument
#' \code{id}. If state is provided, the id of the created element will be \code{GetGuiId(state, id)}.
#' @param id the id of the input parameter for which an input field will be created.
#' @param spec the specification object. Default is \code{state$spec}.
#' @param useLabel logical indicating whether to use the GUILABEL from the spec. Default is TRUE.
#' @param iconValign the vertical alignment of the status icon. Default is "middle". This argument is only
#' used if \code{spec[ID == id, STATUSICON == TRUE]}.
#' @param ... additional arguments to be passed to the input function.
#' 
#' @return a list of td elements: one for the input and, another one for the status icon. If
#' \code{spec[ID == id, STATUSICON == FALSE]}, only a single td element is returned.
#' 
#' @export
#' @md
CreateUIInput <- function(state, id, spec = state$spec, useLabel = TRUE, iconValign="middle", ...) {
  guiId <- if(missing(state) || is.null(state)) {
    id
  } else {
    GetGuiId(state, id)
  }
  
  if( !is.data.frame(spec) ) {
    stop("CreateInput: spec should be a data.frame.")
  }
  spec <- as.data.table(spec)
  if(!id %in% spec$ID) {
    stop("CreateInput: id=",id, " not found in supplied spec object.")
  }
  
  if(useLabel) {
    label <- spec[ID == id, GUILABEL]
  } else {
    label <- NULL
  }
  
  
  listArgs <- list(inputId = guiId, label = label, value = NAVal(id = id, spec = spec))
  list... <- list(...)
  
  for(name in names(list...)) {
    # Overwrite the default value with the value from the list...
    listArgs[[name]] <- list...[[name]]
  }
  
  el <- if(GetType(id = id, spec = spec) == "text input") {
    do.call(textInput, listArgs)
  } else if(GetType(id = id, spec = spec) == "numeric input") {
    if(!"step" %in% names(listArgs)) {
      listArgs$step <- "any"
    }
    do.call(numericInput, listArgs)
  } else if(GetType(id = id, spec = spec) == "radio input") {
    if(!"choices" %in% names(listArgs)) {
      listArgs$choices <- GetRadioChoices(id = id, spec = spec)
    }
    if(!"selected" %in% names(listArgs)) {
      listArgs$selected <- NA_character_
    }
    # make sure that there is no value element as this is not used in radioButtons
    listArgs$value <- NULL
    do.call(radioButtons, listArgs)
  }
  
  else if(GetType(id = id, spec = spec) == "select input") {
    if(!"choices" %in% names(listArgs)) {
      listArgs$choices <- GetRadioChoices(id = id, spec = spec)
    }
    if(!"selected" %in% names(listArgs)) {
      listArgs$selected <- NA_character_
    }
    # IMPORTANT: Shiny's selectInput() uses selectize.js by default. In that mode the
    # underlying <select> element is hidden (display: none), so CSS like background-color
    # applied to the <select> won't be visible. MMVshiny relies on background-color to
    # indicate default/derived values (e.g., azure), therefore we disable selectize by
    # default for "select input".
    if(!"selectize" %in% names(listArgs)) {
      listArgs$selectize <- FALSE
    }
    
    # make sure that there is no value element as this is not used in selectInput
    listArgs$value <- NULL
    do.call(selectInput, listArgs)
  } else if(GetType(id = id, spec = spec) == "action button input") {
    # actionButton does not use a value argument
    listArgs$value <- NULL
    do.call(actionButton, listArgs)
  }
  
  if(spec[ID == id, STATUSICON]) {
    list(tags$td(el), tags$td(valign = iconValign, uiOutput(paste0(guiId,"icon"))))
  } else {
    list(tags$td(el))
  }
}

#' Generate JS code for event handlers on the client's browser side
#' @param spec parameter specification.
#' @param ids a character vector of ids for which event handlers should be generated. Default
#' is all input parameters in spec, i.e. \code{spec[grepl("input", TYPE), ID]}.
#' @param filename a character string path to an R-file where the R-code will be generated. If
#' not specified a tempfile is created.
#' 
#' @return a character string - the filepath to the generated R-script (same as filename if this 
#' argument was provided).
#' @export
GenerateJavaScriptEventHandlers <- function(spec, ids = spec[grepl("input", TYPE), ID], filename) {
  
  spec <- as.data.table(spec)
  
  
  if(missing(filename)) {
    filename <- tempfile(pattern = "HandleEvents", fileext = ".js")
  }
  if(!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  sink(filename) 
  
  cat("// This script is automatically generated by the function GenerateJavaScriptBlurHandlers.\n")
  cat("// This script should be included via a call to htmltools::includeScript.\n\n")
  
  cat("$(document).ready(function() {\n\n")
  
  cat("  // initialize a counter for each input from spec\n")
  
  codeNumericTextInput <-
    'var nID = 0;
   // create a handler
   $("#ID").bind("blur keyup", function(e) {
     if (e.type === "blur" || e.keyCode === 13) {
       // increment the counter each time input loses focus
       nID++;
       Shiny.onInputChange("countID", nID);
     }
   });
  '
  
  codeRadioInput <- 
    'var nID = 0;
   // create a handler 
   $("#ID").bind("click", function(e) {
     // increment the counter each time input is clicked
     nID++;
     Shiny.onInputChange("countID", nID);
   });
  '
  
  codeSelectInput <- 
    'var nID = 0;
   // create a handler 
   $("#ID").bind("change", function(e) {
     // increment the counter each time selection changes
     nID++;
     Shiny.onInputChange("countID", nID);
   });
  '
  
    
  codeActionButton <- 
    'var nID = 0;
   // create a handler 
   $("#ID").bind("click", function(e) {
     // increment the counter each time button is clicked
     nID++;
     Shiny.onInputChange("countID", nID);
   });
  '

for(id in ids) {
    
    if( GetType(id = id, spec = spec) %in% c("numeric input", "text input") ) {
      codeToPut <- gsub('ID', id, codeNumericTextInput, fixed = TRUE)
      cat(codeToPut)
    } else if(GetType(id = id, spec = spec) %in% c("radio input")) {
      codeToPut <- gsub('ID', id, codeRadioInput, fixed = TRUE)
      cat(codeToPut)
    } else if(GetType(id = id, spec = spec) %in% c("select input")) {
      codeToPut <- gsub('ID', id, codeSelectInput, fixed = TRUE)
      cat(codeToPut)
    } else if(GetType(id = id, spec = spec) %in% c("action button input")) {
      codeToPut <- gsub('ID', id, codeActionButton, fixed = TRUE)
      cat(codeToPut)
    }
  }    
  cat("});\n")
  
  sink()
  
  filename
}

#' Generate observeEvent calls and store them in an R-file to be sourced from within shiny server or state environment
#' 
#' @param spec parameter specification.
#' @param inputObjectName a character string indicating the name of the shiny input object (default "input").
#' @param outputObjectName a character string indicating the name of the shiny output object (default "output").
#' @param stateObjectName a character string indicating the name of the MMVSola state object (default: "state").
#' @param ids a character vector of ids for which observers should be generated.
#' @param observerTypes a character vector of observer types to be generated. Possible values 
#' are "countID", "default", "displayed", "SCInput", "reportInput", "resetCount", "min", "max". Bye default, this
#' is a vector of all of these. Some observer types may not be relevant for some input types, for example, 
#' "min" and "max" are only relevant for numeric inputs, and countID and displayed are only relevant for inputs
#' with a GUI label.
#' @param filename a character string path to an R-file where the R-code will be generated. If
#' not specified a tempfile is created.
#' 
#' @return a character string - the filepath to the generated R-script (same as filename if this 
#' argument was provided).
#' @export
GenerateScriptCreatingObservers <- function(
    spec, 
    inputObjectName = 'input', 
    outputObjectName = 'output',
    stateObjectName = 'state', 
    ids = unique(spec$ID), 
    observerTypes = c("countID", "default", "displayed", "SCInput", "reportInput", "resetCount", "min", "max"),
    filename) {
  
  if(missing(filename)) {
    filename <- tempfile(pattern = "GenerateScriptCreatingObservers_", fileext = ".R")
  }
  if(!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  spec <- as.data.table(spec)
  
  
  sink(filename) 
  cat("# This script is automatically generated by the function GenerateScriptCreatingObservers\n")
  cat("# This script should be sourced via `source('",filename,"', local=TRUE)` from within a\n", 
      "# shiny server function.\n\n")
  
  codeTemplates <- list(
    countID = 
      '
  observeEvent(inputObj$countID, {
    cat2("\\nUSER INPUT on ID (count=", inputObj$countID, "): ", sep="")
    ProcessGuiInputEvent(s = stateObj, input = inputObj, output = outputObj, session = session, id = "ID")
  }, ignoreInit = TRUE)',

    default =  
      '
  observeEvent(GetDefault(stateObj, "ID"), {
    cat2("\\nDEFAULT CHANGED on ID: ", GetDefault(stateObj, "ID"), " ")
    ProcessDefaultChangedEvent(s = stateObj, id = "ID")
  })',
    
    displayed =  
      '
  observeEvent(list(GetDisplayed(stateObj, "ID"), GetSource(stateObj, "ID")), {
    cat2("\\nDISPLAYED CHANGED on ", GetGuiId(stateObj, "ID"), ":", GetDisplayed(stateObj, "ID"), " ")
    ProcessDisplayedChangedEvent(s = stateObj, id = "ID")
  })',
    
    SCInput = 
      '
  observeEvent(GetSCInput(stateObj, "ID"), {
    cat2("\\nSC INPUT on ID: ", GetSCInput(stateObj, "ID"), " ")
    ProcessSCInputEvent(s = stateObj, id = "ID")
  }, ignoreInit = FALSE)',
    
    reportInput =  
      '
  observeEvent(GetReportInput(stateObj, "ID"), {
    cat2("\\nREPORT INPUT on ID: ", GetReportInput(stateObj, "ID"), " ")
    ProcessReportInputEvent(s = stateObj, id = "ID")
  }, ignoreInit = FALSE)',
    
    resetCount =  
      '
  observeEvent(GetResetCount(stateObj, "ID"), {
    cat2("\\nRESET on ID: ", GetResetCount(stateObj, "ID"), " ")
    ProcessResetEvent(s = stateObj, id = "ID")
  })',
    
    min = 
      '
  observeEvent(GetMin(stateObj, "ID"), {
    cat2("\\nMIN CHANGED on ID: ", GetMin(stateObj, "ID"), " ")
    ProcessMinChangedEvent(s = stateObj, id = "ID")
  })',
    
    max =  
      '
  observeEvent(GetMax(stateObj, "ID"), {
    cat2("\\nMAX CHANGED on ID: ", GetMax(stateObj, "ID"), " ")
    ProcessMaxChangedEvent(s = stateObj, id = "ID")
  })'
  )
  
  for(id in ids) {
    type <- GetType(id = id, spec = spec)
    
    cat('\n\n# ', id, " - ", type, " ---- ")
    
    # Assemble the code to put
    code <- ""
    if( type %in% c("numeric input", "text input", "radio input", "select input", "action button input") ) {
      
      if( "countID" %in% observerTypes ) {
        code <- paste(code, codeTemplates$countID, "\n")
      }
      if( "displayed" %in% observerTypes && type != "action button input") {
        code <- paste(code, codeTemplates$displayed, "\n")
      }
      if( type == "numeric input" ) {
        if( "min" %in% observerTypes ) {
          code <- paste(code, codeTemplates$min, "\n")
        }
        if( "max" %in% observerTypes ) {
          code <- paste(code, codeTemplates$max, "\n")
        }
      }

      for( obsType in setdiff(observerTypes, c("countID", "displayed", "min", "max")) ) {
        code <- paste(code, codeTemplates[[obsType]], "\n")
      }
    } 
    
    codeToPut <- gsub('ID', id, code, fixed = TRUE)
    codeToPut <- gsub('inputObj', inputObjectName, codeToPut, fixed = TRUE)
    codeToPut <- gsub('outputObj', outputObjectName, codeToPut, fixed = TRUE)
    codeToPut <- gsub('stateObj', stateObjectName, codeToPut, fixed = TRUE)
    
    cat(codeToPut)
  }
  
  sink()
  
  filename
}

#' Generate a R script calling renderUI for each status icon in a spec
#' @param spec parameter specification table.
#' @param prefixGuiId a character string indicating the prefix for the gui id (default ""). 
#' @param outputObjectName a character string indicating the name of the shiny output object (default "output").
#' @param stateObject a character stirng indicating the name of the MMVSola state object (default: "state").
#' @param ids a character vector of ids for which status icons should be rendered. Default is all ids in spec with 
#' STATUSICON == TRUE.
#' @param filename a character string path to an R-file where the R-code will be generated. If
#' not specified a tempfile is created.
#' 
#' @return a character string - the filepath to the generated R-script (same as filename if this 
#' argument was provided).
#' @export
GenerateScriptRenderingIcons <- function(
    spec, prefixGuiId = "", outputObjectName = 'output', stateObjectName = 'state', 
    ids = spec[STATUSICON == TRUE, ID], 
    filename) {
  
  if(missing(filename)) {
    filename <- tempfile(pattern = "RenderStatusIcons", fileext = ".R")
  }
  if(!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  # Pay attention to the '_' as it is inserted in GetGuiId.
  if(prefixGuiId != "") {
    prefixGuiId <- paste0(prefixGuiId, "_")
  }
  
  spec <- as.data.table(spec)
  
  sink(filename)
  cat("# This script is automatically generated by the function GenerateScriptRenderStatusIcons.\n")
  cat("# This script should be sourced via `source('",filename,"', local=TRUE)` from within a\n", 
      "# shiny server function or a state environment where the objects ", outputObjectName, " and ", 
      stateObjectName, " exist.\n\n", sep = "")
  for(id in ids) {
    code <- paste0('output[["prefixGuiIdIDicon"]] <- renderUI({\n',
                   '  tags$i(class = stateObj$statusIconClass[["ID"]],\n',
                   '         title = paste0(stateObj$statusTitle[["ID"]], "\n", stateObj$statusText[["ID"]]),\n',
                   '         style = paste0("color:",stateObj$statusIconColor[["ID"]], ";"))\n',
                   ' })\n\n', sep="")
    codeToPut <- gsub("prefixGuiId", prefixGuiId, code, fixed = TRUE)
    codeToPut <- gsub("ID", id, codeToPut, fixed = TRUE)
    codeToPut <- gsub('stateObj', stateObjectName, codeToPut, fixed = TRUE)
    cat(codeToPut)
  }
  sink()
  
  filename
}

#' Generate an R script creating reactive objects inside the server function for each value in state
#' @param spec parameter specification table.
#' @param stateObject a character stirng indicating the name of the MMVSola state object (default: "state").
#' @param ids a character vector of ids for which reactive objects should be created. Default is all ids in spec.
#' @param filename a character string path to an R-file where the R-code will be generated. If
#' not specified a tempfile is created.
#' 
#' @return a character string - the filepath to the generated R-script (same as filename if this 
#' argument was provided).
#' @export
GenerateScriptCreatingReactives <- function(
    spec, stateObjectName = 'state', ids = spec[, unique(ID)], filename) {
  
  if(missing(filename)) {
    filename <- tempfile(pattern = "CreateReactivesInServer.R", fileext = ".R")
  }
  
  
  spec <- as.data.table(spec)
  
  sink(filename)
  cat("# This script is automatically generated by the function GenerateScriptCreatingReactives.\n")
  cat("# This script should be sourced via `source('",filename,"', local=TRUE)` from within a\n", 
      "# shiny server function where the object ", stateObjectName, " exists.\n\n", sep = "")
  
  code <- 
    'ID <- reactive({Get(stateObj, "ID")})
    '
  
  for(id in spec$ID) {
    codeToPut <- gsub("ID", id, code, fixed = TRUE)
    codeToPut <- gsub('stateObj', stateObjectName, codeToPut, fixed = TRUE)
    cat(codeToPut)
  }
  
  sink()
  
  filename
}

SetInfoIcon <- function(state, id, spec = state$spec) {
  
  type <- state%>%GetType(id)
  status <- state%>%GetStatus(id)
  source <- status$source
  
  statusTitle <- if( startsWith(tolower(source), "default") ) {
    GetDefaultNote(state, id)
  } else {
    source
  }
  statusText <- if( type == "numeric input" ) {
    c(GetValidationNote(state = state, id = id), GetValidRangeNote(state, id), GetMinNote(state, id), GetMaxNote(state, id))
  } else {
    GetValidationNote(state = state, id = id)
  }
  
  if(!is.null(statusText)) {
    hasOK <- isTRUE(any(sapply(statusText, startsWith, "OK:")))
    hasWARN <- isTRUE(any(sapply(statusText, startsWith, "WARN:")))
    hasERROR <- isTRUE(any(sapply(statusText, startsWith, "ERROR:")))
    statusText <- gsub(pattern = "^OK: |^WARN: |^ERROR: ", replacement = "", x = statusText)
    statusText <- paste0(na.omit(statusText), collapse = "\n")
  } else {
    hasOK <- FALSE
    hasWARN <- FALSE
    hasERROR <- FALSE
  }
  
  state$statusTitle[[id]] <- statusTitle
  state$statusText[[id]] <- statusText
  
  if(startsWith(tolower(source), "default") && !hasWARN && !hasERROR) {
    state$statusIconClass[[id]] <- "glyphicon glyphicon-info-sign"
    state$statusIconColor[[id]] <- "#337ab7"
  } else if(hasOK && !hasWARN && !hasERROR) {
    state$statusIconClass[[id]] <- "glyphicon glyphicon-ok-circle"
    state$statusIconColor[[id]] <- "#008000"
  } else if(hasWARN && !hasERROR) {
    state$statusIconClass[[id]] <- "glyphicon glyphicon-exclamation-sign"
    state$statusIconColor[[id]] <- "#ff8533"
  } else if(hasERROR) {
    state$statusIconClass[[id]] <- "glyphicon glyphicon-exclamation-sign"
    state$statusIconColor[[id]] <- "#ff0000"
  } else {
    stop("SetInfoIcon: Case not implemented for id=",id, " source=", source, "; hasOK = ", hasOK, "; hasWARN=", hasWARN, "; hasERROR=", hasERROR, "\n")
  }
}

#' Wrapper of shiny::updateNumericInput that also calls session$setInputs if session is a MockShinySession.
#' @inheritParams shiny::updateNumericInput
#' 
#' @return result of shiny::updateNumericInput.
#' 
#' @details
#' This function is useful for testing shiny applications with shiny::testSever. 
#' @export
UpdateNumericInput <- function(
    session = getDefaultReactiveDomain(),
    inputId,
    label = NULL,
    value = NULL,
    min = NULL,
    max = NULL,
    step = NULL) {
  if(inherits(session, "MockShinySession")) {
    eval(parse(text = paste0("session$setInputs(", inputId, "=", value, ")")))
  }
  updateNumericInput(session = session, inputId = inputId, label = label, value = value, min = min, max = max, step = step)
}

#' Wrapper of shiny::updateTextInput that also calls session$setInputs if session is a MockShinySession.
#' 
#' @inheritParams shiny::updateTextInput
#' @return result of shiny::updateTextInput.
#' 
#' @details
#' This function is useful for testing shiny applications with shiny::testSever.
#' @export
UpdateTextInput <- function(
    session = getDefaultReactiveDomain(),
    inputId,
    label = NULL,
    value = NULL,
    placeholder = NULL) {
  if(inherits(session, "MockShinySession")) {
    eval(parse(text = paste0("session$setInputs(", inputId, "='", value, "')")))
  }
  updateTextInput(session = session, inputId = inputId, label = label, value = value, placeholder = placeholder)
}

#' Wrapper of shiny::updateRadioButtons that also calls session$setInputs if session is a MockShinySession.
#' 
#' @inheritParams shiny::updateRadioButtons
#' @return result of shiny::updateRadioButtons.
#' @details
#' This function is useful for testing shiny applications with shiny::testSever.
#' @export
UpdateRadioButtons <- function(
    session = getDefaultReactiveDomain(), 
    inputId,
    label        = NULL,
    choices      = NULL,
    selected     = NULL,
    inline       = FALSE,
    choiceNames  = NULL, 
    choiceValues = NULL) {
  
  if(inherits(session, "MockShinySession")) {
    eval(parse(text = paste0("session$setInputs(", inputId, "='", selected, "')")))
  }
  updateRadioButtons(session = session, inputId = inputId, label = label, 
                     choices = choices, selected = selected, inline = inline, 
                     choiceNames = choiceNames, choiceValues = choiceValues)
}

#' Wrapper of shiny::updateSelectInput that also calls session$setInputs if session is a MockShinySession.
#' 
#' @inheritParams shiny::updateSelectInput
#' @return result of shiny::updateSelectInput.
#' @details
#' This function is useful for testing shiny applications with shiny::testSever.
#' @export
UpdateSelectInput <- function(
  session = getDefaultReactiveDomain(),
  inputId,
  label = NULL,
  choices = NULL,
  selected = NULL,
  multiple = FALSE,
  selectize = TRUE,
  width = NULL,
  size = NULL
) {
  # For unit testing with MockShinySession
  if(inherits(session, "MockShinySession")) {
    if(!is.null(selected)) {
      eval(parse(text = paste0("session$setInputs(", inputId, "='", selected, "')")))
    }
  }

  updateSelectInput(
    session = session,
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected#,
    #multiple = multiple,
    #selectize = selectize,
    #width = width
  )
}

#' Get the NA value of the correct type corresponding to id
#' @param state the state object
#' @param id the id of the input
#' @param spec the spec object. Default is state$spec.
#' @return the NA value of the correct type
#' @export
NAVal <- function(state, id, spec = state$spec) {
  NA_values <- list(`text input` = NA_character_, 
                    `numeric input` = NA_real_, 
                    `numeric constant` = NA_real_, 
                    `radio input` = NA_character_,
                    `select input` = NA_character_,
                    `action button input` = NULL,
                    `reactive` = NULL)
  type <- GetType(id = id, spec = spec)
  if(!type %in% names(NA_values)) {
    stop("NAVal: Unknown NA value for type: ", type)
  }
  NA_values[[type]]
}


#' Get the value for an id averaged from an uploaded Science Cloud file
#' @param state a state object
#' @param id the id of the input parameter 
#' @return the input value
#' @export
GetSCInput <- function(state, id) {
  state$scInput[[id]]
}

#' Set the value for an id averaged from an uploaded Science Cloud file
#' @param state a state object
#' @param id the id of the input parameter
#' @param value the value to set
#' @param flagSynchronous logical indicating if the SCDATA event should be 
#' processed within this function call (TRUE) or if it should be processed when an observer 
#' on GetSCInput(id) is triggered (FALSE). By default this is set to 
#' \code{getOption("MMVshiny.synchSetSCInput", FALSE)}. 
#' Pay attention that setting this argument to TRUE may lead to unexpected behavior if there
#' is an observer on GetSCInput(id). 
#' @export
SetSCInput <- function(state, id, value, flagSynchronous = getOption("MMVshiny.synchSetSCInput", FALSE)) {
  cat2('SetSCInput: id=',id,', value=', value,'\n')
  state$scInput[[id]] <- value
  if(flagSynchronous) {
    ProcessSCInputEvent(s = state, id = id)
  }
}

#' Read value from a Science Cloud input file
#' @param dataSC a data.frame representing the Science Cloud input file. Can be NULL, in which case NAvalue is returned.  
#' @param filter a named list with names corresponding to columns in dataSC and values 
#' representing filter expressions - in these expressions the '$$' is treated as a blank 
#' and is replaced by the columnName. After blank replacement, the filter elements are 
#' combined into one expression using &, i.e. logical AND. If a non NULL exprFilter is specified, 
#' this argument is ignored. 
#' @param exprValue an R expression evaluated within the environment of the filtered data to calculate the final value.
#' @param NAvalue a value to return in case of an error or if the filtering resulted in empty data. 
#' @param exprFilter NULL (default) or character string denoting a filter expression. If not NULL, the filter
#' argument will be ignored. 
#' @return NAvalue in case of empty filtered data or an error during filtering or value evaluation. Otherwise, the result
#' of evaluating exprValue on the filtered data. 
ReadSCInput <- function(dataSC, filter, exprValue, NAvalue = NA_real_, exprFilter = NULL) {
  if(is.null(dataSC)) {
    NAvalue
  } else if(is.data.frame(dataSC)) {
    
    if(is.null(exprFilter)) {
      stopifnot(is.list(filter))
      stopifnot(!is.null(names(filter)))
      stopifnot(isTRUE(all(names(filter) %in% names(dataSC))))
      
      # replace blanks with corresponding column names in filter values. 
      filter2 <- sapply(names(filter), function(columnName) {
        gsub("$$", columnName, filter[[columnName]], fixed = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
      # Combine filter values into one expression, assuming logical AND
      exprFilter <- do.call(paste, c(filter2, list(sep = " & ")))
    }       
    # Convert dataSC to data.table for easier and faster filtering
    dtSC <- as.data.table(dataSC)
    dtSCFiltered <- try(dtSC[eval(parse(text = exprFilter)),], silent = TRUE)
    if(inherits(dtSCFiltered, "try-error")) {
      warning("ReadSCInput: Returning NA_real_ since there was an error while filtering SC data: ", 
              toString(dtSCFiltered), ", exprFilter: ", exprFilter)
      NAvalue
    } else if(nrow(dtSCFiltered) > 0) {
      value <- try(dtSCFiltered[, eval(parse(text = exprValue))], silent = TRUE)
      if(inherits(value, "try-error")) {
        warning("ReadSCInput: Returning NAvalue since there was an error while evaluating the aggregating expression. exprFilter: ", 
                exprFilter, ", exprValue: ", exprValue)
        NAvalue
      } else {
        value
      }
    } else {
      # return NA_real_ if the filtering resulted in 0 records
      NAvalue
    }
  }
}

#' Calculate SCInput values on the base of SC raw data
#' 
#' Applies the SCFILTER and SCVALUE expressions from the input specification in the context
#' of raw SC data uploaded by the user or changed after a data curation event (the user checking
#' or unchecking a row). 
#' 
#' @param state a state object
#' @param ids character vector containing the ids of input parameters for which the 
#' values have to be updated. 
#' @param flagSetEvent logical (default: TRUE) indicating if an SCDATA event should be 
#' fired. Currently, the purpose of this argument is to prevent firing SCDATA events 
#' when loading Science Cloud raw data from a MMVSola report file. 
#' @param flagSynchronous logical indicating if the SCDATA event should be 
#' processed within this function call (TRUE) or if it should be processed when an observer 
#' on GetSCInput(id) is triggered (FALSE). By default this is set to \code{getOption("MMVshiny.synchSetSCInput", FALSE)}. 
#' Pay attention that setting this argument to TRUE may lead to unexpected behavior if there
#' is an observer on GetSCInput(id). This argument is passed to SetSCInput.
#' 
#' @return nothing - this function only has a side-effect, namely calling SetSCInput and SetEvent("SCDATA").
#' @export
CalculateSCInputs <- function(state, ids, flagSetEvent = TRUE, flagSynchronous = getOption("MMVshiny.synchSetSCInput", FALSE)) {
  scRawData <- GetSCRawData(state)
  if(isSCRawData(scRawData)) {
    for(id in ids) {
      scExprFilter <- state$spec[ID == id, SCFILTER]
      scExprValue <- state$spec[ID == id, SCVALUE]
      
      if(!is.na(scExprFilter) && scExprFilter != "") {
        cat2("Calculating SCInput value for ", id, ", based on SC raw data")
        
        newSCAvgValue <- ReadSCInput(dataSC = scRawData, 
                                     exprFilter = paste0("INCLUDE == TRUE & (", scExprFilter, ")"), 
                                     exprValue = scExprValue, 
                                     NAvalue = NAVal(id = id, spec = state$spec))
        
        currentSCAvgValue <- GetSCInput(state, id)
        
        currentSource <- GetSource(state, id)
        
        cat2("; current value =", currentSCAvgValue)
        cat2("; new value =", newSCAvgValue)
        cat2("; currentSource =", currentSource)
        cat2("; flagSetEvent =", flagSetEvent)
        cat2("\n")
        
        # We don't want to overwrite a validated value entered by the user with an NA value from Science Cloud.
        # Hence, we don't trigger an event in this case. Note that an NA value from Science Cloud can result from 
        # unchecking a row in the Science Cloud raw data. 
        if( flagSetEvent && !(is.na(newSCAvgValue) && tolower(currentSource) != "science cloud") ) {
          SetEvent(state, id, "SCDATA")
        }
        SetSCInput(state, id, newSCAvgValue, flagSynchronous = flagSynchronous)
        
      } 
    }
  }
}

#' Column names of the Science Cloud input file in the preferred order.
#'
#' @return a character vector of the row Science Cloud column names in the order they 
#' should be displayed in the GUI and the report excel sheet "Science Cloud Input"
#' 
#' @export
SCColumns <- function() {
  d <- data.table::as.data.table(
    tibble::tribble(
                              ~NAME,                ~NAMEWRAPPABLE,        ~TYPE,           ~NAVAL, 
      "INCLUDE"                    , "INCLUDE"                    ,    "logical",           "TRUE",
      "REASON TO EXCLUDE"          , "REASON TO EXCLUDE"          ,  "character",  "NA_character_",
      "COMPOUND_ID"                , "COMPOUND ID"                ,  "character",  "NA_character_",
      "ASSAY_PARAMETER_NAME"       , "ASSAY PARAMETER NAME"       ,  "character",  "NA_character_",
      "ASSAY_PARAMETER_DESCRIPTION", "ASSAY PARAMETER DESCRIPTION",  "character",  "NA_character_",
      "ASSAY_RESULT_TYPE_NAME"     , "ASSAY RESULT TYPE NAME"     ,  "character",  "NA_character_",
      "OPERATOR"                   , "OP"                         ,  "character",  "NA_character_",
      "value"                      , "value"                      ,     "double",       "NA_real_",
      "TEST_UNIT"                  , "TEST UNIT"                  ,  "character",  "NA_character_",
      "ASSAY_RESULT_REGIS_DATE"    , "ASSAY RESULT REGIS DATE"    ,  "character",  "NA_character_",
      "Intercept"                  , "Intercept"                  ,     "double",       "NA_real_",
      "Slope"                      , "Slope"                      ,     "double",       "NA_real_",
      "Lag Phase"                  , "Lag Phase"                  ,     "double",       "NA_real_",
      "Hill slope"                 , "Hill slope"                 ,     "double",       "NA_real_",
      "Dose (uM)"                  , "Dose (uM)"                  ,     "double",       "NA_real_",
      "DOSE"                       , "DOSE"                       ,     "double",       "NA_real_",
      "DOSE_UNIT"                  , "DOSE UNIT"                  ,  "character",  "NA_character_",
      "TIME_POINT"                 , "TIME POINT"                 ,  "character",  "NA_character_",
      "TIME_POINT_UNIT"            , "TIME POINT UNIT"            ,  "character",  "NA_character_",
      "ASSAY_NAME"                 , "ASSAY NAME"                 ,  "character",  "NA_character_",
      "ASSAY_TYPE"                 , "ASSAY TYPE"                 ,  "character",  "NA_character_",
      "TRIVIAL_NAME"               , "TRIVIAL NAME"               ,  "character",  "NA_character_",
      "MOLECULAR_WEIGHT"           , "MOLECULAR WEIGHT"           ,     "double",       "NA_real_",
      "PARAMETERS AFFECTED"        , "PARAMETERS AFFECTED"        ,  "character",             "''",
    )
  )
  d
}

#' Check if a data.frame is a Science Cloud format
#' @param df a data.frame
#' @return TRUE or FALSE
#' @export
isSCRawData <- function(df) {
  isTRUE(is.data.frame(df) && all(SCColumns()$NAME[3:10] %in% names(df)))
}

#' Set Science Cloud data in the state object
#' @param state a state object.
#' @param scRawData a data.frame of data from Science Cloud excel sheet
#' @param flagSetEvent logical (default: TRUE) indicating if an SCDATA event should be 
#' fired. Currently, the purpose of this argument is to prevent firing SCDATA events 
#' when loading Science Cloud raw data from a MMVSola report file. 
#' @param flagSynchronous logical indicating if the SCDATA event should be 
#' processed within this function call (TRUE) or if it should be processed when an observer 
#' on GetSCInput(id) is triggered (FALSE). By default this is set to \code{getOption("MMVshiny.synchSetSCInput", FALSE)}. 
#' Pay attention that setting this argument to TRUE may lead to unexpected behavior if there
#' is an observer on GetSCInput(id). This argument is passed to SetSCInput.
#'
#' @return nothing. This function only has side effects.
#' @export
SetSCRawData <- function(state, scRawData, flagSetEvent = TRUE, flagSynchronous = getOption("MMVshiny.synchSetSCInput", FALSE)) {
  if(is.null(scRawData)) {
    # Reset state scRawData object to NULL
    state$scRawData(NULL)
    
  } else if( isSCRawData(scRawData) ) {
    # Function covnerting x to type unless x is already of this type. 
    # type should be one of logical, character, double
    as <- function(type, x) {
      is.type <- get(paste0("is.", type))
      as.type <- get(paste0("as.", type))
      if(!is.type(x)) {
        cat2("Converting from ", class(x), " to ", type, "\n")
        as.type(x)
      } else {
        x
      }
    }
    
    # Convert columns to the right type
    for(column in SCColumns()$NAME) {
      type <- SCColumns()[NAME == column, TYPE]
      if(column %in% names(scRawData)) {
        scRawData[[column]] <- as(type, scRawData[[column]])
      }
    }
    
    ##Select only one drug (First to appear in science cloud sheet
    scRawData <- subset(scRawData, scRawData$COMPOUND_ID == unique(scRawData$COMPOUND_ID)[1])
    scRawData <- data.table::as.data.table(scRawData)
    
    # If INCLUDE column is present (this is in the case of uploaded an MMVSola report with a Science Cloud File tab), 
    # parse it as logical.
    if( !is.null(scRawData$INCLUDE) ) {
      if(is.character(scRawData$INCLUDE)) {
        scRawData$INCLUDE <- as.logical(scRawData$INCLUDE)
      }
    } 
    
    # Add any missing columns and set values to appropriate NA depending on the column TYPE.
    for(missingCol in setdiff(SCColumns()$NAME, names(scRawData))) {
      scRawData[[missingCol]] <- eval(parse(text = SCColumns()[NAME == missingCol, NAVAL]))
    }
    
    
    # Reorder columns in the prefered order for reporting and display in the MMVSola GUI
    scRawData <- scRawData[, c(intersect(SCColumns()$NAME, names(scRawData)), 
                               setdiff(names(scRawData), SCColumns()$NAME)), 
                           with = FALSE]
    
    # (Re)init columns ID and `PARAMETERS AFFECTED`
    scRawData$ID <- scRawData$`PARAMETERS AFFECTED` <- NULL
    
    # Add the ID column which will contain the parameter ID's affected by each row of the SCRowData
    scRawData[, ID:=list(character(0))]
    for(i in seq_len(nrow(state$spec))) {
      id <- state$spec$ID[i]
      scExprFilter <- state$spec$SCFILTER[i]
      scExprValue <- state$spec$SCVALUE[i]
      cat2("Reading SC input for id", id)
      
      if(!is.na(scExprFilter) && scExprFilter != "") {
        cat2(". ", nrow(scRawData[as.logical(eval(parse(text = scExprFilter)))]), " rows matched the filter expression")
        # add id to the ID column for those rows in scRawData which match the scExprFilter 
        scRawData[as.logical(eval(parse(text = scExprFilter))), ID:=lapply(ID, function(x) c(x,id))]
      } else {
        cat2(". ", "No SC filter expression for this parameter.")
      }
      cat2("\n")
    }
    
    # Add a column to scRawData indicating the GUILABELS of the parameters affected by the values on each row 
    # in the Science Cloud data
    scRawData[, `PARAMETERS AFFECTED`:=sapply(ID, function(ids) paste(GetGuiLabel(state, ids), collapse = "; "), simplify = FALSE)]
    
    # set scRawData in the reactiveVal object of the state
    state$scRawData(scRawData)
    
    # All IDs of parameters which can be calculated based on the Science Cloud data
    idsAffected <- intersect(state$spec$ID, unique(do.call(c, scRawData$ID)))
    CalculateSCInputs(state, idsAffected, flagSetEvent = flagSetEvent, flagSynchronous = flagSynchronous)  
  } else {
    stop("SetSCRawData: Not ScienceCloud data supplied")
  }
}

#' Get the Science Cloud raw data in a state object
#' 
#' @param state a state object.
#' @param copy logical (default FALSE), indicating whether a copy of the reactiveVal data.table should be returned
#' Set to TRUE if you intend to manipulate the SCRawData for displaying purpose, e.g. change column-names to 
#' wrappable strings, etc.
#' 
#' @return a data.table or NULL, if no Science Cloud data has been uploaded
#' @export
GetSCRawData <- function(state, copy=FALSE) {
  dt <- state$scRawData()
  if(is.data.table(dt) && copy) {
    data.table::copy(dt)
  } else {
    dt
  }
}


# -------------------------------------------------------------------------#
# Other helpers ----
# -------------------------------------------------------------------------#

#' A function which cats depending on \code{getOption("MMVshiny.verbose", FALSE)}
#'
#' @param ... Code which is passed to \code{\link{cat}}
#'
#' @return Nothing, the result is a side-effect: Printing messages
#' @export
cat2 <- function(...) {
  
  if (getOption("MMVshiny.verbose", FALSE)) {
    cat(...)
  } 
  
}


