library(tidyverse)

###
air_url <- "https://api.airtable.com/v0"

air_api_key <- function() {
  key <- "keyK0cTBq3xvhmWSr"
  if(key == "") {
    stop("AIRTABLE_API_KEY environment variable is empty. See ?airtabler for help.")
  }
  key
}


#' Get a list of records or retreive a single
#'
#' You can retrieve records in an order of a view by providing the name or ID of
#' the view in the view query parameter. The results will include only records
#' visible in the order they are displayed.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id (optional) Use record ID argument to retrieve an existing
#'   record details
#' @param limit (optional) A limit on the number of records to be returned.
#'   Limit can range between 1 and 100.
#' @param offset (optional) Page offset returned by the previous list-records
#'   call. Note that this is represented by a record ID, not a numerical offset.
#' @param view (optional) The name or ID of the view
#' @param sortField (optional) The field name to use for sorting
#' @param sortDirection (optional) "asc" or "desc". The sort order in which the
#'   records will be returned. Defaults to asc.
#' @param combined_result If TRUE (default) all data is returned in the same data
#'   frame. If FALSE table fields are returned in separate \code{fields} element.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#' @export
air_get <- function(base, table_name, record_id = NULL,
                    limit = NULL,
                    offset = NULL,
                    view = NULL,
                    sortField = NULL,
                    sortDirection = NULL,
                    combined_result = TRUE) {
  
  search_path <- table_name
  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)
  
  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "limit", "offset", "view", "sortField", "sortDirection")]
  param_list <- param_list[!sapply(param_list, is.null)]
  request_url <- httr::modify_url(request_url, query = param_list)
  # call service:
  res <- httr::GET(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key()))
  )
  air_validate(res)      # throws exception (stop) if error
  ret <- air_parse(res)  # returns R object
  if(combined_result && is.null(record_id)) {
    # combine ID, Fields and CreatedTime in the same data frame:
    ret <-
      cbind(
        id = ret$id, ret$fields, createdTime = ret$createdTime,
        stringsAsFactors =FALSE
      )
  }
  ret
}

list_params <- function(x, par_name) {
  # converts a list of sublists to a list
  # Example:
  # sort = list(
  #   list(field="Avg Review", direction = "desc"),
  #   list(field="Price/night", direction = "asc"))
  # list_params(sort, "sort")
  #
  # fields = list("Country", "Name")
  # list_params(fields, "field")
  if(!is.list(x)) {
    stop(par_name, " parameter must be a list")
  }
  names(x) <- sprintf("%s[%s]", par_name, 0:(length(x)-1))
  if(is.list(x[[1]])) {
    x <- unlist(x, recursive = FALSE)
    names(x) <- gsub("\\.", "[", names(x))
    names(x) <- gsub("$", "]", names(x))
  }
  x
}


#' Select
#'
#' Select records from table
#'
#' @section View:
#'   You can retrieve records in an order of a view by providing the name or ID of
#'   the view in the view query parameter. The results will include only records
#'   visible in the order they are displayed.
#'
#' @section Filter by formula:
#'   The formula will be evaluated for each record, and if the result is not 0,
#'   false, "", NaN, [], or #Error! the record will be included in the response.
#'   If combined with view, only records in that view which satisfy the formula
#'   will be returned. For example, to only include records where Country isn't
#'   empty, pass in: NOT({Country} = '')
#'
#' @section Sorting:
#'   Each sort object must have a field key specifying the name of
#'   the field to sort on, and an optional direction key that is either "asc" or
#'   "desc". The default direction is "asc".
#'   For example, to sort records by Country, pass in: \code{list(field =
#'   "Country", direction = "desc")}
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id (optional) Use record ID argument to retrieve an existing
#'   record details
#' @param fields (optional) Only data for fields whose names are in this vector
#'   will be included in the records. If you don't need every field, you can use
#'   this parameter to reduce the amount of data transferred.
#' @param filterByFormula (optional) A formula used to filter records.
#' @param maxRecord (optional) The maximum total number of records that will be
#'   returned.
#' @param sort A list of sort objects that specifies how the records will be
#'   ordered.
#' @param view (optional) The name or ID of the view defined in the table
#' @param pageSize (optional) The number of records returned in each request.
#'   Must be less than or equal to 100. Default is 100.
#' @param offset (optional) To fetch the next page of records set this argument
#'   with a value of offset element from previous response
#' @param combined_result If TRUE (default) all data is returned in the same
#'   data frame. If FALSE table fields are returned in separate \code{fields}
#'   element.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#' @export
air_select <- function(
  base, table_name, record_id = NULL,
  fields = NULL,
  filterByFormula = NULL,
  maxRecord = NULL,
  sort = NULL,
  view = NULL,
  pageSize = NULL,
  offset = NULL,
  combined_result = TRUE) {
  
  search_path <- table_name
  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)
  
  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "filterByFormula", "maxRecord", "pageSize", "offset", "view")]
  param_list <- param_list[!sapply(param_list, is.null)]
  if(!is.null(sort)) {
    param_list <- c(param_list, list_params(x = sort, par_name = "sort"))
  }
  if(!is.null(fields)) {
    param_list <- c(param_list, list_params(x = fields, par_name = "fields"))
  }
  
  request_url <- httr::modify_url(url = request_url, query = param_list)
  # call service:
  res <- httr::GET(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key()))
  )
  air_validate(res)      # throws exception (stop) if error
  ret <- air_parse(res)  # returns R object
  offset <- attr(ret, "offset")
  if(combined_result && is.null(record_id) && length(ret) > 0) {
    # combine ID, Fields and CreatedTime in the same data frame:
    ret <-
      cbind(
        id = ret$id, ret$fields, createdTime = ret$createdTime,
        stringsAsFactors =FALSE
      )
    attr(ret, "offset") <- offset
  }
  ret
}

#' Get offset
#'
#' Returns airtable offset id from previous select
#'
#' @param x Last result
#' @return Airtable offset id
#' @export
get_offset <- function(x) {
  attr(x, "offset")
}

air_validate <- function(res) {
  
  if(!inherits(res, "response")) {
    stop("Not a HTTP response object")
  }
  if(res$status_code >= 400) {
    err_message <- httr::content(res, as = "text")
    if(err_message != "") {
      error_info <- jsonlite::fromJSON(err_message)
    } else {
      error_info <- ""
    }
    if(is.list(error_info)) {
      err_message <- paste(error_info$error, collapse = "\n")
    } else {
      err_message <- error_info
    }
    stop("HTTP error: ", res$status_code, "\n", err_message, call. = FALSE)
  }
  if(substr(res$headers$`content-type`, 1, 16) != "application/json") {
    stop("Returned message is not a json", call. = FALSE)
  }
}

air_parse <- function(res) {
  res_obj <- jsonlite::fromJSON(httr::content(res, as = "text"))
  if(!is.null(res_obj$records)) {
    res <- res_obj$records
    if(!is.null(res_obj$offset)) {
      attr(res, "offset") <- res_obj$offset
    }
  } else {
    res <- res_obj
  }
  res
}




#' Insert a new record
#'
#' Creates a new record and returns the created record object if the call
#' succeeded, including a record ID which will uniquely identify the record
#' within the table.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_data Named list of values. You can include all, some, or none
#'   of the field values
#' @export
air_insert <- function(base, table_name, record_data) {
  
  if( inherits(record_data, "data.frame")) {
    return( air_insert_data_frame(base, table_name, record_data))
  }
  
  record_data <- air_prepare_record(as.list(record_data))
  json_record_data <- jsonlite::toJSON(list(fields = record_data))
  
  request_url <- sprintf("%s/%s/%s", air_url, base, table_name)
  
  # call service:
  res <- httr::POST(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"
    ),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

air_insert_data_frame <- function(base, table_name, records) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <-
      as.list(records[i,])
    air_insert(base = base, table_name = table_name, record_data = record_data)
  })
}

air_update_data_frame <- function(base, table_name, record_ids, records) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <-
      unlist(as.list(records[i,]), recursive = FALSE)
    air_update(base = base,
               table_name = table_name,
               record_id = ifelse(is.null(record_ids), record_data$id, record_ids[i]),
               record_data = record_data)
  })
}


#' @rdname air_insert
#' @param x Object to be marked as a multiple value field
#' @export
multiple <- function(x) {
  class(x) <- c("air_multiple", class(x))
  x
}

air_prepare_record <- function(x) {
  # unbox all 1-sized elements which are not marked with "air_multiple" class:
  
  for(i in seq_along(x)) {
    if(inherits(x[[i]], "air_multiple")) {
      class(x[[i]]) <- class(x[[i]])[-1]
    } else {
      if(is.list(x[[i]])) {
        x[[i]] <- unlist(x[[i]])
      } else if(length(x[[i]]) == 1) {
        x[[i]] <- jsonlite::unbox(x[[i]])
      }
    }
  }
  x
}

#' Delete a record
#'
#' Deletes a record and returns the deleted record id if the call
#' succeeded.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id Id of the record to be deleted
#' @export
air_delete <- function(base, table_name, record_id) {
  
  if(length(record_id) > 1) {
    return(air_delete_vec(base, table_name, record_id))
  }
  
  request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)
  
  # call service:
  res <- httr::DELETE(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key())
    )
  )
  
  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

air_delete_vec <- Vectorize(air_delete, vectorize.args = "record_id", SIMPLIFY = FALSE)





#' Update a record
#'
#' Updates a new record. Any fields that are not included will not be updated.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id An id of the record
#' @param record_data Named list of values. You can include all, some, or none
#'   of the field values
#' @export
air_update <- function(base, table_name, record_id, record_data) {
  
  if(inherits(record_data, "data.frame")) {
    return(air_update_data_frame(base, table_name, record_id, record_data))
  }
  record_data <- air_prepare_record(record_data)
  json_record_data <- jsonlite::toJSON(list(fields = record_data))
  
  request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)
  
  # call service:
  res <- httr::PATCH(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"
    ),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

#' Get airtable base object
#'
#' Creates airtable object with tables and functions
#'
#' @param base Airtable base
#' @param tables Table names in the airtable base (character vector)
#' @return Airtable base object with elements named by table names.
#'   Each element contains functions
#'   \item{get}{returns table records, see \code{\link{air_get}} for details}
#'   \item{insert}{insert table record, see \code{\link{air_insert}} for details}
#'   \item{update}{updates table record, see \code{\link{air_update}} for details}
#'   \item{delete}{deletes table record, see \code{\link{air_delete}} for details}
#' @export
#' @examples
#' \dontrun{
#' TravelBucketList <-
#'   airtable(
#'     base = "the_base_id",
#'     tables = c("Destinations", "Hotels", "Travel Partners")
#'   )
#'   hotels <- TravelBucketList$Hotels$get()
#'   destinations <- TravelBucketList$Destinations$get()
#' }
airtable <- function(base, tables) {
  res <- lapply(tables, function(x) air_table_funs(base, x))
  names(res) <- tables
  class(res) <- "airtable.base"
  attr(res, "base") <- base
  res
}

#' @keywords internal
#' @export
print.airtable.base <- function(x, ...) {
  cat("Airtable base object", attr(x, "base"), "\n")
  cat("Tables:", names(x), sep = "\n  ")
}



air_table_funs <- function(base, table_name) {
  
  res_list <- list()
  res_list[["select"]] <-
    function(
      record_id = NULL,
      fields = NULL,
      filterByFormula = NULL,
      maxRecord = NULL,
      sort = NULL,
      view = NULL,
      pageSize = NULL,
      offset = NULL,
      combined_result = TRUE
    ){
      air_select(base, table_name, record_id,
                 fields, filterByFormula, maxRecord, sort, view,
                 pageSize, offset, combined_result)
    }
  res_list[["select_all"]] <- function(
    record_id = NULL,
    fields = NULL,
    filterByFormula = NULL,
    maxRecord = NULL,
    sort = NULL,
    view = NULL,
    pageSize = NULL
  ){
    ret_all <- list()
    ret_offset = NULL
    while({
      ret <- air_select(
        base, table_name, record_id,
        fields, filterByFormula, maxRecord, sort, view,
        pageSize, ret_offset, combined_result = TRUE)
      ret_offset <- get_offset(ret)
      ret_all <- c(ret_all, list(ret))
      !is.null(ret_offset)
    }) {}
    if(length(ret_all) == 0) { return(list())}
    
    .bind_df(ret_all)
  }
  
  res_list[["get"]] <-
    function(
      record_id = NULL,
      limit = NULL, offset = NULL,
      view = NULL,
      sortField = NULL, sortDirection = NULL,
      combined_result = TRUE
    ){
      air_get(base, table_name, record_id, limit, offset, view, sortField, sortDirection, combined_result)
    }
  res_list[["insert"]] <-
    function(record_data) {
      air_insert(base, table_name, record_data)
    }
  res_list[["update"]] <-
    function(record_id, record_data) {
      air_update(base, table_name, record_id, record_data)
    }
  res_list[["delete"]] <-
    function(record_id) {
      air_delete(base, table_name, record_id)
    }
  
  res_list
}

.bind_df <- function(x) {
  # x = list of data frames
  if(length(unique(lengths(x))) != 1) {
    
    # add missing columns
    col_names <- unique(unlist(lapply(x, names)))
    col_missing <- lapply(x, function(x) setdiff(col_names, names(x)))
    
    x <- lapply(seq_along(x), function(i) {
      ret <- x[[i]]
      for(col in col_missing[[i]]) {
        ret[[col]] <- NA
      }
      ret
    })
  }
  
  do.call(rbind, x)
  
}

###

basekey_issues = "appw9tHhofqYj2ywD"
tablekey_issues = "tbl106h1MXjyBVrN9"
tablekey_interpretations = "tblv0mPHXsqYBK2Ws"



issue_airtable <- airtable(
  base = basekey_issues,
  tables = "tbl106h1MXjyBVrN9"
)

issue_table <- issue_airtable$tbl106h1MXjyBVrN9$select() %>%
  mutate(across(where(is.list), ~ sapply(., unlist)))# %>% 
  #dplyr::select(-`Other Agencies`)

data1 <- issue_table
offset <- get_offset(issue_table)

while(!is.null(offset)) {
  
  issue_table <- issue_airtable$tbl106h1MXjyBVrN9$select(offset = offset) %>%
    mutate(across(where(is.list), ~ sapply(., unlist)))
  
  
  
  data1 <- 
    tryCatch({
      data1 %>% 
        rbind(issue_table)
    }, error = function(e) {
      print(e)
      return(data1)
    }
    )
  offset <- get_offset(issue_table)
}

issue_table <-
  data1 %>% 
  filter(Reviewed == TRUE) %>% 
  dplyr::select(Issue,`Interpretation (from Interpretation)`,createdTime) %>% 
  rename(
    "Interpretation" = "Interpretation (from Interpretation)"
  ) %>% 
  mutate(Created = as.Date(createdTime)) %>% 
  dplyr::select(-createdTime)

######

interpretation_airtable <- airtable(
  base = basekey_issues,
  tables = "tblv0mPHXsqYBK2Ws"
)

interpretation_table <- interpretation_airtable$tblv0mPHXsqYBK2Ws$select() %>%
  mutate(across(where(is.list), ~ sapply(., unlist)))


data <- interpretation_table
offset <- get_offset(interpretation_table)

while(!is.null(offset)) {
  
  interpretation_table <- interpretation_airtable$tblv0mPHXsqYBK2Ws$select(offset = offset) %>%
    mutate(across(where(is.list), ~ sapply(., unlist)))
  
  data <- 
    tryCatch({
      data %>% 
        dplyr::bind_rows(interpretation_table)
    }, error = function(e) {
      print(e)
      return(data)
    }
    )
  offset <- get_offset(interpretation_table)
}


interpretation_table <-
  data %>% 
  dplyr::select(`Interpretation`,`State Law Relatedness` = `State Grab Text`,Source,Topic,`Last Modified (Official)`,`Page Number (If Applicable)`) %>%
  mutate(
    `Last Modified` = as.Date(`Last Modified (Official)`)
  ) %>% 
  dplyr::select(-`Last Modified (Official)`) %>% 
  arrange(desc(Interpretation))

interp_mod <-
  interpretation_table %>% 
  mutate(
    `Topic 1` = "",
    `Topic 2` = "",
    `Topic 3` = "",
    `Topic 4` = "",
    `Topic 5` = ""
  )

for(i in 1:nrow(interpretation_table)){
  
  #interpretation_table$topic_mod[i] <- paste(unlist(interpretation_table$Topic[i]), collapse=', ')
  
  temp <- interpretation_table$Topic[i][[1]]
  
  
  if(!is.null(temp)){
    for(j in 1:length(temp)){
      if(j == 1){
        interp_mod$`Topic 1`[i] <- temp[j]
      }else if(j == 2){
        interp_mod$`Topic 2`[i] <- temp[j]
      }else if(j == 3){
        interp_mod$`Topic 3`[i] <- temp[j]
      }else if(j == 4){
        interp_mod$`Topic 4`[i] <- temp[j]
      }else if(j == 5){
        interp_mod$`Topic 5`[i] <- temp[j]
      }
      
    }
  }
}



for(i in 1:nrow(interp_mod)){
  if(is.null(unlist(interp_mod$`State Law Relatedness`[i]))){
    interp_mod$`State Law Relatedness`[i] <- c("","")
  }
}

for(i in 1:nrow(issue_table)){
  if(is.null(unlist(issue_table$Interpretation[i]))){
    issue_table$Interpretation[i] <- c("","")
  }
}


code_display_data <- interp_mod %>%
  dplyr::select(Interpretation,`Last Modified`,`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`) %>% 
  filter(substr(Interpretation,1,4) %in% c("312.","310.","6585","311.")) %>%
  na.omit() %>% 
  rename("Code" = "Interpretation") %>% 
  mutate(
    Code = case_when(
      substr(Code,1,8) == "65852.2("  ~ paste0(substr(Code,1,7),"0",substr(Code,8,nchar(Code))),
      grepl("310.|311.|312.",Code) ~ paste0("CPC ",Code),
      TRUE ~ Code
    )
  ) #%>% 
# arrange(Code) %>% 
# mutate(
#   `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0("Keywords: ",`Topic 1`),""),
#   `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
#   `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
#   `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
#   `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
#   Code = paste0(Code,"<br><br><i><small>Last Modified: ",`Last Modified`,"<br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
# ) %>%
# dplyr::select(Code)

interpretation_display_data <- interp_mod %>%
  dplyr::select(Interpretation,`Last Modified`,`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,`Page Number (If Applicable)`) %>% 
  filter(!substr(Interpretation,1,4) %in% c("312.","310.","6585","311.")) %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A Page ",`Page Number (If Applicable)`,substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  ) %>% 
  dplyr::select(-`Page Number (If Applicable)`)

issue_display_data <- issue_table %>%
  dplyr::select(Issue) 

all_hold_issue <- NULL

for(i in 1:nrow(issue_table)){
  hold_explode <- NULL
  if(length(issue_table$Interpretation[i][[1]]) > 1){
    for(j in 1:length(issue_table$Interpretation[i][[1]])){
      temp <- data.frame(
        "Issue" = issue_table$Issue[i],
        "Interpretation" = issue_table$Interpretation[i][[1]][j],
        "Created" = issue_table$Created[i])
      
      hold_explode <-
        hold_explode %>% 
        rbind(temp)
    }
  }else{
    hold_explode <- data.frame(
      "Issue" = issue_table$Issue[i],
      "Interpretation" = issue_table$Interpretation[i][[1]][1],
      "Created" = issue_table$Created[i])
    
  }
  
  all_hold_issue <-
    all_hold_issue %>% 
    rbind(hold_explode)
}

all_hold_issue <-
  all_hold_issue %>% 
  left_join(
    interpretation_table %>% 
      dplyr::select(Interpretation,`Page Number (If Applicable)`),
    by = "Interpretation"
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A Page ",`Page Number (If Applicable)`,substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      substr(Interpretation,1,8) == "65852.2("  ~ paste0(substr(Interpretation,1,7),"0",substr(Interpretation,8,nchar(Interpretation))),
      grepl("310.|311.|312.",Interpretation) ~ paste0("CPC ",Interpretation),
      TRUE ~ Interpretation
    )
  ) %>% 
  dplyr::select(-Created) %>% 
  left_join(
    interpretation_display_data %>% 
      rbind(code_display_data %>% rename("Interpretation" = "Code")),
    by = "Interpretation"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" <br>Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"<br><br><i><small> Last Modified: ",`Last Modified`,`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i><br><br>")
  ) %>%
  dplyr::select(Issue,Interpretation)


####new stuff

# paren_list <- c("65852\\.2\\(a\\)\\(1\\)","65852\\.2\\(a\\)\\(1\\)\\(A\\)","65852\\.2\\(a\\)\\(1\\)\\(B\\)\\(i\\)","65852\\.2\\(a\\)\\(1\\)\\(B\\)\\(ii\\)","65852\\.2\\(a\\)\\(1\\)\\(C\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)",
#                   "65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(i\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(ii\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(iii\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(iv\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(v\\)",
#                   "65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(vi\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(vii\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(viii\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(ix\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(x\\)\\(I\\)",
#                   "65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(x\\)\\(II\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(x\\)\\(III\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(xi\\)","65852\\.2\\(a\\)\\(1\\)\\(D\\)\\(xii\\)","65852\\.2\\(a\\)\\(2\\)",
#                   "65852\\.2\\(a\\)\\(3\\)","65852\\.2\\(a\\)\\(4\\)","65852\\.2\\(a\\)\\(5\\)","65852\\.2\\(a\\)\\(6\\)\\(A\\)","65852\\.2\\(a\\)\\(6\\)\\(B\\)","65852\\.2\\(a\\)\\(7\\)","65852\\.2\\(a\\)\\(8\\)",
#                   "65852\\.2\\(b\\)","65852\\.2\\(c\\)\\(1\\)","65852\\.2\\(c\\)\\(2\\)","65852\\.2\\(c\\)\\(2\\)\\(A\\)","65852\\.2\\(c\\)\\(2\\)\\(B\\)","65852\\.2\\(c\\)\\(2\\)\\(B\\)\\(i\\)","65852\\.2\\(c\\)\\(2\\)\\(B\\)\\(ii\\)",
#                   "65852\\.2\\(c\\)\\(2\\)\\(C\\)","65852\\.2\\(d\\)","65852\\.2\\(d\\)\\(1\\)","65852\\.2\\(d\\)\\(2\\)","65852\\.2\\(d\\)\\(3\\)","65852\\.2\\(d\\)\\(4\\)","65852\\.2\\(d\\)\\(5\\)","65852\\.2\\(e\\)\\(1\\)\\(A\\)",
#                   "65852\\.2\\(e\\)\\(1\\)\\(A\\)\\(i\\)","65852\\.2\\(e\\)\\(1\\)\\(A\\)\\(ii\\)","65852\\.2\\(e\\)\\(1\\)\\(A\\)\\(iii\\)","65852\\.2\\(e\\)\\(1\\)\\(A\\)\\(iv\\)","65852\\.2\\(e\\)\\(1\\)\\(B\\)","65852\\.2\\(e\\)\\(1\\)\\(B\\)\\(i\\)",
#                   "65852\\.2\\(e\\)\\(1\\)\\(B\\)\\(ii\\)","65852\\.2\\(e\\)\\(1\\)\\(C\\)\\(i\\)","65852\\.2\\(e\\)\\(1\\)\\(C\\)\\(ii\\)","65852\\.2\\(e\\)\\(1\\)\\(D\\)","65852\\.2\\(e\\)\\(2\\)","65852\\.2\\(e\\)\\(3\\)","65852\\.2\\(e\\)\\(4\\)",
#                   "65852\\.2\\(e\\)\\(5\\)","65852\\.2\\(e\\)\\(6\\)","65852\\.2\\(e\\)\\(7\\)","65852\\.2\\(f\\)\\(1\\)","65852\\.2\\(f\\)\\(2\\)","65852\\.2\\(f\\)\\(3\\)\\(A\\)","65852\\.2\\(f\\)\\(3\\)\\(B\\)", "65852\\.2\\(f\\)\\(4\\)",
#                   "65852\\.2\\(f\\)\\(5\\)","65852\\.2\\(g\\)","65852\\.2\\(h\\)\\(1\\)","65852\\.2\\(h\\)\\(2\\)\\(A\\)","65852\\.2\\(h\\)\\(2\\)\\(B\\)","65852\\.2\\(h\\)\\(2\\)\\(A\\)\\(i\\)","65852\\.2\\(h\\)\\(2\\)\\(B\\)\\(ii\\)","65852\\.2\\(h\\)\\(3\\)\\(A\\)",
#                   "65852\\.2\\(h\\)\\(3\\)\\(B\\)","65852\\.2\\(i\\)","65852\\.2\\(j\\)","65852\\.2\\(j\\)\\(1\\)","65852\\.2\\(j\\)\\(1\\)\\(A\\)","65852\\.2\\(j\\)\\(1\\)\\(B\\)","65852\\.2\\(j\\)\\(2\\)","65852\\.2\\(j\\)\\(3\\)","65852\\.2\\(j\\)\\(1\\)",
#                   "65852\\.2\\(j\\)\\(4\\)","65852\\.2\\(j\\)\\(5\\)","65852\\.2\\(j\\)\\(6\\)","65852\\.2\\(j\\)\\(7\\)","65852\\.2\\(j\\)\\(8\\)","65852\\.2\\(j\\)\\(9\\)","65852\\.2\\(j\\)\\(10\\)","65852\\.2\\(k\\)","65852\\.2\\(l\\)",
#                   "65852\\.2\\(m\\)","65852\\.2\\(n\\)","65852\\.2\\(n\\)\\(1\\)","65852\\.2\\(n\\)\\(2\\)","65852\\.2\\(o\\)","65852\\.22\\(a\\)","65852\\.22\\(a\\)\\(1\\)","65852\\.22\\(a\\)\\(2\\)","65852\\.22\\(a\\)\\(3\\)","65852\\.22\\(a\\)\\(4\\)",
#                   "65852\\.22\\(a\\)\\(5\\)","65852\\.22\\(a\\)\\(6\\)","65852\\.22\\(a\\)\\(6\\)\\(A\\)","65852\\.22\\(a\\)\\(6\\)\\(B\\)","65852\\.22\\(b\\)\\(1\\)","65852\\.22\\(b\\)\\(2\\)","65852\\.22\\(c\\)","65852\\.22\\(d\\)","65852\\.22\\(e\\)",
#                   "65852\\.22\\(f\\)","65852\\.22\\(g\\)","65852\\.22\\(h\\)","65852\\.22\\(h\\)\\(1\\)","65852\\.22\\(h\\)\\(2\\)","65852\\.2\\.a\\.1","65852\\.2\\.a\\.1\\.A","65852\\.2\\.a\\.1\\.B\\.i","65852\\.2\\.a\\.1\\.B\\.ii","65852\\.2\\.a\\.1\\.C","65852\\.2\\.a\\.1\\.D",
#                   "65852\\.2\\.a\\.1\\.D\\.i","65852\\.2\\.a\\.1\\.D\\.ii","65852\\.2\\.a\\.1\\.D\\.iii","65852\\.2\\.a\\.1\\.D\\.iv","65852\\.2\\.a\\.1\\.D\\.v",
#                   "65852\\.2\\.a\\.1\\.D\\.vi","65852\\.2\\.a\\.1\\.D\\.vii","65852\\.2\\.a\\.1\\.D\\.viii","65852\\.2\\.a\\.1\\.D\\.ix","65852\\.2\\.a\\.1\\.D\\.x\\.I",
#                   "65852\\.2\\.a\\.1\\.D\\.x\\.II","65852\\.2\\.a\\.1\\.D\\.x\\.III","65852\\.2\\.a\\.1\\.D\\.xi","65852\\.2\\.a\\.1\\.D\\.xii","65852\\.2\\.a\\.2",
#                   "65852\\.2\\.a\\.3","65852\\.2\\.a\\.4","65852\\.2\\.a\\.5","65852\\.2\\.a\\.6\\.A","65852\\.2\\.a\\.6\\.B","65852\\.2\\.a\\.7","65852\\.2\\.a\\.8",
#                   "65852\\.2\\.b","65852\\.2\\.c\\.1","65852\\.2\\.c\\.2","65852\\.2\\.c\\.2\\.A","65852\\.2\\.c\\.2\\.B","65852\\.2\\.c\\.2\\.B\\.i","65852\\.2\\.c\\.2\\.B\\.ii",
#                   "65852\\.2\\.c\\.2\\.C","65852\\.2\\.d","65852\\.2\\.d\\.1","65852\\.2\\.d\\.2","65852\\.2\\.d\\.3","65852\\.2\\.d\\.4","65852\\.2\\.d\\.5","65852\\.2\\.e\\.1\\.A",
#                   "65852\\.2\\.e\\.1\\.A\\.i","65852\\.2\\.e\\.1\\.A\\.ii","65852\\.2\\.e\\.1\\.A\\.iii","65852\\.2\\.e\\.1\\.A\\.iv","65852\\.2\\.e\\.1\\.B","65852\\.2\\.e\\.1\\.B\\.i",
#                   "65852\\.2\\.e\\.1\\.B\\.ii","65852\\.2\\.e\\.1\\.C\\.i","65852\\.2\\.e\\.1\\.C\\.ii","65852\\.2\\.e\\.1\\.D","65852\\.2\\.e\\.2","65852\\.2\\.e\\.3","65852\\.2\\.e\\.4",
#                   "65852\\.2\\.e\\.5","65852\\.2\\.e\\.6","65852\\.2\\.e\\.7","65852\\.2\\.f\\.1","65852\\.2\\.f\\.2","65852\\.2\\.f\\.3\\.A","65852\\.2\\.f\\.3\\.B", "65852\\.2\\.f\\.4",
#                   "65852\\.2\\.f\\.5","65852\\.2\\.g","65852\\.2\\.h\\.1","65852\\.2\\.h\\.2\\.A","65852\\.2\\.h\\.2\\.B","65852\\.2\\.h\\.2\\.A\\.i","65852\\.2\\.h\\.2\\.B\\.ii","65852\\.2\\.h\\.3\\.A",
#                   "65852\\.2\\.h\\.3\\.B","65852\\.2\\.i","65852\\.2\\.j","65852\\.2\\.j\\.1","65852\\.2\\.j\\.1\\.A","65852\\.2\\.j\\.1\\.B","65852\\.2\\.j\\.2","65852\\.2\\.j\\.3","65852\\.2\\.j\\.1",
#                   "65852\\.2\\.j\\.4","65852\\.2\\.j\\.5","65852\\.2\\.j\\.6","65852\\.2\\.j\\.7","65852\\.2\\.j\\.8","65852\\.2\\.j\\.9","65852\\.2\\.j\\.10","65852\\.2\\.k","65852\\.2\\.l",
#                   "65852\\.2\\.m","65852\\.2\\.n","65852\\.2\\.n\\.1","65852\\.2\\.n\\.2","65852\\.2\\.o","65852\\.22\\.a","65852\\.22\\.a\\.1","65852\\.22\\.a\\.2","65852\\.22\\.a\\.3","65852\\.22\\.a\\.4",
#                   "65852\\.22\\.a\\.5","65852\\.22\\.a\\.6","65852\\.22\\.a\\.6\\.A","65852\\.22\\.a\\.6\\.B","65852\\.22\\.b\\.1","65852\\.22\\.b\\.2","65852\\.22\\.c","65852\\.22\\.d","65852\\.22\\.e",
#                   "65852\\.22\\.f","65852\\.22\\.g","65852\\.22\\.h","65852\\.22\\.h\\.1","65852\\.22\\.h\\.2")
# 
# 
# interpretation_table$new_code <- ""
# 
# for(i in 1:nrow(interpretation_table)){
#   
#   if(substr(interpretation_table$Interpretation[i],1,5) != "65852"){
#     
#     code_list <- NULL
#     for(j in 1:length(paren_list)){
#       if(grepl(paren_list[j],interpretation_table$Interpretation[i])){
#         
#         temp <-
#           interpretation_table %>% 
#           filter(grepl(paren_list[j],Interpretation)) %>% 
#           filter(substr(Interpretation,1,5)=="65852")
#         
#         print(j)
#         
#         code_list <- c(code_list,temp$Interpretation[1])
#         
#       } 
#     }
#     if(length(code_list)>0){
#       interpretation_table$new_code[i] <- code_list
#     }
#   }
#   
# }

###

sub_e_list <- c(
  "65852.2(e)(1): Notwithstanding subdivisions (a) to (d), inclusive, a local agency shall ministerially approve an application for a building permit within a residential or mixed-use zone to create any of the following:",
  "65852.2(e)(1)(A): One accessory dwelling unit and one junior accessory dwelling unit per lot with a proposed or existing single-family dwelling if all of the following apply:",
  "65852.2(e)(1)(A)(i): The accessory dwelling unit or junior accessory dwelling unit is within the proposed space of a single-family dwelling or existing space of a single-family dwelling or accessory structure and may include an expansion of not more than 150 square feet beyond the same physical dimensions as the existing accessory structure. An expansion beyond the physical dimensions of the existing accessory structure shall be limited to accommodating ingress and egress.",
  "65852.2(e)(1)(A)(ii): The space has exterior access from the proposed or existing single-family dwelling.",
  "65852.2(e)(1)(A)(iii): The side and rear setbacks are sufficient for fire and safety.",
  "65852.2(e)(1)(A)(iv): The junior accessory dwelling unit complies with the requirements of Section 65852.22.",
  "65852.2(e)(1)(B): One detached, new construction, accessory dwelling unit that does not exceed four-foot side and rear yard setbacks for a lot with a proposed or existing single-family dwelling. The accessory dwelling unit may be combined with a junior accessory dwelling unit described in subparagraph (A). A local agency may impose the following conditions on the accessory dwelling unit:",
  "65852.2(e)(1)(B)(i): A total floor area limitation of not more than 800 square feet.",
  "65852.2(e)(1)(B)(ii): A height limitation of 16 feet.",
  "65852.2(e)(1)(C)(i): Multiple accessory dwelling units within the portions of existing multifamily dwelling structures that are not used as livable space, including, but not limited to, storage rooms, boiler rooms, passageways, attics, basements, or garages, if each unit complies with state building standards for dwellings.",
  "65852.2(e)(1)(C)(ii): A local agency shall allow at least one accessory dwelling unit within an existing multifamily dwelling and shall allow up to 25 percent of the existing multifamily dwelling units.",
  "65852.2(e)(1)(D): Not more than two accessory dwelling units that are located on a lot that has an existing multifamily dwelling, but are detached from that multifamily dwelling and are subject to a height limit of 16 feet and four-foot rear yard and side setbacks.",
  "65852.2(e)(2): A local agency shall not require, as a condition for ministerial approval of a permit application for the creation of an accessory dwelling unit or a junior accessory dwelling unit, the correction of nonconforming zoning conditions.",
  "65852.2(e)(3): The installation of fire sprinklers shall not be required in an accessory dwelling unit if sprinklers are not required for the primary residence.",
  "65852.2(e)(4): A local agency may require owner occupancy for either the primary dwelling or the accessory dwelling unit on a single-family lot, subject to the requirements of paragraph (6) of subdivision (a).",
  "65852.2(e)(5): A local agency shall require that a rental of the accessory dwelling unit created pursuant to this subdivision be for a term longer than 30 days.",
  "65852.2(e)(6): A local agency may require, as part of the application for a permit to create an accessory dwelling unit connected to an onsite wastewater treatment system, a percolation test completed within the last five years, or, if the percolation test has been recertified, within the last 10 years.",
  "65852.2(e)(7): Notwithstanding subdivision (c) and paragraph (1) a local agency that has adopted an ordinance by July 1, 2018, providing for the approval of accessory dwelling units in multifamily dwelling structures shall ministerially consider a permit application to construct an accessory dwelling unit that is described in paragraph (1), and may impose standards including, but not limited to, design, development, and historic standards on said accessory dwelling units. These standards shall not include requirements on minimum lot size."
  )


interpretation_table2 <-
  interpretation_table %>%
  mutate(
    `new code` = case_when(
      grepl("65852\\.2\\(e\\) |65852\\.2\\(e\\)\\.|65852\\.2\\(e\\)\\,|65852\\.2\\.e |65852\\.2\\.e\\.|65852\\.2\\.e\\,|subdivision \\(e\\)",Interpretation) ~ list(sub_e_list),
      TRUE ~ list("NULL")
    )
  )


adu_code_only<-
  interpretation_table %>% 
  filter(Source %in% c("65852.2","65852.22"))

for(i in 1:nrow(interpretation_table)){
  code_list <- interpretation_table$`State Law Relatedness`[i][[1]]
  full_code_list <- NULL
  if(!is.null(interpretation_table$`State Law Relatedness`[i][[1]])){
    for(j in 1:length(code_list)){
      snippet <- sub(":.*", "", code_list[j])
      patterns <- gsub("([()])","\\\\\\1", snippet)
      
      adu_specific_code <-
        adu_code_only %>% 
        filter(grepl(patterns,Interpretation)) %>% 
        dplyr::select(Interpretation) %>% 
        arrange(Interpretation) %>% 
        as.list() %>% 
        .[[1]]
      
      full_code_list <- c(full_code_list,adu_specific_code)
    }
    
    full_code_df <-
      full_code_list %>% 
      as.data.frame() %>% 
      filter(!duplicated(.)) %>% 
      as.list() %>% 
      .[[1]]
    
    interpretation_table$`State Law Relatedness`[i] <- list(full_code_df)
  }
}


####

all_hold_interp <- NULL

for(i in 1:nrow(interpretation_table)){
  hold_explode <- NULL
  if(length(interpretation_table$`State Law Relatedness`[i][[1]]) > 1){
    for(j in 1:length(interpretation_table$`State Law Relatedness`[i][[1]])){
      temp <- data.frame(
        "Interpretation" = interpretation_table$Interpretation[i],
        "Code" = interpretation_table$`State Law Relatedness`[i][[1]][j],
        "Last Modified" = interpretation_table$`Last Modified`[i])
      
      hold_explode <-
        hold_explode %>% 
        rbind(temp)
    }
  }else if(length(interpretation_table$`State Law Relatedness`[i][[1]]) == 1){
    hold_explode <- data.frame(
      "Interpretation" = interpretation_table$Interpretation[i],
      "Code" = interpretation_table$`State Law Relatedness`[i][[1]][1],
      "Last Modified" = interpretation_table$`Last Modified`[i])
    
  }
  
  all_hold_interp <-
    all_hold_interp %>% 
    rbind(hold_explode)
}

all_hold_interp <-
  all_hold_interp %>% 
  left_join(
    interpretation_table %>% 
      dplyr::select(Interpretation,`Page Number (If Applicable)`),
    by = "Interpretation"
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml",Interpretation) ~ "hcdqa30: <br/><strong>Question:</strong> Are solar panels required for new construction ADUs?  <br/><br/><strong>Response:</strong> Yes, newly constructed ADUs are subject to the Energy Code requirement to provide solar panels if the unit(s) is a newly constructed, non-manufactured, detached ADU. Per the California Energy Commission (CEC), the panels can be installed on the ADU or on the primary dwelling unit. ADUs that are constructed within existing space, or as an addition to existing homes, including detached additions where an existing detached building is converted from non-residential to residential space, are not subject to the Energy Code requirement to provide solar panels. 18 Please refer to the CEC on this matter. For more information, see the CEC’s website www.energy.ca.gov. You may email your questions to: title24@energy.ca.gov, or contact the Energy Standards Hotline at 800- 772-3300. CEC memos can also be found on HCD’s website at <a href = https://www.hcd.ca.gov/policyresearch/AccessoryDwellingUnits.shtml.>this link</a>",
      grepl("emailqa13",Interpretation) ~ 
        "emailqa13: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa48",Interpretation) ~ "emailqa48: <br/><strong>Question:</strong> I am writing to inquire where in state law the provisions that direct local municipalities to allow a process for legalizing non-permitted ADUs are codified. Could you please direct me to the proper citation?  <br/><br/><strong>Response:</strong> The section in reference can be found here: <a href = https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?lawCode=HSC&sectionNum=17980.12>Link</a>",
      grepl("emailqa02",Interpretation) ~ "emailqa02:<br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter. Can you help me or give me the contact of someone who can give me info and guidance on this?  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      grepl("emailqa62",Interpretation) ~ "emailqa62: <br/><strong>Question:</strong> I have a duplex and am hoping to convert part and add-on to create an ADU. After completing the Planning Application and Plans I received feedback that seemed to be not in the spirit of some of the new laws. The city wants me to pay for a new ADA ramps on the corner outside of my lot. The city also wants me to replace my existing two water meters and replace them with 1 multi-family meter.  <br/><br/><strong>Response:</strong> The creation of an ADU does not require the applicant to carry out public improvements as this is beyond what is required for the creation of ADUs. ADUs shall also not be considered to exceed the allowable density for the lot upon which it is located, which means, it would not apply to density in the general plan and is considered an accessory use. ADUs converted from existing space and JADUs shall not be considered by a local agency, special district or water corporation to be a new residential use for the purposes of calculating connection fees or capacity charges for utilities, unless constructed with a new single-family dwelling. For additional information, please see HCD’s ADU handbook here: <a href = https://www.hcd.ca.gov/policy-research/docs/ADU_December_2020_Handbook.pdf>Link</a>. If this continues, please feel free to forward any relevant interactions to adu@hcd.ca.gov and we will discuss this with our team.",
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Interpretation = case_when(
      grepl("hcdqa",Interpretation) ~ paste0("HCD Q&A Page ",`Page Number (If Applicable)`,substr(Interpretation,8,nchar(Interpretation))),
      grepl("emailqa", Interpretation) ~ paste0("Email Q&A #",as.character(substr(Interpretation,8,9)),substr(Interpretation,10,nchar(Interpretation))),
      TRUE ~ Interpretation
    )
  ) %>% 
  mutate(
    Code = case_when(
      substr(Code,1,8) == "65852.2("  ~ paste0(substr(Code,1,7),"0",substr(Code,8,nchar(Code))),
      grepl("310.|311.|312.",Code) ~ paste0("CPC ",Code),
      TRUE ~ Code
    )
  ) %>% 
  left_join(
    interpretation_display_data,
    by = "Interpretation"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"<br><br><i><small> Last Modified: ",`Last Modified`,"<br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation,Code) %>% 
  left_join(
    code_display_data,
    by = "Code"
  ) %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Code = paste0(Code,"<br><br><i><small> Last Modified: ",`Last Modified`,"<br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation,Code)

interpretation_display_data <-
  interpretation_display_data %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Interpretation = paste0(Interpretation ,"<br><br><i><small> Last Modified: ",`Last Modified`,"<br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Interpretation) %>% 
  arrange(Interpretation)

code_display_data <-
  code_display_data %>% 
  mutate(
    `Topic 1` = ifelse(nchar(`Topic 1`) > 0,paste0(" Keywords: ",`Topic 1`),""),
    `Topic 2` = ifelse(nchar(`Topic 2`) > 0,paste0("; ",`Topic 2`),""),
    `Topic 3` = ifelse(nchar(`Topic 3`) > 0,paste0("; ",`Topic 3`),""),
    `Topic 4` = ifelse(nchar(`Topic 4`) > 0,paste0("; ",`Topic 4`),""),
    `Topic 5` = ifelse(nchar(`Topic 5`) > 0,paste0("; ",`Topic 5`),""),
    Code = paste0(Code,"<br><br><i><small> Last Modified: ",`Last Modified`,"<br>",`Topic 1`,`Topic 2`,`Topic 3`,`Topic 4`,`Topic 5`,"</small></i>")
  ) %>%
  dplyr::select(Code) %>% 
  arrange(Code)

interpretation_display_data$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = interpretation_display_data$Interpretation)

interpretation_display_data$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = interpretation_display_data$Interpretation)

interpretation_display_data$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = interpretation_display_data$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = all_hold_issue$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = all_hold_issue$Interpretation)

all_hold_issue$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = all_hold_issue$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "\n", replacement = "<br/>", x = all_hold_interp$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "Question:", replacement = "<strong>Question:</strong>", x = all_hold_interp$Interpretation)

all_hold_interp$Interpretation <- gsub(pattern = "Response:", replacement = "<strong>Response:</strong>", x = all_hold_interp$Interpretation)


save(
  issue_table,
  interpretation_table,
  interp_mod,
  all_hold_interp,
  all_hold_issue,
  interpretation_display_data,
  issue_display_data,
  code_display_data,
  file = "casita/airtable_data.rda")


