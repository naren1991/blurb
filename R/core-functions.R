##################################################################################################
# Title: Create Short Time Based Data Stories
# Author: Naren Srinivasan
# Created on: January 10, 2019
# Description: Core functions for creating data blurbs
##################################################################################################

#' @name DataPipe-class
#' @rdname DataPipe-class
#' @title DataPipe class
#' @family Package core functions
#' @exportClass DataPipe
#' @export DataPipe

DataPipe <- setClass(
  "DataPipe",
  slots = c(
    type = "character",
    data_source_name = "character",
    dataset_schema = "data.frame"
  )
)

default_filter_fn <- function(df, indices = 1:10){
  return(df %>% dplyr::slice(indices))
}
get_data_subset <- function(df_name, filter_fn = default_filter_fn, ...){
  df = eval(parse(text = df_name))
  df %>% filter_fn(...) -> filtered_df
  return(filtered_df)
}

get_all_data_filter_fn <-function(df){
  return(df)
}

#' TODO: Provide sorting of data frame based on timeline var

#' DataPipe constructor
#' @docType methods
#' @rdname initialize-methods
#' @title This is the constructor for the \link{DataPipe} class
#' @family Package core functions
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "DataPipe",
  definition = function(.Object, type = "in-memory", data_source_name)
  {
    tryCatch({
      .Object@type <- type
      .Object@data_source_name <- data_source_name
      
      if(type == "in-memory"){
      .Object@dataset_schema = get_data_subset(df_name = data_source_name, indices = 0)
      }else if(.Object@type == "csv"){
        csv_data <- read.csv(data_source_name)
        .Object@dataset_schema = get_data_subset(df_name = 'csv_data', indices = 0)
      }
      return(.Object)
    }, error = function(e) {
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    }, warning = function(w) {
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)

#' @name get_data
#' @rdname get_data
#' @title Get Data
#' @family Package core functions
#' @export
setGeneric(
  name = "get_data",
  def = function(object)
  {
    standardGeneric("get_data")
  }
)

.get_data = function(object)
{
  tryCatch({
    filter_fn <- get_all_data_filter_fn
    df <- get_data_subset(object@data_source_name, filter_fn = filter_fn)
    return(df)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname get_data
setMethod(
  f = "get_data",
  signature = "DataPipe",
  definition = .get_data
)

#' @name Viz-class
#' @rdname Viz-class
#' @title Viz class
#' @family Package core functions
#' @exportClass Viz
#' @export Viz
Viz <- setClass(
  "Viz",
  slots = c(
    name = "character",
    template_generator = "function")
)

default_template_fn <- function(d, y_var, x_var){
  p <- ggplot2::ggplot(data = d, aes(y = !!rlang::sym(y_var),
                                     x = !!rlang::sym(x_var))) +
    geom_line()
  return(p)
}

#' Viz constructor
#' @docType methods
#' @rdname initialize-methods
#' @title This is the constructor for the \link{Viz} class
#' @family Package core functions
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "Viz",
  definition = function(.Object, template_fn = default_template_fn)
  {
    tryCatch({
      .Object@template_generator <- template_fn
      return(.Object)
    }, error = function(e) {
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    }, warning = function(w) {
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)


#' @name Scene-class
#' @rdname Scene-class
#' @title Scene class
#' @family Package core functions
#' @exportClass Scene
#' @export Scene

Scene <- setClass(
  "Scene",
  slots = c(
    data_pipe = "DataPipe",
    visualization = "Viz",
    frame_creation_info = "tbl",
    frame_prep_info = "list"
  )
)


#' Scene constructor
#' @docType methods
#' @rdname initialize-methods
#' @title This is the constructor for the \link{Scene} class
#' @family Package core functions
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "Scene",
  definition = function(.Object, viz, data_pipe, num_frames, timeline_var)
  {
    tryCatch({
      .Object@visualization <- viz
      .Object@data_pipe <- data_pipe
      
      frame_creation_info <- dplyr::tibble(
        parameter = character(),
        value = character()
      )
      
      frame_creation_info %>% dplyr::add_row(parameter = "num_frames", value = num_frames) %>%
        dplyr::add_row(parameter = "timeline_var", value = timeline_var) -> frame_creation_info

      .Object@frame_creation_info <- frame_creation_info
      
      
      return(.Object)
    }, error = function(e) {
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    }, warning = function(w) {
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)

#' @name prep_frames
#' @rdname prep_frames
#' @title Prep frames
#' @family Package core functions
#' @export
setGeneric(
  name = "prep_frames",
  def = function(object)
  {
    standardGeneric("prep_frames")
  }
)

extrapolate_indices <- function(vector, skip_length){
  unique_vals <- unique(vector)
  commuted_seq_indices <- seq(from = 1, to = length(unique_vals), by = skip_length)
  return(unique_vals[commuted_seq_indices])
}

.prep_frames = function(object)
{
  tryCatch({
    num_frames <- object@frame_creation_info %>% dplyr::filter(parameter == 'num_frames') %>% 
                                                                  dplyr::pull(value) %>% as.numeric
    timeline_var <- object@frame_creation_info %>% dplyr::filter(parameter == 'timeline_var') %>% 
                                                                 dplyr::pull(value)
    
    timeline_vals <- object@data_pipe %>% get_data %>%
      dplyr::select(!!rlang::sym(timeline_var)) %>%
      dplyr::pull(!!rlang::sym(timeline_var)) 
    num_unique_timeline_val <- timeline_vals %>% unique %>% length
                                              
                                                              
    skip_length = ceiling(num_unique_timeline_val/ num_frames)
    
    commuted_vector <- extrapolate_indices(timeline_vals, skip_length)
    
    object@frame_prep_info$commuted_timeline_val <- commuted_vector
    
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname prep_frames
setMethod(
  f = "prep_frames",
  signature = "Scene",
  definition = .prep_frames
)

#' @name generate_frames
#' @rdname generate_frames
#' @title Generate frames
#' @family Package core functions
#' @export
setGeneric(
  name = "generate_frames",
  def = function(object, chunking_type = 'cumulative', chunking_fun = mean, ...)
  {
    standardGeneric("generate_frames")
  }
)

# Cumulative average
default_chunking_fun <- function(d, y_var, x_var, timeline_var,
                                 commuted_timeline_vals, current_timeline_val){
  
  
  d %>% dplyr::select(!!rlang::sym(y_var),
                      !!rlang::sym(x_var),
                      !!rlang::sym(timeline_var)) -> d
  
  cut_index <- grep(current_timeline_val, commuted_timeline_vals, value = F)
  subset_vals <- commuted_timeline_vals[1:cut_index]

  
  d %>% dplyr::filter(!!rlang::sym(timeline_var) %in% subset_vals) -> d
  
  d %>% dplyr::group_by(!!rlang::sym(x_var)) %>% 
        dplyr::summarise(!!rlang::sym(y_var) = mean(!!rlang::sym(y_var))) -> aggr_d
  
  return(aggr_d)
  
}

generate_frame <- function(d, viz){
  
}

.generate_frames = function(object, chunking_fun = default_chunking_fun, ...)
{
  tryCatch({
    
    
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname prep_frames
setMethod(
  f = "generate_frames",
  signature = "Scene",
  definition = .generate_frames
)


