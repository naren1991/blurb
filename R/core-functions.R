##################################################################################################
# Title: Create Short Time Based Data Stories
# Author: Naren Srinivasan
# Created on: January 10, 2019
# Description: Core functions for creating data blurbs
##################################################################################################

##### DataPipe class ########
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
subset_data <- function(df_name, filter_fn = default_filter_fn, ...){
  df <- eval(parse(text = df_name))
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
      .Object@dataset_schema = subset_data(df_name = data_source_name, indices = 0)
      }else if(.Object@type == "csv"){
        csv_data <- read.csv(data_source_name)
        .Object@dataset_schema = subset_data(df_name = 'csv_data', indices = 0)
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

##### get_data ########
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
    df <- subset_data(object@data_source_name, filter_fn = filter_fn)
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

##### get_data_subset ########
#' @name get_data_subset
#' @rdname get_data_subset
#' @title Get Data Subset
#' @family Package core functions
#' @export
setGeneric(
  name = "get_data_subset",
  def = function(object, filter_fn, ...)
  {
    standardGeneric("get_data_subset")
  }
)

.get_data_subset = function(object, filter_fn = get_all_data_filter_fn, ...)
{
  tryCatch({
    df <- subset_data(object@data_source_name, filter_fn = filter_fn, ...)
    return(df)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname get_data_subset
setMethod(
  f = "get_data_subset",
  signature = "DataPipe",
  definition = .get_data_subset
)

##### show_data_schema ########
#' @name show_data_schema
#' @rdname show_data_schema
#' @title Show data schema
#' @family Package core functions
#' @export
setGeneric(
  name = "show_data_schema",
  def = function(object)
  {
    standardGeneric("show_data_schema")
  }
)

.show_data_schema = function(object)
{
  tryCatch({
    return(object@dataset_schema)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname show_data_schema
setMethod(
  f = "show_data_schema",
  signature = "DataPipe",
  definition = .show_data_schema
)

##### Viz class ########
#' @name Viz-class
#' @rdname Viz-class
#' @title Viz class
#' @family Package core functions
#' @exportClass Viz
#' @export Viz
Viz <- setClass(
  "Viz",
  slots = c(
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

##### set_template_fn ########
#' @name set_template_fn
#' @rdname set_template_fn
#' @title Set template function
#' @family Package core functions
#' @export
setGeneric(
  name = "set_template_fn",
  def = function(object, template_fn)
  {
    standardGeneric("set_template_fn")
  }
)

.set_template_fn = function(object, template_fn)
{
  tryCatch({
    object@template_generator = template_fn
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname set_template_fn
setMethod(
  f = "set_template_fn",
  signature = "DataPipe",
  definition = .set_template_fn
)



##### Scene class ########
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
    frame_prep_info = "list",
    frame_results = "list",
    annotations = "tbl"
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
  definition = function(.Object, viz, data_pipe, num_frames, timeline_var, 
                        timeline_var_type = 'time-series', y_var,
                        x_var)
  {
    tryCatch({
      .Object@visualization <- viz
      .Object@data_pipe <- data_pipe
      
      frame_creation_info <- dplyr::tibble(
        parameter = character(),
        value = character()
      )
      
      frame_creation_info %>% dplyr::add_row(parameter = "num_frames", value = num_frames) %>%
        dplyr::add_row(parameter = "timeline_var", value = timeline_var) %>%
        dplyr::add_row(parameter = "timeline_var_type", value = timeline_var_type)  %>%
        dplyr::add_row(parameter = "y_var", value = y_var) %>%
        dplyr::add_row(parameter = "x_var", value = x_var) -> frame_creation_info

      .Object@frame_creation_info <- frame_creation_info
      
      annotations <- dplyr::tibble(
        parameter = character(),
        value = character()
      )
      
      annotations %>% dplyr::add_row(parameter = 'title', value = "<Title>") %>%
        dplyr::add_row(parameter = 'subtitle', value = "<Subtitle>") %>%
        dplyr::add_row(parameter = 'note', value = "<Note>") -> annotations
      
      .Object@annotations <- annotations
      
      
      return(.Object)
    }, error = function(e) {
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    }, warning = function(w) {
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)

##### prep_frames ########
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

extrapolate_indices <- function(timeline_vals, num_frames){
  unique_vals <- timeline_vals %>% unique
  num_unique_timeline_val <- unique_vals %>% length
  skip_length <- ceiling(num_unique_timeline_val/ num_frames)
  
  commuted_seq_indices <- seq(to = num_unique_timeline_val, by = skip_length,
                                    length.out = num_frames)
  return(unique_vals[commuted_seq_indices])
}

.prep_frames = function(object)
{
  tryCatch({
    num_frames <- object@frame_creation_info %>% dplyr::filter(parameter == 'num_frames') %>% 
                                                                  dplyr::pull(value) %>% as.numeric
    timeline_var <- object@frame_creation_info %>% dplyr::filter(parameter == 'timeline_var') %>% 
                                                                 dplyr::pull(value)
    timeline_var_type <- object@frame_creation_info %>% dplyr::filter(parameter == 'timeline_var_type') %>% 
                                                                  dplyr::pull(value)
    
    timeline_vals <- object@data_pipe %>% get_data %>%
      dplyr::select(!!rlang::sym(timeline_var)) %>%
      dplyr::pull(!!rlang::sym(timeline_var)) 
    
    commuted_timeline_vals <- extrapolate_indices(timeline_vals, num_frames)
    object@frame_prep_info$commuted_timeline_vals <- commuted_timeline_vals
    
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

##### generate_frames ########
#' @name generate_frames
#' @rdname generate_frames
#' @title Generate frames
#' @family Package core functions
#' @export
setGeneric(
  name = "generate_frames",
  def = function(object, chunking_fn = default_chunking_fn, ...)
  {
    standardGeneric("generate_frames")
  }
)

chunking_fn_numeric_c_avg <- function(d, y_var, x_var, timeline_var, commuted_timeline_vals){
  
  
  d %>% dplyr::select(!!rlang::sym(y_var),
                      !!rlang::sym(x_var),
                      !!rlang::sym(timeline_var)) -> d
  
  # Sapply has faster performance
  d$grouping <- sapply(1:nrow(d), FUN = function(t, vals, timeline_var){
    r <- d[t,timeline_var]
    ind <- which(r <= vals)
    if(length(ind) == 0){
      bucket <- d[nrow(d), timeline_var]
    }else{
      bucket <- vals[ind[1]]  
    }
    
    return(bucket)
  }, commuted_timeline_vals, timeline_var)
  
  d %>% dplyr::mutate(!! y_var := dplyr::cummean(!!rlang::sym(y_var))) %>% 
    dplyr::group_by(grouping) %>% dplyr::summarise(!! y_var := last(!!rlang::sym(y_var))) -> aggr_d
  
  aggr_d %>% dplyr::mutate(!! x_var := grouping) %>% dplyr::select(!!rlang::sym(y_var), 
                                                                   !!rlang::sym(x_var)) -> aggr_d
  return(aggr_d)
}

default_chunking_fn <- function(d, y_var, x_var, timeline_var_type, timeline_var,
                                commuted_timeline_vals, ...){
  if(timeline_var_type == 'time-series'){
    
  }else if(timeline_var_type == 'numeric'){
    aggr_d <- chunking_fn_numeric_c_avg(d, y_var, x_var, timeline_var,
                                        commuted_timeline_vals)
  }else if(timeline_var_type == 'category'){
    #aggr_d <- chunking_fn_category_c_avg(d, y_var, x_var, timeline_var,
      #                                   commuted_timeline_vals)
  }
  return(aggr_d)
}

get_scene_data <- function(df, timeline_var, 
                           current_timeline_val){
  timeline_vals <- df %>% dplyr::pull(!! timeline_var)
  cut_index <- grep(current_timeline_val, timeline_vals, value = F)
  subset_vals <- timeline_vals[1:cut_index]
  df %>% dplyr::filter(!!rlang::sym(timeline_var) %in% subset_vals) -> df
  return(df)
}


.generate_frames = function(object, chunking_fn = default_chunking_fn, ...)
{
  tryCatch({
    y_var <- object@frame_creation_info %>% dplyr::filter(parameter == 'y_var') %>% 
      dplyr::pull(value)
    x_var <- object@frame_creation_info %>% dplyr::filter(parameter == 'x_var') %>% 
      dplyr::pull(value)
    timeline_var <- object@frame_creation_info %>% dplyr::filter(parameter == 'timeline_var') %>% 
      dplyr::pull(value)
    timeline_var_type <- object@frame_creation_info %>% dplyr::filter(parameter == 'timeline_var_type') %>% 
      dplyr::pull(value)
    commuted_timeline_vals <- object@frame_prep_info$commuted_timeline_val
    data_pipe <- object@data_pipe
    
 
    
    object@frame_results$aggr_data <-  purrr::map(commuted_timeline_vals, .f = function(v){
    d <- data_pipe %>% get_data_subset(filter_fn = get_scene_data, timeline_var = timeline_var,
                                       current_timeline_val = v)
    d %>% chunking_fn(y_var = y_var, x_var = x_var,
                      timeline_var_type = timeline_var_type,
                      timeline_var = timeline_var,
                      commuted_timeline_vals = commuted_timeline_vals) -> aggr_d
    return(aggr_d)
  })
    
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname generate_frames
setMethod(
  f = "generate_frames",
  signature = "Scene",
  definition = .generate_frames
)

##### render_frames ########
#' @name render_frames
#' @rdname render_frames
#' @title Render frames
#' @family Package core functions
#' @export
setGeneric(
  name = "render_frames",
  def = function(object, ...)
  {
    standardGeneric("render_frames")
  }
)

.render_frames = function(object, ...)
{
  tryCatch({
    viz <- object@visualization
    aggr_data <- object@frame_results$aggr_data
    
    object@frame_results$rendered_frames <- purrr::map(aggr_data, .f = function(d, ...){
      frame <- viz@template_generator(d = d, ...)
      return(frame)
    }, ...)
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname render_frames
setMethod(
  f = "render_frames",
  signature = "Scene",
  definition = .render_frames
)


##### Blurb class ########
#' @name Blurb-class
#' @rdname Blurb-class
#' @title Blurb class
#' @family Package core functions
#' @exportClass Blurb
#' @export Blurb

Blurb <- setClass(
  "Blurb",
  slots = c(
    scenes = "list",
    annotations = "tbl"
  )
)


#' Blurb constructor
#' @docType methods
#' @rdname initialize-methods
#' @title This is the constructor for the \link{Blurb} class
#' @family Package core functions
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "Blurb",
  definition = function(.Object)
  {
    tryCatch({
      annotations <- dplyr::tibble(
        parameter = character(),
        value = character()
      )
      
      annotations %>% dplyr::add_row(parameter = 'title', value = "<Title>") %>%
        dplyr::add_row(parameter = 'subtitle', value = "<Subtitle>") %>%
        dplyr::add_row(parameter = 'note', value = "<Note>") -> annotations
      
      .Object@annotations <- annotations
      
      
      return(.Object)
    }, error = function(e) {
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    }, warning = function(w) {
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)

##### add_scenes ########
#' @name add_scenes
#' @rdname add_scenes
#' @title Add scenes
#' @family Package core functions
#' @export
setGeneric(
  name = "add_scenes",
  def = function(object, scenes_list)
  {
    standardGeneric("add_scenes")
  }
)

.add_scenes = function(object, scenes_list)
{
  tryCatch({
    object@scenes <- scenes_list
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname add_scenes
setMethod(
  f = "add_scenes",
  signature = "Blurb",
  definition = .add_scenes
)

.create_annotation <- function(title, subtitle, note,
                              filename, width, height,
                              text_color = 'black',
                              background = 'white',
                              font_family = 'monospace'){
  tryCatch({
    header <- magick::image_graph(width, height, res = res)
    print(plot(0,type='n',axes=FALSE,ann=FALSE))
    header <- magick::image_draw(header)
    text(width/2, height/5, title, family = font_familt, cex = 2, srt = 0)
    text(width/2, height/4, subtitle, family = font_family, cex = 1, srt = 0)
    text(width/2, height/3, note, family = font_family, cex = 0.8, srt = 0)
    dev.off()
    
    magick::image_write(image = header, 
                        path = paste0('./blurb_output/', filename))
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

##### make_blurb ########
#' @name make_blurb
#' @rdname make_blurb
#' @title Create a blurb
#' @family Package core functions
#' @export
setGeneric(
  name = "make_blurb",
  def = function(object, type = 'gif')
  {
    standardGeneric("make_blurb")
  }
)
.create_gif <- function(object, width = 600, height = 340, res = 100, name){
  if(!dir.exists('./blurb_output')){
    dir.create('./blurb_output')
  }
  
  main_title <- object@annotations %>% dplyr::filter(parameter == 'title') %>% 
    dplyr::pull(value)
  main_subtitle <- object@annotations %>% dplyr::filter(parameter == 'subtitle') %>% 
    dplyr::pull(value)
  main_note <- object@annotations %>% dplyr::filter(parameter == 'note') %>% 
    dplyr::pull(value)
  main_filnemame <- '0_main_header.png'

  .create_annotation(title = main_title, subtitle = main_subtitle, note = main_note,
                     filename = main_filename, width = width, height = height)

  purrr::imap(object@scenes, function(sc, i){
    
    title <- sc@annotations %>% dplyr::filter(parameter == 'title') %>% 
      dplyr::pull(value)
    subtitle <- sc@annotations %>% dplyr::filter(parameter == 'subtitle') %>% 
      dplyr::pull(value)
    note <- sc@annotations %>% dplyr::filter(parameter == 'note') %>% 
      dplyr::pull(value)
    filename <- paste0('scene_', i,'_0_header.png')
    
    .create_annotation(title = title, subtitle = subtitle, note = note,
                       filename = filename, width = width, height = height)
    
    purrr::imap(sc@frame_results$rendered_frames, function(fr, j){
      img <- magick::image_graph(width, height, res = res)
      print(fr)
      dev.off()
      magick::image_write(image = img, 
                          path = paste0('./blurb_output/scene_', i, '_frame_', j, '.png'))
    })
  })
 
  dev.off()
  system(paste0("convert -delay 80 ./blurb_output/*.png ./", name, ".gif"))
  unlink('./blurb_output', recursive = T)
}

.make_blurb = function(object, type, name = 'blurb_output', ...)
{
  tryCatch({
    if(type == 'gif'){
      create_gif(object, name, ...)
    }else{
      ## Exception
    }
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname make_blurb
setMethod(
  f = "make_blurb",
  signature = "Blurb",
  definition = .make_blurb
)



