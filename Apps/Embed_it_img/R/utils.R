plot_emb <- function(embedding, ...) {
  UseMethod("plot_emb")
}

# function for the user, to get the data in a format that the app and the other functions will use
# the data has to be a .rds dataframe
# embs_cols: a vector containing names or indices of the embedding columns
# vars_cols: a vector containing names or indices of the variable columns
# vars_cat_cols: a vector containing names or indices of the categorical variable columns
# url-col: the name or index of the column containing image urls
# id_col: the name or index of the id column, if one exists
# img: if the dataset is an image dataset

buildInput <- function(data, embs_cols = NULL, vars_cols = NULL, vars_cat_cols = NULL, tf_col = NULL, id_col = NULL,
                       img = TRUE, url_col = NULL) {
  
  #if it is a dataframe:
  checkmate::assertDataFrame(data, min.rows = 1, min.cols = 3) # 2 embedding dims the data itself
  
  df <- data
  
  # if colnames have been used
  if (!is.numeric(embs_cols)) {
    embs_cols <- which(colnames(df) %in% embs_cols)
  }
  
  # rename the embs_cols
  # do not rename, if already aptly named
  uncat_cols <- !startsWith(colnames(df)[embs_cols], "embs_")
  colnames(df)[embs_cols][uncat_cols] <- paste("embs", colnames(df)[embs_cols][uncat_cols], sep = "_")
  
  
  # if names have been used
  if (!is.numeric(vars_cols)) {
    vars_cols <- which(colnames(df) %in% vars_cols)
  }
  # rename the vars_cols
  uncat_cols <- !startsWith(colnames(df)[vars_cols], "vars_")
  colnames(df)[vars_cols][uncat_cols] <- paste("vars", colnames(df)[vars_cols][uncat_cols], sep = "_")
  
  # rename the tf_col / url_col
  if (!img) {
    # if names have been used
    if (!is.numeric(tf_col)) {
      tf_col <- which(colnames(df) %in% tf_col)
    }
    
    
    if (!is.null(tf_col) & !is_tf(df[ , tf_col])) {
      stop("The tf column must be an object of class tf")
    }    
    
    if (!is.null(tf_col) && colnames(df)[tf_col] != "tf_fun") {
      colnames(df)[tf_col] <- "tf_fun"
    }
  } else {
    
    # if names have been used
    if (!is.numeric(url_col)) {
      url_col <- which(colnames(df) %in% url_col)
    }
    
    uncat_cols <- !startsWith(colnames(df)[url_col], "url_")
    colnames(df)[url_col] <- paste("url", colnames(df)[url_col][uncat_cols], sep = "_")
  }
  
  
  # rename or create the id column
  
  if (!is.null(id_col)) {
    colnames(df)[id_col] <- "id"
  } else {
    df$id <- seq(nrow(df))
  }
  
  # if there is just one column
  if (length(vars_cat_cols) == 1) {
    
    if (!startsWith(colnames(df)[vars_cat_cols], "vars_")) {
      colnames(df)[vars_cat_cols] <- paste("vars", colnames(df)[vars_cat_cols], sep = "_")
    }
    
  } else if (nrow(select(df[ , vars_cat_cols], starts_with("vars"))) == 0) { # factorize the categorical cols (and add vars_ if it has not been done yet)
    colnames(df)[vars_cat_cols] <- paste("vars", colnames(df)[vars_cat_cols], sep = "_")
  }
  
  for (col in vars_cat_cols) {
    df[ , col] <- as.factor(df[ , col])
  }
  
  df
 
}


plot_emb.default <- function(pts, color = NULL, shape = NULL, size = 1, legend_title = "", ...) {
  
  dat <- data.frame(dim1 = pts[, 1],
                    dim2 = pts[, 2],
                    color = rep(1, nrow(pts))) 
  alpha <- 0.6
  
  if (!is.null(color)) {
    dat$color <- color
    alpha <- 0.7
  }
  
  if (!is.null(shape)) {
    dat$shape <- shape
  }
  
  p <- ggplot(dat) +
    geom_point(aes(x = dim1,
                   y = dim2,
                   colour = color,
                   shape = shape
    ),
    size = size, alpha = alpha) + 
    theme(legend.position = "None") +
    labs(shape = legend_title)
  
  p
}

### App functions --------------------------------------


plot_emb.matrix <- function(embedding, dim1 = 1, dim2 = 2, color = NULL, labels_off = TRUE, labels = NULL, size = 1,  ...) {
  
  pts <- embedding[, c(dim1, dim2), drop = FALSE]
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (!labels_off) p <- if (is.null(labels)) {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)), size = label_size)
  } else {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = labels), size = size)
  }
  p +
    theme(
      panel.background = element_rect(fill='white'),
      plot.background = element_rect(fill='white'), # , color="orange"
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggtitle("") +
    labs(x = colnames(pts)[1], y = colnames(pts)[2])
}


scale_viridis_stretched <-  function(...) ggplot2::scale_colour_viridis_c(..., values = seq(0,1, l = 10)^2)
# scale_viridis_stretched()

######################################
# ggimage

plot_emb_img <- function(embedding, ...) {
  UseMethod("plot_emb_img")
}

library(ggimage)
# this package should make it possible

plot_emb_img.default <- function(pts, color = NULL, size = 1, imgs, ...) {
  
  dat <- data.frame(dim1 = pts[, 1],
                    dim2 = pts[, 2],
                    color = seq_len(nrow(pts)),
                    image = imgs)
  
  if (!is.null(color)) dat$color <- color
  
 # p <- ggplot(dat, aes(x, y)) +
  p <- ggplot(dat) +
    geom_image(aes(image = image,
                   x = dim1, y = dim2),
                   size = size) +
    theme(legend.position = "Non") +
    ggtitle(label = "2d-embedding")
  p
}

plot_emb_img.matrix <- function(embedding, dim1 = 1, dim2 = 2, color = NULL, labels_off = TRUE, labels = NULL, size = 1, ...) {

  pts <- embedding[, c(dim1, dim2), drop = FALSE] # drop = FALSE -> it stays a matrix, even if it could be vector, maybe??
  p <- plot_emb_img.default(pts, color = color, labels = labels, size = size, ...)
  
  if (!labels_off) p <- if (is.null(labels)) {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label =seq_len(nrow(pts))), size = label_size)
  } else {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = labels), size = size)
  }
  p
}


##### preprocessing the data #################

# return: three seperate dataframes, containing:
# The embeddings(or: one for each embedding?)
# The Variables
# The data

# function for App
preprocessInputData <- function(data) {

  #if it is a rds:
  assertDataFrame(data, min.rows = 1, min.cols = 3) # 2 embedding dims, and the url 
  
  df <- data
  
  
  # extract all colums starting with "Embs_" plus ID (delete except ID)
  df_embs <- df %>% dplyr::select(starts_with("embs_"), "id")
  # delete the "embs_"
  colnames(df_embs) <- sub("embs_", "", colnames(df_embs))
  
  # extract all columns staring with "Vars_" plus ID (Delete except ID)
   df_vars <- df %>% dplyr::select(starts_with("vars_"), "id")
   # delete the "vars_"
   colnames(df_vars) <- sub("vars_", "", colnames(df_vars))
   
  # Extract the column "tf" plus ID (delete except ID)
     df_data <- df %>% dplyr::select(starts_with(c("tf_", "url_")), "id")
     colnames(df_data) <- sub("tf_", "", colnames(df_data))
     colnames(df_data) <- sub("url_", "", colnames(df_data))

  list(embs = df_embs, vars = df_vars, data = df_data, df = merge(df_embs, merge(df_vars, df_data))) 
}

# helper function
getMethodNames <- function(input) {
  
  methods_pure <- unique(gsub("_\\d+", "", colnames(input$embs)))
  methods_pure <- methods_pure[-which(methods_pure == "id")]
  
  method_names <- list()
  
  for (method in seq_along(methods_pure)) {
  
    method_names <- c(method_names, list(colnames(input$embs)[startsWith(colnames(input$embs), methods_pure[[method]])]))
  }
  
  
  names(method_names) <- methods_pure
  method_names

}

##### server processes #########################

get_brushed_ids <- function(pts = input$plot1_brush, embs_cols, input_df, var2, scores) {
  req(nrow(brushedPoints(data.frame(dim1 = input_df$embs_cols[[1]],
                                    dim2 = input_df$embs_cols[[2]]), pts)) >= 1) # no error message if it doesn't exist
  
  dat <- data.frame(dim1 = input_df$MDS_1,
                    dim2 = input_df$MDS_2,
                    label = var2,
                    scores = scores) #input_df2$scores_trans
  
  brushedPoints(dat, pts)
  
}

############################
# hover tooltip function?
hover_function <- function(data, hover, id_hover, color = FALSE, shape = FALSE, label = FALSE){
  #point <- nearPoints(mtcars, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  point <- id_hover
  if (nrow(point) == 0) return(NULL)
  
 #
  left_px <- hover$coords_css$x
  top_px <- hover$coords_css$y
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(229, 238, 233, 0.75) !important; ", #absolute
                  "left:", left_px - 0, "px; top:", top_px - 0, "px;")
  
  #
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Id: </b>", point$id, "<br/>",
                  "<b> dim 1: </b>", round(point$dim1, digits = 2), "<br/>",
                  "<b> dim 2: </b>", round(point$dim2, digits = 2), "<br/>",
                  #"<b> color: </b>", if (!isFALSE(as.logical(color))) {paste0(color, " ")},  point$color, "<br/>",
                  "<b> color: </b>",  point$color, if (!isFALSE(as.logical(color))) {paste0("<i> (", color, ")</i>")}, "<br/>",
                 # "<b> label: </b>",if (!isFALSE(as.logical(label))) {paste0(label, " ")}, point$label, "<br/>",
                  "<b> label: </b>",  point$label, if (!isFALSE(as.logical(label))) {paste0("<i> (", label, ")</i>")}, "<br/>",
                 # "<b> shape: </b>", if (!isFALSE(as.logical(shape))) {paste0(shape, " ")}, point$shape, "<br/>",
                 "<b> shape: </b>",  point$shape, if (!isFALSE(as.logical(shape))) {paste0("<i> (", shape, ")</i>")}, "<br/>"
                  )))
  )
}

drawPlot <- function(input, embs, dim1, dim2, shapes = FALSE, labels = FALSE, col = NULL, imgs_on = FALSE,
                     title = NULL, ids = NULL, high_color = "hotpink", mat = FALSE, size = 4, img = FALSE) {
  
  renderPlot({
    
    drawPlot_single(input = input, embs = embs, dim1 = dim1, dim2 = dim2, shapes = shapes, labels = labels, 
                    col = col, imgs_on = imgs_on,
                    title = title, ids = ids, high_color = high_color, mat = mat, size = size, img = img)
  })
}

drawPlot_single <- function(input, embs, dim1, dim2, shapes = FALSE, labels = FALSE, col = FALSE, imgs_on = FALSE,
                            title = NULL, img = FALSE, ids = NULL, high_color = "hotpink", mat = FALSE, size = 4) {
  
  # 
  if (is.na(as.logical(shapes))) {
    shape <- input$vars[ , shapes]
    if (!mat) {legend <- theme(legend.position = "right")} else {legend <- theme(legend.position = "none")} 
    legend_title = shapes
    
  } else {
    shape <- NULL
    legend <- NULL
    legend_title <- ""
  }
  
  if (img) {
    legend <- theme(legend.position = "right")
  }
  
  if (is.na(as.logical(col))) { 
    color <- input$vars[ , col]
  } else {
    color <- NULL
    colorscale <- scale_color_gradient(na.value = "black", guide = "none")
  }

  colx <- dim1
  coly <- dim2
  
  # size of capellini plots
  width <- max(input$df[[dim1]]) / 3
  height <- max(input$df[[dim2]]) / 3
  
  
  if (imgs_on) {
    
    
    if (is.factor(color)) {
      
      colorscale <- scale_colour_viridis_d(col, option = "inferno", guide = "none") #inputData1()$vars[ ,input$colors_var],
      if (img) colorscale <- scale_colour_viridis_d(col, option = "inferno")
      
    } else if (is.na(as.logical(col))) {
      colorscale <- scale_viridis_stretched(col, guide = "none")
      if (img) colorscale <- scale_viridis_stretched(col)
    }
    
    if (!img) {
      
      fun_var <- colnames(input$df %>% select_if(is_tf))[[1]]
      
      p <- plot.funs.cap(input$df, colx = colx, coly = coly, fun_var = fun_var,
                         labels_var = isolate(color), title = title, colorscale = colorscale, 
                         width = width, height = height) # width / height relative to scale
    } else {
      
      url_sh <- "data/shapes/" # TODO delete this
      #
      if (colnames(input$vars)[1] != "shape") {
        url_sh <- ""
      }
      
      p <- plot_emb_img(embs, color = col, labels_off = TRUE,
                        labels = NULL,
                        size = 0.05,
                        imgs = paste0(url_sh, input$data[[1]])) + scale_viridis_stretched()+ #"/",
        ggtitle(dim1) + xlab(paste("dim ", 1)) + ylab(paste("dim ", 2)) +
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='#e5eee9'), #
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        )
    }
    
    
    
  } else if (!is.factor(color)) { 
    #  
    
    if (is.na(as.logical(col))) {
      colorscale <- scale_viridis_stretched(guide = "none")
      if (img) colorscale <- scale_viridis_stretched(col)
    }
    
    p <- plot_emb(as.matrix(embs), color = color, shape = shape,
                  labels_off = !is.na(as.logical(labels)), #input$labels
                  labels = input$vars[ , labels], size = size, legend_title = legend_title) +
      colorscale +
      ggtitle(title) + legend
    
    
  } else if (is.factor(color)) { 
    
    if (is.na(as.logical(col))) {
      colorscale <- scale_colour_viridis_d(col, option = "inferno", guide = "none")
      if (img) colorscale <- scale_colour_viridis_d(col, option = "inferno")
    }
    
    p <- plot_emb(as.matrix(embs), color = color, shape = shape,
                  labels_off = !is.na(as.logical(labels)), #!input$labels
                  labels = input$vars[ , labels], size = size, legend_title = legend_title) +
      colorscale +
      ggtitle(title) + legend
    
  }
  
  if (!is.null(ids)) { # brushed points to highlight
    
    
    newpoints <- ids[ , 1:2]
    
    newpoints <- input$embs[as.numeric(rownames(ids)), c(dim1, dim2)]
    colnames(newpoints) <- c("dim1", "dim2")
    newpoints[[shapes]] <- input$vars[as.numeric(rownames(ids)), shapes]
    
    
    p <- p +
      
      geom_point(data = newpoints, 
                 aes( x = dim1,
                      y = dim2,
                      shape = shape[as.numeric(rownames(ids))]

                 ),
                 color = high_color,
                 size = size
      ) +
      theme(legend.position = "none")

  }
  
  p
}

drawPlot_all <- function(dims, input, 
                         #       dim1 = dim1, dim2 = dim2,
                         shapes = FALSE, labels = FALSE,
                         col = NULL,
                         imgs_on = FALSE, method = NULL,
                         method_names = NULL,
                         sel_ids = NULL,
                         high_color = "hotpink",
                         mat = FALSE,
                         size = 4, img = FALSE) {
    # 
  dim1 <- dims[[1]]
  dim2 <- dims[[2]]
  title <- dims[[3]]
  
  embs <- input$embs[ , c(dim1, dim2)]
  
  drawPlot(input = input, embs = embs, dim1 = dim1, dim2 = dim2, shapes = shapes, labels = labels, col = col, 
           imgs_on = imgs_on, title = title, ids = sel_ids, high_color = high_color, mat = mat, size = size, img = img)
  
}


###### debug ###########################################