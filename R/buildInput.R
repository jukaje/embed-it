# function for the user, to get the data in a format that the app and the other functions will use
# the data has to be a .rds dataframe
# embs_cols: a vector containing names or indices of the embedding columns
# vars_cols: a vector containing names or indices of the variable columns
# vars_cat_cols: a vector containing names or indices of the categorical variable columns
# tf-col: the name or index of the tfd-column containing the functional data
# id_col: the name or index of the id column, if one exists

buildInput <- function(data, embs_cols = NULL, vars_cols = NULL, vars_cat_cols = NULL, tf_col = NULL, id_col = NULL,
                       img = FALSE, url_col = NULL) {
  #if it is a rds:
  assertDataFrame(data, min.rows = 1, min.cols = 3) # 2 embedding dims the data itself
  
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