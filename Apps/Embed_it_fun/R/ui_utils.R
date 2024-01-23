

buildUI_plots <- function(id, input, label) { # ui element to select plotting dimensions
  
  renderUI({
    dims_select <- as.list(colnames(input$embs)[which(colnames(input$embs) != "id")])
    
    tagList(
      fluidRow(
        column(width = 6,
               shinyjs::useShinyjs(),
               selectInput(id, 
                           label = "Dimension 1",
                           choices = list(
                             `Embs` = dims_select
                           )
               )
        ),
        
        column(width = 6,
               selectInput(paste0(id, "_2d"), 
                           
                           label = "Dimension 2",
                           choices = list(
                             `Embs` = dims_select
                           ),
                           selected = dims_select[[2]]
               ),
        ),
        
      ),
      
      fluidRow(width = 12,
               
               conditionalPanel(
                 condition = "input.selPlot1 === input.selPlot1_2d",
                 div(style = "color: red;",
                     "Same dimension chosen twice"
                 )
               )
               
      ),
      
      fluidRow(width = 12,
               
               uiOutput("warningEmb")
               
      ),
      
    ) # end TagList
  })
  
}



buildUI_sidebar_1 <- function(input) { # Tab Viz: Functional Data

          renderUI({
            
            tagList(
              shinyjs::useShinyjs(),
              buildVar_dropdown(id = "colors", input = input),

              buildVar_dropdown(id = "labels", input = input),
              
              buildVar_dropdown(id = "shapes", input = input, value = FALSE, factor_vars = TRUE),
              
              
              checkboxInput(
                inputId = "imgs_on",
                label = "Glyph plot",
                value = FALSE
              ),
              
              
              conditionalPanel(condition = "input.imgs_on == 0",
                               checkboxInput(
                                 inputId = "plotly",
                                 label = "Plotly",
                                 value = FALSE
                               )
                               ),
              
              textInput("title_plot", "Custom scatter plot title"),
              
              textInput("title_curves", "Custom functions plot title"),
              
              sliderInput("point_size", "scatter point size", min = 1, max = 8, value = 4, step = 0.1),
              
              selectInput("high_color_1",
                          label = "Color to highlight double clicked points",
                          choices = list(
                            "Pink" = "hotpink",
                            "Green" = "green",
                            "Blue" = "blue",
                            "Red" = "red"
                          )
              )
              
              )
             
            
          })
          
}



buildUI_sidebar_matgg <- function(input) { # tab Viz: Matrix
  
  methods_pure <- gsub("_\\d+", "", colnames(input$embs))
  method_names <- unique(methods_pure)
  method_names <- method_names[-which(method_names == "id")]
  
  renderUI({

        tagList(
      shinyjs::useShinyjs(),
      
      # choose the emb method
      selectInput(paste0("embs_vargg"),
                  label = "Embedding method", 
                  choices = list(
                    `Method` = as.list(method_names)
                  )
      ),
      
      buildVar_dropdown(id = "colors_matgg", input = input),
      
      buildVar_dropdown(id = "shapes_matgg", input = input, value = FALSE, factor_vars = TRUE),
      
    sliderInput("point_size_matgg", "scatter point size", min = 1, max = 8, value = 4, step = 0.1)
      
      
    )
  })
  
}



buildUI_sidebar_all <- function(input) { # tab Viz: Embeddings
  
  renderUI({
    tagList(
      
      buildVar_dropdown(id = "colors_all", input = input),
      
      buildVar_dropdown(id = "labels_all", input = input),
      
      buildVar_dropdown(id = "shapes_all", input = input, value = FALSE, factor_vars = TRUE),
      
      
      checkboxInput(
        inputId = "imgs_on_all",
        label = "Glyph plot",
        value = FALSE
      ),

      sliderInput("point_size_all", "scatter point size", min = 1, max = 8, value = 4, step = 0.1),

    )
  })
  
}


# ui helper function
buildVar_dropdown <- function(id, input, value = TRUE, factor_vars = FALSE, label = NULL) { # ui drop down from vars
  
  if (is.null(label)) {label <- str_to_sentence(gsub("_.+", "", id))}
  
  if (factor_vars) {
    vars_select <- colnames(input$vars)[sapply(input$vars, is.factor)] # only include factor variables
    
    if (length(vars_select) == 1) {
      if (length(levels(input$vars[ , unlist(vars_select)])) > 6) {vars_select <- NULL}
    } else { # only include factor vars with 6 or less levels
      vars_select <- vars_select[which(sapply(sapply(input$vars[ ,unlist(vars_select)], levels), length) <= 6)] 
    }
    
  } else {
    vars_select <- as.list(colnames(input$vars)[which(colnames(input$vars) != "id")])
  }
  
  tagList(
                     selectInput(paste0(id, "_var"),
                                 label = label,
                                 choices = list(
                                   `none` = FALSE,
                                   `Variable` = as.list(vars_select)
                                 )
                     )
    )
  
}

output_instructions_init <- function(img = FALSE) {
  if (!img) {
    
    renderUI({
      
      
      HTML(
        r"( <em><span class="" style="font-size: small;"><p>
          <i><b>Instructions</b>: Upload your own data under 'Data' or 
        click 'Plot the plots' to use example data <a href="https://zenodo.org/records/5779876#.YjsK8TW1Kwf"> (ABR mouse curves)</a>. </i>
        </em><br> )"
      ) 

      
    })
    
  } else {
    renderUI({
      
      
      HTML(
        r"( <em><span class="" style="font-size: small;"><p>
          <i><b>Instructions</b>: Upload your own data or 
        click 'Plot the plots' to use example data (images of shapes). </i>
        </em><br> )"
      ) 
      
      
    })
    
  }
  
}


output_instructions <- function(img = FALSE) {
  
  if (!img) {
  
          renderUI({
            
            
            HTML(
              r"( <em><span class="" style="font-size: small;"><p>
          <i> <b>Brush</b> in upper plot for a selection, <b>hover</b> for information 
          or <b>double click</b> to select based on the color variable.
          The selection will be highlighted in lower plot. </i>
        </em><br> )" 
            ) 
            
          })
    
  } else {
    
    renderUI({
      
      
      HTML(
        r"( <em><span class="" style="font-size: small;"><p>
          <i><b>Instructions</b>: Hover on points for information. 
        The image represented by the point will be shown beneath the plot. </i>
        </em><br> )"
      ) 
      
    })
    
  }
  
}


buildUI_plotAndBrush <- function(id, n = 0) { # ui plot container trio for tab Viz: Embeddings
  tagList(
    
    fluidRow(
      column(width = 6, class = "plot-container", plotOutput(id,
                                                             brush = brushOpts(id = paste0(id, "_brush"), fill = "black"),
                                                             hover = hoverOpts(id = paste0(id, "_hover"), delay = 300, delayType = "debounce")),
      ),
      column(width = 6, class = "plot-container", plotOutput(paste0(id, "_right")))
    ),
    
    fluidRow(
      # wellPanel(
        column(width = 12, class = "text-container", verbatimTextOutput(paste0(id, "_text")), style = "background-color: #e5eee9;") 
     # )
    ) # end fluidRow
    
  ) # end TagList
  
  
}

# ui all plot container trios for tab Viz: Embeddings
buildAllEmbs_plots <- function(input, labels = NULL, colors = NULL, shapes = NULL, imgs_on = NULL) { # ui all plot c

  methods_pure <- gsub("_\\d+", "", colnames(input$embs))
  method_names <- unique(methods_pure)
  method_names <- method_names[-which(method_names == "id")]
  n_method <- length(method_names)
  
  res_ns <- numeric()
  # dims per method
  for (method in method_names) {
    resn <- length(which(methods_pure == method))
    res_ns[length(res_ns) + 1] <- resn
    names(res_ns)[length(res_ns)] <- paste0(method)
  }
  
  # number of plots needed
  n_plots <- 0
  for (n in res_ns) {
    n_plots <- n_plots + ((n  * (n-1)) / 2) # formula for combinations
  }
  
  # build the ui plot outputs needed
  plot_names <- character()
  
  for (n in seq_len(n_plots)) {
    plot_names[length(plot_names) + 1] <- paste0("plot_all_", n)
  }
  
  renderUI({
    plot_list <- lapply(plot_names, buildUI_plotAndBrush)
    do.call(tagList, plot_list)
  })
  
  
}


# ui plot containers for tab Viz: Matrix
buildMatggEmbs_plots <- function(input, meth, labels = NULL, colors = NULL, shapes = NULL, imgs_on = NULL) {

# build some necessary vars
  methods_pure <- meth
  method_names <- unique(methods_pure)
  n_method <- length(method_names)
  
  res_ns <- numeric()
  # dims per method
  methods_pure <- gsub("_\\d+", "", colnames(input$embs))
  for (method in method_names) {
    resn <- length(which(methods_pure == method))
    res_ns[length(res_ns) + 1] <- resn
    names(res_ns)[length(res_ns)] <- paste0(method)
  }

  # number of plots needed
  n_plots <- 0
  for (n in res_ns) {
    n_plots <- n_plots + ((n  * (n-1)) / 2) # formula for combinations
  }
  
  #build the ui plot outputs needed
  
  plot_names <- character()

  for (n in seq_len(n_plots)) {
    plot_names[length(plot_names) + 1] <- paste0("plot_matgg_", n)
  }
  
  renderUI({
    
    plot_list <- plot_names
    
    n_cols <- (-1 + sqrt(1 + 8 * length(plot_list))) / 2
    
    plots_left <- list()
    
    output_plots <- tagList()
    for (i in seq_len(n_cols)) {
     
      
      fillplots <- c(rep(NA, (n_cols - (n_cols - (i-1)))), plot_list[1:(n_cols - (i - 1))])
      plot_list <- plot_list[-c(1:(n_cols - (i - 1)))] #(i + 1):length(plot_list)
      
      height <- 1000/n_cols
      
      output_plots <- append(output_plots, 
                             tagList(fluidRow(
        lapply(fillplots, buildUI_plotAndBrush_matgg, n_cols)
                             )
      ))
    }
    
    do.call(tagList, output_plots)
  })
  

}

buildUI_plotAndBrush_matgg <- function(id, n = 0) { # helper function for buildMatggEmbs_plots()
  tagList(
    

      column(width = 12/n, class = "plot-container", plotOutput(id,
                                                             brush = paste0(id, "_brush"),
                                                             hover = hoverOpts(id = paste0(id, "_hover"), delay = 300, 
                                                                               delayType = "debounce"))
     ) # end column
    
  ) # end TagList
  
}