#functs_df <- readRDS("mause1.rds")

shapes_trans_df <- readRDS("data/shapes_df.rds")
url_sh <- "data/shapes"

server <- function(input, output) {
  
  # increase the allowed upload size, default is 5 MB
  options(shiny.maxRequestSize = 30000 * 1024^2)
  
  #### Tab image data ####################
  inputData2 <- reactiveVal(preprocessInputData(buildInput(shapes_trans_df, img = TRUE, embs_cols = 4:9, 
                                                           vars_cols = c(1, 2, 10),
                                       vars_cat_cols = 1, 
                                       id_col = NULL, url_col = 2)))
  
  newdata <- reactiveVal(FALSE)
  observe({ # new data uploaded
    
    req(input$file_data)
    
    tryCatch(inputData2(preprocessInputData(readRDS(input$file_data$datapath))), 
             error = function(e) shinyalert("Please upload a suitable dataframe.", 
                                            closeOnClickOutside = TRUE, type = "error"))
    
    newdata(TRUE)
    
  })
  
  #### Plot1_2 ----------------------------
  
  
    # input data
    observe({
      req(inputData2())
      
      output$selPlot1_img_ui <-  buildUI_plots_2(id = "selPlot1_2", input = inputData2(), label = "Plot 1")
      output$toggles_img <- buildUI_sidebar_2(input = inputData2())
      
      
      req(input$file_data2)
      
      tryCatch(inputData2(preprocessInputData(readRDS(input$file_data2$datapath))), 
               error = function(e) shinyalert("Please upload a suitable dataframe.", 
                                              closeOnClickOutside = TRUE, type = "error"))
    })
  
  
  # Show/Hide Settings -----------------------------------------------------------------
  # Hide settings at start of new Shiny session
  observe({
    req(input$colors_2_var)

    if (input$settings_img == 0) {
      hide("selPlot1_2")
      shinyjs::hide("selPlot1_2_2d")
      shinyjs::hide("colors_2_var", anim = FALSE)
      shinyjs::hide("labels_2_var", anim = FALSE)
      shinyjs::hide("shapes_2_var", anim = FALSE)
      shinyjs::hide("plotly2", anim = FALSE)
      shinyjs::hide("imgs_on2", anim = FALSE)
      shinyjs::hide("exImgs_var", anim = FALSE)
      shinyjs::hide("title_plot", anim = FALSE)
      shinyjs::hide("point_size", anim = FALSE)
      shinyjs::hide("high_color_1", anim = FALSE)
      
    }
    
  })
  
  # Toggle visibility of settings
  observeEvent(input$settings_img, {
    
    shinyjs::toggle("selPlot1_2", anim = TRUE)  # toggle is a shinyjs function
    shinyjs::toggle("selPlot1_2_2d", anim = TRUE)
    shinyjs::toggle("colors_2_var", anim = TRUE)
    shinyjs::toggle("labels_2_var", anim = TRUE)
    shinyjs::toggle("shapes_2_var", anim = TRUE)
    shinyjs::toggle("plotly2", anim = TRUE)
    shinyjs::toggle("imgs_on2", anim = TRUE)
    shinyjs::toggle("exImgs_var", anim = TRUE)
    shinyjs::toggle("title_plot", anim = TRUE)
    shinyjs::toggle("point_size", anim = TRUE)
    shinyjs::toggle("high_color_1", anim = TRUE)
    
  })
  
  
  
  observeEvent(input$selPlot1_2_2d, {
    
    output$warningEmb_2 <- renderUI({
      emb1 <- sub("_(\\d+)$", "", input$selPlot1_2)
      emb2 <- sub("_(\\d+)$", "", input$selPlot1_2_2d)
      
      
      conditionalPanel(
        condition = paste0("'", emb1, "' !== '", emb2, "'"),
        
        div(style = "color: red;",
            "Embedding methods not matching"
        )
      )
    })
  })
  
  output$instructions_img_init <- output_instructions_init(img = TRUE)
  
  # hover actions
  id_hover_img <- reactiveVal(NULL)
  
  observeEvent(input$plot1_2_hover, {
    
    dat <- data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                      dim2 = inputData2()$embs[[input$selPlot1_2_2d]],
                      color = if (is.na(as.logical(input$colors_2_var))) {inputData2()$vars[[input$colors_2_var]]} else {
                        rep("none", nrow(inputData2()$embs))},
                      label = if (is.na(as.logical(input$labels_2_var))) {inputData2()$vars[[input$labels_2_var]]} else {
                        rep("none", nrow(inputData2()$embs))},
                      shape = if (is.na(as.logical(input$shapes_2_var))) {inputData2()$vars[[input$shapes_2_var]]} else {
                        rep("none", nrow(inputData2()$embs))},
                      id = inputData2()$embs[["id"]]
    )
    
    id_hover_img(nearPoints(dat, input$plot1_2_hover, maxpoints = 1))
    
  })
  
  
  output$hover_info_img <- renderUI({
    
    req(id_hover_img())
    hover <- input$plot1_2_hover
    hover_function(inputData2(), hover, id_hover = id_hover_img(),
                   color = input$colors_2_var, shape = input$shapes_2_var, label = input$labels_2_var)
  })
  
  ids_brush_img <- reactiveVal()
  
  observe({
    
    ids_brush_img(NULL)
    req(input$plot1_brush_img)

    if (nrow(brushedPoints(data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                                      dim2 = inputData2()$embs[[input$selPlot1_2_2d]]), input$plot1_brush_img)) >= 1)
      
    {
    dat <- data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                      dim2 = inputData2()$embs[[input$selPlot1_2_2d]],
                      id = inputData2()$embs[["id"]]

    )
    ids_brush_img(brushedPoints(dat, input$plot1_brush_img))

        } else {
      ids_brush_img(NULL)
        }
    
  })
  
  
  output$brushed_ids_names_img <- renderText({
    
    paste0("Ids of brushed Points: \n", paste0(ids_brush_img()$id, collapse = ", "),
           "\nId of hovered function: ", id_hover_img()$id)
    
  })
  
  ################## Plots
  plot1_2_out <- reactiveVal()
  plot_2_save <- reactiveVal() # for plotly option
  
  exImg_1 <- reactiveVal()
  exImg_2 <- reactiveVal()
  exImg_3 <- reactiveVal()
  
  title_plot <- reactiveVal("Embedding plot")
  
  
  observeEvent(input$title_plot, { # update the plot title
    req(input$title_plot)
    title_plot(input$title_plot)
  })
  
  observeEvent(input$submit_img, {
    
    isolate({
      
      p <- drawPlot_single(input = inputData2(), embs = inputData2()$embs[ ,c(input$selPlot1_2, input$selPlot1_2_2d)], 
                           dim1 = input$selPlot1_2, dim2 = input$selPlot1_2_2d, shapes = input$shapes_2_var,
                           labels = input$labels_2_var,
                           col = input$colors_2_var, imgs_on = input$imgs_on2, title = title_plot(),
                           size = input$point_size, img = TRUE) + 
        theme(legend.position = "right")
      
    })
    
    if (input$plotly2) {
      plot_2_save(p)
    }
    
    plot1_2_out(p)
    
    ##### get the example images
    
    req(input$exImgs_var)
    # is there a variable to show them by?
    if (is.na(as.logical(input$exImgs_var))) { # yes, there is one
      
      
      vars_coll <- character()
      
      for (var in levels(inputData2()$vars[[input$exImgs_var]])) {
        vars_coll[length(vars_coll) + 1] <- var
      }
      
      # if there are more example images than categories
      if (length(vars_coll) < 3) {vars_coll <- rep(vars_coll, 3)}
      
      url1 <- inputData2()$vars[inputData2()$vars == vars_coll[1], "id"]
      url1 <- url1[runif(1, min = 1, max = length(url1))]
      url1 <- inputData2()$data[inputData2()$data$id == url1, ][[1]]
      exImg_1(url1)
      
      url2 <- inputData2()$vars[inputData2()$vars == vars_coll[2], "id"]
      url2 <- url2[runif(1, min = 1, max = length(url2))]
      url2 <- inputData2()$data[inputData2()$data$id == url2, ][[1]]
      exImg_2(url2)
      
      url3 <- inputData2()$vars[inputData2()$vars == vars_coll[3], "id"]
      url3 <- url3[runif(1, min = 1, max = length(url3))]
      url3 <- inputData2()$data[inputData2()$data$id == url3, ][[1]]
      exImg_3(url3)
      
    } else {
      
      # Just show 3 random pictures
      exImg_1(inputData2()$data[[1]][runif(1, min = 1, max = length(inputData2()$data[[1]]))])
      exImg_2(inputData2()$data[[1]][runif(1, min = 1, max = length(inputData2()$data[[1]]))])
      exImg_3(inputData2()$data[[1]][runif(1, min = 1, max = length(inputData2()$data[[1]]))])
    }
    
    output$img_titles <-img_titles()
    
    output$instructions_img <- output_instructions(img = TRUE)
    shinyjs::hide("instructions_img_init")
    
  })
  
  observeEvent(input$plot1_2_dblclick, { # dble click
    
    ids_brush_img(NULL)
    req(input$plot1_2_dblclick)
    
    if (nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                                   dim2 = inputData2()$embs[[input$selPlot1_2_2d]]), input$plot1_2_dblclick)) >= 1) { # no error message if it doesn't exist
      
      dat <- data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                        dim2 = inputData2()$embs[[input$selPlot1_2_2d]],
                        id = inputData2()$embs[["id"]]
      )
      
      if (input$colors_2_var == "FALSE") {
        
        ids_brush_img(NULL)
      } else {
        
        id_click <- nearPoints(dat, input$plot1_2_dblclick)$id[[1]]
        chosen_var <- inputData2()$vars[inputData2()$vars$id == id_click, ][[input$colors_2_var]] 
        
        ids_brush_img(inputData2()$vars[inputData2()$vars[[input$colors_2_var]] == chosen_var, ])
      }
     
    } else {
      ids_brush_img(NULL)
    }
    
    
    isolate({
      
      p <- drawPlot_single(input = inputData2(), embs = inputData2()$embs[ ,c(input$selPlot1_2, input$selPlot1_2_2d)], 
                           dim1 = input$selPlot1_2, dim2 = input$selPlot1_2_2d, shapes = input$shapes_2_var,
                           labels = input$labels_2_var,
                           col = input$colors_2_var, imgs_on = input$imgs_on2, title = title_plot(),
                           size = input$point_size, img = TRUE, ids = ids_brush_img(), high_color = input$high_color_1) + 
        theme(legend.position = "right")
      
      plot1_2_out(p)
      

      
    })
    
  })
  
  output$plot1_2 <- renderPlot({
    
    req(input$submit_img)
    
    par(bg = "white")
    
    plot1_2_out()
    
  })
  
  
  output$plot3_2_1 <- renderImage({
    
    req(exImg_1())
   # 
    if (newdata()) {
      return(list(src = exImg_1(),
                  width = "auto", #"80%",
                  height = "auto"  #"80%"
      ))
      
    } else { # example data
      
      return(list(src = paste0(url_sh, "/", exImg_1()),
                  width = "auto", #"80%",
                  height = "auto"  #"80%"
      ))
      
    }
    
  }, deleteFile = FALSE)
  
  
  output$plot3_2_2 <- renderImage({
    
    req(exImg_2())
    
    if (newdata()) {
      return(list(src = exImg_2(),
                  width = "auto", #"80%",
                  height = "auto"  #"80%"
      ))
      
    } else { # example data
    
    return(list(src = paste0(url_sh, "/", exImg_2()),
                width = "auto", #"80%",
                height = "auto"  #"80%"
    ))
    }
    
  }, deleteFile = FALSE)
  
  
  output$plot3_2_3 <- renderImage({
    
    req(exImg_3())
    
    if (newdata()) { 
      return(list(src = exImg_3(),
                  width = "auto", #"80%",
                  height = "auto"  #"80%"
      ))
      
    } else { # example data
    
    return(list(src = paste0(url_sh, "/", exImg_3()),
                width = "auto", #"80%",
                height = "auto" ,  #"80%"
                alt = "example picture"
    ))
    }
    
    
  }, deleteFile = FALSE)
  
  
  #### Plot4_2 ----------------------------
  
  output$plot4_2 <- renderImage({ 
    req(input$plot1_2_hover)
    
    input_df2 <- inputData2()
    
      req(nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                                     dim2 = inputData2()$embs[[input$selPlot1_2_2d]]
                                  #   input = var2
                                     ), input$plot1_2_hover, maxpoints = 1)) > 0) # no error message if it doesn't exist
      
      
      dat <- data.frame(dim1 = inputData2()$embs[[input$selPlot1_2]],
                        dim2 = inputData2()$embs[[input$selPlot1_2_2d]],
                        url = inputData2()$data[[1]]
                        )
    
    
    ids <- nearPoints(dat, input$plot1_2_hover, maxpoints = 1)
    filename <- ids$url
    
    
    if (newdata()) {
      
      return(list(src = filename,
                  width = "auto", #"80%",
                  height = "auto"  #"80%"
      ))
      
    } else { # example data
      
    return(list(src = paste0(url_sh, "/", filename),
                width = "auto", #"80%",
                height = "100px",  #"80%"
                alt = "hovered picture"
                ))
    
    }
    
  }, deleteFile = FALSE)
  
  output$plot5_2 <- renderPlotly({
    
    req(input$plotly2)
    req(plot_2_save())
    
    if (input$plotly2) {
      ggplotly(plot_2_save())
    }
    
    
  })


  #### allTab #######################################
  output$toggles_all <- buildUI_sidebar_all(input = inputData2())

  observeEvent(input$submit_all, {
    
    output$plots_all <- buildAllEmbs_plots(input = inputData2())
    
    method_names <- getMethodNames(inputData2())
    n <- 0
    
    dim_combs <- data.frame()
    
    for (method in seq_along(method_names)) {
      for (dim1 in method_names[[method]][-length(method_names[[method]])]) {
        
        i <- which(method_names[[method]] == dim1)
        for (dim2 in method_names[[method]][(i + 1) : length(method_names[[method]])]) {
          
          n <- n + 1
          
          # add row to df
          dim_combs <- rbind(dim_combs, c(dim1, dim2, 
                                          names(method_names)[[method]], n))
        }
      }
    }
    
    # use apply on dim_combs to draw the plots
    
    isolate({
      
      waiter::Waiter$new(id = "plot_all_1")$show()

      apply(dim_combs, 1, function(dims) {
        output[[paste0("plot_all_", dims[[4]])]] <- drawPlot_all(dims, input = inputData2(), 
                                                                 #       dim1 = dim1, dim2 = dim2,
                                                                 shapes = isolate(input$shapes_all_var),
                                                                 labels = isolate(input$labels_all_var),
                                                                 col = isolate(input$colors_all_var),
                                                                 imgs_on = input$imgs_on_all,
                                                                 size = input$point_size_all,
                                                                 img = TRUE
                                                                 )
        
        output[[paste0("plot_all_", dims[[4]], "_right")]] <- renderImage({
          
          if ( # empty plots for hovered images
            (nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                                        dim2 = inputData2()$embs[[dims[[2]]]]), 
                             input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) ) {
            
            
            outfile <- tempfile(fileext='.png')
            
            # Generate a png
            png(outfile, width=400, height=400)
            dev.off()
            
            
            return(list(src = outfile))
            
          } else {
            
            ################ allTab Plots on the right ####################
            
            dat <- data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                              dim2 = inputData2()$embs[[dims[[2]]]],
                              url = inputData2()$data[[1]],
                              id = inputData2()$embs[["id"]]
                              )
            
            ids <- brushedPoints(dat, input[[paste0("plot_all_", dims[[4]], "_brush")]])
            id_hov <- nearPoints(dat, input[[paste0("plot_all_", dims[[4]], "_hover")]], maxpoints = 1)
            
            isolate({
              
              req(nrow(id_hov) > 0) # no error message if it doesn't exist
              filename <- id_hov$url
              
            }) # end isolate() of plot_right
            
         
          
          if (newdata()) {
            
            return(list(src = filename,
                        width = "auto", #"80%",
                        height = "auto"  #"80%"
            ))
            
          } else { # example data
            
            return(list(src = paste0(url_sh, "/", filename),
                        width = "auto", #"80%",
                        height = "100px",  #"80%"
                        alt = "hovered picture"
            ))
            
          }
          } # end else{}
          
        }, deleteFile = FALSE) # end plot_right
        
        ############ allTab TextOutputs ###################
        
        output[[paste0("plot_all_", dims[[4]], "_text")]] <- renderText({
          # this needs to ba a reactive val to work, but a seperate one for each plot, hmmm
          
          ids <- ""
          id_hov <- ""
          
          if (
            (nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                                        dim2 = inputData2()$embs[[dims[[2]]]]), 
                             input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) &
            (nrow(brushedPoints(data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                                           dim2 = inputData2()$embs[[dims[[2]]]]), 
                                input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1)) {
            NULL
          } else {#if (!is.null(input[[paste0("plot_all_", dims[[4]], "_brush")]] | !is.null(input[[paste0("plot_all_", dims[[4]], "_hover")]]))) { # | !is.null(id_hover())
            
            dat <- data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                              dim2 = inputData2()$embs[[dims[[2]]]],
                              id = inputData2()$embs[["id"]]
                              )
   
            ids <- brushedPoints(dat, input[[paste0("plot_all_", dims[[4]], "_brush")]])
            id_hov <- nearPoints(dat, input[[paste0("plot_all_", dims[[4]], "_hover")]], maxpoints = 1)
            
            if (nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                                           dim2 = inputData2()$embs[[dims[[2]]]]), input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) {
              id_hov <- ""
            }
            
            if (nrow(brushedPoints(data.frame(dim1 = inputData2()$embs[[dims[[1]]]],
                                              dim2 = inputData2()$embs[[dims[[2]]]]), 
                                   input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1) {
              ids <- ""
            }
          }
          
          paste0("Ids of brushed Points: ", if (!(ids == "")) {paste0(ids[["id"]], collapse = ", ")}, 
                 "\nId of hovered function: ", if (!(id_hov == "")) {id_hov$id})
          
        }) # end renderText
        
        
      }) # end apply
      
    }) # end isolate() number 1
    
  }) # end observeEvent submit_all allTab
  
  ########### Matrix ###################
  
  observe({
    req(inputData2())
    
    output$toggles_matgg <- buildUI_sidebar_matgg(input = inputData2())
    
  })
  
  # Show/Hide Settings -----------------------------------------------------------------
  # Hide settings at start of new Shiny session
  hide_settings_matgg <- reactiveVal(TRUE)
  observe({
    req(input$colors_matgg_var)

    if (input$settings_matgg == 0 && hide_settings_matgg()) {
      
      shinyjs::hide("colors_matgg_var", anim = FALSE)
      shinyjs::hide("labels_matgg_var", anim = FALSE)
      shinyjs::hide("shapes_matgg_var", anim = FALSE)
      shinyjs::hide("high_color", anim = FALSE)
      shinyjs::hide("point_size_matgg", anim = FALSE)
      
      hide_settings_matgg(FALSE)
      
    }
    
  })
  
  # Toggle visibility of settings
  observeEvent(input$settings_matgg, {
    
    shinyjs::toggle("colors_matgg_var", anim = TRUE)
    shinyjs::toggle("labels_matgg_var", anim = TRUE)
    shinyjs::toggle("shapes_matgg_var", anim = TRUE)
    shinyjs::toggle("high_color", anim = TRUE)
    shinyjs::toggle("point_size_matgg", anim = TRUE)
    
  })
  #--------------------------------------------------
  
  output$brushed_ids_printgg <- renderText({
    
    paste0("ids of brushed points: \n", paste0(ids_matgg_brush()$id, collapse = ", "),
    "\nId of hovered image: ", paste0(id_matgg_hover()$id, collapse = ", "))
  })
  
  output$choose_plot_numbergg <- renderUI({ # drop down menu, to choose plot to brush in
    
    req(inputData2())
    req(input$embs_vargg)
    
    n_dims <- length(colnames(inputData2()$embs)[which(gsub("_\\d+", "", colnames(inputData2()$embs)) == input$embs_vargg)])
    n_plots <- ((n_dims - 1) * (n_dims)) / 2
    
    plot_numbers <- seq_len(n_plots)
    
    selectInput("plot_numbergg", 
                label = "Plot to brush and hover",
                choices = list(
                  `Plot number` = plot_numbers
                )
    )
    
  })
  
  ###### plot Matrixgg #######################
  
  brushedpoints_object <- reactiveVal(NULL) # brush
  ids_matgg_brush <- reactiveVal(NULL) # result of brushedpoints(df, brush)
  dim1_brush <- reactiveVal(NULL) # TODO
  dim2_brush <- reactiveVal(NULL)
  
  id_matgg_hover <- reactiveVal(NULL)
  
  observeEvent(input$undo_matgg, {
    brushedpoints_object(NULL) # brush
    ids_matgg_brush(NULL) # result of brushedpoints(df, brush)
    dim1_brush(NULL) # TODO
    dim2_brush(NULL)
  })
  
  # save the ids of the brush and the hovered id in their respective reactive values
  observe({
    
    req(inputData2())
    req(input$embs_vargg)
   req(input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]])
    
    n_dims <- sum(startsWith(colnames(inputData2()$embs), input$embs_vargg))
    n_plots <- ((n_dims - 1) * (n_dims)) / 2
    
    
    ###############
    
    dim_pairs <- combn(colnames(inputData2()$embs)[startsWith(colnames(inputData2()$embs), input$embs_vargg)], 2)
    

    if (!is.null(input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]])) {
      brush <- input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]]
    }
    
    plot_number <- as.numeric(input$plot_numbergg)
    
    if (!is.null(brush)) {
      if (nrow(brushedPoints(data.frame(dim1 = inputData2()$embs[[dim_pairs[1,plot_number]]],
                                        dim2 = inputData2()$embs[[dim_pairs[2,plot_number]]]), brush)) >= 1) {
        # no error message if it doesn't exist
        
        dat <- data.frame(dim1 = inputData2()$embs[[dim_pairs[1,plot_number]]],
                          dim2 = inputData2()$embs[[dim_pairs[2,plot_number]]],
                          id = inputData2()$embs[["id"]]
    
        )
        
        dim1_brush(dim_pairs[1,plot_number])
        dim2_brush(dim_pairs[2,plot_number])
        
        brushedpoints_object(brush)
        ids_matgg_brush(brushedPoints(dat, brush))
        
       
      }
    }
    
  }) # end observe (brush)
  
  observe({ # hover
    
    req(inputData2())
    req(input$embs_vargg)
    # req(input$plot_matgg_1_brush)
    req(input[[paste0("plot_matgg_", input$plot_numbergg, "_hover")]])
    
    n_dims <- sum(startsWith(colnames(inputData2()$embs), input$embs_vargg))
    n_plots <- ((n_dims - 1) * (n_dims)) / 2
    
    ###############
    
    dim_pairs <- combn(colnames(inputData2()$embs)[startsWith(colnames(inputData2()$embs), input$embs_vargg)], 2)
    
    ## same for hovered object -----------------------------
    
    if (!is.null(input[[paste0("plot_matgg_", input$plot_numbergg, "_hover")]])) {
      hover <- input[[paste0("plot_matgg_", input$plot_numbergg, "_hover")]]
    }
    
    plot_number <- as.numeric(input$plot_numbergg)
    
    if (!is.null(hover)) {
      if (nrow(nearPoints(data.frame(dim1 = inputData2()$embs[[dim_pairs[1,plot_number]]],
                                     dim2 = inputData2()$embs[[dim_pairs[2,plot_number]]]), hover)) >= 1) {
        # no error message if it doesn't exist
        
        dat <- data.frame(dim1 = inputData2()$embs[[dim_pairs[1,plot_number]]],
                          dim2 = inputData2()$embs[[dim_pairs[2,plot_number]]],
                          url = inputData2()$data[[1]],
                          id = inputData2()$embs[["id"]]
                          #    label = inputData2()$vars[ , input$colors_var],
                          # scores = inputData2()$vars$scores_umap
        )

        id_matgg_hover(nearPoints(dat, hover, maxpoints = 1))
        
      }
    }
    
    output$plot4_matgg <- renderImage({
      
      req(input$colors_matgg_var)
      req(id_matgg_hover())
      
      if ( # empty plots for hovered images
        (nrow(id_matgg_hover()) < 1) ) {
        
        outfile <- tempfile(fileext='.png')
        
        # Generate a png
        png(outfile, width=400, height=400)
        dev.off()
        
        
        return(list(src = outfile))
        
      } else {
        
        ################ allTab Plots on the right ####################
        dat <- data.frame(dim1 = inputData2()$embs[[dim_pairs[1,plot_number]]],
                          dim2 = inputData2()$embs[[dim_pairs[2,plot_number]]],
                          url = inputData2()$data[[1]],
                          id = inputData2()$embs[["id"]]
        )
        
        isolate({
          
          req(nrow(id_matgg_hover()) > 0) # no error message if it doesn't exist
          filename <- id_matgg_hover()$url
          
        }) # end isolate() of plot_right
        
        
        
        if (is.character(newdata())) {
          
          return(list(src = filename,
                      width = "auto", #"80%",
                      height = "auto"  #"80%"
          ))
          
        } else {
          
          return(list(src = paste0(url_sh, "/", filename),
                      width = "auto", #"80%",
                      height = "100px",  #"80%"
                      alt = "hovered picture"
          ))
          
        }
      } # end else{}
      
    }, deleteFile = FALSE) # end plot_right
    
    
  }) # end observe (hover)
 
  
  ############ plot all combs ################
  
  observe({
    
    req(input$embs_vargg)
    req(input$colors_matgg_var)
    req(input$shapes_matgg_var)
    req(input$point_size_matgg)
    
    meth <- input$embs_vargg
    
    dim_combs <- tryCatch(combn(colnames(inputData2()$embs)[which(gsub("_\\d+", "", 
                                                                       colnames(inputData2()$embs)) == meth)], 2), 
                          error = function(e)  dim_combs <- NULL) # when new data is loaded, to only re-plot when ready
    
    req(dim_combs)
    
    
    # define rows and cols of plots, and plot names (UI elements)
    output$plots_matgg <- buildMatggEmbs_plots(input = inputData2(), meth = meth)
    
    dim_combs <- t(as.data.frame(dim_combs))
    
    dim_combs <- cbind(dim_combs, as.character(1:nrow(dim_combs)))
    
    # plot the plots (server elements)
    if (is.null(ids_matgg_brush())) {
      apply(dim_combs, 1, function(dims) {
        output[[paste0("plot_matgg_", dims[[3]])]] <- drawPlot_all(dims, input = inputData2(), 
                                                                   shapes = isolate(input$shapes_matgg_var),
                                                                   col = isolate(input$colors_matgg_var),
                                                                   sel_ids = ids_matgg_brush(),
                                                                   mat = TRUE,
                                                                   size = input$point_size_matgg)
        
      }) # end apply
    }
    
    req(ids_matgg_brush())
    req(input$shapes_matgg_var)
    
    
    apply(dim_combs, 1, function(dims) { # draw the plots if sth has been brushed
      
      output[[paste0("plot_matgg_", dims[[3]])]] <- drawPlot_all(dims, input = inputData2(), 
                                                                 shapes = input$shapes_matgg_var,
                                                                 col = isolate(input$colors_matgg_var),
                                                                 sel_ids = ids_matgg_brush(), # these are the brushed points
                                                                 high_color = input$high_color,
                                                                 mat = TRUE,
                                                                 size = input$point_size_matgg)
      
    }) # end apply
    
  }) # end observe
  
  
  #### Tab Data #######################################
  
  output$dataframe<- renderTable(striped = TRUE, {
    
    df <- subset(inputData2()$df, select = !unlist(map(inputData2()$df, is_tf))) #fun, -tf_fun
    
    if (!is.na(as.numeric(df$id[[1]]))) { # if the IDs are numbers, sort by number, not alphabetically
      df <- df[order(as.numeric(df$id)), ]
    }
    
    df <- subset(inputData2()$df) 
    
    df
  })

  
} # end server

