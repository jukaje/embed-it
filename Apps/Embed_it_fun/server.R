functs_df <- readRDS("data/mause1.rds")


server <- function(input, output) {
  
  # increase the allowed upload size, default is 5 MB
  options(shiny.maxRequestSize = 30000 * 1024^2)
  
  #### Tab1 functional data ####################

  # load example data
  inputData1 <- reactiveVal(preprocessInputData(buildInput(functs_df, embs_cols = c(5:11), 
                                                           vars_cols = c(1:3, 12, 13), 
                                                           vars_cat_cols = c(1:3), tf_col = 4)))
  
  
  ####### build InputData1() with newly uploaded Data ############################
  observe({
    
    req(input$file_data)
    
    tryCatch(inputData1(preprocessInputData(readRDS(input$file_data$datapath))), 
             error = function(e) shinyalert("Please upload a suitable dataframe.", 
                                            closeOnClickOutside = TRUE, type = "error"))
    
  })
  
  observe({  # ui sidebar
    req(inputData1())
    
    output$selPlot1_ui <- buildUI_plots(id = "selPlot1", input = inputData1(), label = "Plot 1")
    
    output$toggles <- buildUI_sidebar_1(input = inputData1()) 
    
  })
  
  # Show/Hide Settings -----------------------------------------------------------------
  # Hide settings at start of new Shiny session
  hide_settings <- reactiveVal(TRUE)
  observe({
    req(input$colors_var)
    #shinyjs::toggle(id = "selPlot1", condition = (input$settings[1] %% 2 != 0))
    
    if (input$settings == 0 && hide_settings()) {
    hide("selPlot1")
    shinyjs::hide("selPlot1_2d")
    shinyjs::hide("colors_var", anim = FALSE)
    shinyjs::hide("labels_var", anim = FALSE)
    shinyjs::hide("shapes_var", anim = FALSE)
    shinyjs::hide("plotly", anim = FALSE)
    shinyjs::hide("imgs_on", anim = FALSE)
    shinyjs::hide("title_plot", anim = FALSE)
    shinyjs::hide("title_curves", anim = FALSE)
    shinyjs::hide("point_size", anim = FALSE)
    shinyjs::hide("high_color_1", anim = FALSE)
    
    hide_settings(FALSE)
    
    }
    
  })

  # Toggle visibility of settings
  observeEvent(input$settings, {
    
    shinyjs::toggle("selPlot1", anim = TRUE)  # toggle is a shinyjs function
    shinyjs::toggle("selPlot1_2d", anim = TRUE)
    shinyjs::toggle("colors_var", anim = TRUE)
    shinyjs::toggle("labels_var", anim = TRUE)
    shinyjs::toggle("shapes_var", anim = TRUE)
    shinyjs::toggle("plotly", anim = TRUE)
    shinyjs::toggle("imgs_on", anim = TRUE)
    shinyjs::toggle("title_plot", anim = TRUE)
    shinyjs::toggle("title_curves", anim = TRUE)
    shinyjs::toggle("point_size", anim = TRUE)
    shinyjs::toggle("high_color_1", anim = TRUE)
    
    #shinyjs::toggle(id = "selPlot1", condition = (input$settings[1] %% 2 == 0))
  })
 
  
  ### Display a warning if different embedding methods are chosen for the dims
  observeEvent(input$selPlot1_2d, {
    
    output$warningEmb <- renderUI({
      emb1 <- sub("_(\\d+)$", "", input$selPlot1)
      emb2 <- sub("_(\\d+)$", "", input$selPlot1_2d)
      
      conditionalPanel(
        condition = paste0("'", emb1, "' !== '", emb2, "'"),
        
        div(style = "color: red;",
            "Embedding methods not matching"
        )
      )
    })
  })
  
  
  observeEvent(input$file_data, { # reset plot
    
    plot1_out(NULL)
    dblclicked(FALSE)

  })
  
  
  ids_brush <- reactiveVal(NULL)
  id_hover <- reactiveVal(NULL)
  plot_save <- reactiveVal(NULL)
  
  
  plot1_out <- reactiveVal()
  title_plot <- reactiveVal("Embedding Plot")
  title_curves <- reactiveVal("Functions")
  title_curves_brush <- reactiveVal("Brushed Data")
  
  dblclicked <- reactiveVal(FALSE) # to trigger events only when wanted, to not clash with brush
  
  # save the ids of the brush and the hovered id in their respective reactive values
   observeEvent(input$plot1_brush, {
     
       ids_brush(NULL)
       req(input$plot1_brush)
       dblclicked(FALSE)
     
      if (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[input$selPlot1]],
                                        dim2 = inputData1()$embs[[input$selPlot1_2d]]), input$plot1_brush)) >= 1) { # no error message if it doesn't exist
     
      dat <- data.frame(dim1 = inputData1()$embs[[input$selPlot1]],
                        dim2 = inputData1()$embs[[input$selPlot1_2d]],
                        id = inputData1()$embs[["id"]]
                    )
      
    ids_brush(brushedPoints(dat, input$plot1_brush))

          } else { 
        ids_brush(NULL)
      }
    
     
  })
     
     observeEvent(input$plot1_dblclick, { # save double clicked ids and replot

       ids_brush(NULL)
       req(input$plot1_dblclick)
       dblclicked(TRUE)
      
       if (nrow(nearPoints(data.frame(dim1 = inputData1()$embs[[input$selPlot1]],
                                         dim2 = inputData1()$embs[[input$selPlot1_2d]]), input$plot1_dblclick)) >= 1) { # no error message if it doesn't exist
         
         dat <- data.frame(dim1 = inputData1()$embs[[input$selPlot1]],
                           dim2 = inputData1()$embs[[input$selPlot1_2d]],
                           id = inputData1()$embs[["id"]]
         )
         
         if (input$colors_var == "FALSE") {
           ids_brush(NULL)
         } else {
           
           id_click <- nearPoints(dat, input$plot1_dblclick)$id[[1]]
           chosen_var <- inputData1()$vars[inputData1()$vars$id == id_click, ][[input$colors_var]] 
           ids_brush(inputData1()$vars[inputData1()$vars[[input$colors_var]] == chosen_var, ])
         }
         
       } else {
         ids_brush(NULL)
       }
       
       isolate({
         
         p <- drawPlot_single(input = inputData1(), embs = inputData1()$embs[ ,c(input$selPlot1, input$selPlot1_2d)], 
                              dim1 = input$selPlot1, dim2 = input$selPlot1_2d, shapes = input$shapes_var, labels = input$labels_var,
                              col = input$colors_var, imgs_on = input$imgs_on, title = title_plot(), size = input$point_size,
                              ids = ids_brush(), high_color = input$high_color_1) #"Embedding Plot"
         
         plot1_out(p)
         
         
       })
       
     })
     
  
  observeEvent(input$plot1_hover, { # save hovered id
  
    if (!input$hover_off) {
      dat <- data.frame(dim1 = inputData1()$embs[[input$selPlot1]],
                        dim2 = inputData1()$embs[[input$selPlot1_2d]],
                        color = if (is.na(as.logical(input$colors_var))) {inputData1()$vars[[input$colors_var]]} else {
                          rep("none", nrow(inputData1()$embs))},
                        label = if (is.na(as.logical(input$labels_var))) {inputData1()$vars[[input$labels_var]]} else {
                          rep("none", nrow(inputData1()$embs))},
                        shape = if (is.na(as.logical(input$shapes_var))) {inputData1()$vars[[input$shapes_var]]} else {
                          rep("none", nrow(inputData1()$embs))},
                        id = inputData1()$embs[["id"]]
      )
      
      id_hover(nearPoints(dat, input$plot1_hover, maxpoints = 1))
      
    } else {
      id_hover(NULL)
    }
      
    

  })
  
  output$hover_info <- renderUI({ # hover tooltip
    
    req(id_hover())
    
    hover <- input$plot1_hover
    hover_function(inputData1(), hover, id_hover = id_hover(), 
                   color = input$colors_var, shape = input$shapes_var, label = input$labels_var)
  })
  
  output$instructions_init <- output_instructions_init()
  
  
  
  ############ render the Plots Tab Functional Data ####################
  
  observeEvent(input$title_plot, { #update the scatter plot title 
    req(input$title_plot)
    title_plot(input$title_plot)
  })
  
  observeEvent(input$title_curves, { # update the curves plot title
    req(input$title_curves)
    title_curves(input$title_curves)
    title_curves_brush(input$title_curves)
  })
  

  
  observeEvent(input$submit, { # submit button clicked
    
    ids_brush(NULL)
    dblclicked(FALSE)
    
    isolate({
      
      p <- drawPlot_single(input = inputData1(), embs = inputData1()$embs[ ,c(input$selPlot1, input$selPlot1_2d)], 
                           dim1 = input$selPlot1, dim2 = input$selPlot1_2d, shapes = input$shapes_var, labels = input$labels_var,
                           col = input$colors_var, imgs_on = input$imgs_on, title = title_plot(), size = input$point_size,
                           )
      
    
    
    if (input$plotly) {
      plot_save(p)
    }
    
    plot1_out(p)
    
    output$instructions <- output_instructions()
    shinyjs::hide("instructions_init")
      
    
    })
      
  })
  
  
  
  output$plot1 <- renderPlot({ # scatterplot

    req(input$submit)
    
    plot1_out()

  })
  
  
  ####### Plot 4 (curves) ################
  output$plot4 <- renderPlot({ 
    
    req(input$submit)
    req(plot1_out())
    

    df <- data.frame(dim1 = inputData1()$embs[[isolate(input$selPlot1)]],
                     dim2 = inputData1()$embs[[isolate(input$selPlot1_2d)]])
    
    # Using the function disabeles the strategically set isolate() functions
    # return(plot.funs_compact(input = inputData1(), df = df, hover = input$plot1_hover, brush = input$plot1_brush, 
    #                   dblclick = input$plot1_dblclick, 
    #                   id_hover = id_hover(), ids_brush = ids_brush(), ids_dblclick = ids_dblclick(), 
    #                   colors_var = input$colors_var))
  
        if (!is.null(ids_brush()) | !is.null(id_hover())) {
        
        ids <- ids_brush()
        id_hov <- id_hover()
        
        if (is.na(as.logical(isolate(input$colors_var)))) {
          color <- isolate(inputData1()$vars[ ,input$colors_var])
        } else {
          color <- NULL
          colorscale <- scale_color_manual(values = list("black", "#468499"))
        }
        
        
        if (nrow(nearPoints(df, isolate(input$plot1_hover))) < 1) {
          id_hov <- NULL
        }
        
        # To unselect brushed points, but it also redraws functions when dblclick colored
        if (nrow(brushedPoints(df, input$plot1_brush)) < 1 & #isolate(
            !isolate(dblclicked())) {
          ids <- NULL
        }
        
        if (!is.factor(isolate(color))) {
          
          if (is.na(as.logical(isolate(input$colors_var)))) {
            colorscale <- scale_viridis_stretched(isolate(input$colors_var), option = "D")
          }
          
          p <-plot.funs_tf(inputData1()$data, ids = ids, id_hov = id_hov, 
                           col = isolate(color), colorscale = colorscale, hov_col = "hotpink") + 
            ggtitle(isolate(title_curves_brush()))
          
        } else { 
          
          if (is.na(as.logical(isolate(input$colors_var)))) {
            colorscale <- scale_colour_viridis_d(isolate(input$colors_var), option = "inferno")
          }
          
          p <- plot.funs_tf(inputData1()$data, ids = ids, id_hov = id_hov, 
                            col = isolate(color), colorscale = colorscale, hov_col = "turquoise1") + 
            ggtitle(isolate(title_curves_brush()))
        }
      } else if (is.null(ids_brush())) {
        
        ############ Block for plotting all functions, if nothing is brushed/hovered ###################
        isolate({
          if (is.na(as.logical(input$colors_var))) { #input$shapes
            color <- inputData1()$vars[ ,input$colors_var]
          } else {
            color <- NULL
            colorscale <- scale_color_manual(values = list("black", "#468499"), name = "")
          }
          
          if (!is.factor(isolate(color))) {
            
            if (is.na(as.logical(input$colors_var))) {
              colorscale <- scale_viridis_stretched(isolate(input$colors_var), option = "D")
            }
            
            p <- plot.funs_tf(inputData1()$data, ids = NULL, id_hov = NULL,
                              col = isolate(color), colorscale = colorscale, hov_col = "hotpink") +
              ggtitle(isolate(title_curves()))
            
          } else {
            
            if (is.na(as.logical(input$colors_var))) {
              colorscale <- scale_colour_viridis_d(isolate(input$colors_var), option = "inferno")
              # TODO if there are too many categories, do not show legend
              
            }
            
            p <-  plot.funs_tf(inputData1()$data, ids = NULL, id_hov = NULL,
                               col = isolate(color), colorscale = colorscale, hov_col = "turquoise1") +
              ggtitle(isolate(title_curves()))
          }
        }) # end isolate no brush/hover
      }
      
      # end_time <- Sys.time()
      # message("plot4 time server: ", end_time - start_time)
  
          p
  })
  
  output$brushed_ids_names <- renderText({ # print brushed and hovered ids
    
    if (!input$hover_off) {
      paste0("ids of brushed points: \n", paste0(ids_brush()$id, collapse = ", "),
             "\nid of hovered function: ", id_hover()$id)
    } else {
      paste0("ids of brushed points: \n", paste0(ids_brush()$id, collapse = ", "))
    }
    
  })
  
  
  output$plot5 <- renderPlotly({ # Plotly plot
    
    req(input$plotly)
    req(plot_save())

    if (input$plotly) {
      ggplotly(plot_save())
    }


  })
  
  
  #### allTab #######################################
  output$toggles_all <- buildUI_sidebar_all(input = inputData1())
  
  observeEvent(input$submit_all, { # submit button clicked
    
    output$plots_all <- buildAllEmbs_plots(input = inputData1()) # ui create plots
    
    method_names <- getMethodNames(inputData1())
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
    
    isolate({ # use apply on dim_combs to draw the plots
      

      waiter::Waiter$new(id = "plot_all_1")$show()

      apply(dim_combs, 1, function(dims) {
        output[[paste0("plot_all_", dims[[4]])]] <- drawPlot_all(dims, input = inputData1(), 
                                                                 #       dim1 = dim1, dim2 = dim2,
                                                                 shapes = isolate(input$shapes_all_var),
                                                                 labels = isolate(input$labels_all_var),
                                                                 col = isolate(input$colors_all_var),
                                                                 imgs_on = input$imgs_on_all,
                                                                 size = input$point_size_all)
        
        output[[paste0("plot_all_", dims[[4]], "_right")]] <- renderPlot({
          
          if ( # empty plots if nothing is brushed
            (nrow(nearPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                        dim2 = inputData1()$embs[[dims[[2]]]]), 
                             input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) &
            (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                           dim2 = inputData1()$embs[[dims[[2]]]]), 
                                input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1)) {
            NULL
            
            
          } else {
            
            ################ allTab Plots on the right ####################
            
            dat <- data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                              dim2 = inputData1()$embs[[dims[[2]]]],
                              id = inputData1()$embs[["id"]]
                              )
            
            ids <- brushedPoints(dat, input[[paste0("plot_all_", dims[[4]], "_brush")]])
            id_hov <- nearPoints(dat, input[[paste0("plot_all_", dims[[4]], "_hover")]], maxpoints = 1)
            
            isolate({
              
              if (is.na(as.logical(input$colors_all_var))) { #input$shapes
                color <- inputData1()$vars[ ,input$colors_all_var]
              } else {
                color <- NULL
                colorscale <- scale_color_manual(values = list("black", "#468499"))
              }
              
              
              if (nrow(nearPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                             dim2 = inputData1()$embs[[dims[[2]]]]), 
                                  input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) {
                id_hov <- NULL
              }
              
              if (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                                dim2 = inputData1()$embs[[dims[[2]]]]),
                                     input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1) {
                ids <- NULL
              }
              
              
              if (!is.factor(isolate(color))) {
                
                if (is.na(as.logical(input$colors_all_var))) {
                  colorscale <- scale_viridis_stretched(isolate(input$colors_all_var), option = "D")
                }
                
                plot.funs_tf(inputData1()$data, ids = ids, id_hov = id_hov, 
                             col = isolate(color), colorscale = colorscale, hov_col = "hotpink") + 
                  ggtitle("Brushed Data")
                
              } else { 
                
                if (is.na(as.logical(input$colors_all_var))) {
                  colorscale <- scale_colour_viridis_d(isolate(input$colors_all_var), option = "inferno")
                }
                
                plot.funs_tf(inputData1()$data, ids = ids, id_hov = id_hov, 
                             col = isolate(color), colorscale = colorscale, hov_col = "turquoise1") + 
                  ggtitle("Brushed Data") # id_hov = id_hov,
              }
              
            }) # end isolate() of plot_right
            
          } # end else{}
          
          
        }) # end plot_right
        
        ############ allTab TextOutputs ###################
        
        output[[paste0("plot_all_", dims[[4]], "_text")]] <- renderText({
          
          ids <- ""
          id_hov <- ""
          
          if (
            (nrow(nearPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                        dim2 = inputData1()$embs[[dims[[2]]]]), 
                             input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) &
            (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                           dim2 = inputData1()$embs[[dims[[2]]]]), 
                                input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1)) {
            NULL
          } else {
            
            dat <- data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                              dim2 = inputData1()$embs[[dims[[2]]]],
                              id = inputData1()$embs[["id"]]
                              )
            
            ids <- brushedPoints(dat, input[[paste0("plot_all_", dims[[4]], "_brush")]])
            id_hov <- nearPoints(dat, input[[paste0("plot_all_", dims[[4]], "_hover")]], maxpoints = 1)
            
            if (nrow(nearPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                           dim2 = inputData1()$embs[[dims[[2]]]]), input[[paste0("plot_all_", dims[[4]], "_hover")]])) < 1) {
              id_hov <- ""
            }
            
            if (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[dims[[1]]]],
                                              dim2 = inputData1()$embs[[dims[[2]]]]), 
                                   input[[paste0("plot_all_", dims[[4]], "_brush")]])) < 1) {
              ids <- ""
            }
          }
          
          paste0("ids of brushed points: ", if (!(ids == "")) {paste0(ids[["id"]], collapse = ", ")}, 
                 "\nid of hovered function: ", if (!(id_hov == "")) {id_hov$id})
          
        }) # end renderText
        
        
      }) # end apply
      
    }) # end isolate() number 1
    
  }) # end observeEvent submit_all allTab

  
  ############ Matrixgg #############
  
  observe({
    req(inputData1())
    
    output$toggles_matgg <- buildUI_sidebar_matgg(input = inputData1())
    
  })
  
  # Show/Hide Settings -----------------------------------------------------------------
  # Hide settings at start of new Shiny session
  hide_settings_matgg <- reactiveVal(TRUE)
  observe({
    req(input$colors_matgg_var)
    #shinyjs::toggle(id = "selPlot1", condition = (input$settings[1] %% 2 != 0))
    
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
    
    #shinyjs::toggle(id = "selPlot1", condition = (input$settings[1] %% 2 == 0))
  })
  #--------------------------------------------------
  
  output$brushed_ids_printgg <- renderText({
    
    paste0("ids of brushed points: \n", paste0(ids_matgg_brush()$id, collapse = ", "))
    
  })
  
  output$choose_plot_numbergg <- renderUI({ # drop down menu, to choose plot to brush in
    
    req(inputData1())
    req(input$embs_vargg)
    
    n_dims <- length(colnames(inputData1()$embs)[which(gsub("_\\d+", "", colnames(inputData1()$embs)) == input$embs_vargg)])
    n_plots <- ((n_dims - 1) * (n_dims)) / 2
    
    plot_numbers <- seq_len(n_plots)
    
    selectInput("plot_numbergg", 
                label = "Plot to brush",
                choices = list(
                  `Plot number` = plot_numbers
                )
    )
    
  })
  
  ###### plot Matrixgg #######################
  
  brushedpoints_object <- reactiveVal(NULL) # brush
  ids_matgg_brush <- reactiveVal(NULL) # result of brushedpoints(df, brush)
  dim1_brush <- reactiveVal(NULL)
  dim2_brush <- reactiveVal(NULL)
  
  id_matgg_hover <- reactiveVal(NULL)
  
  observeEvent(input$undo_matgg, {
    brushedpoints_object(NULL) # brush
    ids_matgg_brush(NULL) # result of brushedpoints(df, brush)
    dim1_brush(NULL)
    dim2_brush(NULL)
  })
  
  # save the ids of the brush and the hovered id in their respective reactive values
  observe({
    
    req(inputData1())
    req(input$embs_vargg)
    req(input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]])
  
    n_dims <- sum(startsWith(colnames(inputData1()$embs), input$embs_vargg))
    n_plots <- ((n_dims - 1) * (n_dims)) / 2

    ###############
    
    dim_pairs <- combn(colnames(inputData1()$embs)[startsWith(colnames(inputData1()$embs), input$embs_vargg)], 2)
   
      if (!is.null(input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]])) {
        brush <- input[[paste0("plot_matgg_", input$plot_numbergg, "_brush")]]
      }
    
    plot_number <- as.numeric(input$plot_numbergg)
      
      if (!is.null(brush)) {
        if (nrow(brushedPoints(data.frame(dim1 = inputData1()$embs[[dim_pairs[1,plot_number]]],
                                          dim2 = inputData1()$embs[[dim_pairs[2,plot_number]]]), brush)) >= 1) {
          # no error message if it doesn't exist
          
          dat <- data.frame(dim1 = inputData1()$embs[[dim_pairs[1,plot_number]]],
                            dim2 = inputData1()$embs[[dim_pairs[2,plot_number]]],
                            id = inputData1()$embs[["id"]]
          )
          
          dim1_brush(dim_pairs[1,plot_number])
          dim2_brush(dim_pairs[2,plot_number])
          
          brushedpoints_object(brush)
          ids_matgg_brush(brushedPoints(dat, brush))
          
      }
      
    }
  }) # end observe (brush)
  
  
  output$plot4_matgg <- renderPlot({ # plot the curves
    
    req(input$colors_matgg_var)
    
    if (is.null(dim1_brush())) {
      df <- inputData1()$embs[ , 1:2]
    } else {
      df <- data.frame(dim1 = inputData1()$embs[ , dim1_brush()],
                       dim2 = inputData1()$embs[ , dim2_brush()],
                       id = inputData1()$embs[ , "id"])
    }
    
    
    brush <- NULL
    if (!is.null(ids_matgg_brush())) brush <- ids_matgg_brush()

    plot.funs_compact(input = inputData1(), df = df, hover = input$plot1_matgg_hover, brush = brushedpoints_object(), 
                      dblclick = input$plot1_matgg_dblclick, 
                      id_hover = id_hover(), ids_brush = brush, #ids_matgg_dblclick = NULL, 
                      colors_var = input$colors_matgg_var)
      
  })

  
  ############ plot all combs ################

  observe({
    
   req(input$embs_vargg)
   req(input$colors_matgg_var)
   req(input$shapes_matgg_var)
   req(input$point_size_matgg)
   
   meth <- input$embs_vargg
   
   
   dim_combs <- tryCatch(combn(colnames(inputData1()$embs)[which(gsub("_\\d+", "", 
                                                                      colnames(inputData1()$embs)) == meth)], 2), 
                         error = function(e)  dim_combs <- NULL) # when new data is loaded, to only re-plot when ready
   
   req(dim_combs)
   
    
    # define rows and cols of plots, and plot names (UI elements)
    output$plots_matgg <- buildMatggEmbs_plots(input = inputData1(), meth = meth)
    
    dim_combs <- t(as.data.frame(dim_combs))

    dim_combs <- cbind(dim_combs, as.character(1:nrow(dim_combs)))
    
    # plot the plots (server elements)
    if (is.null(ids_matgg_brush())) {
  apply(dim_combs, 1, function(dims) {
    output[[paste0("plot_matgg_", dims[[3]])]] <- drawPlot_all(dims, input = inputData1(), 
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
   
    output[[paste0("plot_matgg_", dims[[3]])]] <- drawPlot_all(dims, input = inputData1(), 
                                                               shapes = input$shapes_matgg_var,
                                                               col = isolate(input$colors_matgg_var),
                                                               sel_ids = ids_matgg_brush(), # these are the brushed points
                                                               high_color = input$high_color,
                                                            mat = TRUE,
                                                            size = input$point_size_matgg)
    
  }) # end apply
  
  }) # end observe
  
  #### Tab Data #######################################
  
  output$dataframe<- renderTable(striped = TRUE, { # Show table of uploaded data
    
    df <- subset(inputData1()$df, select = !unlist(map(inputData1()$df, is_tf))) # delete the tf-column
    
    if (!is.na(as.numeric(df$id[[1]]))) { # if the IDs are numbers, sort by number, not alphabetically
      df <- df[order(as.numeric(df$id)), ]
      
    }
    
    df
  })
  
  
} # end server

