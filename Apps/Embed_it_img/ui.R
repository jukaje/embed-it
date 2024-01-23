library(bslib)
library(ggplot2)
library(dbscan)
library(dplyr)
library(umap)
library(tidyverse)
library(plotly)
library(patchwork)
library(png)
library(data.table)
library(tidyfun)
library(checkmate)
library(shinyFeedback)
library(waiter)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)

for (file in list.files("R")) {
  source(paste0("R/", file))
}

##### UI #################
info_loading <- "Loading. Please wait..."
your_color01 <- "black" # define a color for the text
your_color02 <- "#e5eee9" # define a color for the background of the banner


ui <- fluidPage(
  # Apply the bslib theme
  theme = bslib::bs_theme(version = 4,  bg = "#fafbfb", fg = "rgb(0, 0, 0)", secondary = '#95A5A6', 
                          bootswatch = "flatly"),
  tags$style(' .well  { background-color: #e5eee9 !important;}'),
  

 ##### for the toolitp
 tags$head(tags$style(type="text/css",
                      paste0("
                                             #loadmessage {
                                             position: fixed;
                                             top: 0px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: ", your_color01,";
                                             background-color: ", your_color02,";
                                             z-index: 105;
                                             }
                                             "))),
 
 # show busy status
 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                  tags$div(info_loading,id="loadmessage")),
 
 useShinyFeedback(), # include shinyFeedback
 
  ##### Main UI layout with five tabs #################
  
  navbarPage(
   
    title=div(img(src=('logo2.png'), height=50, width=80), ""), # include Logo

    id = "navtabs",
    

    ##### Tab Viz: Image Data #################
    ### sidebar ----------------------------
    tabPanel(
      "Viz: Image Data",
      
      uiOutput("instructions_img_init"),
      uiOutput("instructions_img"),
      
      div(id = "app_info", class = "collapse out", 
          p("This tab enables you to plot two chosen embedding dimensions against each other. To explore 
            the visualization possibilities, click on the 'plot the plots' button to use the example dataset. 
            For information on the format for new data to upload, please move to the tab 'Data', and click on
            'Data set information'."),
        p(""),
        p("When plotting the embedding scatterplot in the top plot, some example images from the data set will be shown beneath 
          the scatterplot (if available). If the check box for 'Plotly' has been checked, there is a third plot, showing another version 
          of the embedding scatterplot, with additional in-plot options, e.g. zooming in and out, choosing points by variable
          (en- and disabling points by variable)."),
        p("By hovering above a point in the top scatterplot, the corresponding image will be shown underneath in the box titled 
          'hovered image'."),
        p(""),
        
        p("When hovering to see an image or selecting multiple images by brushing a selection of points in the scatterplot, 
          the ids of all selected points are printed in the box on the left hand side."),
        
        p(""),
        
        p("There are several options for changing the visual aspects of the scatterplot. They can be accessed by clicking
          'Plotting Options'."),
        p(tags$ul(tags$li("Choose the embedding dimensions to be plotted."),
                  tags$li("Color the points according to a chosen cariable (continuous or categorical)"),
                  tags$li("Label the points with the value of a chosen variable."),
                  tags$li("Differentiate the points by shape according to a chosen variable (categorical with six or 
                          less categories)"),
                  tags$li("By checking 'plot images', instead of points, the images will be shown directly in the 
                          scatterplot."),
                  tags$li("By checking 'Plotly', a third plot will be plotted (see above)."),
                  tags$li("When choosing a variable under 'Example Images by:', the example images shown under the plot
                          will have unique values for the chosen cariable, provided there are at least 3 unique values 
                          present."),
                  
        )
        ),
        
        p("All variables chosen in 'Plotting Options' will be indicated in the box appearing when hovering on a point 
          in the scatterplot."),
        
        p("For further information on the example data, see: "),
        tags$a(href = "https://www.kaggle.com/datasets/cactus3/basicshapes/", "https://www.kaggle.com/datasets/cactus3/basicshapes/")  
      ),
      p(""),
      
      #'more info'-button
      HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#app_info'><span 
           class='glyphicon glyphicon-collapse-down'></span> More information </button>"),
      p(""),
      
      sidebarLayout(
        sidebarPanel(width = 3,
                     
                     shinyjs::useShinyjs(),
                     
                     actionButton("settings_img", label = "Plotting Options", icon = icon("sliders-h"), class = "btn-light"),
                     
                     HTML(r"(<br><br>)"), 
                     
                     
                     shinyjs::useShinyjs(),
                     uiOutput("selPlot1_img_ui"),
                     
                     uiOutput("toggles_img"),
                     
                     actionButton("submit_img", "Plot the plots", icon = icon("chart-line"), class = "btn-primary"),
                     
                     # box for the id
                     div(
                         style = 'padding-top: 10px; padding-bottom:2px',
                         width = '20%',
                         verbatimTextOutput("brushed_ids_names_img")
                     ), # end div
                     
                     tags$head( # show colors in drop-down menu
                       tags$style(HTML("
        .option[data-value=hotpink], .item[data-value=Pink]{
          background: hotpink !important;
          color: white !important;
        }
        .option[data-value=green], .item[data-value=Pink]{
          background: rgba(0,255,0,1) !important;
          color: white !important;
        }
        .option[data-value=red], .item[data-value=Pink]{
          background: red !important;
          color: white !important;
        }
        .option[data-value=blue], .item[data-value=Pink]{
          background: blue !important;
          color: white !important;
        }
  "))
                     ),
                     
                     
        ), # end sidebarPanel imgs
        
        ### Plots -------------------------------
        mainPanel(
          # Plots for Tab Image Data
          
          fluidRow(
            column(width = 12, class = "plot-container", 
                   div(
                     style = "position:relative",
                   
                           plotOutput("plot1_2",
                                     brush = brushOpts(id = "plot1_brush_img", fill = "black"),
                                     hover = hoverOpts(id = "plot1_2_hover"),
                                    dblclick = "plot1_2_dblclick"),
                   
                   uiOutput("hover_info_img", style = "pointer-events: none")
                       ), #end div
                   style="border:1px solid #e5eee9"
            ) # end column
          ), #end fluidRow
          
          uiOutput("img_titles"),
          
          fluidRow(
            style = 'padding-top: 30px; padding-bottom:2px',
            
              column(width = 3, class = "plot-container", plotOutput("plot3_2_1")),
              column(width = 3, class = "plot-container", plotOutput("plot3_2_2")),
              column(width = 3, class = "plot-container", plotOutput("plot3_2_3")),
            
            column(width = 3, class = "plot-container", plotOutput("plot4_2"), style="border:1px solid #e5eee9")
            
            
          ),
          
          fluidRow(
            style = 'padding-top: 30px; padding-bottom:2px',
            column(width = 12, class = "plot-container", plotlyOutput("plot5_2")),
          )
          
          
        ) # end mainPanel
      ) # end sidbarLayout
    ), # end Tab 'Image Data' tabPanel
   
   ##### allTab #################
   tabPanel(
     "Viz: Embeddings",
     waiter::use_waiter(),
     # 
     div(id = "app_info2", class = "collapse out", 
         p("This tab plots all given embedding dimensions of corresponding methods against each other.
        Click on the 'Plot the plots' button to plot them, you can use either the example dataset or upload your own in the 
        'Data' tab."),
        p(""),
        p("Each possible combination results in a trio: On the left, the embedding scatterplot. On the right,
          the represented image is plotted when hovered on. Beneath, there is a field with
          information on the ids of brushed and hovered points."),
        
        p(""),
        
        p("There are several options for changing the visual aspects of the scatterplot. They can be accessed by clicking
          'Plotting Options'."),
        p(tags$ul(
          tags$li("Color the points according to a chosen variable (continuous or categorical)."),
          tags$li("Label the points with the value of a chosen variable."),
          tags$li("Differentiate the points by shape according to a chosen variable (categorical with six or 
                          less categories)."),
          tags$li("By checking 'Plot images', instead of points, small images are shown in the 
                          scatterplots."),
          tags$li("Using the slider lets you set the size of the points in the scatterplots to make them
                          more easily differentiable or visible."),
          
        )
        ),
        
     ),
     
     p(""),
     
     
     HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#app_info2'><span 
           class='glyphicon glyphicon-collapse-down'></span> More information </button>"),
     p(""),
     
     sidebarLayout(
       sidebarPanel( width = '20%',
                     
                     uiOutput("toggles_all"),
                     
                     actionButton("submit_all", "Plot the plots", icon = icon("chart-line")),
       ),
       
       
       mainPanel(
         # Plots for Tab 1
         ########### uiOutput("plots") ######################
         # build them depending on the number of dims in the embeddings in the data
         
         uiOutput("plots_all")
         
         
         
       )
     )
   ),
    
    
  ##### Tab Matrix #################
  
  tabPanel(
    "Viz: Matrix",
    
    tags$head( # show colors in drop-down menu
      tags$style(HTML("
        .option[data-value=hotpink], .item[data-value=Pink]{
          background: hotpink !important;
          color: white !important;
        }
        .option[data-value=green], .item[data-value=Pink]{
          background: rgba(0,255,0,1) !important;
          color: white !important;
        }
        .option[data-value=red], .item[data-value=Pink]{
          background: red !important;
          color: white !important;
        }
        .option[data-value=blue], .item[data-value=Pink]{
          background: blue !important;
          color: white !important;
        }
  "))
    ),
  
  
  div(id = "app_info", class = "collapse out",
      p("This tab plots all given dimensions of a chosen 'Embedding method' against each other. The plots are arranged
        as a scatterplot matrix, to facilitate insights into spatial patterns."), 
      p(""),
  
      p("As in the tabs 'Image Data' and 'Embeddings', you can choose points by brushing in the plots:"),
      
      p(tags$ul(tags$li("Choose a collection of points by brushing in the first plot. All
                        selected points are highlighted in each scatterplot. Beneath the plotting options on the left side,
                        there is a field with information on the ids of brushed points."),
                tags$li("To change the plot which you want to select points in, use the drop down menu called 'Plot to brush'
                      and select the desired plot number."),
                tags$li("If you have a selection of points, and would like to see their position in other embedding methods,
                      just change the 'Embedding method'. The chosen points will still be highlighted."),
                tags$li("To undo the brushed selection and see the original plots, click the button 'Undo brush'.")
                
      )),
      
      p(""),
      
      p("There are a couple of options for changing the visual aspects of the scatterplot. They can be accessed by clicking
          'Plotting Options'."),
      p(tags$ul(
        tags$li("Color the points according to a chosen variable (continuous or categorical)."),
        tags$li("Differentiate the points by shape according to a chosen variable (categorical with six or
                          less categories)."),
        tags$li("By changing the 'Color to highlight brushed points', these points might be more visible, depending
                on the variables chosen to be represented by color."),
        tags$li("Using the slider lets you set the size of the points in the scatterplots to make them
                          more easily differentiable or visible."),
        
      )
      ),
      
  ),
  
  p(""),
  
  
  HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#app_info'><span
           class='glyphicon glyphicon-collapse-down'></span> More information </button>"),
  p(""),
  
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 div(style = "height: 80%; background-color: #e5eee9; padding: 10px;",
                     
                     shinyjs::useShinyjs(),
                     
                     actionButton("settings_matgg", label = "Plotting Options",
                                  icon = icon("sliders-h"), class = "btn-light"),
                     
                     HTML(r"(<br><br>)"),
                     
                     uiOutput("toggles_matgg"),
                     
                     uiOutput("choose_plot_numbergg"),
                     
                     selectInput("high_color",
                                 label = "Color to highlight brushed points", 
                                 choices = list(
                                   "Pink" = "hotpink",
                                   "Green" = "green",
                                   "Blue" = "blue",
                                   "Red" = "red"
                                 )
                     ),
                     
                     actionButton("undo_matgg", " Undo brush", icon = icon("trash"), 
                                  style = "color: #fff; background-color: #91c7b8"),
                     
                     
                     
                 ), # end div()
                 
                 div(
                   
                   style = 'padding-top: 10px; padding-bottom:2px',
                   width = '20%',
                   verbatimTextOutput("brushed_ids_printgg")
                 ) # end div
                 
    ), # end sideBarPanel
    
    mainPanel(
      
      ########### uiOutput("plots") ######################
      
      fluidRow(
        column(width = 12, class = "plot-container",
               div(
                 style = "position:relative",
                 
                 uiOutput("plots_matgg")
                 
               ),
               style="border:1px solid #e5eee9"),
        
      ),
      
      fluidRow(
        style = 'padding-top: 10px; padding-bottom:2px',
        column(width = 12, class = "plot-container", plotOutput("plot4_matgg"),
               style="border:1px solid #e5eee9"),

      )
      
    ) # end mainPanel
  ) # end sidebarLayout
    
 
  ), # end TabPanel Matrix
  
  ##### Tab Data #################
  tabPanel(
    "Data",
    
    div(id = "data_info", class = "collapse out", 
        p("The data set being uploaded has to have a certain format. Please upload a .rds dataframe. The embedding 
            columns should be preceded by 'embs_', the variable columns by 'vars_'. Categorical variables are stored 
            as factors. The column storing the functional data is stored a a tf-object, see", 
          tags$a(href = "https://tidyfun.github.io/tidyfun/", "the tidyfun package.")),
        
        p("For more detailed information and helper functions, see",
          tags$a(href = "https://github.com", "the package or the github page.")),
        
        
        p(""),
    ),
    
    p(""),
    
    HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#data_info'><span 
           class='glyphicon glyphicon-collapse-down'></span> Data set information </button>"),
    p(""),
    
    
    fluidRow( 
      column(width = 6,
             div(
               style = "color:#e5eee9 !important;",

               tags$style(
                 HTML('
                   #dataframe table tr:nth-child(odd) {
                     background-color: #e5eee9;
                   }

                   #dataframe table tr:nth-child(even) {
                     background-color: #FFFFFF;
                   }
                   
                    #dataframe table th {
                     background-color: #2C3E50;
                     color: white; /* Set text color for column names */
                   }

                   #dataframe table td {
                     color: #333;
                   }
                 ')
               ),
               
               fileInput("file_data",
                         "Upload Data:",
                         buttonLabel = "Upload...",
                         accept = c(".rds")
               ),
               
               tableOutput("dataframe"))
      )
      
      
      # )
      
      
    )
    # )
  ), # end TabPanel Data
  
  
  ########## Tab "About" ################
  tabPanel(
    "About",
    
    p("Dimensionality reduction techniques allow the visualization of complex data like image datasets. Given a data set
      of image data and its embeddings, this app offers a variety of visualization options to handily explore
      the embedding space. In a 2d embedding each dot represents an image. In this graph you can see an 
      example as seen in the tab", strong("Viz: Image data"),
      ". All the dots representing the images are plotted in the upper plot
      as a scatter plot. Some example images are shown beneath the scatter plot. By hovering on a dot you can see 
      the represented image."),
    
    img(src = 'help_1.jpg', align = "center"),
    
    p("The first tab", strong("Viz: Image data"), "plots a single plot of two chosen embedding dimensions, and offers a wider variety
      of plotting options.
      The tab ", strong("Viz: Embeddings"), "plots all dimensions of all given embedding  methods against each other. This gives an overview of
      the results of all the given embeddings. The tab", strong("Viz: Matrix"), "plots all dimensions of a chosen embedding method in a scatter
      plot matrix for intuitive visualization of embedding dimensionalities that are higher than 2D.
      The tab" , strong("Data"), "shows the currently loaded dataset and lets you upload your own.
      
      For more detailed information on the availilable functionalities in each tab, click on" , strong("More information"), "."),

    img(src = 'help_2.jpg', align = "center"),
    
    
    p("This app has been developed as part of a master's thesis on the visualization of high-dimensional data. 
      See:"),             
    tags$a(href = "https://github.com/jukaje/embed-it", "github page"),
    p(""),
    p("For further information, please contact Judith Jennert at: judith@jennert.net"),

  ) # end Tab Panel "About"
  
) # end navBarPage
) # end UI


