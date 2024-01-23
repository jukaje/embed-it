library(bslib) # version 0.3.1
library(ggplot2) # version ‘3.3.5’
library(dbscan)  # version ‘1.1.11’
library(dplyr)  # version ‘1.1.4’
library(umap)  # version  ‘0.2.10.0’
library(tidyverse)  # version  ‘1.3.1’
library(plotly)  # version  ‘4.10.1’
library(patchwork)  # version ‘1.1.2’
library(png)  # version ‘0.1.7’
library(data.table)  # version ‘1.14.2’
library(tidyfun)  # version ‘0.0.97’
library(checkmate)  # version ‘2.0.0’
library(shinyFeedback)  # version ‘0.4.0’
library(waiter)  # version ‘0.2.5’
library(shinycssloaders)  # version ‘1.0.0’
library(shinyalert)  # version ‘3.0.0’
library(shinyjs)  # version ‘3.0.0’

# shiny version ‘1.7.1’

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
    
    ##### Tab Functional Data #################
    tabPanel(
      "Viz: Functional Data",
      
      uiOutput("instructions_init"),
      uiOutput("instructions"),
      

      div(id = "app_info1", class = "collapse out", 
      p("This tab makes it possible to plot two chosen embedding dimensions. To explore the visualization possibilities,
        click on the 'Plot the plots' button to use the example dataset or upload your own in the 
        'Data' tab. For information on the format for new data to 
        upload, please move to the tab 'Data', and click on 'More information'."),
        p(""),
        p("When plotting the embedding scatterplot in the top plot, the represented functions are plotted in the bottom
          plot (if available). If the check box for 'Plotly' has been checked, there is a third plot, showing another version 
          of the embedding scatterplot, with additional in-plot options, e.g. zooming in and out, choosing points by variable
          (en- and disabling points by variable)."),
        p("By hovering above a point in the top scatterplot, the corresponding function is highlighted in the function 
          plot. This functionality can be disabled by clicking the 'disable hover' check box. For highlighting multiple 
          functions in the function plot, there are two possibilities:"),
        p(""),
      
        p(tags$ul(tags$li("Choose a collection of points by brushing (via click and drag) in the top plot. All curves 
                          represented by the brushed points are highlighted."),
                  tags$li("Choose all points of a certain variable value indicated by color by double clicking on a point. 
             All curves with that variable characteristic are highlighted, as well as the selected scatterplot points.")
                  
                  )),
        p("The ids of all selected points are printed in the box beneath the function plot."),
      
        p(""),
        
        p("By clicking 'Plotting Options' you can change the visual depiction of the scatterplot:"),
        p(tags$ul(tags$li("Choose the embedding dimensions to be plotted."),
                  tags$li("Color the points according to a chosen variable (continuous or categorical)"),
                  tags$li("Label the points with the value of a chosen variable."),
                  tags$li("Differentiate the points by shape according to a chosen variable (categorical with six or 
                          less categories)"),
                  tags$li("By checking 'Glyph plot', instead of points, small curve plots are shown in the 
                          scatterplot."),
                  tags$li("By checking 'Plotly', a third plot is plotted (see above)."),
                  tags$li("By entering a title in 'Custom scatter plot title', you can change the title shown in the 
                          scatterplot."),
                  tags$li("By entering a title in 'Custom functions plot title', you can change the title shown in the 
                          plot showing the functions."),
                  tags$li("Using the slider lets you set the size of the points in the scatterplot. This can be be 
                          helpful in case there are too many points to differentiate, for example."),
                  tags$li("By changing the 'Color to highlight double clicked points', these points might be more visible,
                          depending on the variables chosen to be represented by color."),
                  
                  )
          ),
      
        p("All variables chosen in 'Plotting Options' are indicated when hovering on a point in the scatterplot."),
      
        p("For further information on the example data, see the publication by the Helmholtz center Munich: "),
           tags$a(href = "https://zenodo.org/records/5779876#.YjsK8TW1Kwf", "https://zenodo.org/records/5779876#.YjsK8TW1Kwf")  
       ),
       p(""),
               
      #'more info'-button
      HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#app_info1'><span 
           class='glyphicon glyphicon-collapse-down'></span> More information </button>"),
      p(""),
      
      sidebarLayout(
        sidebarPanel(width = 3,
                     div(style = "height: 100%; background-color: #e5eee9; padding: 10px;",

                      shinyjs::useShinyjs(),
                      
                      actionButton("settings", label = "Plotting Options", icon = icon("sliders-h"), class = "btn-light"),
                      
                      HTML(r"(<br><br>)"), 

                      
                      uiOutput("selPlot1_ui"),
                      
                      
                      uiOutput("toggles"),
                    
                      actionButton("submit", "Plot the plots", icon = icon("chart-line"), class = "btn-primary"),
                    
                   
                      
                     ), # end div()
                     
                    # build a box with extra global options
                     div(style = "border: 1px solid #ccc; height: 20%; background-color: #e5eee9; padding: 10px;",

                           width = '20%',
                           tags$p(strong("Global Options:")),
                         
                         checkboxInput(
                           inputId = "hover_off",
                           label = "Disable hover",
                           value = FALSE
                         ),
                         
                        # ), # end sidebarPanel
                     ) # end div()
                   
        ), # end sidebarPanel
        
        
        mainPanel( 
          # Plots for Tab Functional Data
          ########### uiOutput("plots") ######################
          
          fluidRow(
            column(width = 12, class = "plot-container", 
                   div(
                     style = "position:relative",
                     
                     plotOutput("plot1",
                                brush = brushOpts(id = "plot1_brush", fill = "black"),
                                hover = hoverOpts(id = "plot1_hover", delay = 500, delayType = "debounce"),
                                dblclick = "plot1_dblclick"
                                ),
                     
                     uiOutput("hover_info", style = "pointer-events: none")
                   ),
                   style="border:1px solid #e5eee9"),
          ),
          
          fluidRow(
            style = 'padding-top: 10px; padding-bottom:2px',
            column(width = 12, class = "plot-container", plotOutput("plot4"),
                   style="border:1px solid #e5eee9"),
            
          ),
          
          fluidRow(
            style = 'padding-top: 10px; padding-bottom:2px',
            wellPanel(
            column(width = 12, class = "text-container", verbatimTextOutput("brushed_ids_names"))
            )
          ),
          
          fluidRow(
            style = 'padding-top: 10px; padding-bottom:2px',
            column(width = 12, class = "plot-container", plotlyOutput("plot5")),
            
          ),
          
        ) # end mainPanel
      ) # end sidebarLayout
    ), # end tabPanel Tab 'Functional Data'
    
    
   ##### allTab #################
   tabPanel(
     "Viz: Embeddings",
     waiter::use_waiter(),
     
     div(id = "app_info2", class = "collapse out", 
         p("This tab plots all given embedding dimensions of corresponding methods against each other.
        Click on the 'Plot the plots' button to plot them, you can use either the example dataset or upload your own in the 
        'Data' tab."),
        p(""),
        p("Each possible combination results in a trio: On the left, the embedding scatterplot. On the right,
          the represented functions are plotted when hovered on or brushed. Beneath, there is a field with
          information on the ids of brushed and hovered points."),
        p("By hovering above a point in the left scatterplot, the corresponding function is highlighted in the function 
          plot. For highlighting multiple functions in the 
          function plot, you can:"), 
        p(""),
        
        p(tags$ul(tags$li("Choose a collection of points by brushing (click and drag) in the left plot. All curves 
                          represented by the brushed points are then highlighted."),
             
        )),
        
        p(""),
        
        p("There are several options for changing the visual aspects of the scatterplot."),
        p(tags$ul(
                  tags$li("Color the points according to a chosen variable (continuous or categorical)."),
                  tags$li("Label the points with the value of a chosen variable."),
                  tags$li("Differentiate the points by shape according to a chosen variable (categorical with six or 
                          less categories)."),
                  tags$li("By checking 'Glyph plot', instead of points, small curve plots are shown in the 
                          scatterplot."),
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
                    #loadingButton("submit_all", "plot the plots")
       ),
       
       
       mainPanel(
         # Plots for Tab
         ########### uiOutput("plots") ######################
         # build them depending on the number of dims in the embeddings in the data
         
         uiOutput("plots_all")
         
         
         
       )
     )
   ),
    
    

  ##### Tab Matrixgg #################

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
      p("All the curves represented by the points in the scatterplots are plotted above in a separate plot."),
      p(""),
      p("As in the tabs 'Functional Data' and 'Embeddings', you can choose points by brushing in the plots:"),

      p(tags$ul(tags$li("Choose a collection of points by brushing in the first plot. All curves represented by the
                        selected points are highlighted. Beneath the plotting options on the left side,
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
      #                   less categories)"),
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

                       #uiOutput("selPlot1_matgg_ui"),

                       uiOutput("toggles_matgg"),

                       uiOutput("choose_plot_numbergg"),

                       selectInput("high_color",
                                   label = "Color to highlight brushed points", # gsub("_\\+", "", id) | str_to_sentence(id)
                                   choices = list(
                                     "Pink" = "hotpink",
                                     "Green" = "green",
                                     "Blue" = "blue",
                                     "Red" = "red"
                                   )
                       ),

                        actionButton("undo_matgg", " Undo brush", icon = icon("trash"), #class = "btn-secondary",
                                     #trash, ban, eraser, brush, backward
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
          style = 'padding-top: 10px; padding-bottom:2px',
          column(width = 12, class = "plot-container", plotOutput("plot4_matgg"),
                 style="border:1px solid #e5eee9"),
          
        ),
        
        fluidRow(
          column(width = 12, class = "plot-container",
                 div(
                   style = "position:relative",

                   uiOutput("plots_matgg")

                 ),
                 style="border:1px solid #e5eee9"),

        ),



      ) # end mainPanel
    ) # end sidebarLayout



  ), # end tabPanel matrixgg

  
  ##### Tab Data #################
  tabPanel(
    "Data",
    
    div(id = "data_info", class = "collapse out", 
        p("The data set must be a .rds dataframe. The embedding 
            columns should be preceded by 'embs_', the variable columns by 'vars_'. Categorical variables are stored 
            as factors. The column storing the functional data is stored as a tf-object, see", 
          tags$a(href = "https://tidyfun.github.io/tidyfun/", "the tidyfun package.")),
        
        p("For more detailed information and helper functions, see",
          tags$a(href = "https://github.com", "the package or the github page.")),
        
        
        p(""),
    ),
    
    p(""),
    
    
    
    HTML("<button type='button' class='btn' data-toggle='collapse' style='float:center' data-target='#data_info'><span 
           class='glyphicon glyphicon-collapse-down'></span> More information </button>"),
    p(""),
    
    
    fileInput("file_data",
              "Upload Data:",
              buttonLabel = "Upload...",
              accept = c(".rds")
    ),
    
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
               
               tableOutput("dataframe"))
      )
      
      
    )
    
  ), # end TabPanel Data
  
  ########## Tab "About" ################
  tabPanel(
    "About",
    
    p("Dimensionality reduction techniques allow the visualization of complex data like functional data. Given a data set
      of functional data and its embeddings, this app offers a variety of visualization options to handily explore
      high - dimensional functional data. In a 2d embedding each dot represents a function. In this graph you can see an 
      example as seen in the tab", strong("Viz: Functional data"),
    ". All the dots representing the functions are plotted in the upper plot
      as a scatter plot. The lower plot shows the functions that are being represented. By brushing using drag and click 
      in the upper plot or by hovering on a dot you can see the function highlighted in the lower plot."),
    
    
    # include image
    img(src = 'info_connect.png', align = "center"),
    
    p("The first tab", strong("Viz: Functional data"), "plots a single plot of two chosen embedding dimensions, and offers a wider variety
      of plotting options.
The tab ", strong("Viz: Embeddings"), "plots all dimensions of all given embedding methods against each other. This gives an overview of
the results of all the given embeddings. The tab", strong("Viz: Matrix"), "plots all dimensions of a chosen embedding method in a scatter
plot matrix for intuitive visualization of embedding dimensionalities that are higher than 2D.
The tab" , strong("Data"), "shows the currently loaded dataset and lets you upload your own.

For more detailed information on the availilable functionalities in each tab, click on" , strong("More information"), "."),
    
 # include image
img(src = 'more_infoinfo.png', align = "center"),
    
    p("This app has been developed as part of a master's thesis on the visualization of high-dimensional data. 
      See:"),             
    tags$a(href = "https://github.com/jukaje/embed-it", "https://github.com/jukaje/embed-it"),
    p(""),
    p("For further information, please contact Judith Jennert at: judith@jennert.net"),


    
  ) # end Tab Panel "About"
  
) # end navBarPage
) # end UI

