#================================================================
# Author: William O. Agyapong
# Purpose: A demo Shiny application development: Feel free to
# customize it as you wish. Happy coding!
# Date created: 2024-03-18
# Date modified: 2024-03-18
#=================================================================

# load required packages
library(shiny)
library(dplyr)
library(ggplot2)

# load data
data("iris")

# Do some initial data preprocessing as needed
var_names <- names(iris)
num_vars <- var_names[-length(var_names)]
iris_first <- iris[1,]



#===== Define the user interface (UI)
frontend <- fluidPage(
  
  title = "Iris Awesome Dashboard",
  
  #------ create a custom header
  # Use fluidRow() to define a row across the browser and use the column() function
  # inside it to appropriately allocate the 12 grid system available to you.
  # With every fluidRow() giving us 12 boxes, a quick way to obtain 2 sections
  # of varying widths is to allocate 4 boxes to the first and remaining 8 to the 
  # last column. Note, this allocation is informed by our design choice! Feel
  # free to choose any allocation of your choice as best suit your design.
  # To learn about more style options like background color, text color, etc., visit:
  # https://www.w3schools.com/css/default.asp; https://www.w3schools.com/cssref/index.php
  fluidRow(style="background:purple; color:white;",
    column(4,
           h3(paste0("Welcome, ", Sys.getenv("USERNAME"),Sys.getenv("USER"),"!"))
           ),
    column(8, style = "text-align:center; text-decoration:underline; color:lightblue;",
      h1("Iris Flower Dashboard")
    )
    
  ),

  #------ layout the page
  # You can nest as many tabPanel() as you need inside the tabsetPanel()
  # to create different pages for the app
  # Within each tabPanel(), page, use the sideBarLayout() together with the children
  # sidebarPanel() and mainPanel() elements to further divide each page into a 
  # sidebar for the user input controls (left) and the output (right, i.e. plot
  # and table areas).
  
  tabsetPanel(
    # first page
    tabPanel("Explore",style="margin-top:20px",
             sidebarLayout(
               sidebarPanel(
                 varSelectInput("xvar", "X-axis variable",
                                data = iris,
                                selected = num_vars[1]
                 ),
                 
                 varSelectInput("yvar", "Y-axis variable",
                                data = iris,
                                selected = num_vars[2]
                 ),
                 
                 
                 hr(),
                 # plot title and alignment
                 textInput("plot_title", "Title", value = "Relationship between iris attributes"),
                 sliderInput("title_align", "Align Title", 
                             min = 0, max = 1,  value = 0.5
                             ),
                 
                 # color by species
                 hr(),
                 checkboxGroupInput(
                   "species", "Filter by species",
                   choices = unique(iris$Species),
                   selected = unique(iris$Species)
                 ),
                 
                 # legend position 
                 hr(),
                 selectInput("legend_position", "Legend Position",
                             choices = c("bottom", "top", "right", "left", "none")
                 )
                 
                 
               ),
              
              mainPanel(
                plotOutput("scatter"),
                
                fluidRow( style="background-color:#f5f5f5; padding:19px;
                          border: 1px solid #e3e3e3; border-radius:4px;",
                  column(6,
                         # shape type
                         numericInput("shape_type", "Shape type (0-25)",
                                      min = 0, max = 25, value = 16),
                         # shape size
                         sliderInput("shape_size", "Shape size",
                                     min = 1, max = 8, value = 3
                         )
                         )
                )
              )
             )
      ),
    
    # Iris flower plant image gallery
    tabPanel("Gallery", style="margin-top:20px",
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel(
                 fluidRow(
                   column(4,
                          img(src="iris-flower.jpeg", height="200", width="200")
                   ),
                   column(4,
                          img(src="iris-flower.jpeg", height="200", width="200")
                   ),
                   column(4,
                          img(src="iris-flower.jpeg", height="200", width="200")
                   )
                 )
               )
             ),
      
    ),
    
    # Classification of iris plant
    tabPanel("Classify Iris", style = "margin-top:20px;",
      sidebarLayout(
        sidebarPanel(
          h3("Features:", style="color:black;"),
          numericInput("sepal_length", "Sepal Length",
                       value = iris_first$Sepal.Length),
          
          numericInput("sepal_width", "Sepal Width",
                       value = iris_first$Sepal.Width),
          
          numericInput("petal_length", "Petal Length",
                       value = iris_first$Petal.Length),
          
          numericInput("petal_width", "Petal Width",
                       value = iris_first$Petal.Width)
        ),
        
        mainPanel(
          fluidRow(
            column(6,
                   h3("Predicted Species"),
                   textOutput("predicted_species")
                   ),
            column(6,
                   h3("Model Summary"),
                   verbatimTextOutput("model_summary")
                   )
          )
          
        )
      )
    ),
    
    #------ Data download panel
    # Codes adapted from the Shiny feature demo examples available
    # through the execution of runExample() function to demonstrate how you
    # can utilize existing codes as plugins in your own applications. 
    # 1. First run runExample() in the RStudio console. This shows you all 
    # the available examples to choose from.
    # 2. Copy or type in the name of the example you want like so:
    # > runExample("10_download")
    # Copy and paste the UI part of the codes into the sidebarPanel() as 
    # done below. Remember to declare the output UI, tableOutput("data_preview"),
    # inside the mainPanel(). 
    #Next, copy and paste the server side codes into the server function,
    # here named "backend".
    tabPanel("Data Download", style="margin-top:20px",
     sidebarLayout(
       sidebarPanel(
         # Input: Choose dataset ----
         selectInput("dataset", "Choose a dataset:",
                     choices = c("iris", "mtcars","pressure", "cars")),
         # download button
         downloadButton("download_data", "Download")
       ),
       mainPanel(
         tableOutput("data_preview")
       )
     )
  ),
  
  #------ file upload panel
  # Codes obtained directly from the Shiny feature demo examples available
  # through the execution of runExample() function to demonstrate how you
  # can utilize existing codes as plugins in your own applications. 
  # 1. First run runExample() in the RStudio console. This shows you all 
  # the available examples to choose from.
  # 2. Copy or type in the name of the example you want like so:
  # > runExample("09_upload")
  # Copy and paste the UI part of the codes into the sidebarPanel() as 
  # done below. Remember to declare the output UI, tableOutput("contents"),
  # inside the mainPanel(). 
  # Next, copy and paste the server side codes into the server function,
  # here named "backend".
  tabPanel("File Upload", style="margin-top:20px",
           sidebarLayout(
             sidebarPanel(
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head")
               
             ),
             mainPanel(
               # Output: Data file ----
               tableOutput("contents")
             )
           )
  )
  
),
  
  
  
  #------ footer section
  # Observe how the footer section is declared outside the tabSetPanel() 
  # to make it available on all pages.
  # Use fluidRow to define a row across the browser and use the column() function
  # to appropriately allocate the 12 grid system available to you.
  # With every fluidRow() giving us 12 boxes, a quick way to obtain 3 sections
  # of equal width is to allocate 4 boxes to each column.
  fluidRow(
    style = "background:purple; height:100px; color:white;
   clear:both; position: relative; margin-top:-100px; padding-top:30px;
    ",
    column(4,
           span("William Agyapong, all rights reserved"),
           HTML("&copy;", strsplit(as.character(Sys.Date()), "-")[[1]][1])
           ),
    
    column(4,
           a("Data-Tech Explorer YouTube channel",
             href="https://www.youtube.com/channel/UC_9MyIqG8b7H40dJYXebjtQ",
             target = "_blank")
           ),
    column(4,
           a("williamagyapong@github.com", 
             href="https://www.github.com/williamagyapong",
             target = "_blank")
           )
  )
)


#===== Define the back end logic (server)
# This is where the magic begins. The backend (server) function helps bind 
# user inputs to the anticipated outputs.
# Every data processing or analysis computations must be done in a reactive 
# environment. Use the reactive({--}) function to create your own reactive expressions
# (can think of these are variables that can be used in other reactive environments),
# and custom made reactive contexts such as renderPlot(), renderTable(), and 
# renderPrint() to target the respective output endpoints in the UI. For example,
# renderplot() corresponds to plotOutput() in the UI definition above, while
# renderTable() corresponds to tableOutput().
backend <- function(input, output) {

  # prepare data 
  # This creates a special variable that can be used in different parts of the 
  # server. Remember that such variables must be called as if they are functions,
  # like so: iris_prepared()
  iris_prepared <- reactive({
    iris |> filter(Species %in% input$species
                   )
  })


  output$scatter <- renderPlot({
    # Good to start with a static plot and progressively add the interactive 
    # features: 
    # ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    #   geom_point()

    ggplot(iris_prepared()) +
      geom_point(aes(x=!!input$xvar, y=!!input$yvar, color=Species),
                 size=input$shape_size, shape=input$shape_type) +
      labs(title = input$plot_title, color="Species key: ") +
      theme_bw() +
      theme(legend.position = input$legend_position,
            plot.title = element_text(
              hjust = input$title_align
            )
            )
  }, res = 96)
  
  
  #------ downloading data
  # Notice another custom make reactive context.
  dataset_input <- reactive({
    switch (input$dataset,
      "iris" = iris,
      "mtcars" = mtcars,
      "cars" = cars,
      "pressure" = pressure
    )
  })
  
  # preview selected data
  output$data_preview <- renderTable({
    dataset_input()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      write.csv(dataset_input(), file = file, row.names = FALSE)
    }
  )
  
  #------ uploading data
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })

}

# Run the application
shinyApp(ui=frontend, server=backend)



