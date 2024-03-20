library(shiny)
library(DT)
library(readxl)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Tesis - Cuadro de Mando"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Elige Archivo Excel", accept = ".xlsx"),
      uiOutput("checkboxes"),
      uiOutput("filters")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read Excel file
  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # Dynamically create checkboxes for column selection
  output$checkboxes <- renderUI({
    req(data())
    
    # Exclude Matriculados and % Mujeres from checkboxes choices
    choices <- setdiff(names(data()), c("Matriculados", "% Mujeres"))
    
    checkboxGroupInput("group_by_columns", "Select columns to group by:",
                       choices = choices, selected = NULL)
  })
  
  # Dynamically create dropdown menus for filtering
  output$filters <- renderUI({
    req(data())
    
    # Filter options for Rama, Curso, and Comunidad autónoma
    rama_options <- unique(data()$Rama)
    curso_options <- unique(data()$Curso)
    comunidad_options <- unique(data()$`Comunidad autónoma`)
    
    # Create dropdown menus
    fluidRow(
      column(4, selectizeInput("filter_rama", "Filter by Rama:", choices = c("All", rama_options), multiple = TRUE, selected = "All")),
      column(4, selectizeInput("filter_curso", "Filter by Curso:", choices = c("All", curso_options), multiple = TRUE, selected = "All")),
      column(4, selectizeInput("filter_comunidad", "Filter by C.A:", choices = c("All", comunidad_options), multiple = TRUE, selected = "All"))
    )
  })
  
  # Create summary table based on selected group by columns and filters
  output$table <- renderDT({
    req(data())
    
    # Filter data based on dropdown menus
    filtered_data <- data() %>%
      filter(if ("All" %in% input$filter_rama) TRUE else Rama %in% input$filter_rama,
             if ("All" %in% input$filter_curso) TRUE else Curso %in% input$filter_curso,
             if ("All" %in% input$filter_comunidad) TRUE else `Comunidad autónoma` %in% input$filter_comunidad)
    
    # Convert character columns to numeric
    educ <- filtered_data
    educ$Matriculados <- as.numeric(educ$Matriculados)
    educ$`% Mujeres` <- as.numeric(educ$`% Mujeres`)
    
    # Check if group by columns are selected
    if (length(input$group_by_columns) > 0) {
      # Group by selected columns and calculate mean of Matriculados and % Mujeres
      summarized_data <- educ %>%
        group_by(across(all_of(input$group_by_columns))) %>%
        summarize(Matriculados = sum(Matriculados, na.rm = TRUE),
                  `% Mujeres` = mean(`% Mujeres`, na.rm = TRUE)) %>%
        # Format Matriculados to have zero decimals
        mutate(Matriculados = round(Matriculados, 0)) %>%
        # Format % Mujeres to be in 100% and two decimal places
        mutate(`% Mujeres` = round(`% Mujeres` * 100, 2))
    } else {
      # If no group by columns selected, show the data without summarization
      summarized_data <- educ %>%
        # Format Matriculados to have zero decimals
        # Format % Mujeres to be in 100% and two decimal places
        mutate(`% Mujeres` = round(`% Mujeres` * 100, 2))
    }
    
    datatable(summarized_data, options = list(format = "unformat")) %>%
      # Apply formatting to Matriculados and % Mujeres columns
      formatStyle(columns = c("Matriculados", "% Mujeres"),
                  `border-collapse` = 'collapse',
                  `font-size` = '90%',
                  `text-align` = 'center')
  })
}

# Run the application
shinyApp(ui = ui, server = server)


