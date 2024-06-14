library(shiny)
library(DT)
library(readxl)
library(dplyr)


ui <- fluidPage(
  titlePanel("Tesis - Cuadro de Mando"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "", accept = ".xlsx"),
      uiOutput("checkboxes"),
      uiOutput("filters")
    ),
    mainPanel(
      DTOutput("table"),
      style = "background-color: #E6F4FA;"
    )
  ),
  tags$style(HTML("
    .shiny-output-error { color: #FFFFFF; background-color: #0000FF; }
    .shiny-output-error:before { content: 'Error: '; }
    .shiny-input-container { color: #000000; background-color: #ADD8E6; }
    .shiny-input-container:hover { background-color: #87CEEB; }
    body { background-color: #E6F4FA; }  /* Cambia el color de fondo de todo el cuerpo de la página */
  "))
)


server <- function(input, output) {
  

  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  output$checkboxes <- renderUI({
    req(data())
    
    choices <- setdiff(names(data()), c("Matriculados", "% Mujeres"))
    
    checkboxGroupInput("group_by_columns", "Agrupar por:",
                       choices = choices, selected = NULL)
  })
  
  output$filters <- renderUI({
    req(data())
    

    rama_options <- unique(data()$Rama)
    curso_options <- unique(data()$Curso)
    comunidad_options <- unique(data()$`Comunidad autónoma`)
    
    # Create dropdown menus
    fluidRow(
      column(4, selectizeInput("filter_rama", "Filtrar por Rama:", choices = c("Todo", rama_options), multiple = TRUE, selected = "Todo")),
      column(4, selectizeInput("filter_curso", "Filtrar por  Curso:", choices = c("Todo", curso_options), multiple = TRUE, selected = "Todo")),
      column(4, selectizeInput("filter_comunidad", "Filtrar por C.A:", choices = c("Todo", comunidad_options), multiple = TRUE, selected = "Todo"))
    )
  })
  
  output$table <- renderDT({
    req(data())
    
    filtered_data <- data() |> 
      filter(if ("Todo" %in% input$filter_rama) TRUE else Rama %in% input$filter_rama,
             if ("Todo" %in% input$filter_curso) TRUE else Curso %in% input$filter_curso,
             if ("Todo" %in% input$filter_comunidad) TRUE else `Comunidad autónoma` %in% input$filter_comunidad)
    
    educ <- filtered_data
    educ$Matriculados <- as.numeric(educ$Matriculados)
    educ$`% Mujeres` <- as.numeric(educ$`% Mujeres`)
    
    if (length(input$group_by_columns) > 0) {
      summarized_data <- educ |> 
        group_by(across(all_of(input$group_by_columns))) |> 
        summarize(Matriculados = sum(Matriculados, na.rm = TRUE),
                  `% Mujeres` = mean(`% Mujeres`, na.rm = TRUE)) |> 
        mutate(Matriculados = round(Matriculados, 0)) |> 
        mutate(`% Mujeres` = round(`% Mujeres` * 100, 2))
    } else {
      summarized_data <- educ |>
        mutate(`% Mujeres` = round(`% Mujeres` * 100, 2))
    }
    
    datatable(summarized_data, options = list(format = "unformat")) |> 
      formatStyle(columns = c("Matriculados", "% Mujeres"),
                  `border-collapse` = 'collapse',
                  `font-size` = '90%',
                  `text-align` = 'center')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
