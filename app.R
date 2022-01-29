#
# Author: Manuel Alén Sánchez
# Date: 29-01-2022
# Version: 1.0
#

library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
    output$plot <- renderPlot({
        pv <- read.csv("shini.csv", header = TRUE,sep = ",")
        data <- reactive({
            req(input$sel_variable)
            df <- pv %>% filter(Variable %in% input$sel_variable)%>% group_by(Year) %>% summarise(Cantidad = sum(Cantidad))  
        })
        
        #plot
        output$plot <- renderPlot({
            g <- ggplot(data(), aes(x = Year, y = Cantidad, fill = Year,color=Year))
            g + geom_bar(stat = "identity", position = "dodge")
        })
        
        
    })
    
}
ui <- fluidPage(
    h1("Análisis Marxista"),
    h3("Variables básicas dentro de la teoría marxista"),
    selectInput(inputId = "sel_variable",
                label = "Selecciona la variable a estudiar",
                list("Plusvalía","Tasa de Plusvalía","Tasa de ganancia","Ciclos del capital","Acumulación de capital","Plustrabajo por trabajador")),
    plotOutput("plot")
)

# Run the application 
shinyApp(ui = ui, server = server)