#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predictor App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h1("Predictor Variable!"),
            radioButtons("var","Options:",choices=c("Cylinder"="cyl","Displacement"="disp","Horsepower"="hp","Carburetors"="carb"),selected="cyl")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Plot with Linear Regression:"),
            plotlyOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1=renderPlotly({
        
        x1 <- input$var
        
        if (x1 == "cyl")
        {
            x2 <- "Cylinder"
        }
        
        if(x1 == "disp")
        {
            x2 <- "Displacement"
        }
        
        if(x1 == "hp")
        {
            x2 <- "HorsePower"
        }
        
        if(x1 == "carb")
        {
            x2 <- "Carburetor"
        }
        
        mdl <- lm(mtcars$mpg~mtcars[,x1])
        
        
        plot_ly(x=~mtcars[,x1]) %>% 
        add_markers(y=~mtcars$mpg, name="Data Values") %>% add_lines(x=~mtcars[,x1],y=fitted(mdl),name="Linear Model") %>% layout(xaxis=list(title=x2),yaxis=list(title="MPG"))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
