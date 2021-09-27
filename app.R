#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

my_cars <- as_tibble(mtcars)

my_cars %<>%
    mutate(am = recode(am,`0`='Automatic',`1`='Manual')) %>%
    mutate(vs = recode(vs, `0`='V-shaped', `1`='Straight')) %>%
    mutate(across(c('cyl','gear','carb'), as_factor))

abbrs <- names(my_cars)
var_names <- c('Miles per Gallon', 'No. of Cylinders','Displacement',
               'Horsepower', 'Rear axle ratio','Weight, in 1000 lbs', 
               'Quarter mile time', 'Engine type', 'Transmission', 
               'No. of forward gears', 'No. of Carburetors')

var_table <- tibble(Var_names=abbrs, Meanings = var_names)
names(var_names) <- abbrs

ui <- fluidPage(

    # Application title
    titlePanel("Cars"),
    p("This is a simple app to explore the mtcars dataset in R. Pick an X 
    variable; pick a Y variable;pick a color (if you don't want any color 
      just choose \"No Color\"). The app also displays the the result of 
      fitting a linear regression model with the selected variables."),
      
    p("The plotly is a ggplotly plot so it is interactive."),

    
    sidebarLayout(
        sidebarPanel(
        selectInput('X_Var', 'Select X Variable',
                    c('Miles per Gallon' = 'mpg',
                      'Displacement'='disp',
                      'Horsepower'='hp',
                      'Rear axle ratio'='drat',
                      'Weight, in 1000 lbs'='wt',
                      'Quarter mile time' = 'qsec'
                      ),
                    selected = 'wt'
                    
        ),
        selectInput('Y_Var', 'Select Y Variable',
                    c('Miles per Gallon' = 'mpg',
                      'Displacement'='disp',
                      'Horsepower'='hp',
                      'Rear axle ratio'='drat',
                      'Weight, in 1000 lbs'='wt',
                      'Quarter mile time' = 'qsec'
                    ),
                    selected='mpg'
                    
        ),
        selectInput('Color_Var', 'Select Color Variable',
                    c('No. of Cylinders' = 'cyl',
                      'Engine type'='vs',
                      'Transmission'='am',
                      'No. of forward gears'='gear',
                      'No. of Carburetors' = 'qsec',
                      'No color'='NULL'
                    ),
                    selected='NULL'
                    
        ),
        tableOutput('my_table')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           verbatimTextOutput('my_summary')
        )
    )
)


server <- function(input, output) {
    x_var <- reactive(input$X_Var)
    y_var <- reactive(input$Y_Var)
    color <- reactive(input$Color_Var)
    
    output$my_table <- renderTable(var_table)
    output$distPlot <- renderPlotly({

        p <- ggplot(my_cars, mapping=aes_string(x=x_var(), y=y_var(), col=color())) +
            geom_point() + geom_smooth(method=lm) + xlab(var_names[x_var()]) +
            ylab(var_names[y_var()]) + labs(color=var_names[color()])
        
        ggplotly(p)
    })
    
    output$my_summary <- renderPrint({
        formula <-paste(y_var(), '~', x_var(), '+', color())
        print(summary(lm(formula=formula, my_cars)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
