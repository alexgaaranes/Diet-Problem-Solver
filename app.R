library(shiny)
library(bslib)
source("diet_funcs.R")  # Source the functions that will do the calculations

# Define UI for application that draws the table 
ui <- page_sidebar(
    title = "Diet Problem Solver",
    sidebar = sidebar( # Sidebar for choosing the food
        width = 300,
        checkboxGroupInput(
            "food_indices",
            "Check the food in your diet",
            choiceNames = nutritionTable$Foods,
            choiceValues = 1:(length(nutritionTable$Foods)),
            selected = c(1:20)
        ),
    ),
    card(
      "The Optimized Menu",
      textOutput("optimal_cost"),
      tableOutput("optimal_menu")
    )
)

# Define server logic required to draw the table 
server <- function(input, output) {
    output$optimal_menu <- renderTable({
        indices <- as.numeric(input$food_indices)
        result <- data.frame() # Init
        tryCatch(
            {
              # TRY
              message("Fetching result...")
              result <- getOptimalMenu(indices)$menu
              
              #output$optimal_cost <- renderText({
             #   paste("The cost of this optimal diet is", as.character(result$cost), "per day")
              #})
              
            },
              # CATCH
            error = function(e){
                message("Encountered an error..")
                print(e)
                result <- data.frame(
                    Status = c("Infeasible")
                )
            },
            finally = {
                result
            } 
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
