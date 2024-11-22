library(shiny)
library(bslib)
source("nutri_table.R") # Source the nutrition table for constrainst and obj func
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
    "Diet Plan",
    tableOutput("diet_plan")
)

# Define server logic required to draw the table 
server <- function(input, output) {
    output$diet_plan <- renderTable({
        indices <- as.numeric(input$food_indices)
        result <- data.frame() # Init
        tryCatch(
            {
                # TRY
                message("Fetching result...")
                result <- getDietPlan(indices)
            },
            error = function(e){
                message("Encountered an error..")
                print(e)
            },
            finally = {
                result
            } 
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
