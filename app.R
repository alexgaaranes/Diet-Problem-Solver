library(shiny)
library(bslib)
source("diet_funcs.R")  # Source the functions that will do the calculations

# Define UI for application that draws the table 
ui <- page_sidebar(
    title = "Diet Problem Solver",
    sidebar = sidebar( # Sidebar for choosing the food
        actionButton("select_all", "Select All"),
        actionButton("unselect_all", "Unselect All"),
        width = 300,
        checkboxGroupInput(
            "food_indices",
            "Check the box of the food you want to include in your diet",
            choiceNames = nutritionTable$Foods,
            choiceValues = 1:(length(nutritionTable$Foods)),
            selected = 1:20
        ),
    ),
    card(
      "The Optimized Menu",
      htmlOutput("optimal_cost"),
      tableOutput("optimal_menu")
    )
)

# Define server logic required to draw the table 
server <- function(input, output) {
  # Event handling for inputs
  observeEvent(input$select_all, {  # Select All
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = 1:length(nutritionTable$Foods)
      )
  })
  
  observeEvent(input$unselect_all,{
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = 0
    )
  })

  # Reactive Result Display
  output$optimal_menu <- renderTable({
        indices <- as.numeric(input$food_indices)
        tryCatch(
            {
              # TRY
              message("Fetching result...")
              result <- getOptimalMenu(indices)
              table <- result$menu
              cost <- result$cost
              
              colnames(table) <- c("Food","Serving","Cost($)")
              
              output$optimal_cost <- renderUI({
                text <- paste(
                  "<h4>The cost of this optimal diet is <b>",
                  sprintf("%.2f", cost),
                  "</b> per day.</h4>
                  <br> <b> Cost Breakdown by Food<b>
                  "
                  )
                
                HTML(text)
              })
              
              table
            },
              # CATCH
            error = function(e){
                message("Encountered an error..")
                print(e)
                output$optimal_cost <- renderUI({
                  text <- paste(
                    "<h4 style='color:red;'>It is not possible to meet the nutritional constraints
                    with the food that you have selected.</h4>"
                    )
                  
                  HTML(text)
                })
                table <- data.frame(
                    Status = c("Infeasible")
                )
            },
            finally = {
                table
            } 
        )
    },
    striped = T,
    width = "auto",
    bordered = T,
    spacing = "s"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
