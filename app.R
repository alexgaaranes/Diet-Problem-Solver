library(shiny)
library(bslib)
source("diet_funcs.R")  # Source the functions that will do the calculations

# Define UI for application that draws the table 
ui <- navbarPage( "Diet Problem Solver",
  tabPanel( "Solve",
    page_sidebar(
      
      sidebar = sidebar( # Sidebar for choosing the food
        title = "Food Selection",
        selectInput("presets","Diet Presets",
                    choice = c("Default","Vegan","Vegetarian","Pescatarian","Paleo",
                               "Gluten-Free","Diabetic")
        ),
        
        actionButton("apply","Apply Preset"),
        actionButton("select_all", "Select All"),
        actionButton("unselect_all", "Unselect All"),
        width = 300,
        checkboxGroupInput(
            "food_indices",
            "",
            choiceNames = nutritionTable$Foods,
            choiceValues = 1:(length(nutritionTable$Foods)),
            selected = 1:20,
        ),
      ),
      card(
        card_header(
          "The Optimized Menu"
        ),
        card_body(
          htmlOutput("optimal_cost"),
          tableOutput("optimal_menu")
        )
      ),
      card(
        htmlOutput("")
      )
    )
  ),
  tabPanel("Details",
    
  ),
  tabPanel("About",
    
  )
)

# Define server logic required to draw the table 
server <- function(input, output) {
  # Global variables
  preset <- c()
  
  # Event handling for inputs
  observeEvent(input$select_all, {  # Select All
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = 1:length(nutritionTable$Foods)
      )
  })
  
  observeEvent(input$unselect_all,{ # Unselect All
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = 0
    )
  })
  
  observeEvent(input$presets, {     # Preset
    preset <<- switch(input$presets,
    "Default" = 1:20,
    "Vegan" = c(1:6,8,11:19,46:48,53,55:56),
    "Vegetarian" = c(1:6,8,11:19,24:29,46:48,53,55:56),
    "Pescatarian" = c(1:8,11:19,28:29,35:36,40,46:48,51:52,55),
    "Paleo" = c(1:3,5:6,9:12,14:15,17,30,33,49,51:52),
    "Gluten-Free" = c(1:3,9,12:16,24:27,51:52),
    "Diabetic" = c(1:3,5:6,9,11:12,15:18,26:29,35:36,40,48,51:53)
    )
  })
  
  observeEvent(input$apply,{        # Apply Selected Preset 
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = preset
    )
  })

  # Reactive Result Display
  output$optimal_menu <- renderTable({
        indices <- as.numeric(input$food_indices)
        print(indices)
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
                  "<h4>The cost of this optimal diet is <b>$",
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
                  if (length(indices) == 0){
                    text <- paste("<h4 style='color:red;'>No food selected</h4>")
                  } else
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
    width = "100%",
    bordered = T,
    spacing = "m"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
