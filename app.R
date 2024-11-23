library(shiny)
library(bslib)
source("diet_funcs.R")  # Source the functions that will do the calculations

# Define UI for application that draws the table 
ui <- navbarPage( "Diet Problem Solver",
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="styles/default.css")
  ),
  tabPanel( "Solve",
    page_sidebar(
      sidebar = sidebar( # Sidebar for choosing the food
        title = "Food Selection",
        selectInput("presets","Diet Presets",
                    choice = c("Default","Vegan","Vegetarian","Pescatarian","Paleo",
                               "Gluten-Free","Diabetic")
        ),
        actionButton("apply","Apply Preset", class="apply-preset"),
        actionButton("custom_select","Custom Select", class="custom-select"),
        width = 300,
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
    )
  ),
  tabPanel("Details",
    
  ),
  tabPanel("About",
    
  ),
)

# Define server logic required to draw the table 
server <- function(input, output) {
  # Server Global Variables
  preset <- c()
  selected_food <- reactiveValues(choices = c())
  checkBox <- checkboxGroupInput(
          "food_indices",
          "",
          choiceNames = nutritionTable$Foods,
          choiceValues = 1:(length(nutritionTable$Foods)),
          selected = selected_food,
          inline = T
      )
  
  # Custom input Modal
  observeEvent(input$custom_select,{
    showModal(modalDialog(
      title="Choose Food in your Diet",
      
      div(class="selection-action-buttons",
        actionButton("select_all", "Select All", class="select-all"),
        actionButton("unselect_all", "Unselect All", class="unselect-all"),
      ),
      
      checkBox,
      easyClose = T,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_button", "Apply")
      )
    ))
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = selected_food$choices
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
    selected_food$choices <- preset
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = selected_food$choices
    )
  })
  
  # Event handling for inputs
  observeEvent(input$select_all, {  # Select All
    selected_food$choices <- c(1:length(nutritionTable$Foods))    
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = selected_food$choices
    )
  })
  
  observeEvent(input$unselect_all,{ # Unselect All
    selected_food$choices <- c(0)
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = selected_food$choices
    )
  })
  
  observeEvent(input$apply_button,{ # Apply Selected in Modal
    indices <- as.numeric(input$food_indices)
    selected_food$choices <- indices
    updateCheckboxGroupInput(
      getDefaultReactiveDomain(),
      "food_indices",
      selected = selected_food$choices
    )
    removeModal()
  })

  # Reactive Result Display
  output$optimal_menu <- renderTable({
        c(input$apply_button,input$apply)
        tryCatch(
            {
              # TRY
              message("Fetching result...")
              result <- getOptimalMenu(selected_food$choices)
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
                  if (length(selected_food$choices) == 0){
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
