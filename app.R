library(shiny)
library(bslib)
# library(rsconnect)    # Deploy on shinyapps.io (R 4.4.2 is not yet supported lol)
source("diet_funcs.R")  # Source the functions that will do the calculations


# Define UI for application that draws the table 
ui <- navbarPage("Diet Problem Solver",
  tags$head( # Styling using class identifier
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
        div(class="preset-buttons",
          actionButton("apply","Apply", class="apply-preset"),
          actionButton("clear","Clear", class="clear")
        ),
        actionButton("custom_select","View Selection", class="custom-select"),
        checkboxInput(
          "serving_units",
          "Show Serving with Units",
        ),
        width = "30%",
      ),
      card(
        card_header(
          "The Optimized Menu"
        ),
        card_body(
          htmlOutput("optimal_cost"),
          tableOutput("optimal_menu")
        ),
      ),
      accordion(
        accordion_panel(
          title="Show Tableau per Iteration",
          uiOutput("iteration_tableau"),
        ),
        open = F,
      )
    )
  ),
  tabPanel("Info",
    card(
      card_header("What does this app do?"),
      includeHTML("www/templates/info.html")
    )
  ),
  tabPanel("About",
    card(
      card_header("Who, why, and when?"),
      includeHTML("www/templates/about.html")
    )
  ),
  nav_spacer(),
  nav_item(input_dark_mode(id="mode", mode=NULL)),
  selected = "Solve",
  collapsible = T,
  fluid = T,
  windowTitle = "Diet Problem Solver"
)

# Define server logic required to draw the table 
server <- function(input, output) {
  # Server Global Variables
  preset <- c()
  selected_food <- reactiveValues(choices =c(1:20))
  
  # Custom input Modal
  observeEvent(input$custom_select,{
    showModal(modalDialog(
      class="selection-modal",
      
      title="Choose Food in your Diet",
      
      div(class="selection-action-buttons",
        actionButton("select_all", "Select All", class="select-all"),
        actionButton("unselect_all", "Unselect All", class="unselect-all"),
      ),
      
      splitLayout(
        cellWidths = "33.33%",
        checkBox <- checkboxGroupInput(
         inputId = "food_indices0",
         label = "",
         choiceNames = nutritionTable$Foods[1:22],
         choiceValues = 1:22,
         selected = selected_food$choices 
        ),
        checkBox <- checkboxGroupInput(
         inputId = "food_indices1",
         label = "",
         choiceNames = nutritionTable$Foods[23:43],
         choiceValues = 23:43,
         selected = selected_food$choices
        ),
        checkBox <- checkboxGroupInput(
         inputId = "food_indices2",
         label = "",
         choiceNames = nutritionTable$Foods[44:64],
         choiceValues = 44:64,
         selected = selected_food$choices 
        )
      ),
      easyClose = T,
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_button", "Apply")
      )
    ))
    updatePartitionSelection(selected_food$choices)
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
    updateSelection(selected_food)
  })
  
  observeEvent(input$clear,{        # Clear Applied Preset
    selected_food$choices <- c(0)
    updateSelection(selected_food)
  })
  
  # Event handling for inputs
  observeEvent(input$select_all, {  # Select All
    selected <- c(1:length(nutritionTable$Foods))    
    updatePartitionSelection(selected)
  })
  
  observeEvent(input$unselect_all,{ # Unselect All
    selected <- c(0)
    updatePartitionSelection(selected)
  })
  
  observeEvent(input$apply_button,{ # Apply Selected in Modal
    selected_food$choices <- c(
      as.numeric(input$food_indices0),
      as.numeric(input$food_indices1),
      as.numeric(input$food_indices2))
    updateSelection(selected_food)
    removeModal()
  })

  # Reactive Result Display
  output$optimal_menu <- renderTable({
      tryCatch(
        {
          # TRY
          message("Fetching result...")
          result <- getOptimalMenu(selected_food$choices, input$serving_units)
          table <- result$menu
          cost <- result$cost
          tableau_list <- result$perIter$tab
          basSol <- result$perIter$sol
          
          colnames(table) <- c("Food","Serving","Cost($)")
          
          output$optimal_cost <- renderUI({
            text <- paste(
              "<h4>The cost of this optimal diet is <span style='color: green;'><b>$",
              sprintf("%.2f", cost),
              "</b></span> per day.</h4>
              <br> <b> Cost Breakdown by Food<b>
              "
              ,sep="")
            HTML(text)
          })
          
          output$iteration_tableau <- renderUI({
            len = length(tableau_list)
            fluidRow(
              lapply(1:len, function(i){ 
                card(
                  card_header(paste("Iteration", i)),
                  ifelse(i==len, "Final Tableau", "Tableau"),
                  tableOutput(outputId = paste("table",i,sep="")),
                  ifelse(i==len, "Final Solution","Basic Solution"),
                  tableOutput(outputId = paste("basSol",i,sep=""))
                )
              })
            )
          })
          
          for(i in 1:length(tableau_list)){
            index <- i
            output[[paste("table",index,sep="")]] <- renderTable({
              tableau_list[[index]]
            })
            output[[paste("basSol",index,sep="")]] <- renderTable({
              t(basSol[[index]])
            })
          }
          
          table
        },
        # CATCH
        error = function(e){
          message("Encountered an error..")
          print(e)
          output$optimal_cost <- renderUI({
            if ((length(selected_food$choices) == 1 && selected_food$choices == c(0)) ||
                length(selected_food$choices) == 0){
              text <- paste("<h4 style='color:red;'>No food selected</h4>")
            } else text <- paste(
              "<h4 style='color:red;'>It is not possible to meet the nutritional constraints
              with the food that you have selected.</h4>"
              )
            
            HTML(text)
          })
          output$iteration_tableau <- renderUI({
            HTML(
              "<p style='color:red;'>No iterations made...</p>"
            )
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

# Utility Func
updateSelection <- function(selected_food){
  updateCheckboxGroupInput(
    getDefaultReactiveDomain(),
    "food_indices",
    selected = selected_food$choices
  )
  updatePartitionSelection(selected_food$choices)
}
updatePartitionSelection <- function(selected_indices){
  updateCheckboxGroupInput(
    getDefaultReactiveDomain(),
    "food_indices0",
    selected = selected_indices
  )
  updateCheckboxGroupInput(
    getDefaultReactiveDomain(),
    "food_indices1",
    selected = selected_indices
  )
  updateCheckboxGroupInput(
    getDefaultReactiveDomain(),
    "food_indices2",
    selected = selected_indices
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
