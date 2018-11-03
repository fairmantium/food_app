###########################
# START LOADING LIBRARIES #
###########################

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(googlesheets)
library(DT)

########################
# END LOADING LIBARIES #
########################



#####################
# START UI FUNCTION #
#####################

ui <- dashboardPage(
  
  # Start Dashboard Header
  dashboardHeader(
    title = "Weekly Meal Planning",
    titleWidth = 250
  ),
  # End Dashboard Header
  
  # Start Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    
    withSpinner(uiOutput("weekSelect")),
    withSpinner(uiOutput("recipeSelect"))
    
  ),
  # End Dashboard Sidebar
  
  # Start Dashboard Body
  dashboardBody(
  
    # Start Tabs
    tabsetPanel(
      
      id = "tabs",
      
      # Start Grocery List Tab
      tabPanel(
        
        title = "Grocery List",
        value = "groceryList",
        
        withSpinner(
          size = 3,
          DT::dataTableOutput("groceryList")
        )
        
        
      ),
      # End Grocery List Tab
      
      # Start Data Table Tab
      tabPanel(
        
        title = "Raw Data",
        value = "datapage",
        
        # Output Joined Table
        withSpinner(
          size = 3,
          DT::dataTableOutput("recipesTable")
        )
        
      )
      # End Data Table Tab
      
    )
    # End Tabs
    
  )
  # End Dashboard Body
  
)

###################
# END UI FUNCTION #
###################



#########################
# START SERVER FUNCTION #
#########################

server <- function(input, output, session) {
  
  
  ################################
  # START DATA LOADING & PARSING #
  ################################
  
  
  # Authenticate With Google Using Pre-Stored Token
  gs_auth(token="/home/fairman_jim/Github/sheets_token.rds")
  
  # Get Meal Planning Sheet
  sheet <- gs_title("Meal Planning")
  
  # Get Data From Each Sheet With 8 Seconds Between Each Request
  recipes <- sheet %>% gs_read(ws = "Recipes")
  Sys.sleep(6)
  joining_table <- sheet %>% gs_read(ws = "JoiningTable")
  Sys.sleep(6)
  ingredients <- sheet %>% gs_read(ws = "Ingredients")
  Sys.sleep(6)
  units <- sheet %>% gs_read(ws = "Units")
  
  # Join Datatables Together For Master Dataset
  joined_df <- left_join(joining_table, ingredients, by = c("ingredient" = "ingredient"))
  joined_df <- left_join(joined_df, units, by = c("units" = "units"))
  joined_df <- left_join(recipes, joined_df, by = c("recipe" = "recipe"))
  
  
  ##############################
  # END DATA LOADING & PARSING #
  ##############################
  
  
  ############################
  # START DROPDOWN FILTERING #
  ############################
  
  
  # Start Dropdown Menu For Week Number
  output$weekSelect <- renderUI({
    
    req(recipes)
    
    selectInput(inputId = "weekSelect",
                label = "Week Selection:",
                choices = sort(unique(recipes$week_number)),
                width = "100%",
                multiple = TRUE
    )
    
  })
  # End Dropdown Menu for Week Number
  
  # Start Dropdown Menu For Recipes
  output$recipeSelect <- renderUI({
    
    req(recipes)
    
    recipe_df <- recipes %>% filter(week_number %in% input$weekSelect)
    
    selectInput(inputId = "recipeSelect",
                label = "Recipe(s) Selection:",
                choices = sort(unique(recipe_df$recipe)),
                width = "100%",
                multiple = TRUE
    )
    
  })
  # End Dropdown Menu for Recipes
  
  
  ##########################
  # END DROPDOWN FILTERING #
  ##########################
  
  
  ###############################
  # START REACTIVE DATAFRAME(S) #
  ###############################
  
  
  groceries <- reactive({
    
    req(joined_df)
    req(input$weekSelect)
    req(input$recipeSelect)
    
    # Filter Base on Inputs
    filtered <- joined_df %>% filter(week_number %in% input$weekSelect) %>%
      filter(recipe %in% input$recipeSelect)
    
    # Empty DF to Hold Data From Loop
    loopdf <- data.frame()
    
    # For Each Ingredient Count Up The Amount
    for (n in unique(filtered$ingredient)) {
      
      # Subset Data for Ingredient
      ingredient_df <- filtered %>% filter(ingredient == n)
      
      loop_out <- data.frame(ingredient = n,
                             amount = NA,
                             units = NA
                             )
      
      loopdf <- rbind(loopdf, loop_out)
      
    }
    
    loopdf
    
  })
  
  
  ###############################
  # START REACTIVE DATAFRAME(S) #
  ###############################
  
  
  ############################
  # START DATA TABLE VIEW(S) #
  ############################
  
  # Start Grocery List Table
  output$groceryList <- DT::renderDataTable({
    
    req(groceries())
    
    DT:: datatable(groceries(),
                   extensions = c("Scroller","ColReorder","KeyTable"),
                   options = list(pageLength = 20,
                                  paging = TRUE,
                                  searching = TRUE,
                                  scroller = TRUE,
                                  ordering = TRUE,
                                  searchHighlight = TRUE,
                                  scrollY = 700,
                                  scrollX = TRUE,
                                  colReorder = TRUE,
                                  keys = TRUE)
                   )
  })
  # End Grocery List Table
  
  # Start Raw DataTable Output
  output$recipesTable <- DT::renderDataTable({
    
    req(joined_df)
    
    DT::datatable(joined_df,
                  extensions = c("Scroller","ColReorder","KeyTable"),
                  options = list(pageLength = 20,
                                 paging = TRUE,
                                 searching = TRUE,
                                 scroller = TRUE,
                                 ordering = TRUE,
                                 searchHighlight = TRUE,
                                 scrollY = 700,
                                 scrollX = TRUE,
                                 colReorder = TRUE,
                                 keys = TRUE)
    )
    
  })
  # End Raw DataTable Output
  
  
  ##########################
  # END DATA TABLE VIEW(S) #
  ##########################
  
  
}


#######################
# END SERVER FUNCTION #
#######################



#################
# START RUNNING #
#################

shinyApp(ui = ui, server = server)

###############
# END OF FILE #
###############