###########################
# START LOADING LIBRARIES #
###########################

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(googlesheets4)
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
    withSpinner(uiOutput("recipeSelect")),
    textInput(inputId = "googleAccount",
              label = "Google Account ID:"
              ),
    passwordInput(inputId = "googlePass",
                  label = "Google Account Password:"
                  )
    
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
  # gs_auth(token="sheets_token.rds")
  
  # Get Meal Planning Sheet
  mealSheet <- "1Bb4KDv5hbMZj6RIOtum9MsPKH8jw1VhhBngQrqJBwHg"
  
  # Get Data From Each Sheet With 8 Seconds Between Each Request
  recipes <- read_sheet(ss = mealSheet,
                        sheet = "Recipes"
                        )
  
  joining_table <- read_sheet(ss = mealSheet,
                              sheet = "JoiningTable"
                              )

  ingredients <- read_sheet(ss = mealSheet,
                            sheet = "Ingredients"
                            )

  units <- read_sheet(ss = mealSheet,
                      sheet = "Units"
                      )
  
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
    
    selected_df <- joined_df %>% filter(recipe %in% input$recipeSelect)
    
    # Group By To Add Up All Ingredients
    sum_df <- selected_df %>% group_by(.dots=c("Category","ingredient","units")) %>%
      summarise(Amount = sum(amount)) %>%
      select(ingredient, Category, Amount, units) %>%
      arrange(Category, ingredient, units)
    
    names(sum_df) <- c("Ingredient","Category","Amount","Units")
    
    sum_df
    
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