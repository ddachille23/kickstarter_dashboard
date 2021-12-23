# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(lubridate)
library(ggrepel)
library(bslib)
library(plotly)

# add kickstarter image with link as a title
title <- tags$a(href = "https://www.kickstarter.com/",
                tags$image(src = "kickstarter.jpg", height = '22', width = '200'),
                )

# Import data
kickstarter <- read_csv("all_kickstarter.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

## TABLE WIDGETS (DAN)

# change from factor to char
kickstarter$country_displayable_name <- as.character(kickstarter$country_displayable_name) 

# make successful and failed uppercase
kickstarter <- kickstarter %>%
  mutate(outcome = case_when(outcome == "successful" ~ "Successful",
       outcome == "failed" ~ "Failed",
       TRUE ~ outcome))

# For selectizeInput choices, pull directly from data
category_choices <- c("All", unique(kickstarter$main_category))
country_choices <- c("All", unique(kickstarter$country_displayable_name))
outcome_choices <- c("All", unique(kickstarter$outcome))

############
#    ui    #
############
ui <- navbarPage(
  theme = bs_theme(bootswatch = "minty",
                   primary = "#05ce78",
                   secondary = "#05ce78"),
  title = title,
  
  # TAB 1 (DAN): Table
  
  tabPanel(
    title = "",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "category",
                       label = "Category",
                       choices = category_choices,
                       selected = "All",
                       multiple = F),
        selectizeInput(inputId = "subcategory",
                       label = "Subcategory",
                       choices = NULL,
                       selected = "All",
                       multiple = F),
        selectizeInput(inputId = "country",
                       label = "Country:",
                       choices = NULL,
                       selected = "All",
                       multiple = F),
        selectizeInput(inputId = "outcome",
                       label = "Outcome",
                       choices = outcome_choices,
                       selected = "All",
                       multiple = F),
        sliderInput(inputId = "raised",
                    label = "Amount Raised ($)",
                    min = 0,
                    max = 6000000,
                    value = c(0,2000000)),
        sliderInput(inputId = "length",
                    label = "Campaign Length (days)",
                    min = 0,
                    max = 90,
                    value = c(0,90)),
        dateRangeInput(inputId = "date",
                       label = "Date Launched",
                       start = "2009-01-01",
                       end = "2020-12-30"
        )
      ),
      
      mainPanel(DT::dataTableOutput(outputId = "table"))
    )
  )
)
  

############
# server   #
############
server <- function(session, input, output){
  
  # TAB 1 (DAN): Table
  # dynamically update subcategory options based on category input
  observe({ 
    print(input$category)
    if (input$category != "All"){
      x <- kickstarter$sub_category[kickstarter$main_category == input$category]
      x <- c("All", x)
      updateSelectInput(session, "subcategory", "Subcategory", choices = unique(x), selected = "All")
    }
    else {
      x <- kickstarter$sub_category
      updateSelectInput(session, "subcategory", "Subcategory", choices = "All")
    }
  })
  
  # dynamically update country options based on subcategory input
  observe({
    if (input$subcategory != "All"){
      x2 <- kickstarter$country_displayable_name[kickstarter$sub_category == input$subcategory]
      x2 <- c("All", x2)
      updateSelectInput(session, "country", "Country", choices = x2)
    }
    else {
      x2 <- kickstarter$country
      updateSelectInput(session, "country", "Country", choices = country_choices)
    }
  })
  
  # make table
  data_for_table <- reactive({
    # store the categorical inputs
    category_input <- input$category
    subcategory_input <- input$subcategory
    country_input <- input$country
    outcome_input <- input$outcome
    
    # make links clickable
    kickstarter$project_urls <- paste0("<a href='",kickstarter$project_urls,"'>",kickstarter$project_urls,"</a>")
    
    # remove time from date
    kickstarter <- kickstarter %>% mutate(launched_at = date(launched_at))
    
    # account for "All" input
    if (input$category == "All") {
      cat_list <- unique(kickstarter$main_category) # store all categories in holding var if input is "All"
      category_input <- cat_list # transfer data from holding var to used input var
    }
    if (input$subcategory == "All") {
      subcat_list <- unique(kickstarter$sub_category)
      subcategory_input <- subcat_list 
    }
    if (input$country == "All") {
      country_list <- unique(kickstarter$country_displayable_name)
      country_input <- country_list
    }
    if (input$outcome == "All") {
      outcome_list <- unique(kickstarter$outcome)
      outcome_input <- outcome_list
    }
    
    # filter data for the table
    data <- kickstarter %>%
      select(name, backers_count, pledged_usd, goal_usd, pct_funded, outcome, 
             main_category, sub_category, launched_at, campaign_length, country_displayable_name, project_urls) %>%
      arrange(desc(backers_count)) %>%
      filter(main_category %in% category_input, # filtering by various inputs
             sub_category %in% subcategory_input,
             country_displayable_name %in% country_input,
             outcome %in% outcome_input,
             pledged_usd >= input$raised[1], pledged_usd <= input$raised[2], 
             launched_at >= input$date[1], launched_at <= input$date[2],
             campaign_length >= input$length[1], campaign_length <= input$length[2]
      ) %>% 
      rename("Name" = name, "Backers" = backers_count, "Raised ($)" = pledged_usd, "Goal ($)" = goal_usd,
             "% of Goal Reached" = pct_funded, "Outcome" = outcome, "Main Category" = main_category,
             "Subcategory" = sub_category, "Launch Date" = launched_at, "Campaign Length (days)" = campaign_length, "Country" = country_displayable_name,
             "URL" = project_urls)
  })
  
  output$table <- DT::renderDataTable({ 
    datatable(data_for_table(), escape = FALSE, # escape = FALSE allows links to be clickable
              extensions = c("Buttons"), 
              options = list(dom = 'Bflrtip',
                             buttons = c('csv', 'excel', 'print'))) %>% 
      formatCurrency(c('Backers', 'Raised ($)', 'Goal ($)', '% of Goal Reached'),currency = "", interval = 3, mark = ",", digits = 0)
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)