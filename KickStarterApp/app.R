# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
#library(fivethirtyeight)

# Import data
kickstarter <- read_csv("all_kickstarter.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

## HISTOGRAM WIDGETS (KARLA)

## SCATTERPLOT WIDGETS (LOUIS)

## TABLE WIDGETS (DAN)


############
#    ui    #
############
ui <- navbarPage(theme = shinytheme("superhero"),
                 title = "Kick Starter Trends"
                 
)


############
# server   #
############
server <- function(input, output){
    
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)