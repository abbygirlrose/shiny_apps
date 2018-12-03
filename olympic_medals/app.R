
library(shiny)
library(tidyverse)
library(readr)
summer <- read_csv("../summer.csv")
winter <- read_csv("../winter.csv")
dictionary <- read_csv("../dictionary.csv")
olympic <- bind_rows(summer, winter)
#https://www.kaggle.com/the-guardian/olympic-games
col <- c(colnames(olympic))

ui <- fluidPage(h1("Olympic Medals"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Filter", "Select Filter",
                                choices = col, 
                    multiple = TRUE),
                selectInput("Year", "Select Year",
                            choices = sort(unique(olympic$Year)),
                            multiple = FALSE),
                selectInput("City", "Select City",
                            choices = sort(unique(olympic$City)),
                            multiple = FALSE),
                selectInput("Sport", "Select Sport",
                            choices = sort(unique(olympic$Sport)),
                            multiple = FALSE),
                selectInput("Discipline", "Select Discipline",
                            choices = sort(unique(olympic$Discipline)),
                            multiple = FALSE),
                selectInput("Country", "Select Country",
                            choices = sort(unique(olympic$Country)),
                            multiple = FALSE),
                radioButtons("Gender", "Select Gender",
                             choices = sort(unique(olympic$Gender))),
                selectInput("Event", "Select Event",
                            choices = sort(unique(olympic$Event)),
                            multiple = FALSE),
                radioButtons("Medal", "Select Medal",
                             choices = sort(unique(olympic$Medal))
                             )),
                mainPanel("outputs go here")))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)
