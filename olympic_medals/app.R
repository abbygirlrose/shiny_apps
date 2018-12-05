
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(tidyr)
library(rsconnect)
summer <- read_csv("../summer.csv")%>%
  mutate(Season= "Summer")
winter <- read_csv("../winter.csv")%>%
  mutate(Season = "Winter")
dictionary <- read_csv("../dictionary.csv")

olympic <- bind_rows(summer, winter)%>%
  select(Year, Sport, Country, Gender, Medal, Season) 

#https://www.kaggle.com/the-guardian/olympic-games
col <- c(colnames(olympic))

ui <- fluidPage(theme = shinytheme("simplex"),
  h1("Olympic Medals"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(inputId = "Year", 
                            label = "Select Year",
                            min = min(olympic$Year),
                            max = max(olympic$Year),
                            value = c(min(olympic$Year),max(olympic$Year)),
                            sep = ""), #got rid of 1000s separator
                selectInput("Country", "Select Country",
                            choices = sort(unique(olympic$Country)),
                            multiple = FALSE),
                radioButtons("Gender", "Select Gender",
                             choices = sort(unique(olympic$Gender))),
                radioButtons("Season", "Select Season",
                             choices = sort(unique(olympic$Season))),
                uiOutput("sportSelector")
                ),
                mainPanel(plotOutput("medalsPlot"),
                          tableOutput("medalsTable"))))
server <- function(input, output) {
  medal_filter <- reactive({
    olympic %>%
      filter(
        # filter by season
        Season == input$Season,
        # filter by hourly wage
        Year >= input$Year[[1]],
        Year <= input$Year[[2]], 
        #filter by country
        Country == input$Country,
        Gender == input$Gender,
        Sport == input$Sport
      )    
  })
  
  output$medalsPlot <- renderPlot({
      ggplot(medal_filter(), aes(Year, color = Medal, fill = Medal)) +
      geom_histogram()+
      labs(y = "Number of Medals") +
      scale_color_manual(values=c("#cd7f32", "#E69F00", "#999999")) + #make colors Bronze, Silver, Gold
      scale_fill_manual(values=c( "#cd7f32","#E69F00",  "#999999"))
    })
  output$medalsTable <- renderTable({
    medal_filter()%>%
      count(Medal, Year, Sport, Gender)%>%
      mutate("Number of Medals" = n) %>%
      select(-n) %>%
      spread(Medal, `Number of Medals`)
  }) 
  output$sportSelector <- renderUI({
    medals <- olympic
    
    # filter by country
    if(!is.null(input$Country)) {
      medals <- filter(medals, Country %in% input$Country)
    }
    medals <- filter(medals, Gender == input$Gender)
    
    medals <- filter(medals, Season == input$Season)
    
    # filter by year
    medals <- filter(medals,
                     Year >= input$Year[[1]],
                     Year <= input$Year[[2]])
    selectInput("Sport", "Select Sport",
                choices = sort(unique(medals$Sport)),
                multiple = FALSE)
  })
    
}
shinyApp(ui = ui, server = server)
