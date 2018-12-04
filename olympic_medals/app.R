
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
summer <- read_csv("../summer.csv")%>%
  mutate(Season= "Summer")
winter <- read_csv("../winter.csv")%>%
  mutate(Season = "Winter")
dictionary <- read_csv("../dictionary.csv")

olympic <- bind_rows(summer, winter)%>%
  select(Year, Sport, Country, Gender, Medal, Season)


medal_country_year <- olympic%>%
  count(Year, Country, Medal)

medal_gender_year <- olympic%>%
  count(Year, Gender, Medal)

med <- medal_gender_year %>%
  group_by(Medal)%>%
ggplot(aes(Year, n, color = Medal, fill = Medal)) +
  geom_col() +
  labs(y = "Number of Medals") +
  scale_color_manual(values=c("#cd7f32", "#E69F00", "#999999")) + #make colors Bronze, Silver, Gold
  scale_fill_manual(values=c( "#cd7f32","#E69F00",  "#999999")) 

country <- medal_country_year %>%
  ggplot(aes(Country, Year, color = Medal, fill = Medal)) +
  geom_col() +
  labs(y = "Number of Medals") +
  scale_color_manual(values=c("#cd7f32", "#E69F00", "#999999")) + #make colors Bronze, Silver, Gold
  scale_fill_manual(values=c( "#cd7f32","#E69F00","#999999")) 
 

#https://www.kaggle.com/the-guardian/olympic-games
col <- c(colnames(olympic))

ui <- fluidPage(h1("Olympic Medals"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "Filter",
                                label = "Select Filters",
                                choices = col,
                                multiple = TRUE),
                sliderInput(inputId = "Year", 
                            label = "Select Year",
                            min = min(olympic$Year),
                            max = max(olympic$Year),
                            value = c(min(olympic$Year),max(olympic$Year)),
                            sep = ""), #got rid of 1000s separator
                selectInput("Sport", "Select Sport",
                            choices = sort(unique(olympic$Sport)),
                            multiple = FALSE),
                selectInput("Country", "Select Country",
                            choices = sort(unique(olympic$Country)),
                            multiple = FALSE),
                radioButtons("Gender", "Select Gender",
                             choices = sort(unique(olympic$Gender))),
                radioButtons("Season", "Select Season",
                             choices = sort(unique(olympic$Season)))
                ),
                mainPanel(plotOutput("medalsPlot"))))
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
        Gender == input$Gender
      )    
  })
  output$medalsPlot <- renderPlot({
      ggplot(medal_filter(), aes(Year, color = Medal, fill = Medal)) +
      geom_histogram()+
      labs(y = "Number of Medals") +
      scale_color_manual(values=c("#cd7f32", "#E69F00", "#999999")) + #make colors Bronze, Silver, Gold
      scale_fill_manual(values=c( "#cd7f32","#E69F00",  "#999999"))
  })
  
}
shinyApp(ui = ui, server = server)
