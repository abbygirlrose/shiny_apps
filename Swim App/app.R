
library(shiny)
library(tidyverse)
library(tidytext)
library(tibble)
library(dplyr)
library(rjson)
library(rvest)
library(knitr)
library(ggplot2)
library(stringr)
library(tm) 
library(lubridate)
library(shinythemes)
library(rsconnect)
library(leaflet)

#get water temp data
santamonicapier <- read_html("http://www.surf-forecast.com/breaks/Santa-Monic-Pier/seatemp")
smp_temp <- html_nodes(santamonicapier, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

santamonicaop <- read_html("http://www.surf-forecast.com/breaks/Ocean-Park-Santa-Monica/seatemp")
smop_temp <- html_nodes(santamonicaop, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

venicebw <- read_html("http://www.surf-forecast.com/breaks/Venice-Breakwater/seatemp")
venicebw_temp <- html_nodes(venicebw, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

venicepier <- read_html("http://www.surf-forecast.com/breaks/Venice-Pier/forecasts/latest/six_day")
venicep_temp <- html_nodes(venicepier, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

mb <- read_html("http://www.surf-forecast.com/breaks/Manhattan-Beach-Pier/forecasts/latest/six_day")
mb_temp <- html_nodes(mb, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

wr <- read_html("http://www.surf-forecast.com/breaks/Will-Rogers/forecasts/latest/six_day")
wr_temp <- html_nodes(wr, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

zuma <- read_html("http://www.surf-forecast.com/breaks/Zuma-Beach/forecasts/latest/six_day")
zuma_temp <- html_nodes(zuma, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

cab <- read_html("http://www.surf-forecast.com/breaks/Cabrillo-Point/forecasts/latest/six_day")
cab_temp <- html_nodes(cab, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

es <- read_html("http://www.surf-forecast.com/breaks/El-Segundo-Beach-Jetty/forecasts/latest/six_day")
es_temp <- html_nodes(es, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

malibu <- read_html("http://www.surf-forecast.com/breaks/Will-Rogers/forecasts/latest/six_day")
malibu_temp <- html_nodes(malibu, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

dock <- read_html("http://www.surf-forecast.com/breaks/Dockweiler-Beach/forecasts/latest/six_day")
dock_temp <- html_nodes(dock, css = "b .temp") %>%
  html_text()%>%
  as.numeric() %>%
  as.data.frame()

#make temp vector
temp_vector <- c(smp_temp, smop_temp, venicebw_temp, venicep_temp, mb_temp, wr_temp, zuma_temp, cab_temp, es_temp, malibu_temp, dock_temp)%>%
  as.vector()

#make beach name vector
beach <- c("Santa Monica Pier", "Santa Monica Ocean Park", "Venice Break Water", "Venice Pier", "Manhattan Beach", "Will Rogers", "Zuma", "Cabrillo", "El Segundo", "Malibu", "Dockweiler")

beach1 <- c("Santa Monica Pier", "Santa Monica Ocean Park", "Venice Break Water", "Venice Pier", "Manhattan Beach", "Will Rogers", "Zuma", "Cabrillo", "El Segundo", "Malibu", "Dockweiler")%>%
  as.character()%>%
  as.data.frame()

#make latitude vector->df
lat <- c(-118.496573, -118.487819, -118.476588, -118.469692, -118.413228, -118.539843, -118.822407, -118.283379, -118.427079, -118.656931, -118.436952)%>%
  as.data.frame()

#make longitude vector->df
long <- c(34.008435, 34.000065, 33.985374, 33.977486, 33.883766, 34.036400,34.014107, 33.708509, 33.911477,34.038464, 33.929901) %>%
  as.data.frame()

#change column names
colnames(long) <- c("long")
colnames(lat) <- c("lat")
colnames(beach1) <- c("Beach")

#make location df
location <- bind_cols(beach1, lat, long)

water_temp <- bind_rows(smp_temp, smop_temp, venicebw_temp, venicep_temp, mb_temp, wr_temp, zuma_temp, cab_temp, es_temp, malibu_temp, dock_temp)

colnames(water_temp) <- c("TemperatureC")

#make df and convert tempC to tempF
water_temp <- water_temp %>%
  mutate(Beach = beach)%>%
  mutate(TemperatureC = as.numeric(TemperatureC)) %>%
  mutate(TemperatureF = (TemperatureC * 9/5)+32)

#make full df
full_temp <- left_join(water_temp, location, by = "Beach")

#make df with names and temps
vector_temp <- full_temp %>%
  select(Beach, TemperatureF) %>%
  unite(loc_temp, sep = " ")

#turn it into vectors 
vector_temp <- vector_temp[['loc_temp']]

temp_vec <- water_temp[['TemperatureF']]%>%
  as.character()%>%
  as.data.frame()

#get water quality data
grade <- read_html("http://publichealth.lacounty.gov/phcommon/public/eh/water_quality/beach_grades.cfm")
grades <- html_nodes(grade, css = "br:nth-child(3)~ p") %>%
  html_text() 

grades1 <- grades%>%
  as.data.frame()

grade_char <- grades %>%
  as.character()

grade_count <- sapply(strsplit(grade_char, " "), length)

ui <- fluidPage(theme = shinytheme("cerulean"),
   titlePanel("Can I Swim Today?"),
   sidebarLayout(
      sidebarPanel(
        selectInput("Beach", "Select Beach",
                    choices = sort(unique(water_temp$Beach)),
                    multiple = FALSE),
        numericInput("min_temp",
                     "Minimum Temperature",
                     min = 35,
                     max= 90,
                     value = 55
        ),
        numericInput("max_temp",
                     "Maximum Temperature",
                     min = 35,
                     max= 90,
                     value = 75
        ),
        img(src='thermo.png'),
        br(),
        uiOutput("link")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Temperature",
            br(),
            textOutput("twt"), tags$head(tags$style(HTML("
                            #twt {
                                                         font-size: 20px;
                                                         }
                                                         "))), 
            textOutput("location"),tags$head(tags$style(HTML("
                            #location {
                                                            font-size: 20px;
                                                            }
                                                            "))), 
            textOutput("is"), tags$head(tags$style(HTML("
                            #is {
                                                        font-size: 20px;
                                                        }
                                                        "))), 
            textOutput("temp"), tags$head(tags$style(HTML("
                            #temp {
                                                          font-size: 20px;
                                                          }
                                                          "))), 
            textOutput("mintemp"), tags$head(tags$style(HTML("
                            #mintemp {
                              color: blue;
                              font-size: 20px;

                           }
                            "))),
            textOutput("maxtemp"), tags$head(tags$style(HTML("
                            #maxtemp {
                                          font-size: 20px;
                                                             }
                                                             "))),
            br(),
            img(src='wave.png')),
          tabPanel("Locations within your range", 
                   leafletOutput("beachmap"),
                   tableOutput("temptable")),
          
         tabPanel("Water Quality",
                  br(),
                  textOutput("date"),tags$head(tags$style(HTML("
                            #date {
                                                               font-size: 20px;
                                                               }
                                                               "))),
                  br(),
                  br(),
                  textOutput("rain"),
                  tags$head(tags$style(HTML("
                            #rain {
                                            font-size: 20px;
                                            }
                                            "))),
                  br(),
                  br(),
                  img(src='wave.png'))
         )
      )
   )
   )

server <- function(input, output){
  temp_filter <- reactive({
    water_temp %>%
      filter(Beach == input$Beach)
  })
  beach_filter <- reactive({
    water_temp %>%
      filter(TemperatureF >= input$min_temp & TemperatureF <= input$max_temp)
  })
  map_filter <- reactive({
    full_temp %>%
      filter(TemperatureF >= input$min_temp & TemperatureF <= input$max_temp)
  })
  url <- a("abbybergman.com", href="https://www.abbybergman.com/")
  output$link <- renderUI({
    tagList( url)
  })
  output$beachmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(map_filter()$lat, map_filter()$long, popup = vector_temp)
  })
  output$rain <- renderText({
    if(grade_count > 25){
      "There is a rain advisory for Los Angeles County, wait 72 hours after rain to swim."
    }else{
      "There is no rain advisory."
    }
  })
  output$mintemp <- renderText({
    if(input$max_temp < water_temp$TemperatureF & input$min_temp < input$max_temp){
      "This water too warm for you."
    }else if(input$min_temp < water_temp$TemperatureF & input$min_temp < input$max_temp){
      "This water is warm enough for you."
    }else if(input$min_temp > input$max_temp){
      "Error: Minimum temperature must be less than maximum temperature"
    }else{
      "This water is too cold for you."
    }
  })
  output$date <- renderText({
    date()
  })
  output$twt <- renderText({
    "The water temperature at"
  })
  output$location <- renderText({
    input$Beach
  })
  output$temp <- renderText({
    input$Beach
    temp_filter()$TemperatureF
  })
  output$is <- renderText({
    "is"
  })
  output$temptable <- renderTable({
    water_temp %>%
      select(Beach, TemperatureF) %>%
      filter(TemperatureF >= input$min_temp & TemperatureF <= input$max_temp)
  })
}

shinyApp(ui = ui, server = server)

