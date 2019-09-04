# Please Set Path Accordingly!!
setwd("C:/Users/gupta/Desktop/FIT5147 Data Exploration and Visualisation/vis project")

# Importing Required Libraries
library(shiny)
library(plotly)
library("dplyr")
library(shiny)
library(shinydashboard)
library(leaflet)
library(xts)
library(dygraphs)
library(rsconnect)

# UI Function
ui <- dashboardPage(
  # Dash Board Created witjh header
  dashboardHeader(title = "Crime in Los Angeles"),
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      # Menu items
      menuItem("Crime Arrest", tabName = "CrimeArrest", icon = icon("th")),
      menuItem("Year", tabName = "dashboard", icon = icon("dashboard"),
               menuSubItem("Crime",tabName = "dashboardCrime"),
               menuSubItem("Arrest",tabName = "dashboardArrest")),
      # menuItem("Arrest in each Year", tabName = "dashboardArrest", icon = icon("dashboard")),
      menuItem("Cluster Map", tabName = "Cluster", icon = icon("th"),
               menuSubItem("Crime",tabName = "ClusterCrime"),
               menuSubItem("Arrest",tabName = "ClusterArrest")),
      menuItem("Time Series", tabName = "Time", icon = icon("th"),
               menuSubItem("Crime",tabName = "CrimeTime"),
               menuSubItem("Arrest",tabName = "ArrestTime"))
      
    )),
  
  # Dashboardy
  dashboardBody(
  
  fluidPage(
    
  fluidRow(
    # TAbs
    tabItems(
      
      
      tabItem(tabName = "dashboardCrime",
    
    column(6, plotlyOutput(outputId = "bar", height = "700px")),
    column(6, plotlyOutput(outputId = "pie1", height = "700px"))
    
    ),
    
    tabItem(tabName = "dashboardArrest",
           
            column(6, plotlyOutput(outputId = "bar_arrest", height = "700px")),
            column(6, plotlyOutput(outputId = "pie1_arrest", height = "700px"))
            
    ),
    
    tabItem(tabName = "ClusterCrime",
            column(6, plotlyOutput(outputId = "bar2", height = "700px")),
            column(6, leafletOutput(outputId = "leaf", height = "700px"))
            
    ),
    
    tabItem(tabName = "ClusterArrest",
            column(6, plotlyOutput(outputId = "bar2_arrest", height = "700px")),
            column(6, leafletOutput(outputId = "leaf_arrest", height = "700px"))
            
    ),

    tabItem(tabName = "CrimeTime",
            dygraphOutput(outputId = "timeplot", height = "600px"),
            column(8,selectInput("Crime", "CRIME TYPE:", 
                        c("BATTERY - SIMPLE ASSAULT" = "Battery", 
                          "VEHICLE - STOLEN" = "Vehicle",
                          "THEFT PLAIN" = "Theft",
                          "INTIMATE PARTNER - SIMPLE ASSAULT" = "Partner",
                          "VANDALISM - FELONY" = "Felony",
                          "VANDALISM - MISDEAMEANOR" = "Misdeameanor",
                          "ASSAULT WITH DEADLY WEAPON" = "Weapon",
                          "ROBBERY" = "Robbery",
                          "THEFT-GRAND" = "Grand Theft",
                          "CRIMINAL THREATS" = "Threats"))
            )
                         ),
    
    
    tabItem(tabName = "ArrestTime",
            dygraphOutput(outputId = "timeplot_arrest", height = "600px"),
            column(8,selectInput("Arrest", "ARREST TYPE:", 
                                 c("DRINKING IN PUBLIC" = "Drinking", 
                                   "DRUNK DRIVING ALCOHOL/DRUGS" = "Driving",
                                   "POSSESSION CONTROLLED SUBSTANCE" = "Possession",
                                   "CORPORAL INJURY ON SPOUSE/COHABITANT/ETC" = "Spouse",
                                   "FTA AFTER WRITTEN PROMISE" = "Fta",
                                   "POSSESSION NARCOTIC CONTROLLED SUBSTANCE" = "Narcotic",
                                   "SIT/LIE/SLEEP SIDEWALK OR STREET" = "Sleep",
                                   "OPEN ALCOHOLIC BEV IN PUBLIC PARK/PLACE" = "Alcohol",
                                   "PROSTITUTION" = "Prostitution",
                                   "GRAND THEFT (OVER $400)" = "Theft"))
            )
    ),
    
    tabItem(tabName = "CrimeArrest",
    sliderInput("year", "Year:", min = 2010, max = 2017, value = 2010),
    h2("Number of Crimes & Arrests"),
    column(12, leafletOutput(outputId = "crime_arrest", height = "600px")))
    
            )
            )
  
)
))

# Server Function
server <- function(input, output) {
  #Read data
  crimeCode <- read.csv(file = "crimeCode.csv", sep =",", header = TRUE)
  
  #group by
  crimeYear <- crimeCode %>% 
    group_by(Year) %>%
    summarise(number = n())
  
  #Bar plot
  output$bar <- renderPlotly({
    p <- plot_ly( source = "source",
      x = crimeYear$Year,
      y = crimeYear$number,
      name = "Crime",
      type = "bar",
      color = crimeYear$Year, colors = "RdGy"
    ) %>% layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Year'),title ="Number of Crimes in each Year", showlegend = T )
  })
  
 #pie plot
  output$pie1 <- renderPlotly({
    
    eventdata <- event_data("plotly_click", source = "source")
    validate(need(!is.null(eventdata), "Click over the Bar Chart to check the percentage of different crimes"))
    
    datapoint <- as.numeric(eventdata$pointNumber)
    
    window <- 10
    
    rng <- (datapoint - window):(datapoint + window)
    
    crime2010 = subset(crimeCode, Year == eventdata$x)
    crimeD <- crime2010 %>% group_by(Crime.Code.Description) %>% summarize(count = n()) %>% arrange(desc(count))
    crimeD <- crimeD[c(1:10),]
    
    plot_ly(crimeD,source = "piesource",labels = ~Crime.Code.Description, values = ~count) %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Percentage of Crime in a Year",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  })
  
  
  
  # ARREST
  # Read data
  arrestCode <- read.csv(file = "arrestCode.csv", sep =",", header = TRUE)
  
  #group by
  arrestYear <- arrestCode %>% 
    group_by(Year) %>%
    summarise(number = n())
  
  #Bar plot
  output$bar_arrest <- renderPlotly({
    p <- plot_ly( source = "source arrest",
                  x = arrestYear$Year,
                  y = arrestYear$number,
                  name = "arrest",
                  type = "bar",
                  color = arrestYear$Year, colors = "RdGy"
    ) %>% layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Year'),title ="Number of Arrests in each Year", showlegend = T )
    
  })
  
  #Pie plot
  output$pie1_arrest <- renderPlotly({
    
    eventdata_arrest <- event_data("plotly_click", source = "source arrest")
    validate(need(!is.null(eventdata_arrest), "Click over the Bar Chart to check the percentage of different arrests"))
    
    datapoint <- as.numeric(eventdata_arrest$pointNumber)
    
    window <- 10
    
    rng <- (datapoint - window):(datapoint + window)
    
    arrest2010 = subset(arrestCode, Year == eventdata_arrest$x)
    arrestD <- na.omit(arrest2010) %>% group_by(Charge.Description) %>% summarize(count = n()) %>% arrange(desc(count))
    arrestD <- arrestD[c(1:10),]
    
    plot_ly(arrestD,source = "piesource",labels = ~Charge.Description, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Percentage of Arrests in a Year",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  
  
  # SECOND TAB
  
  crimeArea <- read.csv(file = "crimeArea.csv", sep =",", header = TRUE)
  
  crimeCount <- crimeArea %>% 
    group_by(Area.Name) %>%
    summarise(number = n())
  
  output$bar2 <- renderPlotly({
  
    p <- plot_ly(
      source = "source2",
      x = crimeCount$Area.Name,
      y = crimeCount$number,
      type = "bar",
      color = crimeCount$Area.Name, colors = "RdGy"
    ) %>% layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Area Name'),title ="Click on Bar to view Different Crime Percentage in each year", showlegend = T )
  })
  
  crimeD <- crimeArea %>% group_by(Crime.Code.Description) %>% summarize(count = n()) %>% arrange(desc(count))
  crimeD <- crimeD[c(1:10),]
  crimeArea <- subset(crimeArea, Crime.Code.Description == c('BATTERY - SIMPLE ASSAULT','VEHICLE - STOLEN','THEFT PLAIN - PETTY ($950 & UNDER)','INTIMATE PARTNER - SIMPLE ASSAULT',
                                                             'VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) 0114','VANDALISM - MISDEAMEANOR ($399 OR UNDER)','ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT',
                                                             'ROBBERY','THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD0036','CRIMINAL THREATS - NO WEAPON DISPLAYED')
                      , select = c('Date.Occurred','Area.Name','Crime.Code.Description','Latitude','Longitude'))
  
  output$leaf <- renderLeaflet({
    
    eventdata2 <- event_data("plotly_click", source = "source2")
    validate(need(!is.null(eventdata2), "Click over the Bar Chart to check the percentage of different crimes"))
    
    datapoint <- as.numeric(eventdata2$pointNumber)
    print(eventdata2)
  
    crimeAreaex <- subset(crimeArea, Area.Name == eventdata2$x)
  
    l <- leaflet() %>% addProviderTiles(provider = providers$Stamen.TonerHybrid) %>%
      addMarkers(data = crimeAreaex, clusterOptions = markerClusterOptions(),label = ~as.character(Crime.Code.Description))
  })
  
  # Arrest
  
  arrestArea <- read.csv(file = "arrestArea.csv", sep =",", header = TRUE)
  
  arrestCount <- arrestArea %>% 
    group_by(Area.Name) %>%
    summarise(number = n())
  
  output$bar2_arrest <- renderPlotly({
    
    p <- plot_ly(
      source = "source2 arrest",
      x = arrestCount$Area.Name,
      y = arrestCount$number,
      type = "bar",
      color = arrestCount$Area.Name, colors = "RdGy"
    ) %>% layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Area Name'),title ="Click on Bar to view Different Crime Percentage in each year", showlegend = T )
  })
  
  arrestD <- arrestArea %>% group_by(Charge.Description) %>% summarize(count = n()) %>% arrange(desc(count))
  arrestD <- arrestD[c(1:11),]
  arrestD
  arrestArea <- subset(arrestArea, Charge.Description == c('DRINKING IN PUBLIC','DRUNK DRIVING ALCOHOL/DRUGS','POSSESSION CONTROLLED SUBSTANCE','CORPORAL INJURY ON SPOUSE/COHABITANT/ETC',
                                                           'FTA AFTER WRITTEN PROMISE','POSSESSION NARCOTIC CONTROLLED SUBSTANCE','SIT/LIE/SLEEP SIDEWALK OR STREET',
                                                           'OPEN ALCOHOLIC BEV IN PUBLIC PARK/PLACE','PROSTITUTION','GRAND THEFT (OVER $400)')
                       , select = c('Arrest.Date','Area.Name','Charge.Description','Latitude','Longitude'))
  
  output$leaf_arrest <- renderLeaflet({
    
    eventdata2_arrest <- event_data("plotly_click", source = "source2 arrest")
    validate(need(!is.null(eventdata2_arrest), "Click over the Bar Chart to check the percentage of different crimes"))
    
    datapoint <- as.numeric(eventdata2_arrest$pointNumber)
    print(eventdata2_arrest)
    
    arrestAreaex <- subset(arrestArea, Area.Name == eventdata2_arrest$x)
    
    l <- leaflet() %>% addProviderTiles(provider = providers$Stamen.TonerHybrid) %>%
      addMarkers(data = arrestAreaex, clusterOptions = markerClusterOptions(),label = ~as.character(Charge.Description))
    
  })
  
  
  
  # THIRD TAB
  
  output$timeplot <- renderDygraph({
  
    
    crimeArea <- read.csv(file = "crimeArea.csv", sep =",", header = TRUE)
    
    if (input$Crime == "Battery") {
      crimeArea = subset(crimeArea, Crime.Code.Description == 'BATTERY - SIMPLE ASSAULT')
    }
    else if(input$Crime == "Vehicle"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'VEHICLE - STOLEN')
    }
    else if(input$Crime == "Theft"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'THEFT PLAIN - PETTY ($950 & UNDER)')
    }
    else if(input$Crime == "Partner"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'INTIMATE PARTNER - SIMPLE ASSAULT')
    }
    else if(input$Crime == "Felony"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) 0114')
    }
    else if(input$Crime == "Misdeameanor"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'VANDALISM - MISDEAMEANOR ($399 OR UNDER)')
    }
    else if(input$Crime == "Weapon"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT')
    }
    else if(input$Crime == "Robbery"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'ROBBERY')
    }
    else if(input$Crime == "Grand Theft"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD0036')
    }
    else if(input$Crime == "Threats"){
      crimeArea = subset(crimeArea, Crime.Code.Description == 'CRIMINAL THREATS - NO WEAPON DISPLAYED')
    }
    
    crimeArea$Date <- format(as.Date(as.character(crimeArea$Date.Occurred), format="%m-%d-%Y"),"%m/%d/%Y")
    crimeArea$Date[is.na(crimeArea$Date)] <- as.character(crimeArea$Date.Occurred[is.na(crimeArea$Date)])
    # crimeArea$Date
    
    # crimeArea$Date <- strptime(x = as.character(crimeArea$Date),format ="%Y/%m/%d")
    
    # crimeArea$Date <- as.POSIXct.Date(crimeArea$Date)
    crimeD <- na.omit(crimeArea) %>% group_by(Date) %>% summarize(count = n()) 
    
    crimeD$Date <- as.Date(crimeD$Date,"%m/%d/%Y")
    
    timeser <- xts(crimeD$count, order.by = as.POSIXct(crimeD$Date))
    
    plotting <- dygraph(timeser) %>%
      dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="#008080") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1) %>%
      dyAxis("y", label = "Crime Count")%>%
      dyAxis("x", label = "Date") 
       
    
  })
 
  
 # ARREST
  output$timeplot_arrest <- renderDygraph({
    
    
    arrestArea <- read.csv(file = "arrestArea.csv", sep =",", header = TRUE)
    
    if (input$Arrest == "Drinking") {
      arrestArea = subset(arrestArea, Charge.Description == 'DRINKING IN PUBLIC')
    }
    else if(input$Arrest == "Driving"){
      arrestArea = subset(arrestArea, Charge.Description == 'DRUNK DRIVING ALCOHOL/DRUGS')
    }
    else if(input$Arrest == "Possession"){
      arrestArea = subset(arrestArea, Charge.Description == 'POSSESSION CONTROLLED SUBSTANCE')
    }
    else if(input$Arrest == "Spouse"){
      arrestArea = subset(arrestArea, Charge.Description == 'CORPORAL INJURY ON SPOUSE/COHABITANT/ETC')
    }
    else if(input$Arrest == "Fta"){
      arrestArea = subset(arrestArea, Charge.Description == 'FTA AFTER WRITTEN PROMISE')
    }
    else if(input$Arrest == "Narcotic"){
      arrestArea = subset(arrestArea, Charge.Description == 'POSSESSION NARCOTIC CONTROLLED SUBSTANCE')
    }
    else if(input$Arrest == "Sleep"){
      arrestArea = subset(arrestArea, Charge.Description == 'SIT/LIE/SLEEP SIDEWALK OR STREET')
    }
    else if(input$Arrest == "Alcohol"){
      arrestArea = subset(arrestArea, Charge.Description == 'OPEN ALCOHOLIC BEV IN PUBLIC PARK/PLACE')
    }
    else if(input$Arrest == "Prostitution"){
      arrestArea = subset(arrestArea, Charge.Description == 'PROSTITUTION')
    }
    else if(input$Arrest == "Theft"){
      arrestArea = subset(arrestArea, Charge.Description == 'GRAND THEFT (OVER $400)')
    }
    
    arrestArea$Date <- format(as.Date(as.character(arrestArea$Arrest.Date), format="%m-%d-%Y"),"%m/%d/%Y")
    arrestArea$Date[is.na(arrestArea$Date)] <- as.character(arrestArea$Date[is.na(arrestArea$Date)])
    # crimeArea$Date
    
    # crimeArea$Date <- strptime(x = as.character(crimeArea$Date),format ="%Y/%m/%d")
    
    # crimeArea$Date <- as.POSIXct.Date(crimeArea$Date)
    arrestD <- na.omit(arrestArea) %>% group_by(Date) %>% summarize(count = n()) 
    
    arrestD$Date <- as.Date(arrestD$Date,"%m/%d/%Y")
    
    timeser_arrest <- xts(arrestD$count, order.by = as.POSIXct(arrestD$Date))
    
    plotting <- dygraph(timeser_arrest) %>%
      dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="#008080") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)%>%
      dyAxis("y", label = "Arrest Count")%>%
      dyAxis("x", label = "Date") 
    
  })
  
  
  output$crime_arrest <- renderLeaflet({
    
    crime_data <- read.csv(file = "crime_data.csv", sep =",", header = TRUE)
    crime_data <- subset(crime_data, select = c('Crime.Code.Description','Latitude','Longitude','Area.Name','Year'))
    
    arrest_data <- read.csv(file = "arrest_data.csv", sep =",", header = TRUE)
    arrest_data <- subset(arrest_data, select = c('Charge.Description','Latitude','Longitude','Area.Name','Year'))
    
    if (input$year == "2010") {
      crime_data = subset(crime_data, Year == "2010")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2011"){
      crime_data = subset(crime_data, Year == "2011")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2012"){
      crime_data = subset(crime_data, Year == "2012")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2013"){
      crime_data = subset(crime_data, Year == "2013")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2014"){
      crime_data = subset(crime_data, Year == "2014")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2015"){
      crime_data = subset(crime_data, Year == "2015")
      arrest_data = subset(arrest_data, Year == "2010")
    }
    else if(input$year == "2016"){
      crime_data = subset(crime_data, Year == "2016")
      arrest_data = subset(arrest_data, Year == "2010")
    }
  
    crimeNumber <- crime_data %>% 
      group_by(Area.Name,Year) %>%
      summarise(Latitude =(mean(Latitude)),
                Longitude =(mean(Longitude)),
                Count = length(as.character(Crime.Code.Description)))
    crimeNumber <- na.omit(crimeNumber)
    
    arrestNumber <- arrest_data %>% 
      group_by(Area.Name,Year) %>%
      summarise(Latitude =(mean(Latitude)),
                Longitude =(mean(Longitude)),
                Count = length(as.character(Charge.Description)))
    arrestNumber <- na.omit(arrestNumber)
   
    crime_map <- leaflet(crimeNumber) %>%  addProviderTiles(provider = providers$Stamen.Toner) %>%
      addCircleMarkers(~crimeNumber$Longitude,~crimeNumber$Latitude, label = ~as.character(crimeNumber$Area.Name), color = "red",radius = ~crimeNumber$Count/500, group = "crime",popup = paste(h4("Crime Count:",crimeNumber$Count)) )%>%
      addCircleMarkers(~arrestNumber$Longitude,~arrestNumber$Latitude, label = ~as.character(arrestNumber$Area.Name), color = "blue",radius = ~arrestNumber$Count/500,group ="arrest",popup = paste(h4("Arrest Count:",arrestNumber$Count))) %>%
      addLayersControl(
        overlayGroups = c("crime", "arrest"),
        options = layersControlOptions(collapsed = FALSE) 
      )
  
  })
  
}
  

shinyApp(ui, server)
