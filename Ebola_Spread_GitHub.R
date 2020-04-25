if(!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, data.table, dplyr, ggplot2, shiny, devtools, leaflet, leaflet.extras, maps, shinyWidgets, zoo, devtools, lubridate, RColorBrewer)

if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")



ebola <- read_csv("ebola_2014_2016.csv")
summary(ebola)
str(ebola)
ebola$Date <- as.Date(parse_date_time(ebola$Date,"mdy"))



summary(is.na(ebola))
#Changing Names
colnames(ebola)
colnames(ebola) <- c("country", "date", "suscases", "probcases", "confcases","cpscases","susdeaths","probdeaths","confdeaths","cpsdeaths")

#world.cities is an R dataset
cities <- world.cities[world.cities$capital == 1,]
cities <- cities[,-c(3,6)]
#cities[order(cities$country.etc),]  
cities[cities$name=="London", "country.etc"] <- "United Kingdom"
cities[cities$name=="Washington", "country.etc"] <- "United States of America"

#merge

ebola <- cities %>%
  dplyr::select(country = country.etc, lat, lng = long) %>%
  left_join(ebola, ., by = "country")

ebolacum <- setDT(ebola)[,sum(cpsdeaths),by = date]
colnames(ebolacum) <- c("date","cumulativedeaths")
ebolacum

setDT(ebola)[country == "United States of America",]
unique_countries <- as.data.frame(unique(ebola$country))
colnames(unique_countries) <- "country"
unique_countries

unique_countries <- as.data.frame(unique(subset(ebola, cpsdeaths>100, select=country))) #For purposes of this plot, I removed countries that had less than 100 total deaths
colnames(unique_countries) <- "country"

ebola[date >= "2015-07-03" & country == "Liberia"]$cpsdeaths <- 4806 #imputed 4806
#Performed same imputations on other columns
ebola[date >= "2015-07-03" & country == "Liberia"]$suscases <- 5636 
ebola[date >= "2015-07-03" & country == "Liberia"]$probcases <- 1879 
ebola[date >= "2015-07-03" & country == "Liberia"]$confcases <- 3151
ebola[date >= "2015-07-03" & country == "Liberia"]$susdeaths <- 10666

str(ebola)

mybins <- c(0,500,1000, 1500, 2000, 2500, 3500,4000,4500,5000)
binpal <- colorBin("YlOrRd", ebola$cpsdeaths, bins = mybins, pretty = FALSE)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 100, left = 200, draggable = TRUE, width = 400, height = 400, style="padding: 16px; border-bottom: 1px solid #CCC; background: #FFFFFF; opacity: 0.60",
      sliderInput("date","Select Date", min = min(ebolacum$date), max = max(ebolacum$date), value = range(ebolacum$date)),
       plotOutput("plot", height="230px", width="100%")),
  absolutePanel(top = 550,left = 200, draggable = TRUE, width = 800, style="padding: 16px; border-bottom: 1px solid #CCC; background: #FFFFFF; opacity: 0.60",
    pickerInput("Country","Select Country", choices=as.character(unique_countries$country), options = list(`actions-box` = TRUE),multiple = T),
    plotOutput("plot2", height="180px", width="100%")),
  absolutePanel(top = 50, left = 950, draggable = FALSE, height = "100px", width = 500, style="padding: 16px; border-bottom: 1px solid #CCC; background: #FFFFFF; opacity: 0.6",
                h1("Ebola In West Aftrica", align= "center"))
  
)
server <- function(input, output){
  
  reactiveoutput <- reactive({
    ebolacum[ebolacum$date >= input$date[1] & ebolacum$date <= input$date[2],]
  }) 
  reactiveoutput2 <- reactive({
    ebola[ebola$date >= input$date[1] & ebola$date <= input$date[2],]
  })
  
    
  output$plot <- renderPlot({
     ggplot(data = reactiveoutput(), aes(date,cumulativedeaths)) + geom_line() + ggtitle("Cumulative Deaths Over Time") +
      scale_y_continuous() +
      theme(
        plot.title = element_text(size=16, hjust = .5),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.border = element_rect(linetype = "dashed", fill = NA))
   })
  output$plot2 <- renderPlot({
    df <- subset(ebola, ebola$country == input$Country)
    ggplot(data=df, aes(x = date,y = cpsdeaths, color = country)) + geom_line() + ggtitle("Deaths By Country Over Time") +
      scale_y_continuous("cumulative deaths") +
      theme(
        plot.title = element_text(size=16, hjust = .5),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.border = element_rect(linetype = "dashed", fill = NA))
      
  })
  output$map <- renderLeaflet({
    leaflet(ebola) %>% addTiles()  %>% setView(lng = -12, lat =  8, zoom = 5) %>% 
      addLegend(ebola, position = c("bottomright"),
                pal = binpal, title = "<small>Deaths</small>")

  })

  observe({
    
    
    proxy <- leafletProxy("map", data = reactiveoutput2())
    
    proxy %>% addTiles %>%
      
      addCircleMarkers(~lng, ~lat, radius = 30, stroke = TRUE, color = "#000000", opacity = .7, weight = 1,
                       fill = TRUE, fillColor = ~binpal(cpsdeaths), fillOpacity = 0.01,
                       label = ~as.character(country), labelOptions = labelOptions(style=list(
                         'background'='rgba(255,255,255,0.95)',
                         'border-color' = 'rgba(0,0,0,1)',
                         'border-radius' = '4px',
                         'border-style' = 'solid',
                         'border-width' = '4px')))
    
  })
  
}  
  
shinyApp(ui = ui, server = server)