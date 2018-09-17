require(DT)
require(ggplot2)
require(ggthemes)
#require(hrbrthemes)
require(leaflet)
require(markdown)
require(raster)
require(RColorBrewer)
require(scales)
require(shiny)
require(shinythemes)
#require(showtext)
require(tidyverse)
require(wordcloud)
require(wordcloud2)

# Import Roboto Condensed
# font_add_google(name = "Roboto Condensed", family = "Roboto Condensed",
#                 regular.wt = 400, bold.wt = 700)
# showtext_auto()
# showtext_opts(dpi = 112)
# hrbrthemes::import_roboto_condensed() 
options("scipen" = 100, "digits" = 4)
options(encoding = "UTF-8")
# system('fc-cache -f ~/.fonts/Roboto_Condensed') 

# Load data
weather_transit_data <- readRDS("data/weather_transit_data.rds")
UberWeekdays <- readRDS("data/u1.rds")
MTAWeekdays <- readRDS("data/mt3.rds")
CompareWeekdays <- readRDS("data/transit.rds")
StationIncomeElasticity <- readRDS("data/station_income_elasticity.rds")
UbersPerDate <- readRDS("data/full_ubers_income.rds")
mta_entry_df <- readRDS("data/mta_entry_df.rds")
mta_exit <- readRDS("data/mta_exit.rds")

# Generate aesthetics for leaflet maps
UbersPerDate$inc2015 <- 
  cut(UbersPerDate$inc2015, 
      breaks = c(11179, 25000, 50000, 75000, 100000, 250001), 
      labels = c("< $25,000", "$25,000 - $50,000", "$50,000 - $75,000", "$75,000 - $100,000", " > $100,000"))

pal <- colorFactor(c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#7f0000"), 
                   domain = UbersPerDate$inc2015)
color_Prop <- pal(UbersPerDate$inc2015)

pal_elasticity_MTA <- colorNumeric(
  palette = "Oranges",
  domain = StationIncomeElasticity$ElasticityMTA)

pal_elasticity_Uber <- colorNumeric(
  palette = "Oranges",
  domain = StationIncomeElasticity$ElasticityUber)

pal_coefficients_MTA <- colorNumeric(
  palette = "Blues",
  domain = StationIncomeElasticity$MTAEstimate)

pal_coefficients_Uber <- colorNumeric(
  palette = "Blues",
  domain = StationIncomeElasticity$UberEstimate)

content_transit_income <- paste("Closest station:", StationIncomeElasticity$Station, "<br/>", 
                                "Median income:", StationIncomeElasticity$inc2015, "<br/>",
                                "Median rent:", StationIncomeElasticity$rnt2015, "<br/>",
                                "Tweets:", StationIncomeElasticity$twts_nr, "<br/>")

content_elasticity <- paste("Closest station:", UbersPerDate$Station, "<br/>", 
                            "MTA Entries:", UbersPerDate$entries, "<br/>",
                            "MTA Exits:", UbersPerDate$exits, "<br/>",
                            "Uber Elasticity:", UbersPerDate$UberRides, "<br/>",
                            "Median income:", UbersPerDate$inc2015, "<br/>",
                            "Median rent:", UbersPerDate$rnt2015, "<br/>",
                            "Tweets:", UbersPerDate$twts_nr, "<br/>")

# Load helper functions
source("helpers.R")

# User interface
ui <- fluidPage(
  theme = shinytheme("readable"),
  navbarPage("NYC Transit Patterns",
             tabPanel("Introduction",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Ride or Drive?"),
                          h3("A Columbia QMSS project"),
                          p("Team Members: Mikael Brunila, Jon Campbell, Maliha Tariq, Nathan Taylor.")
                        ),
                        mainPanel(
                          includeMarkdown("project_outline.md")
                        )
                      )
             ),
             tabPanel("Transit by weekday",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "weekday", label = "Select mode of transportation",
                                      choices = c("Uber", "MTA Entries", "Comparison"),
                                      selected = 1),
                          h2("Transit by weekday"),
                          p("Uber rides by weekday follow a typical pattern. 
                            Most rides take place in the evening (5-9pm) and the fewest occur between 1-4am. 
                            Friday and Saturdays break this trend, however, as people stay out later at those time intervals. 
                            Sunday is the least traveled day for Uber ride, although the afternoon and evening still 
                            see a smaller uptick in rides compared to the rest of the day."),
                          p("NYC-MTA entrance swipes reflect a similar trend as Uber rides. Morning ridership spikes much higher than morning ridership for Uber. This may reflect people's preference of the MTA in getting to work. Like Uber data, MTA swipes hit their peak in the evening and fall in the early morning hours. Saturday appears to be a more lightly traveled day for MTA than for Uber."),
                          p("The comparison between the two transit options is dramatic. If the y-axis didn't become clear in the preceding graphs, it does now. MTA accounts for around 400,000 'riders' in their busiest time intervals. Uber, on the other hand, tops at about 8,000 rides when it's at full steam. Note the time frame, however. These values have likely changed in present day as Uber and other ride-sharing firms expand.")
                          ),
                        mainPanel(
                          plotOutput(outputId = "weekday")
                        )
                        )
                      ),
             navbarMenu(
               "Rides and rain",
               tabPanel("Bar Chart",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "month", label = "Select month",
                                        choices = weather_transit_data$month_names,
                                        selected = 1),
                            h2("Rides and rain"),
                            p("This bar graph shows how daily rates in Uber rides changes with two main weather patterns, rain and temperature. Blue bars represent days with rain, while golden bars indicate days without rain. The line on the graph reflects the temperature in Fahrenheit experienced on a given day. While one might think temperature may influence Uber ridership, a quick eyeballing of the months does not show that to be the case. In short, the more it rains, the more New Yorkers enjoy Uber rides.")
                          ),
                          mainPanel(
                            plotOutput(outputId = "barplot")
                          )
                        )
               ),
               tabPanel("Regression tables",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "dependentVars", label = "Select dependent variable",
                                        choices = c("Uber", "MTA Exits", "MTA Entries"), selected = 1),
                            h2("Rides and rain: Regression tables"),
                            p("This regression table expands on the bar chart in this same section, exploring which covariates affect the use of MTA entries, MTA exits as well as Uber rides.")
                          ),
                          mainPanel(
                            dataTableOutput("dependentVars")
                          )
                        )
               )
             ),
             tabPanel("Income and transit",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Income and transit"),
                          p("This leaflet map depicts the relationship between income and transit usage throughout NYC neighborhoods. Each circle represents a subway station for the MTA. The color of the circle represents the neighborhood’s income (via the US Census) as its size represents the magnitude of usage -- how many subway entrances, subway exits, or Uber rides have occurred closest to that station."),
                          p("Most subway entries take place in the relatively wealthy and residential centers of Manhattan below Harlem. Middle- and lower-middle income areas in western Brooklyn and Queens trail behind. The areas with the highest median income appear to have the most number of subway stations."),
                          p("When examining subway exits by income, you can see that transportation hubs in midtown Manhattan around 34th and 42nd Streets show a slightly higher proportion of exits than entries. These stations are likely used to access non-subway transportation systems such as Metro North, LIRR, PATH, and buses."),
                          p("The Uber rides by income layer shows a similar pattern. A majority of rides take place in Manhattan south of Harlem. Low income neighborhood appear to have substantially fewer Uber rights. That seems to present a barrier to accessibility for Uber and the MTA.")
                        ),
                        mainPanel(
                          tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                          leafletOutput("mymap")
                        )
                      )
             ),
             navbarMenu(
               "Elasticity and coefficients",
               tabPanel("Maps",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "elasticity", label = "Select map visualisation",
                                        choices = c("Elasticity", "Coefficients"), 
                                        selected = 1),
                            h2("Precipitation elasticity: Maps"),
                            p("In Economics, the price elasticity of demand is a measure that show how sensitive demand for a good or service are to changes in response to price. In the map and regression tables in this section, 
                              we look at how elastic the use of Uber and MTA are to rain. Using the standard formula for price elasticity of demand, we replace price with the precipitation on a given day and demand with the amount of 
                              Uber rides or entries around an MTA subway station. In the map, the size of the points reflects the median income in that area. We also added the coefficients factored by station when regressing Uber rides and
                              MTA use against precitipitation. These coefficients were used to calculate elasticity, so no wonder they point to similar conclusions!")
                          ),
                          mainPanel(
                            tags$style(type = "text/css", "#elasticity {height: calc(100vh - 80px) !important;}"),
                            leafletOutput(outputId = "elasticity")
                            
                          )
                        )
               ),
               tabPanel("Regression tables",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "elasticityReg", label = "Select transit",
                                        choices = c("Uber", "MTA"), 
                                        selected = 1),
                            h2("Precipitation elasticity: Regression tables"),
                            p(" In this regression table, we measure the effect of rents on the rain elasticity of transit use around a subway stop. As a quirky addition, we added the amount of geotagged tweets around a station normalised by population for each tract the tweets
                              were sent from. This data is from an old project in the autumn of 2017.")
                          ),
                          mainPanel(
                            DT::dataTableOutput(outputId = "elasticityReg")
                          )
                        )
               )
             )
        )
)

# Server
server <- function(input, output) {
  output$barplot <- renderPlot({
    title <- "New York weather during selected month"
    BarChart(weather_transit_data, input$month)
  })
  output$dependentVars <- renderDataTable({
    title <- "Coefficients for predicting Uber rides in New York"
    if(input$dependentVars == "Uber") {
      datatable(lmFromData(weather_transit_data, weather_transit_data$UberRides),
                    class = 'cell-border stripe', options = list(searching = FALSE,
                                                                 paging = FALSE))
    } 
    else if (input$dependentVars == "MTA Entries") {
      datatable(lmFromData(weather_transit_data, weather_transit_data$entries),
                    class = 'cell-border stripe', options = list(searching = FALSE,
                                                                 paging = FALSE)) 
    } 
    else if (input$dependentVars == "MTA Exits") {
      datatable(lmFromData(weather_transit_data, weather_transit_data$exits),
                    class = 'cell-border stripe', options = list(searching = FALSE,
                                                                 paging = FALSE)) 
    }
  })
  output$mymap <- renderLeaflet({
    UbersPerDate %>%
      leaflet() %>%
      setView(lat = 40.7484405, lng = -73.9878531, zoom = 11) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircles(~lat.x, ~lon.x,
                 col = ~color_Prop, radius = ~as.numeric(entries) / 5490,
                 group = "MTA Entries / Income", popup = content_elasticity) %>%
      addLegend(pal = pal, values = ~inc2015, title = "Median Income for Tract Around Station") %>%
      addCircles(~lat.x, ~lon.x,
                 col = ~color_Prop, radius = ~as.numeric(exits) / 5490,
                 group = "MTA Exits / Income", popup = content_elasticity) %>%
      addCircles(~lat.x, ~lon.x,
                 col = ~color_Prop, radius = ~as.numeric(UberRides) / 183,
                 group = "Uber / Income", popup = content_elasticity ) %>%
      addLayersControl(
        baseGroups = c("MTA Entries / Income", "MTA Exits / Income", "Uber / Income"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  output$weekday <- renderPlot({
    if(input$weekday == "MTA Entries") {
      plotWeekDayMTA(MTAWeekdays)
    } 
    else if (input$weekday == "Uber") {
      plotWeekDayUber(UberWeekdays)
    } 
    else if (input$weekday == "Comparison") {
      plotWeekDayCompare(CompareWeekdays)
    }
  })
  output$elasticity <- renderLeaflet({
    if(input$elasticity == "Elasticity") {
      StationIncomeElasticity %>%
        leaflet() %>%
        setView(lat = 40.7906568, lng = -73.8809692, zoom = 10.5) %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
        addCircles(~lon, ~lat,
                   col = ~pal_elasticity_MTA(ElasticityMTA), 
                   radius = ~inc2015 / 500,
                   group = "Rain elasticity MTA",
                   popup = content_elasticity) %>%
        addLegend(pal = pal_elasticity_MTA, values = ~ElasticityMTA, title = "MTA precipitation elasticity", 
                  group = "Rain elasticity MTA") %>%
        addCircles(~lon, ~lat,
                   col = ~pal_elasticity_Uber(ElasticityUber), 
                   radius = ~inc2015 / 500,
                   group = "Rain elasticity Uber",
                   popup = content_elasticity) %>%
        addLegend(pal = pal_elasticity_MTA, values = ~ElasticityUber, title = "Uber precipitation elasticity", 
                  group = "Rain elasticity Uber") %>%
        addLayersControl(
          baseGroups = c("Rain elasticity MTA", "Rain elasticity Uber"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    else if (input$elasticity == "Coefficients") {
      StationIncomeElasticity %>%
        leaflet() %>%
        setView(lat = 40.7906568, lng = -73.8809692, zoom = 10.5) %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
        addCircles(~lon, ~lat,
                   col = ~pal_coefficients_MTA(MTAEstimate), 
                   radius = ~inc2015 / 500,
                   group = "Precitipation coefficient MTA",
                   popup = content_elasticity ) %>%
        addLegend(pal = pal_coefficients_MTA, values = ~MTAEstimate, title = "Station coefficients for precipitation (MTA)") %>%
        addCircles(~lon, ~lat,
                   col = ~pal_coefficients_Uber(UberEstimate), 
                   radius = ~inc2015 / 500,
                   group = "Precitipation coefficient Uber",
                   popup = content_elasticity) %>%
        addLegend(pal = pal_coefficients_Uber, values = ~UberEstimate, title = "Station coefficients for precipitation (Uber)") %>%
        addLayersControl(
          baseGroups = c("Precitipation coefficient MTA", "Precitipation coefficient Uber"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
  })
  output$elasticityReg <- renderDataTable({
    title <- "Rain elasticity of Uber and MTA use"
    if(input$elasticityReg == "Uber") {
      datatable(lmIncomeRent(StationIncomeElasticity, StationIncomeElasticity$ElasticityUber),
                    class = 'cell-border stripe', options = list(searching = FALSE,
                                                                 paging = FALSE))  
    } 
    else if (input$elasticityReg == "MTA") {
      datatable(lmIncomeRent(StationIncomeElasticity, StationIncomeElasticity$ElasticityMTA),
                class = 'cell-border stripe', 
                options = list(searching = FALSE, 
                               paging = FALSE)) 
    } 
  })
  # We ended up using this just as an image, due to rendering problems last minute
  # output$wordcloud <- renderPlot({
  #   if(input$wordcloud == "Entries") {
  #     entrycloud <- wordcloud2(mta_entry_df, size = 0.35, 
  #                              color = "random-dark", shape = "circle")
  #     entrycloud
  #   } 
  #   else if (input$wordcloud == "Exits") {
  #     exitcloud <- wordcloud2(mta_exit, color = "random-dark", 
  #                             size = 0.35, backgroundColor = "white", 
  #                             shape = "circle")
  #     exitcloud
  #   } 
  # })
}

# Render app
shinyApp(ui = ui, server = server)


