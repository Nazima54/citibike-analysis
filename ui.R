library(shinydashboard)
library(shiny)
Agegroup = sort(unique(df$agegroup))
Gender = unique(df$gender)
Dayofweek = unique(df$dayofweek)
Startstationname = sort(unique(df$start.station.name))
Stopstationname = sort(unique(df$end.station.name))
Startrange = unique(df$startrange)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Citibike App'),
  #sidebar content
  dashboardSidebar(
    sidebarUserPanel(
      "DSLA Team"),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Step 1- When can I Ride", tabName = "when", icon = icon("calendar-times-o")),
      menuItem("Step 2- Where can I Ride", tabName = "where", icon = icon("map-marker")),
      menuItem("Step 3- How can I ride", tabName = "how", icon = icon("map-signs"))
    )
  ),
  
  # body content
  dashboardBody(
    tabItems(
      tabItem(tabName = 'when',
              fluidRow(
                box(title = h3("Step 1- Find the best day and time of the day"),
                    plotlyOutput('heatmap', height = 600),
                    width = 8),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_heatmap', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  selectInput(inputId = 'agegroup_heatmap', 
                              label = 'Age:', 
                              choices = Agegroup, 
                              selected = Agegroup[1]),
                  width = 4
                )
              )
      ),
      
      tabItem(tabName = "where",
              fluidRow(
                box(title = h3('Step 2- Locate the most concentrated areas'),
                    leafletOutput('tile', height = 600),
                    width = 8),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_tile', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  sliderInput(inputId = "age_tile", 
                              label = "Age range:", 
                              17, 98, c(17,29)),
                  selectInput(inputId = 'dayofweek_tile', 
                              label = 'Day of week:', 
                              choices = Dayofweek, 
                              selected = Dayofweek[1]),
                  radioButtons(inputId = 'startstop_tile',
                               label = 'Time of day:',
                               choices = c('departing', 'arriving'),
                               inline = TRUE),
                  sliderInput(inputId = "time_tile", 
                              label = "", 
                              0, 24, c(6,10)),
                  width = 4
                )
              )
      ),
      
      
      tabItem(tabName = "how",
              fluidRow(infoBoxOutput("durationGoogle"),
                       infoBoxOutput("durationCitibike")),
              fluidRow(box(title = h3('Step 3- Plan your ride'),
                           leafletOutput('map', height = 500), 
                           width = 8),
                       
                       box(selectInput(inputId = 'start_map', 
                                       label = 'Departing from:', 
                                       choices = Startstationname, 
                                       selected = Startstationname[1]),
                           selectInput(inputId = 'stop_map', 
                                       label = 'Going to:', 
                                       choices = Stopstationname, 
                                       selected = Stopstationname[1]),
                           selectInput(inputId = 'dayofweek_map',
                                       label = 'Day of week:',
                                       choices = Dayofweek,
                                       selected = Dayofweek[1]),
                           selectInput(inputId = 'startrange_map',
                                       label = 'Leave at (time of the day):',
                                       choices = Startrange,
                                       selected = Startrange[1]),
                           width = 4)
              ))
    ))
))

shinyServer(function(input, output) {
  
})
shinyApp(ui,server)


