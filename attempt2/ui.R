## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
#gtd=read.csv("globalTerrorism.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Tracking Terror"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Motivation", tabName = "start", icon = icon("globe")),
      menuItem("Regional", tabName = "reg", icon = icon("globe")),
      menuItem("World Map", tabName = "map2", icon = icon("globe")),
      menuItem("Deeper look By Country", tabName = "map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    
    #tab 1 will be an intro page
    
    tabItems(    tabItem(tabName = "start",
                         fluidRow(box(class = "header",tags$h4("Understanding Modern Day Terrorism"), width = 12),align = 'center'),
                         fluidRow(box(tags$p("One of the key requirements developing effective foreign policy in the 
                              modern age is thourough understanding of terrorism throughout the Globe.This tool is designed to help present a comprehensive overview of global terrorism in the 
                              last decade as well as allowing provinding specifics on indivual groups and countries"),width=12),align='center'),
                         fluidRow(box(
                           tags$p("Definition of Terrorism: The threatened or actual use of illegal force and violence by a non-state actor to attain a political, 
                                  economic, religious, or social goal through fear, coercion, or intimidation."),
                           tags$p('Geography: Worldwide'),
                           tags$p("Time period: 2006-2016,"),
                           tags$p("Variable:Location, tactics, perpetrators, targets, and outcomes")
                           ,width=12),align='center'),
                         fluidRow(box(
                                tags$p("Warning"),
                                tags$p("Sources: Unclassified media articles (NOTE: Please interpret changes over time with caution. 
Global patterns are driven by diverse trends in particular regions, and data collection is influenced 
by fluctuations in access to media coverage over both time and place.)",tags$a(href = "https://www.kaggle.com/START-UMD/gtd",
                                                                               "(reference)"))
                                      ,width=12),align='center')
                         
                         ),
      
      
      # Tab 2 will be a general overview of conflict region to region
      tabItem(tabName = "reg",
              fluidRow(
                box( #PLots the bar graph from the server labeled plot1
                  plotOutput('plot1', height = 300, width = 600)
                  ),
                box(#ploty is the line plot ofver years
                  plotOutput('plotyr', height = 300, width = 600)
                ),
                box(#lets user choose the year
                  title = "Year",
                  sliderInput("slider", "Time Frame", 2006, 2016, 1,animate = TRUE)
                ),
                box(#Lets the user choose the region
                  title="Region",
                  selectizeInput(inputId = "area",
                                 label = "World Region",
                                 choices = unique(gtd[, "region_txt"]))
                )
              )
      ),
      
      # Tab 3 will plot incidents on a world
      tabItem(tabName = "map2",
              fluidRow(
                box(title = "Most Active Groups",
                    tableOutput('tableTerror')
                ),
                box(#lets user input region
                  selectizeInput(inputId = "reg3",
                                 label = "World Region",
                                 choices = unique(gtd[, "region_txt"]))
                ),
                box(#lets user input year
                  sliderInput("format", "Time Frame",
                              min = 2006, max = 2016,
                              value = 0, step = 1
                  )
                ),
                leafletOutput("plotmp2", height = 500, width = 1200)
                #plot world map
                )
      ),
      

      # 4th tab 
      tabItem(tabName = "map",
              fluidRow(
                box(#let user pick region
                  selectizeInput(inputId = "reg2",
                                 label = "World Region",
                                 choices = unique(gtd[, "region_txt"]))
                    ),
                box(#select country
                selectizeInput(inputId = "Nat",
                               label = "Country",
                              choices = unique(gtd[, "country_txt"]))
                    ),
              box(#a table of terrorist targets
                tableOutput('plotcountry')
                  ),
              box(#a table of terrorist targets
                tableOutput('targ')
                  ),
              box(#a table of terrorist Perpertraote
                tableOutput('perp')
                 ),
              box(#ploty is the line plot ofver years
                plotOutput('plotyrnat', height = 300, width = 600)
              )
                    )
                
              )

      
              
              )
    )
)

