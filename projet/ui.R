#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- dashboardPage(
  dashboardHeader(title = 'Campagnes'),
  dashboardSidebar(
    sidebarMenu(
    menuItem(
      "Graphique1",
      tabName = "Graphique_1",
      icon = icon("chart-line", selected = TRUE)
    ),
    menuItem(
      "Graphique2",
      tabName = "Graphique_2",
      icon = icon("chart-line")
    ),
    menuItem(
      "Graphique3",
      tabName = "Graphique_3",
      icon = icon("chart-line")
    ),
    menuItem(
      "Top10",
      tabName = "Top10",
      icon = icon("fas fa-table")
    ),
    menuItem(
      "Animate_Chart",
      tabName = "Animate_Chart",
      icon = icon("chart-line")
    )
    
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "Graphique_1",
            box(plotOutput("plot1"), width = 500),
            box(
              pickerInput("category1",'Sélectionné au moins une catégorie', choices=data %>% distinct(category), options = list(`actions-box` = TRUE),multiple = T,selected = "Musique" )
              ,
              actionButton("submit1", "Appliquer",type="primary")
              )
            ),
    tabItem(tabName = "Graphique_2",
            box(plotOutput("plot2"), width = 500),
            box(
              pickerInput("category2",'Sélectionné au moins une catégorie', choices=data %>% distinct(category), options = list(`actions-box` = TRUE),multiple = T,selected = 'Musique' )
              ,
              actionButton("submit2", "Appliquer",type="primary")
            )),
    tabItem(tabName = "Graphique_3",
            box(plotOutput("plot3"), width = 500),
            box(
              pickerInput("category3",'Sélectionné au moins une catégorie', choices=data %>% distinct(category), options = list(`actions-box` = TRUE),multiple = T,selected = 'Musique' )
              ,
              actionButton("submit3", "Appliquer",type="primary")
            )),
    tabItem(tabName = "Top10",
            box(dataTableOutput("table"), width = 500),
            box(
              pickerInput("category4",'Sélectionné au moins une catégorie', choices=data %>% distinct(category), options = list(`actions-box` = TRUE),multiple = T,selected = 'Musique' )
              ,
              actionButton("submit4", "Appliquer",type="primary"),
              downloadButton("dltab", "Download")
            )), 
    tabItem(tabName = "Animate_Chart",
                        box(plotOutput("plot6"), width = 1000,height = 500),
        
                        box(
                          pickerInput("category6",'Sélectionné au moins une catégorie', choices=data %>% distinct(category), options = list(`actions-box` = TRUE),multiple = T,selected = 'Musique' )
                          ,
                          actionButton("submit6", "Appliquer",type="primary")
                        ))
  
    
                                
    )
  
  )
  )
  