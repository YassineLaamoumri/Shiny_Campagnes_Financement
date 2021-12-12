#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(shiny)
# library(plotly)
# library(tidyverse)
# library(scales)
# library(DT)
# library(readr)
# library(readxl)
# library(skimr)
# library(dplyr)
# library(purrr)
# library(lubridate)
# library(ggplot2)
# library(data.table)
# library(stringr)
# library(forcats)
# library(tidyr)
# library(stringi)
# library(gganimate)
# library(scales)

source('global.R')

ui <- dashboardPage(
  dashboardHeader(title = 'Campagnes Ulule'),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Nombre de Campagnes",
        tabName = "Graphique_1",
        icon = icon("chart-line", selected = TRUE)
      ),
      menuItem(
        "Montant moyen",
        tabName = "Graphique_2",
        icon = icon("chart-line")
      ),
      menuItem(
        "Montant médian",
        tabName = "Graphique_3",
        icon = icon("chart-line")
      ),
      menuItem("Top 10",
               tabName = "Top10",
               icon = icon("fas fa-table")),
      menuItem(
        "Animate Chart",
        tabName = "Animate_Chart",
        icon = icon("chart-line")
      )
      
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "Graphique_1",
      box(plotlyOutput("plot1"), width = 500),
      box(
        pickerInput(
          "category1",
          'Sélectionné au moins une catégorie',
          choices = data %>% distinct(category),
          options = list(`actions-box` = TRUE),
          multiple = T,
          selected = "Musique"
        )
        ,
        actionButton("submit1", "Appliquer", type = "primary")
      )
    ),
    tabItem(
      tabName = "Graphique_2",
      box(plotlyOutput("plot2"), width = 500),
      box(
        pickerInput(
          "category2",
          'Sélectionné au moins une catégorie',
          choices = data %>% distinct(category),
          options = list(`actions-box` = TRUE),
          multiple = T,
          selected = 'Musique'
        )
        ,
        actionButton("submit2", "Appliquer", type = "primary")
      )
    ),
    tabItem(
      tabName = "Graphique_3",
      box(plotlyOutput("plot3"), width = 500),
      box(
        pickerInput(
          "category3",
          'Sélectionné au moins une catégorie',
          choices = data %>% distinct(category),
          options = list(`actions-box` = TRUE),
          multiple = T,
          selected = 'Musique'
        )
        ,
        actionButton("submit3", "Appliquer", type = "primary")
      )
    ),
    tabItem(
      tabName = "Top10",
      box(dataTableOutput("table"), width = 500),
      box(
        pickerInput(
          "category4",
          'Sélectionné au moins une catégorie',
          choices = data %>% distinct(category),
          options = list(`actions-box` = TRUE),
          multiple = T,
          selected = 'Musique'
        )
        ,
        actionButton("submit4", "Appliquer", type = "primary"),
        downloadButton("dltab", "Download")
      )
    ),
    tabItem(
      tabName = "Animate_Chart",
      box(
        img(
          src = "anim_multiple_courbe.gif",
          align = "left",
          height = '600px',
          width = '1000px'
        ),
        width = 500
      ),
      
      box(
        pickerInput(
          "category6",
          'Sélectionné au moins une catégorie',
          choices = data %>% distinct(category),
          options = list(`actions-box` = TRUE),
          multiple = T,
          selected = 'Musique'
        )
        ,
        actionButton("submit6", "Appliquer", type = "primary"),
        downloadButton("dlmp4", "Download")
        
      ),
      p("Ce graphique est animé grâce à gganimate. Le graphique est normalement réactive mais il y a des soucis de compatibilité entre le serveur Shinyapss.io et gganimate.
      En local, le graphique est réactif. Cependant, il est tout de même possible de télécharger le graphique sous format GIF, il faut attendre environ 1 minute pour le générer.
      Il suffit de choisir les catégories que l'on souhaite représentées et télécharger le GIF correspondant.")
    )
    
    
    
    
  ))
)
