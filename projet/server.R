#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(DT)
install.packages('rsconnect')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data_perimetre1 <-  data
  data_perimetre2 <- data
  data_perimetre3 <- data
  data_perimetre4 <- data
  data_perimetre1 <- eventReactive(input$submit1, {
    data %>% filter(category %in% input$category1)
  }, ignoreNULL = FALSE)
  
  data_perimetre2 <- eventReactive(input$submit2, {
    data %>% filter(category %in% input$category2)
  }, ignoreNULL = FALSE)
  
  data_perimetre3 <- eventReactive(input$submit3, {
    data %>% filter(category %in% input$category3)
  }, ignoreNULL = FALSE)
  
  data_perimetre4 <- eventReactive(input$submit4, {
    data %>% filter(category %in% input$category4) %>% group_by(category) %>% arrange(desc(amount_raised)) %>% slice(1:10) %>% select(category,id,amount_raised,year) 
  }, ignoreNULL = FALSE)
  
  data_perimetre5 <- eventReactive(input$dltab, {
    data %>% filter(category %in% input$category4) %>% group_by(category) %>% arrange(desc(amount_raised)) %>% slice(1:10) %>% select(category,id,amount_raised,year) 
    
  }, ignoreNULL = FALSE)
  
  data_perimetre6 <- eventReactive(input$dltab, {
    data %>%  filter(category %in% input$category4) %>% group_by(category, year_month) %>% arrange(year_month) %>% summarise(total = sum(amount_raised))    
  }, ignoreNULL = FALSE)
  
  


  
  output$plot1 <- renderPlot({
    ## Nombre total nombre total de campagnes
    data_perimetre1() %>%  group_by(year_month) %>% summarise(total = n()) %>% ggplot(mapping = aes(x =
                                                                                                   year_month, y = total)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
      labs(title =
             'Variabilité du nombre de campagnes par mois', x = 'Date', y = 'Nombre de campagnes')
    
    
  })
  
  output$plot2 <- renderPlot({
    # montant moyen des campagnes financées 
    
    data_perimetre2() %>% group_by(category) %>% summarise(mean_amount =mean(amount_raised)) %>% ggplot(aes(x=category,y=mean_amount,fill=category)) +geom_bar(stat = 'identity') + theme_minimal() +
      labs(title =
             'Montant moyen des campagnes financées par catégorie', x = 'Catégorie', y = 'Montant moyen')
    
    
  })
  
  
  output$plot3 <- renderPlot({
    # Durée médiane des campagnes financées 
    
    data_perimetre3() %>% group_by(category) %>% summarise(median_amount =median(amount_raised)) %>% ggplot(aes(x=category,y=median_amount,fill=category)) +geom_bar(stat = 'identity') + theme_minimal() +
      labs(title =
             'Montant médian des campagnes financées par catégorie', x = 'Catégorie', y = 'Montant moyen')
    
  })
  
  
  
  output$table <- renderDataTable({
    # Top 10 campagnes avec les plus hauts financement
    
    data_perimetre4() %>% group_by(category) %>% arrange(desc(amount_raised)) %>% slice(1:10) %>% select(category,id,amount_raised,year) 

  })
  
  
  output$dltab<-downloadHandler(filename = function(){"top10_campagnes.csv"}, 
                                content = function(fname){

                                  write.csv(data_perimetre5(), fname)
                                }
    
  )
  
  output$plot6 <- renderImage({
    
    multiple_courbe <- data_courbe %>% ggplot(mapping = aes(x =
                                                              year_month, y = total,color = category)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
      labs(title =
             'Montant des financements de campagnes par mois', x = 'Date', y = 'Nombre de campagnes') + scale_y_continuous()
    
    anim_multiple_courbe <- data_courbe %>% ggplot(mapping = aes(x =
                                                                   year_month, y = total,color = category)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
      labs(title =
             'Montant des financements de campagnes par mois', x = 'Date', y = 'Nombre de campagnes')  + transition_reveal(year_month) +theme_light()
    anim_multiple_courbe
    
    animate(
      plot = anim_multiple_courbe,
      render = gifski_renderer(),
      height = 400,
      width = 700,
      duration = 10,
      fps = 10
    )
    
    anim_save('anim_multiple_courbe.gif', animate(anim_multiple_courbe))
    
    # Return a list containing the filename
    list(src = "anim_multiple_courbe.gif", contentType = "image/gif", width = 700,height = 400)
  },
  deleteFile = FALSE
    
  )
  
  
  
  
  
  
  
})

