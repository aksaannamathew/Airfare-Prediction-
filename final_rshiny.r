
library(shiny)
library(readxl)
library(forecast)
library(fpp)
library(smooth)
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(shinyTime)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(DT)

## domestic data
domestic<-read.csv("C:\\Users\\God\\Desktop\\domesticfare.csv")
domestic$Date<-as.Date(domestic$Date,format = "%d-%m-%Y")
domestic$Air<-na_ma(domestic$Air,k=3,weighting = "exponential")
domestic$Hotel=na_ma(domestic$Hotel,k=3,weighting = "simple")
dom.hotel<-ts(domestic$Hotel,start=c(2018,4,1),frequency=365)
dom.air<-ts(domestic$Air,start=c(2018,4,1),frequency=365)

##international data

inter<-read.csv("C:\\Users\\God\\Desktop\\Excelr Project\\INT_AIR_HOTLE.csv")
inter$Date<-as.Date(inter$Date,format = "%d-%m-%Y")
inter$Air<-na_ma(inter$Air,k=2,weighting = "exponential")
inter$Hotel<-na_interpolation(inter$Hotel,option = "linear") ## Fixed
inter.air<-ts(inter$Air,start=c(2018,4,1),frequency=365)
inter.hotel<-ts(inter$Hotel,start=c(2018,4,1),frequency=365)


ui=fluidPage(
  titlePanel("FARE PREDICTION"),
  
  sidebarPanel(
    selectInput("product_type","select the product type",choices = c("Domestic Air","Domestic Hotel","International Air","International Hotel")),
    numericInput("ahead","Days to forcast ahead",min=1,max=30,value = 7),
    submitButton("submit")
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel("NNETAR forecast",dataTableOutput("nnetar_forecast")),
      tabPanel("Forecast plot",plotOutput("forecast_plot"))
    )
  )
)


server=function(input,output){
  output$caption<-renderText({paste("FARE and FORECAST PLOT")})
  output$forecast_plot<-renderPlot({
    producttype<-input$product_type
    if(producttype=="Domestic Air"){
      fit.air<-nnetar(dom.air)
      plot(forecast(fit.air,h=input$ahead))}
      else if(producttype=="Domestic Hotel"){
     fit.hotel<-nnetar(dom.hotel)
     plot(forecast(fit.hotel,h=input$ahead))}
    else if(producttype=="International Air"){
      fit.int.air<-nnetar(inter.air)
      plot(forecast(fit.int.air,h=input$ahead))}
    else{
      fit.int.hotel<-nnetar(inter.hotel)
      plot(forecast(fit.int.hotel,h=input$ahead))}
    
  })
  output$nnetar_forecast<-DT::renderDataTable({
    producttype<-input$product_type
    if(producttype=="Domestic Air"){
      fit.air<-nnetar(dom.air)
      pred.air<-forecast(fit.air,h=input$ahead)
      pred<-data.frame(predict(pred.air,n.head=input$ahead))
      pred$Date<-seq(from= as.Date("2019-06-11"),length.out =input$ahead, by = 'day' )
      pred1<-pred[,c(2,1)]
      rownames(pred1) <- 1:nrow(pred1)
      pred1 
      }
    
    else if(producttype=="Domestic Hotel"){
      fit.hotel<-nnetar(dom.hotel)
      pred.hotel<-forecast(fit.hotel,h=input$ahead)
      pred.hotel<-data.frame(predict(pred.hotel,n.head=input$ahead))
      pred.hotel$Date<-seq(from= as.Date("2019-06-11"),length.out =input$ahead, by = 'day' )
      pred2<-pred.hotel[,c(2,1)]
      rownames(pred2) <- 1:nrow(pred.hotel)
      pred2 
    }
    else if(producttype=="International Air"){
      fit.int.air<-nnetar(inter.air)
      pred.int.air<-forecast(fit.int.air,h=input$ahead)
      pred.int.air<-data.frame(predict(pred.int.air,n.head=input$ahead))
      pred.int.air$Date<-seq(from= as.Date("2019-06-11"),length.out =input$ahead, by = 'day' )
      pred3<-pred.int.air[,c(2,1)]
      rownames(pred3) <- 1:nrow(pred.int.air)
      pred3
      
       }
    else{
      fit.int.hotel<-nnetar(inter.hotel)
      pred.int.hotel<-forecast(fit.int.hotel,h=input$ahead)
      pred.int.hotel<-data.frame(predict(pred.int.hotel,n.head=input$ahead))
      pred.int.hotel$Date<-seq(from= as.Date("2019-06-11"),length.out =input$ahead, by = 'day' )
      pred4<-pred.int.hotel[,c(2,1)]
      rownames(pred4) <- 1:nrow(pred.int.hotel)
      pred4 
      }
    
  })

  }

shinyApp(ui=ui,server = server)
