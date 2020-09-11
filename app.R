library(shiny)
library(shinydashboard)
library(ggplot2)
library(tseries)
library(forecast)
library(magrittr)
library(timeDate)
library(dplyr)
library(zoo)

header <- dashboardHeader(title = "Basic Dashboardd")  
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        
        selectInput("graphs", "Select a graph:",
                    choices = c('Select','TimeSeries','DecomposedSeries','DifferencedSeries','HoltWinter'),
                    selected = 'Select'),
        selectInput("acfbutton", "Select VALUES:",
                    choices = c('Select','ACF','PACF'),
                    selected = 'Select'),
        numericInput("pval", "P", value=""),
        numericInput("dval", "D", value=""),
        numericInput("qval", "Q", value=""),
        
        selectInput("arima", "Select a model:",
                    choices = c('Select','ARIMA'),
                    selected = 'Select'),
        
        numericInput("ndays", "Number of days to forecast:", value=0),
        
        selectInput("forecast", "Select to Forecast:",
                    choices = c('Select','Forecast-Plot','Forecast-Values'), 
                    selected = 'Select')
        )
)
recommendation <- read.csv('/Users/harmeshrana/Desktop/salerecord.csv',stringsAsFactors = F,header=T)
frow1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")
)


frow2 <- fluidRow( 
    box(
        title = "Profit Per Product"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,div(style = 'height: 50vh;overflow-x: auto;overflow-y: auto',plotOutput("revenuebyPrd", height = "500px",width = "800px"))
    ),
    box(
        title = "Profit Per Region"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,div(style = 'height: 50vh;overflow-x: auto;overflow-y: auto',plotOutput("revenuebyRegion", height = "500px",width = "800px"))
    ), 
    box(
        title = "Sale Channel"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("chart")
    ),
    box(
        title = "Profit By offlineMode"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,div(style = 'height: 50vh;overflow-x: auto;overflow-y: auto',plotOutput("saleoffline", height = "500px",width = "800px"))
    ),
    box(
        title = "Graphs"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE,
    tabBox(
        id = "",
        side = "right",
        title = "Graphs",
        width = 100,
        div(
        tabPanel(plotOutput("graphs"),title="Time Series Graph"))
    )),
    box(
        title = "ACF/PACF VALUES"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE,
        div(
    tabBox(
        id = "acfpacf",
        side = "right",
        title = "Graphs",
        width = 100,
       tabPanel(id="acff",plotOutput("acfff"),title="ACF Graph"),
        tabPanel(id="pacff",plotOutput("acfff"),title="PACF Graph")
    ))),
    
    box(
        title = "Arimaa Model Value"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,verbatimTextOutput('arimaPlot')
    ),
    box(
        title = "Forecasting Of data"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,div(style = 'height: 50vh;overflow-x: auto;overflow-y: auto',plotOutput("forecastPlot", height = "500px",width = "800px"))
    )
    
)

body <- dashboardBody(frow1,frow2)
ui <- dashboardPage(title = 'This is my Page title', header, sidebar,body,skin="blue")               

server <- function(input, output) {
    total.revenue <- sum(recommendation$TotalRevenue)
    most.sale <- recommendation %>% group_by(Region) %>% summarise(value = sum(TotalRevenue)) %>% filter(value==max(value))
    prof.prod <- recommendation %>% group_by(itemtype) %>% summarise(value = sum(TotalRevenue)) %>% filter(value==max(value))
    
    output$value1 <- renderValueBox({
        valueBox(
            formatC(most.sale$value, format="d", big.mark=',',width = 1)
            ,paste('Top Region :',most.sale$Region)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")  
    })
    output$value2 <- renderValueBox({ 
        valueBox(
            formatC(total.revenue, format="d", big.mark=',')
            ,'Total Expected Revenue'
            ,icon = icon("gbp",lib='glyphicon')
            ,color = "green")  
    })
    
    output$value3 <- renderValueBox({
        valueBox(
            formatC(prof.prod$value, format="d", big.mark=',')
            ,paste('Top Product:',prof.prod$itemtype)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "yellow")   
    })
    output$revenuebyPrd <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=itemtype, y=TotalProfit, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Product") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
             labs(fill = "Region")
    })
    output$revenuebyRegion <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=Region, y=TotalProfit, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Region") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
          labs(fill = "Region")
    })
    
    
    output$saleoffline <- renderPlot({
        ggplot(data = subb, 
               aes(x=itemtype, y=TotalProfit, fill=factor(itemtype))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Profit (in Euros)") + 
            xlab("Product") + theme(legend.position="bottom" 
                                   ,plot.title = element_text(size=15, face="bold")) + 
            labs(fill = "Product")
    })
    output$chart <- renderPlot({
        pie(z,labels = c("offline","online"))
    })
    output$graphs <- renderPlot({
        if(input$graphs == 'TimeSeries'){
            offprofit=c(subb$TotalProfit)
            tsoff=ts(offprofit,start = c(2016,1),frequency = 12)
            plot.ts(tsoff, main = "Time-Series plot", col = "blue")
        }
    })
    
    output$acfff <- renderPlot({
        if(input$acfbutton=='ACF'){
            acff=acf(tsoff,lag.max = 20)
            if(input$acfbutton=='PACF'){
                pacff=pacf(tsoff,lag.max = 20)
                }
        
        }})
    
    output$value <- renderText({ input$pval })
    output$value <- renderText({ input$qval })
    output$value <- renderText({ input$dval })
    
       
        
    output$arimaPlot <- renderText({ 
        if(input$arima == 'ARIMA'){
            ##  Augmented Dickey-Fuller Test for Stationarity
            amv <<- arimafunc()
            paste("AIC:",amv$aic," AICc:",amv$aicc," BIC:",amv$bic)
        }
    })
    
    arimafunc <- function(){
        M_arima <- Arima(tsoff, order=c(input$pval,input$dval,input$qval))
        return(M_arima)
    }
    
    output$forecastPlot <- renderPlot({
        if(input$forecast == 'Forecast-Plot'){
            if(input$ndays == 0){
                error()
            } else {
                fc <<- forecastfunc()
                plot(fc, col = "darkgreen")
            }
        }
    })
    forecastfunc <- function(){
        Mforecasts <- forecast(amv, h = input$ndays)
        return(Mforecasts)
    }
    
   
     }



shinyApp(ui = ui, server = server)
