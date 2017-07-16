#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(REmap)
library(leaflet)
library(leafletCN)
library(plyr)
library(reshape2)
library(plotly)
library(htmltools)
library(lubridate)


options(shiny.maxRequestSize=100*1024^2) 


# Define UI for application that draws a histogram


header <- dashboardHeader(title = "各城市房价月同比Dashboard")

body <- dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, 
               leafletOutput("map", height = 600)
           ),
          box(width = NULL,
              plotlyOutput("plot"),height = 480)
          ),
    column(width = 4,
           box(width = NULL, status = "warning",
               title = '上传CSV文件',
               fileInput('datafile',
                         label = "",accept=c('text/csv',
                                             'text/comma-separated-values,text/plain',
                                             '.csv'))
           ),
           # box(width = NULL,
           #     status = "warning",
           #     title = '下载表格文件',
           #     downloadButton('downloadData', '下载表格文件')),
           box(width = NULL,status = "warning",
               title = '选择日期(只作用于地图)',
               sliderInput("date", "日期",
                           min = as.Date("2005-07-01"), max = Sys.Date(),
                           value = as.Date("2005-07-01"), ticks = TRUE,animate = animationOptions(interval=1000, loop=TRUE),timeFormat = "%Y-%m",step = 30,dragRange = FALSE),
               p(
                 class = "text-muted",
                 paste("Note: 点击开始按钮观察各城市月同比（气泡大小）变化"
                 )
               )
           ),
                  box(width = NULL,status = "warning",
                  title = '选择城市',
                  selectInput("city",
                              "城市:",
                              "",multiple = FALSE)),
           box(width = NULL,
                 dataTableOutput("table")
           )
    )
  ))


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output, session) {

  data_default<-read.csv("房地产价格.csv", header = T)
  
  data_raw <- reactive({


    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      tmp<-data_default
      return(tmp)
      #return(NULL)
    }
    else {
      tmp<-read.csv(infile$datapath)
      return(tmp)
      }
    tmp
  })

  choices<-reactive({
    data_raw<-data_raw()
    tmp_1<-colnames(data_raw)[-1]
    tmp<-append('全部',tmp_1)
  })
  
  observe({
   
    updateSelectInput(session, "city",
                      choices = choices()
    )})
  
  data_combined<-eventReactive(data_raw(),({ 
    data_raw<-data_raw()  
    data_modified<-melt(data_raw, na.rm=TRUE, '频率')
    
    colnames(data_modified)<-c('date','city','mom')
    data_modified$mom<-as.numeric(data_modified$mom)
    
    cit_list<-as.vector(unique(data_modified$city))
    
    city_geolocation<-get_geo_position(cit_list)
    
    data_combined<-merge(data_modified, city_geolocation, by='city')
    return(data_combined)
    }
  ))
    
  data_final<-reactive({
    data_combined<-data_combined()
    if (input$city != "全部") {
      tmp <- subset(data_combined, city==input$city)
      data_combined_city<-tmp[1:3]
    }
    else {data_combined_city<-data_combined[1:3]}
    colnames(data_combined_city)<-c('城市','日期','月同比')
    
    # input_year_1<-sapply(as.character(input$date[1]),function(x){strsplit(x,"-")[[1]][1]})
    # input_month_1<-sapply(as.character(input$date[1]),function(x){strsplit(x,"-")[[1]][2]})
    # year_month_1<-as.character(paste0(input_year_1,'-',input_month_1))
    # 
    # input_year_2<-sapply(as.character(input$date[2]),function(x){strsplit(x,"-")[[1]][1]})
    # input_month_2<-sapply(as.character(input$date[2]),function(x){strsplit(x,"-")[[1]][2]})
    # year_month_2<-as.character(paste0(input_year_2,'-',input_month_2))
    
    #data_combined_city_subet<-subset(data_combined_city, date>=input$year[1]&date<=input$year[1])
    
    data_combined_city
  })
  
  output$table<-renderDataTable({
    data_final()
  }, options = list(searching= TRUE,lengthMenu = list(c( 10,15, 50, -1), c( '10','15', '50', 'All')),
                    pageLength = 10, searching = FALSE,pagingType = 'simple'))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_final", ".csv", sep="")
    },
    content = function(file) {
      
      write.csv(data_final(), file, row.names = F,fileEncoding='GBK')
    })
    
  output$plot<-renderPlotly({
    plot_ly(data_final(), x = ~日期, y= ~月同比, split=~城市, type = 'scatter', mode = 'lines', name = '月同比')%>%
      layout(xaxis = list(title = '日期', autorange=TRUE,tickmode="array", tickvals=~日期, ticktext=format(~日期, "%b-%Y")),yaxis = list(title = '房价月同比',tickmode="auto"))
  })
  
  
  output$map <- renderLeaflet({
    data_combined<-data_combined()
    if (input$city != "全部") {
      data_combined_city <- subset(data_combined, city==input$city)
      tmp<-data_combined_city
    }
    else {tmp<-data_combined}
    
    input_year_1<-sapply(as.character(input$date),function(x){strsplit(x,"-")[[1]][1]})
    input_month_1<-sapply(as.character(input$date),function(x){strsplit(x,"-")[[1]][2]})
    
    year_month_1<-as.character(paste0(input_year_1,'-',input_month_1))
    
    data_combined_date<-subset(tmp, date==year_month_1)
    
    factpal <- colorFactor('red', data_combined$city,alpha = FALSE)
    
    map<-leaflet(data = data_combined_date,options = leafletOptions(zoom = 6, minZoom = 4, maxZoom = 8)) %>%amap()  %>% addMiniMap()%>% addCircleMarkers(~as.numeric(lon),~as.numeric(lat),popup = ~as.character(paste0('日期:',date,' ／ ','城市:',city,' ／ ','月同比:',mom)),color=~factpal(city), radius=~((as.integer(mom)+20)),stroke = F,weight = 1)
    map
  })
  
}

shinyApp(ui, server)