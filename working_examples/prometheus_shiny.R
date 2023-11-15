library(shiny)
library(httr)
library(jsonlite)
library(xts)
library(zoo)
library(plotly)

ui<-fluidPage(
  titlePanel("Prometheus Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("query_expression","Query Expression",
                 choices=c("temperature","humidity","pressure","lux","proximity",
                             "NH3","reducing","oxidising",
                             "PM1","PM10","PM25","up"),selected="temperature"),
      dateRangeInput("date_range","Date Range",start="2023-06-01",end=Sys.time()),
      actionButton("submit_button","Submit")),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server<-function(input,output) {
  prometheus_url<-"http://pip1.crabdance.com:1505/api/v1/query_range"
  observeEvent(input$submit_button,{
    query_expression<-input$query_expression
    start_time<-format(input$date_range[1],"%Y-%m-%dT%H:%M:%SZ")
    end_time<-format(input$date_range[2],"%Y-%m-%dT%H:%M:%SZ")
    step<-"1h"
    
    query_params<-list(query=query_expression,start=start_time,end=end_time,step=step)
    response<-GET(prometheus_url,query=query_params)
    
    if (status_code(response) == 200) {
      query_result<-content(response,as="text",encoding="UTF-8")
      query_result_json<-fromJSON(query_result,flatten=TRUE,simplifyDataFrame=TRUE)
      time_series_data<-data.frame(apply(query_result_json$data$result[[1]][[1]],2,as.numeric))
      names(time_series_data)[2]<-query_expression
      
      xts_data<-xts(as.numeric(time_series_data[,2]),order.by=as.POSIXct(time_series_data[,1]))
      names(xts_data)<-query_expression
      ts_data<-as.ts(zooreg(xts_data,deltat=1/24))
      smoothed_data<-zoo::rollmean(as.numeric(xts_data[,query_expression]),k=10,align="center",fill=NA)
      
      xtsdf<-data.frame(xts_data)
      xtsdf$date<-row.names(xtsdf)
      
      output$plot<-renderPlotly({
        plot_ly(data=xtsdf,x=~date,y=~get(query_expression),type="scatter",mode="lines",name=query_expression) %>%
          add_lines(x=~date,y=~smoothed_data,type="scatter",mode="lines",name="Smoothed") %>%
          layout(title="",xaxis=list(title=""),yaxis=list(title=query_expression))
      })
    } else {
      print(paste0("Error: ",status_code(response)," - ",status_reason(response)))
    }
  })
}

shinyApp(ui,server)
