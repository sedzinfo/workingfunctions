library(psycholatefunctions)
library(httr)
library(jsonlite)
library(xts)
library(zoo)
library(plotly)

prometheus_url<-"http://pip1.crabdance.com:1507/api/v1/query_range"
query_expression<-"humidity"

start_time<-"2023-06-01T00:00:00Z"
end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
step<-"1h"

query_params<-list(query=query_expression,start=start_time,end=end_time,step=step)
response<-GET(prometheus_url,query=query_params)

query_result<-content(response,as="text",encoding="UTF-8")
query_result_json<-fromJSON(query_result,flatten=TRUE,simplifyDataFrame=TRUE)
time_series_data<-data.frame(apply(query_result_json$data$result[[1]][[1]],2,as.numeric))
names(time_series_data)[2]<-query_expression

xts_data<-xts(as.numeric(time_series_data[,2]),
              order.by=as.POSIXct(time_series_data[,1]))

names(xts_data)<-query_expression
ts_data<-as.ts(zooreg(xts_data,deltat=1/24))
autoplot(decompose(ts_data))

xtsdf<-data.frame(xts_data)
xtsdf$date<-row.names(xtsdf)

plot_ly(data=xtsdf,x=~date,y=xtsdf[,query_expression],type="scatter",mode="lines",name=query_expression) %>%
  add_lines(x=~date,y=~zoo::rollmean(as.numeric(xtsdf[,query_expression]),k=10,align="center",fill=NA),
            type="scatter",mode="lines",name="Smoothed") %>%
  layout(title="",xaxis=list(title="Date"),
         yaxis=list(title=query_expression))




