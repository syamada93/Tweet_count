library(shiny)

if(!require(data.table))
  install.packages("data.table")
library(data.table)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(tidyr))
  install.packages("tidyr")
library(tidyr)

if(!require(rtweet))
  install.packages("rtweet")
library(rtweet)

if(!require(lubridate))
  install.packages("lubridate")
library(lubridate)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(dygraphs))
  install.packages("dygraphs")
library(dygraphs)

# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  # Application title
  titlePanel("検索ワードのツイート数の推移"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    sidebarPanel(
      column(2,
             "抽出する単語",
             textInput("wd",
                       NULL,
                       "大雨")),
      br(),
      actionButton("button","抽出開始"),
      # submitButton()
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(6,
             textOutput("TM"),
             dygraphOutput("Hdy"),
             dygraphOutput("cmHdy")),
      column(6,
             downloadButton("CSV","リツイート数上位のツイートのダウンロード"),
             tableOutput("RTweet")),
      width = 12
    )
  )
)

#SERVER####
# Define server logic required to draw a histogram
server <- function(input, output) {
  refreshPlot0 <- reactiveTimer(intervalMs = 1)
  # refreshPlot <- reactiveTimer(intervalMs = 60000)
  
  TDS <- data.frame()
  wd="大雨"
  wd="コロナ"

  WD <- eventReactive(input$button,{
    if(file.exists("TDS.csv"))
      file.remove("TDS.csv")
    return(input$wd)
  })
 
  observe({
    output$TM <- renderText({
      as.character(Sys.time())
    })
    refreshPlot0()
    wd=WD()
    if(file.exists("TDS.csv"))
      TDS <- fread("TDS.csv",encoding = "UTF-8") %>%
      data.frame() %>%
      mutate(YMD_HM=as.POSIXct(YMD_HM))
    if(floor(second(Sys.time()))!=0&nrow(TDS)!=0)
      return()

    td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)#,retryonratelimit = T)
    tds <-
      td %>%
      mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
      mutate(YMD_HM=ymd_hm(format(JTime,"%Y%m%d %H%M"))) %>%
      mutate(RT=is_retweet|is_quote) %>%
      arrange(desc(status_id)) %>%
      mutate(ID=paste0("Row",1:n())) %>% 
      data.frame()
    
    day=format(max(tds$YMD_HM),"%Y%m%d_%H%M")
    md=min(tds$YMD_HM)
    
    TDS <-
      TDS %>%
      rbind(tds) %>%
      mutate_at(vars(ends_with("id")),funs(gsub("x","",.))) %>%
      distinct(status_id,.keep_all = T)
    
    write_as_csv(TDS,"TDS.csv")
    
    TDC <-
      TDS %>%
      count(YMD_HM,RT)
    
    print(head(TDC %>% arrange(desc(YMD_HM))))
    
    write_as_csv(TDC,"TDC.csv")
    
    TDSS <-
      TDS %>%
      mutate(RID=ifelse(is_retweet,retweet_status_id,ifelse(is_quote,quoted_status_id,status_id))) %>%
      arrange(status_id) %>%
      group_by(RID) %>%
      mutate(RTc=n()) %>%
      ungroup() %>%
      distinct(RID,.keep_all = T) %>%
      arrange(desc(RTc)) %>%
      mutate(Rank=dense_rank(-RTc)) %>%
      filter(Rank<=10) %>%
      select(RID,Time=JTime,Tweet=text,RTc=RTc)
    
    output$CSV <- downloadHandler(
      filename = function(){
        paste0("リツイート数上位のツイート_",day,".csv")},
      content <- function(file){
        write_as_csv(TDSS,file)
      }
    )
    
    output$Hdy <- renderDygraph({
      Comp <- 
        # data.frame(YMD_HM=(max(TDC$YMD_HM)-60*60):(max(TDC$YMD_HM))) %>%
        data.frame(YMD_HM=rep(seq(min(TDC$YMD_HM,na.rm = T),max(TDC$YMD_HM,na.rm=T),60),each=2),
                   RT=c(F,T))
      TDCS <-
        Comp %>%
        left_join(TDC) %>%
        complete(YMD_HM,RT,fill=list(n=0)) %>%
        mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
        select(YMD_HM,RTs,n) %>%
        spread(RTs,n) %>%
        select(Retweet,Origin)
      
      rownames(TDCS) <- unique(Comp$YMD_HM)
      
      dygraph(TDCS,main = paste0(max(TDC$YMD_HM)-2*60*60,"～",max(TDC$YMD_HM),br(),"ツイート数 1分ごとの推移")) %>% #
        dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                  axisLabelFontSize = 20,axisLabelWidth = 100,titleHeight = 30,labelsKMB = T) %>%
        dyRangeSelector(height = 50,keepMouseZoom = T,dateWindow = c(max(TDC$YMD_HM)-11*60*60,max(TDC$YMD_HM)-9*60*60)) %>%
        dyLegend(width = 175)
    })
    
    output$cmHdy <- renderDygraph({
      Comp <- 
        # data.frame(YMD_HM=(max(TDC$YMD_HM)-60*60):(max(TDC$YMD_HM))) %>%
        data.frame(YMD_HM=rep(seq(min(TDC$YMD_HM,na.rm = T),max(TDC$YMD_HM,na.rm = T),60),each=2),
                   RT=c(F,T))
      TDCS <-
        Comp %>%
        left_join(TDC) %>%
        complete(YMD_HM,RT,fill=list(n=0)) %>%
        mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
        group_by(RTs) %>%
        mutate(n=cumsum(n)) %>%
        mutate(tn=1:n()) %>%
        mutate(rate=n/tn) %>%
        ungroup() %>%
        select(YMD_HM,RTs,n=rate) %>%
        spread(RTs,n) %>%
        select(Retweet,Origin)
      
      rownames(TDCS) <- unique(Comp$YMD_HM)
      
      dygraph(TDCS,main = paste0("平均ツイート数")) %>% #,main = paste0(max(TDC$YMD_HM)-2*60*60,"～",max(TDC$YMD_HM))
        dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                  axisLabelFontSize = 20,axisLabelWidth = 100,titleHeight = 30,labelsKMB = T) %>%
        dyRangeSelector(height = 50,keepMouseZoom = T,dateWindow = c(max(TDC$YMD_HM)-11*60*60,max(TDC$YMD_HM)-9*60*60)) %>%
        dyLegend(width = 175)
    })
    
    output$RTweet <- renderTable({
      TDSS %>%
        select(-RID) %>%
        mutate(Time=as.character(Time))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)