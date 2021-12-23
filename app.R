library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(rtweet)
library(dygraphs)


# library(stringi)
# library(ggraph)
# library(tidygraph)
# library(leaflet)
# library(leafletCN)
# library(maptools)
# library(sf)
# library(rsconnect)
# library(RMeCab)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("検索ワードのツイート数の推移"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("wd",
                "抽出する単語",
                "コロナ"),
      actionButton("button","抽出開始")
      # submitButton()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(6,
             textOutput("TM"),
             dygraphOutput("Hdy"),
             dygraphOutput("cmHdy")),
      column(6,
             tableOutput("RTweet")),
      width = 12
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  refreshPlot0 <- reactiveTimer(intervalMs = 1)
  # refreshPlot <- reactiveTimer(intervalMs = 60000)
  
  TDS <- data.frame()
  wd="コロナ"
  # tk=get_tokens()
  # print(tk$app)

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
      TDS <- fread("TDS.csv") %>%
      data.frame()
    if(floor(second(Sys.time()))!=0&nrow(TDS)!=0)
      return()
    
    print(wd)
    print(Sys.time())
    td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)#,retryonratelimit = T)
    print(Sys.time())
    tds <-
      td %>%
      mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
      mutate(YMD_HM=ymd_hm(format(JTime,"%Y%m%d %H%M"))) %>%
      mutate(RT=is_retweet|is_quote) %>%
      arrange(desc(status_id)) %>%
      mutate(ID=paste0("Row",1:n())) %>% 
      data.frame()
    
    day=max(tds$YMD_HM)
    mid=max(tds$status_id)
    print(paste(min(tds$JTime),max(tds$JTime),nrow(tds)))
    
    # write_as_csv(tds,paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"))
    # tds <- fread(paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"))
    
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
    
    write_as_csv(TDSS,"TDSS.csv")
    
    output$Hdy <- renderDygraph({
      Comp <- 
        # data.frame(YMD_HM=(max(TDC$YMD_HM)-60*60):(max(TDC$YMD_HM))) %>%
        data.frame(YMD_HM=rep(seq(min(TDC$YMD_HM),max(TDC$YMD_HM),60),each=2),
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
      
      dygraph(TDCS,main = paste0(max(TDC$YMD_HM)-2*60*60,"～",max(TDC$YMD_HM))) %>% #
        dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                  axisLabelFontSize = 20,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
        dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$YMD_HM)-11*60*60,max(TDC$YMD_HM)-9*60*60)) %>%
        dyLegend(width = 175)
    })
    
    output$cmHdy <- renderDygraph({
      Comp <- 
        # data.frame(YMD_HM=(max(TDC$YMD_HM)-60*60):(max(TDC$YMD_HM))) %>%
        data.frame(YMD_HM=rep(seq(min(TDC$YMD_HM),max(TDC$YMD_HM),60),each=2),
                   RT=c(F,T))
      TDCS <-
        Comp %>%
        left_join(TDC) %>%
        complete(YMD_HM,RT,fill=list(n=0)) %>%
        mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
        group_by(RTs) %>%
        mutate(n=cumsum(n)) %>%
        ungroup() %>%
        select(YMD_HM,RTs,n) %>%
        spread(RTs,n) %>%
        select(Retweet,Origin)
      
      rownames(TDCS) <- unique(Comp$YMD_HM)
      
      dygraph(TDCS) %>% #,main = paste0(max(TDC$YMD_HM)-2*60*60,"～",max(TDC$YMD_HM))
        dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                  axisLabelFontSize = 20,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
        dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$YMD_HM)-11*60*60,max(TDC$YMD_HM)-9*60*60)) %>%
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