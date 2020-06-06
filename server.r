library(syuzhet)
library(tm)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(SnowballC)
library(shiny)
library(shinydashboard)
library(qdap)
library(plotly)
library(tools)
library(vroom)
library(xfun)
library(httr)
library(rvest)
library(purrr)






shinyServer(function(session,input,output){
  observe({
    showNotification("The system may take some time to extract the data. Thank you for your patience :)")
  }) 
  


  
  #Reactive function -web scraping and read dataset 
  reactloaddata <-reactive({
    
    #Read data from file
    #dataset1 <- fread("C:/Users/galaxy/Documents/Icms CSE/YEAR 3/Dissertation/Extract/MDHotelUpdated20.csv")
    
    tryCatch({
            hoteltest<-data.frame()
            #urll<-"https://www.marideal.mu/hotel-deals.html"
            
            q<-c("https://www.marideal.mu/hotel-deals.html","https://www.marideal.mu/hotel-deals.html?p=2","https://www.marideal.mu/hotel-deals.html?p=3")
            
            for(i in q){
              
            url2<-i
            
            subTitle<-read_html(url2)%>% html_nodes(".product-item-link")%>% html_text()
            p<-length (subTitle)
            s<-html_session(url2)
            
            #hoteltest<-data.frame()
            for(j in subTitle[1:p]){
              page <- s%>% follow_link(j)%>%read_html()
              
              
              Title<-page%>% html_nodes("#product_addtocart_form h1")%>% html_text()
              Review<-page%>% html_nodes(".review-details")%>% html_text()
              Time<-page%>% html_nodes(".reviewed_date")%>% html_text()
              if(length(Review) != 0){
                hoteltest1<-data.frame(Title,Review,Time)
                hoteltest<-rbind(hoteltest,hoteltest1)
              }
              else{}
            }
            }
            #remove duplicate rows
            hoteltest<-distinct(hoteltest,Review,.keep_all = TRUE)
            hoteltest<-hoteltest
    },
    error = function(err){
      showNotification(paste0("Error: URL not found. Please try later"), type = 'err')
      return (NULL)
    })
    
  })
  
  
  #customise dataset
  reactcustdataset <-reactive({
    if (is.null(reactloaddata()))
      return(NULL)
    
    datasetv2<-reactloaddata()
    colnames(datasetv2)[1] <-"Title"
    datasetv2 <- datasetv2[order(datasetv2$Title),]
    #remove word  between brackets in title
    datasetv2$Title <- genX(datasetv2$Title," (", ")")
    #remove rows with empty value
    datasetv2<-datasetv2[!(is.na(datasetv2$Review) | datasetv2$Review==""), ]
    datasetv2<-datasetv2
  })
  
  #output dataset
  output$datasetAll <- renderTable({
    
    reactcustdataset()
    
    })
  
  
  
  #update list box
  observeEvent(
    reactcustdataset(),
    updateSelectInput(session,"hotel","Select a Hotel",choices=reactcustdataset()$Title)
  )
  
  datahotels<-reactive({
    hotelFilter <- subset(reactcustdataset(), reactcustdataset()$Title == input$hotel)
    
  })
  
  #output filter datasets
  output$datasethotel<- renderTable({
    datahotels()
  })
  
  #download datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$hotel, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datahotels(), file)
    }
  )
  
  #function calculate number of hotel
  numtitle <-reactive({
    lengthdisplay <- length(unique(reactcustdataset()$Title))
  })
  output$databasetitle <- renderValueBox({
    valueBox(numtitle(), "Hotels", icon = icon("address-book"),color = "light-blue")
  })
  
  #function calculate numbers reviews in dataset
  rowsCount <-reactive({
    rowcount <-nrow(reactcustdataset())
  })
  output$databaserows <- renderValueBox({
    valueBox(rowsCount(), "Reviews", icon = icon("id-card-o"),color = "light-blue")
  })
  
  
  observeEvent(
    #input$file,
    reactcustdataset(),
    updateSelectInput(session,"hotelAnalysis","Select a Hotel",choices=reactcustdataset()$Title)
  )
  
  #function select value from combobox 
  review <- reactive({
   #validate if combovalue is checked
    req(input$hotelAnalysis)
    
    #convert column 'Time' to date format
    dataset2 <-mutate(reactcustdataset(),Time=mdy(Time))
    g <- (subset(dataset2,dataset2$Title == input$hotelAnalysis))
    dataset2<-dataset2
    h <-g$Review
    list(dataset2=dataset2,h=h)
  })
  
  #preprocessing, calculate emotion
  mtreact <-reactive({
        
      lpmydata <- VCorpus(VectorSource(review()$h))
      lpmydata <- tm_map(lpmydata, content_transformer(tolower))
      lpmydata <- tm_map(lpmydata, removeWords, stopwords("english"))
      lpmydata <-  tm_map(lpmydata, stripWhitespace)
      lpmydata <- tm_map(lpmydata, removePunctuation)
      lpmydata <- tm_map(lpmydata, stemDocument)
      lpdtm <- TermDocumentMatrix(lpmydata)
      
      lpmydataCopy <- lpmydata
      lpresult <- get_nrc_sentiment(as.character(lpmydataCopy))
      lpresult1<-data.frame(t(lpresult))
      
      lpnew_result <- data.frame(rowSums(lpresult1))
      names(lpnew_result)[1] <- "score"
      lpnew_result <- cbind("emotion" = rownames(lpnew_result), lpnew_result)
      rownames(lpnew_result) <- NULL
      lpnew_result <-lpnew_result
      lpnew_result2 <-lpnew_result[1:8,]
      list(lpnew_result=lpnew_result,lpnew_result2=lpnew_result2)

  }) 
  
  #table display emotion result
  mtreact2 <-reactive({
    req(input$hotelAnalysis)
    
    review()$h
    if (review()$h != ""){
      mtreact()$lpnew_result2}
    else{print("No review")}
    
  })
  
  output$emotionTable <- renderTable({mtreact2()})

  #plot display emotion analysis result
  mtreact3 <-reactive({
    qplot(emotion, data=mtreact()$lpnew_result2, weight=score, geom="bar",fill=emotion)+xlab("Emotion") + ylab("Total Score")+ggtitle("Emotion Analysis")
   
  })
  
  output$emotionPlot<- renderPlot({
    if (is.null(reactloaddata()))
      return(NULL)
    
    review()$h
    if (review()$h != ""){
      mtreact3()}
    else{print("No review")}
    
  })
  
  
  #create plot title
  output$titleselect <-renderText({
    if (is.null(reactloaddata()))
      return(NULL)
    
    paste("Emotion analysis for '",input$hotelAnalysis,"' based on All Reviews")})
  
  #download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$hotelAnalysis, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(mtreact3())
      dev.off()
    }
  )
  
  
  #calculate num of rows for selected hotel
  mtreact4 <-reactive({
    as<-review()$dataset2
    g1 <- (subset(as,as$Title == input$hotelAnalysis))
    h1 <-as.vector(g1$Review)
    cal <- get_nrc_sentiment(h1)
    # g2 <- cbind(g1, cal)
    g2<-data.frame(g1)
    rowsum <-nrow(g2)
    list(g1=g1,cal=cal,rowsum=rowsum)
  })
  
  #valuebox display no of reviews
  output$rowsum <- renderValueBox({
    if(!is.null(input$hotelAnalysis)){
        review()$h
        if (review()$h != ""){
        valueBox(mtreact4()$rowsum, "Reviews", icon = icon("address-book"),color = "light-blue")
        }
        else{
          valueBox(0, "Review")
        }
    }else
    {
      print("No Data")
    }
  })

  #seperating date, month,year
  reactdatacopy <-reactive({
    if (is.null(reactloaddata()))
      return(NULL)
    req(input$hotelAnalysis)
    # review()$dataset2
    # g1 <-subset(dataset2,dataset2$Title==input$hotelAnalysis)
    # h1 <-as.vector(g1$Review)
    # # stopwords = readLines("C:/Users/galaxy/Documents/R/win-library/3.4/Project/stopwords.txt")
    # # h1 <- removeWords(h1,stopwords)
    # # g1$Review <-h1
    # #newdataS <-g1$Review
    # #newdataS<-dplyr::pull(h1, Review)
    # cal <- get_nrc_sentiment(h1)
    g<-mtreact4()$g1
    cal2<-mtreact4()$cal
    g2 <- cbind(g,cal2)

    dataset2copy <- data.frame(g2)
    dataset2copy$month <- lubridate::month(dataset2copy$Time,label=TRUE)
    dataset2copy$year  <- year(ymd(dataset2copy$Time))
    dataset2copy <-dataset2copy
  })

  #no ofreviews per month
  mtreactNumReviews <-reactive({
    selectReview <- (subset(reactdatacopy(),reactdatacopy()$Title == input$hotelAnalysis))
    reviewTable <-data.frame(table(selectReview$year,selectReview$month))
    colnames(reviewTable) <-c("Year","Month","NumberofReviews")
    reviewTable <- reviewTable[order(reviewTable$Year),]
    c3 <- spread(reviewTable,Month,NumberofReviews)
    list(reviewTable=reviewTable,c3=c3)

  })
  output$numReviews<-renderTable({
    review()$h
    if (review()$h != ""){
      mtreactNumReviews()$c3
    }
    else{
      print("No review")
    }
    })

  #num of review plot
  output$numreviewplot<- renderPlot(

    ggplot(data=mtreactNumReviews()$reviewTable, aes(x=Month, y=NumberofReviews, fill=Year)) +
      geom_bar(stat="identity", position=position_dodge(), colour="black")
  )


  #calculate and display reviews based on selected emotion
  mtreact5 <-reactive({
    req(input$hotelAnalysis)
    
    g<-mtreact4()$g1
    h1<-as.vector(g$Review)
    # stopwords = readLines("C:/Users/galaxy/Documents/R/win-library/3.4/Project/stopwords.txt")
    # h1 <- removeWords(h1,stopwords)
    g$Review<-h1
    cal2 <- get_nrc_sentiment(g$Review)
    #cal2<-mtreact4()$cal
    g2 <- cbind(g,cal2)
    
    colm <-as.numeric(input$reviewEmo)
      col<- (g2[,colm])
      colReview <-data.frame(g2[col>0, "Review"])
      names(colReview)[1] <- "Reviews that contain selected Emotion"
      colReview <-colReview

   })
  output$reviewSelect <-renderTable({
    mtreact5()
  })

  #populate years in combo box based on selected hotel
  observeEvent(
    input$hotelAnalysis,
    updateSelectInput(session,"year","Sort By Year",choices=reactdatacopy()$year[reactdatacopy()$Title==input$hotelAnalysis])
  )


  #calculate emotion over years
  mtreactOveryears<-reactive({

    reactdatacopy()

    emotime <- reactdatacopy() %>%dplyr::group_by(year) %>%
      dplyr::summarise(anger =mean(anger),
                anticipation = mean(anticipation),
                disgust = mean(disgust),
                fear = mean(fear),
                joy = mean(joy),
                sadness = mean(sadness),
                surprise = mean(surprise),
                trust = mean(trust)) %>% gather(Emotion,meanvalue,2:9)
    emotime$meanvalue<-round(emotime$meanvalue,digit=2)
    emotime <-emotime

    # ggplot(data = emotime, aes(x = year, y = meanvalue, group = Emotion)) +
    #   geom_line(size = 2.5, alpha = 0.7, aes(color = Emotion)) +
    #   geom_point(size = 1.75) +
    #   ylim(0, NA) + scale_x_continuous(name = "",breaks_width(1,offset = 0))+
    #   theme(legend.title=element_blank()) +
    #   ylab("Average Emotion score")

  })


    output$emotionOveryear <-renderPlotly({
      req(input$hotelAnalysis)
      mtreactOveryears()
      p<-plot_ly(mtreactOveryears(),x=~year,y=~meanvalue,color=~Emotion, type="scatter",mode ="line",hoverinfo="text",text=paste("Year:",mtreactOveryears()$year,"<br>","Score:",mtreactOveryears()$meanvalue)) %>%
        layout(yaxis=list(title="Average Emotion score"),xaxis=list(type="category"))
    })

    


  #calculate emotion overtime per month
  mtreact6 <-reactive({

    reactdatacopy()

    selectYear <- (subset(reactdatacopy(),reactdatacopy()$year == input$year))

    if("All Emotion" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(anger =mean(anger),
                  anticipation = mean(anticipation),
                  disgust = mean(disgust),
                  fear = mean(fear),
                  joy = mean(joy),
                  sadness = mean(sadness),
                  surprise = mean(surprise),
                  trust = mean(trust))
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:9)

    }
    else if("anger" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          anger=mean(anger)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)
    }
    else if("anticipation" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          anticipation=mean(anticipation)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("disgust" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          disgust=mean(disgust)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("fear" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          fear=mean(fear)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("joy" %in% input$selectemotion){
        monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
          dplyr::summarise(
            joy=mean(joy)
          )
        monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("sadness" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          sadness=mean(sadness)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("surprise" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          surprise=mean(surprise)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }
    else if("trust" %in% input$selectemotion){
      monthsentimnt <- selectYear %>% dplyr::group_by(month) %>%
        dplyr::summarise(
          surprise=mean(surprise)
        )
      monthsentimnt2 <-monthsentimnt %>% gather(Emotion,meanvalue,2:2)

    }

    monthsentimnt2$meanvalue<-round(monthsentimnt2$meanvalue,digit=2)
    
    list(monthsentimnt=monthsentimnt,monthsentimnt2=monthsentimnt2)


  })

  output$test<-renderTable({
    req(input$hotelAnalysis)
    mtreact6()$monthsentimnt
  })
  # output$plotovertime <-renderPlot({
  #   req(input$hotelAnalysis)
  #   ggplot(data = mtreact6()$monthsentimnt2, aes(x = month, y = meanvalue, group = Emotion)) +
  #     geom_line(size = 2.5, alpha = 0.7, aes(color = Emotion)) +
  #     geom_point(size = 1.75) +
  #     ylim(0, NA) +
  #     theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  #     ylab("Average Emotion score") +
  #     ggtitle("Emotion for the Year",input$year)
  # })
  
  output$titleselect2 <-renderText({
    if (is.null(reactloaddata()))
      return(NULL)
    
    paste("Emotion for the Year: ",input$year)})
  
  output$plotovertime <-renderPlotly({
    req(input$hotelAnalysis)
    mtreactOveryears()
    pi<-mtreact6()$monthsentimnt2
    p<-plot_ly(pi,x=~month,y=~meanvalue,color=~Emotion, type="scatter",mode ="line",hoverinfo="text",text=paste("Month:",pi$month,"<br>","Score:",pi$meanvalue)) %>%
      layout(yaxis=list(title="Average Emotion score"),xaxis=list(type="category"))
  })


  #sentiment over the year
  mtreactsentiment <- reactive({
    req(input$hotelAnalysis)
    reactdatacopy()


    sentimentoveryear <- reactdatacopy() %>%  dplyr::group_by(Time = cut(Time, breaks="2 months")) %>%
      dplyr::summarise(negative = mean(negative),
                positive = mean(positive))%>% gather(sentiment,meanvalue,2:3)

    ggplot(data = sentimentoveryear, aes(x = as.Date(Time), y = meanvalue, group = sentiment)) +
      geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
      geom_point(size = 2) +
      ylim(0, NA) +
      scale_colour_manual(values = c("firebrick3","springgreen4")) +
      theme(legend.title=element_blank(), axis.title.x = element_blank()) +
      scale_x_date(breaks = date_breaks("4 months"),
                   labels = date_format("%Y-%b")) +
      ylab("Average sentiment score") +
      ggtitle("The graph represents how the sentiment (positive and negative) have varied over the years for ",input$hotelAnalysis)
  })

  output$sentimentplot<- renderPlot({
    mtreactsentiment()
  })

  
})






























