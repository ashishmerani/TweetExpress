#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
require(twitteR)
require(streamR)
require(stringr)
require(RCurl)
require(tm)
require(wordcloud)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)
require(reshape2)
library(plotrix)
require(rCharts)
require(devtools)
install_github('ramnathv/rCharts')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  cleanText <- function(some_txt)
  {
    #some_txt <- sapply(some_txt, function(x) x$getText())
    some_txt <- sapply(some_txt,function(x) iconv(x, "latin1", "ASCII", sub=""))
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    some_txt = gsub("@\\w+", "", some_txt)
    some_txt = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", some_txt)
    some_txt = gsub("[[:punct:]]", "", some_txt)
    some_txt = gsub("[[:digit:]]", "", some_txt)
    some_txt = gsub("http\\w+", "", some_txt)
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
    
    
    # define "tolower error handling" function
    try.tolower = function(x)
    {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = tolower(x)
      return(y)
    }
    
    some_txt = sapply(some_txt, try.tolower)
    
    some_txt = some_txt[some_txt != ""]
    some_txt = gsub("uber", "", some_txt)
    names(some_txt) = NULL
    return(some_txt)
  }
  
  GetTweets <- function(){
    teSearchOutput <- searchTwitteR(input$teSearch, n=input$tweetcount, lang="en")
    return (teSearchOutput)
  }
  
  AuthTwitteR <- function(){
    consumer_key <- 'EiKOaGAAQOyrrvQaQqJ8Bq7I4'
    consumer_secret <- 'MrM2GKWCLy8S3WyEzCcqLQlNuvXawrdeXJomaAH92TSPQNvHFM'
    access_token <- '4675883312-GSsLw4z2cNA6X4GMTO1FoJWEu6x91dA6LDkIEjY'
    access_secret <- 'jHiT0CxFhWYHM3GP5KhDZpOZFEWjBQi6tpiAlb12I7kfN'
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  }

  observeEvent(input$startProcess1, {
    
    
    withProgress(message = "Computing results", detail = "fetching data........", value = 0, {
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    dframe <- read.table(file='combinedWords.csv',header=FALSE,
                         sep=',', col.names=c('Key','Value'))
    dtable <- data.table(dframe, key='Key')
    
    tweets_result = GetTweets()
   
    for (tweet in tweets_result){
      text = cleanText(tweet$text)
      
      
      word_list = strsplit(as.character(text), " ")[[1]]
      word_list = unique(tolower(word_list))
      
      tot = 0
      flag = 0
      for (word in word_list){
        if(paste(dtable[word])[[2]] == 1 || paste(dtable[word])[[2]] == -1){
          #print(paste(dtable[word][[2]], ":", word))
          #print(dtable[word][[2]])
          tot = tot + dtable[word][[2]]
          flag = 1
        }
      }
      if(flag == 1){
        if(tot == 0){
          neutral_text <- c(neutral_text, text)
          count_neutral = count_neutral + 1
        }
        else if(tot < 1){
          negative_text <- c(negative_text, text)
          count_negative = count_negative + 1
        }
        else {
          positive_text <- c(positive_text, text) # Add the positive text
          count_positive = count_positive + 1
        }
      }
      
    }
    incProgress(detail = paste("Computing tweets..."))
    })
    
    positive_text <- Corpus(VectorSource(positive_text))
    positive_text <- tm_map( positive_text, removeWords, stopwords("english"))
    positive_text <- tm_map(positive_text, removeWords, c(input$teSearch))
    negative_text <- Corpus(VectorSource(negative_text))
    negative_text <- tm_map(negative_text, removeWords, stopwords("english"))
    negative_text <- tm_map(negative_text, removeWords, c(input$teSearch))
    neutral_text <- Corpus(VectorSource(neutral_text))
    neutral_text <- tm_map(neutral_text, removeWords, stopwords("english"))
    neutral_text <- tm_map(neutral_text, removeWords, c(input$teSearch))
  
   
    output$positive_wordcloud <- renderPlot( {wordcloud(positive_text, scale=c(3,1), max.words=50 ,colors=brewer.pal(8, "Dark2"))  })
    output$negative_wordcloud  <- renderPlot({wordcloud(negative_text, scale=c(3,1),max.words=50 ,colors=brewer.pal(8, "Dark2"))   })
    output$neutral_wordcloud <- renderPlot({wordcloud(neutral_text, scale=c(3,1), max.words=50 ,colors=brewer.pal(8, "Dark2"))   })

    
    x <-  c(count_positive, count_negative, count_neutral)
    lbl <-  c("Positive Reviews","Negative Reviews","Neutral Reviews")
   output$piechart <- renderPlot(pie3D(x,labels = lbl,explode = 0.3, theta=pi/6, labelcol=par("fg"), main = "Reviews on Tweets "))
 
   
   output$positive <- renderText({count_positive })
   output$negative <- renderText({count_negative})
   output$neutral <- renderText({ count_neutral })
    
  })
  
  observeEvent(input$userProcess, {
    
    withProgress(message = "Computing results", detail = "fetching data.....", value = 0, {
      user <- getUser(input$userSearch)
    userFriends <- user$getFriends()
    userFollowers <- user$getFollowers()
    userNeighbors <- union(userFollowers, userFriends)
    userNeighbors.df = twListToDF(userNeighbors)
    
    userNeighbors.df$logStatusesCount = log10(userNeighbors.df$statusesCount+1)
    
    
    userNeighbors.df$logFollowersCount <-log(userNeighbors.df$followersCount)
    
    userNeighbors.df$logFriendsCount <-log(userNeighbors.df$friendsCount)
    kObject.log <- data.frame(userNeighbors.df$logFriendsCount,userNeighbors.df$logFollowersCount)

    
    kObject.log$userNeighbors.df.logFriendsCount[is.infinite(kObject.log$userNeighbors.df.logFriendsCount)]  <- 0
    kObject.log$userNeighbors.df.logFollowersCount[is.infinite(kObject.log$userNeighbors.df.logFollowersCount)] <- 0
    
    mydata <- kObject.log
    
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    
    for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
    
    output$cluster <- renderPlot(plot(1:15, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares"))
    
    
    

    ##Run the K Means algorithm, specifying 2 centers
    user2Means.log <- kmeans(kObject.log, centers=2, iter.max=10, nstart=100)
    
    #Add the vector of specified clusters back to the original vector as a factor
    
    userNeighbors.df$cluster <-factor(user2Means.log$cluster)
    
    output$idk <- renderPlot(plot(logFollowersCount ~ logFriendsCount, group = 'cluster', data = userNeighbors.df, type = 'scatterChart'))
    incProgress(detail = paste("Computing tweets..."))
    })
    
 
  })
  
  AuthTwitteR()
  
  displaytrend <- function(){
    trends <- getTrends(23424977 )

      tt = head(world$name, 10)
      
      for (h in tt){
        print(h)
      }
      
      output$choose_dataset <- renderUI({
        selectInput("dataset", "Data set", as.list(tt))
      })
  }
  
  displaytrend()
  
  observeEvent(input$trendProcess, {
    withProgress(message = "Computing results", detail = "fetching data.....", value = 0, {
    
    
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    dframe <- read.table(file='combinedWords.csv',header=FALSE,
                         sep=',', col.names=c('Key','Value'))
    dtable <- data.table(dframe, key='Key')
    
    tweets_result <- searchTwitteR(input$dataset, n=200, lang="en")
    
    for (tweet in tweets_result){
      text = cleanText(tweet$text)
      
      
      word_list = strsplit(as.character(text), " ")[[1]]
      word_list = unique(tolower(word_list))
      
      tot = 0
      flag = 0
      for (word in word_list){
        if(paste(dtable[word])[[2]] == 1 || paste(dtable[word])[[2]] == -1){
          #print(paste(dtable[word][[2]], ":", word))
          #print(dtable[word][[2]])
          tot = tot + dtable[word][[2]]
          flag = 1
        }
      }
      if(flag == 1){
        if(tot == 0){
          neutral_text <- c(neutral_text, text)
          count_neutral = count_neutral + 1
        }
        else if(tot < 1){
          negative_text <- c(negative_text, text)
          count_negative = count_negative + 1
        }
        else {
          positive_text <- c(positive_text, text) # Add the positive text
          count_positive = count_positive + 1
        }
      }
      
    }
    

    incProgress(detail = paste("Computing tweets..."))
    })
    
    positive_text <- Corpus(VectorSource(positive_text))
    positive_text <- tm_map( positive_text, removeWords, stopwords("english"))
    positive_text <- tm_map(positive_text, removeWords, c(input$teSearch))
    negative_text <- Corpus(VectorSource(negative_text))
    negative_text <- tm_map(negative_text, removeWords, stopwords("english"))
    negative_text <- tm_map(negative_text, removeWords, c(input$teSearch))
    neutral_text <- Corpus(VectorSource(neutral_text))
    neutral_text <- tm_map(neutral_text, removeWords, stopwords("english"))
    neutral_text <- tm_map(neutral_text, removeWords, c(input$teSearch))
    
    
    output$trendpositive <- renderPlot( {wordcloud(positive_text, scale=c(3,1), max.words=50 ,colors=brewer.pal(8, "Dark2"))  })
    output$trendnegative  <- renderPlot({wordcloud(negative_text, scale=c(3,1),max.words=50 ,colors=brewer.pal(8, "Dark2"))   })
    output$trendneutral <- renderPlot({wordcloud(neutral_text, scale=c(3,1), max.words=50 ,colors=brewer.pal(8, "Dark2"))   })
    
    
    x <-  c(count_positive, count_negative, count_neutral)
    lbl <-  c("Positive Reviews","Negative Reviews","Neutral Reviews")
    output$trendpie <- renderPlot(pie3D(x,labels = lbl,explode = 0.3, theta=pi/6, labelcol=par("fg"), main = "Reviews on Tweets ")) 
    
    output$tpositive <- renderText({count_positive })
    output$tnegative <- renderText({count_negative})
    output$tneutral <- renderText({ count_neutral })
    
    
    
    
  })
  
  
  
})
  