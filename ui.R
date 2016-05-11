
library(shiny)
library(shinythemes)


environment(textInput) <- environment(sliderInput)

# Define UI for application
shinyUI(
  navbarPage(theme = shinytheme("cerulean"),
    ("TweetExpress"),
                   tabPanel("Analyze Tweets & Topics",
                            sidebarLayout(
                              sidebarPanel(
                                textInput("teSearch", label = "Enter the Search Term", 
                                          value = ""),
                                wellPanel(
                                  sliderInput("tweetcount", "Maximum Tweets to be Processed", min = 10, max = 300, value = 30, step = 1, ticks = FALSE, animate = TRUE  )
                                ),
                                actionButton("startProcess1", class="btn btn-primary", icon = icon("twitter"), "Start Processing")
                              ),
                              mainPanel(
                                  h5("Positive Count: "),textOutput("positive"),
                                  h5("Negative Count: "),textOutput("negative"),
                                  h5("Neutral Count: "),textOutput("neutral"),
                                  h1("Pie Chart Analysis"),
                                
                                  plotOutput("piechart"),
                                  
                                  h1("Positive Sentiment Analysis"),
          
                                  plotOutput("positive_wordcloud"), #Cloud for positive words
                                  h1("Negative Sentiment Analysis"),
                        
                                  plotOutput("negative_wordcloud"), #Cloud for negative words
                          
                                  h1("Neutral Sentiment Analysis"),
                                  plotOutput("neutral_wordcloud") #Cloud for neutral words
                                
                              )
                            )
                   ),
                   tabPanel("Cluster Based on User",
                            sidebarLayout(
                              sidebarPanel(
                                textInput("userSearch", label = "Enter the twitter Username", 
                                          value = ""),
                                
                                actionButton("userProcess", class="btn btn-info", icon = icon("twitter"), "Start Processing")
                                
                              ),
                              
                            
                              mainPanel(
                                plotOutput("cluster"),
                                plotOutput("idk")
                              )
                            )
                   ),
                   tabPanel("Trends",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("choose_dataset"),
                                                     
                                actionButton("trendProcess", class="btn btn-success", icon = icon("twitter"), "Start Processing")
                                
                              ),
                              
                              mainPanel(
                                h5("Positive Count: "),textOutput("tpositive"),
                                h5("Negative Count: "),textOutput("tnegative"),
                                h5("Neutral Count: "),textOutput("tneutral"),
                                h1("Pie Chart Analysis"),
                                plotOutput("trendpie"),
                                h1("Positive Sentiment Analysis"),
                                plotOutput("trendpositive"),
                                h1("Negative Sentiment Analysis"),
                                plotOutput("trendnegative"),
                                h1("Neutral Sentiment Analysis"),
                                plotOutput("trendneutral")
                              )
                            )
                   )
))