

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(syuzhet)
library(tm)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(SnowballC)
library(qdap)
library(plotly)
library(tools)


shinyUI(fluidPage(

  dashboardPage(
    
    dashboardHeader(title="Emotion Analysis"),
    dashboardSidebar(
      sidebarMenu(
       # menuItem("Home",tabName="home",icon = icon("building")),
       menuItem("Dataset",tabName="dataset",icon = icon("address-card")),
       menuItem("Analysis",icon = icon("atlas")),
        menuSubItem("Emotion Analysis",tabName="Emanalysis"),
        menuSubItem("Sentiment Analysis",tabName="Stmanalysis")
       )
      ),
    dashboardBody(
      #shinyjs::useShinyjs(),
      
      tags$head(
        tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');,
                        .box.box-solid.box-primary{

                        background:#222d32
                        }
                        "))
        ), 
      tabItems(
    
        tabItem(tabName="dataset",
                fluidRow
                  (valueBoxOutput(width=2,"databasetitle"),valueBoxOutput(width=3,"databaserows")),
                fluidRow(
                  tabBox(width = 10,
                    tabPanel(title=h4("Dataset"),tableOutput("datasetAll")),
                    tabPanel(title=h4("Filter Dataset"),
                      selectInput("hotel","Select a Hotel",choices="",selected =" "),
                      tableOutput("datasethotel"),downloadButton("downloadData", "Download Data")
                      ))
                    )
                ),
        tabItem(tabName="Emanalysis", h2("Emotion Analysis"),
                fluidRow(
                  column(width=6,
                    box(status="primary",selectInput("hotelAnalysis","Select a Hotel",choices="",selected =" " )),
                    box(status="primary",tableOutput("emotionTable"))),
                    box(width=6,title=(textOutput("titleselect")),status="primary",solidHeader=T,plotOutput("emotionPlot"),downloadButton("downloadPlot", "Download Plot"))
                ),
                fluidRow(h3(" Review Details"),status="primary",
                         box(width=12,status="primary",
                             fluidRow(
                               valueBoxOutput(width=2,"rowsum"),
                               box(title="Number of Review per Month/Year ",tableOutput("numReviews"),
                               
                                 actionButton("btnreviewplot","View Graphical Display",icon = icon("bar-chart"))
                               ,
                               bsModal(id="numreviewdisplay",title = "Number of Reviews per Month/Year", trigger = "btnreviewplot",size="large",plotOutput("numreviewplot"))
                              )
                             ),
                             fluidRow(
                               box(width=3,selectInput("reviewEmo","Sort By Emotion",choices=c("anger"=4,"disgust"=6,"fear"=7,"sadness"=9,"joy"=8,"surprise"=10,"trust"=11))),
                               box(tableOutput("reviewSelect"))
                             )
                         )
                ),
                fluidRow(h3(" Emotion Over Time"),status="primary",
                         box(width=12,status="primary",
                         box(title="Emotion over the Years",width=12,plotlyOutput("emotionOveryear",width = "100%")),
                         box(width=3,selectInput("year","Sort By Year",choices="",selected ="")),
                         box(width=2,title="Sort by Emotion",radioButtons("selectemotion","Select Emotion",choices = c("All Emotion", "anger","anticipation","disgust","fear","joy","sadness","surprise","trust"))),
                         box(title="Emotion score per Month",tableOutput("test")),
                         box(width=12,title=(textOutput("titleselect2")),plotlyOutput("plotovertime",width = "100%",height = "400px"))
                         )
                )      
               ),
        tabItem(tabName = "Stmanalysis",h3("Sentiment Analysis over the Years"),
                box(width=12,plotOutput("sentimentplot",width = "100%",height = "400px"))
                )
      )
        )
    )
  )
)

