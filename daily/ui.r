
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(DT)

data <-read.csv("dailys.csv")
data[is.na(data)] <- "None"

plan <- unique(as.character(data$plan))
action <- unique(as.character(data$action))
recurrence <- unique(as.character(data$recurrence))
segment <- unique(as.character(data$segment))

ui<-navbarPage(
    title = "Tabs:",
    tabPanel('Main Tab',fluidPage(titlePanel("Main Accounts"),
                                  fluidRow(downloadButton('downloadData', 'Download')),
                                  
                                  fluidRow(
                                      column(4,selectInput("plan1","Plans:",c("All",plan))),
                                      column(4,selectInput("segment1","Segment:",c("All",segment))),
                                      column(4,selectInput("action1","Actions:",c("All",action)))
                                  ),
                                  # Create a new row for the table.
                                  fluidRow(
                                      DT::dataTableOutput("table")
                                  ),
                                #   create graph
                                  fluidRow(
                                      plotOutput(outputId = "graph")
                                  )
                               )
            ),
    tabPanel('Full Data Tab',DT::dataTableOutput('tab1')),
    tabPanel('Weekly MRR/N',selectInput('variables',"MRR or N: ",c('MRR','N')),
             fluidPage(navlistPanel(
                 tabPanel('Plan',fluidRow(DT::dataTableOutput('plan'))),
                 tabPanel('Recurrence',fluidRow(column(width = 5,DT::dataTableOutput('recurrence')))),
                 tabPanel('Segment',fluidRow((DT::dataTableOutput('segment'))))
             )))

    )
