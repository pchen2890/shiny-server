
library(shiny)
library(ggplot2)

data  <-read.csv("hulu_clean.csv")

min_clip <- min(data$show_clips_count)
max_clip <- max(data$show_clips_count)
min_ep <- min(data$show_episodes_count)
max_ep <- max(data$show_episodes_count)
min_season <- min(data$show_seasons_count)
max_season <- max(data$show_seasons_count)
min_video <- min(data$show_videos_count)
max_video <- max(data$show_videos_count)

fluidPage( headerPanel("Data"),
    sidebarLayout(
        sidebarPanel(sliderInput(inputId = "range_clip",label ="Show Clip Count",
                        min=min_clip,max=max_clip,value=c(min_clip,max_clip),step = 1),
                    sliderInput(inputId = "range_ep",label ="Show Episode Count",
                        min=min_ep,max=max_ep,value=c(min_ep,max_ep),step = 1),
                    sliderInput(inputId = "range_season",label ="Show Season Count",
                        min=min_season,max=max_season,value=c(min_season,max_season),step = 1),
                    sliderInput(inputId = "range_video",label ="Show Video Count",
                        min=min_video,max=max_video,value=c(min_video,max_video),step = 1)
                     ),
        mainPanel(
            tabsetPanel(
            tabPanel("By Genre",navlistPanel(
                tabPanel("Avg # of reviews",plotOutput(outputId='hist_graph1')),
                tabPanel("N of shows",plotOutput(outputId = 'n_graph2')),
                tabPanel("Data_table",DT::dataTableOutput("table"))
            )
                    ),
            tabPanel("By Media Company",navlistPanel(
                tabPanel("Avg # of reviews",plotOutput(outputId='hist_graph')),
                tabPanel("N of shows",plotOutput(outputId = 'n_graph')),
                tabPanel("Data_table",DT::dataTableOutput("table"))
                )
                    )
            )
        )
    )
)
