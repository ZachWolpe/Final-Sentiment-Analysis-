

library(shiny)

# source('Sentiment/functions.R')



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinythemes::shinytheme('flatly'),

    # Application title
    titlePanel("Equity Sentiment Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width=3,
            dateRangeInput('dateRange', 'Stock date range:', start = Sys.Date()-365, end = Sys.Date()),
            selectizeInput('available_stocks', 'Select Stocks', jse_tickers['stock_name'], multiple=T),
            tags$div(),
            tags$br(),
            sliderInput("no_tweets", "Number of Tweets (per user):", min = 10, max = 2500, value = 1000),
            numericInput("n", "Twitter Users", value=1, min=1, step=1, max=10),
            uiOutput('users_names'),
            textOutput('twitter_users'),
            tags$div(),
            tags$br(),
            actionButton('Submit', 'Submit'),
        ),


        # Show a plot of the generated distribution
        mainPanel(
            tableOutput('user_stats'),
            plotOutput('diversified_sentiment'),
            plotOutput('sent_time_series'),
            plotOutput('stockPlot'),
            plotOutput('volumePlot')
        )
    )
))



