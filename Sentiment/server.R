

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
    
    # ________________________________ Twitter Users Dynamic Username Inputs ________________________________
    col_names <- reactive(paste0("users", seq_len(input$n)))
    output$users_names <- renderUI({
        map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
        })

   # view twitter usernames 
    output$twitter_users <- renderText(twitter_handles())
    
    # ________________________________ Twitter Users Dynamic Username Inputs ________________________________
    
    
    
    
    # __________________________________ Compute: Stock Data  __________________________________
    stock_results <- eventReactive(
      # await 'submit'
      input$Submit,{
        
        # get Yahoo tickers
        tickers <- jse_tickers$jse_code_jo[jse_tickers$stock_name %in% input$available_stocks]
        
        # get stock data (saved as same names)
        res <- call_stock_data(tickers, start=input$dateRange[1], end=input$dateRange[2])
        res
        
      })
    # __________________________________ Compute: Stock Data  __________________________________
    

    
    
    
    # __________________________________ Compute: Twitter User Analysis  __________________________________
    # list of Twitter handles
    twitter_handles <- reactive(map_chr(col_names(), ~ input[[.x]]))
    
    # download tweets 
    tweets <- eventReactive(input$Submit, get_timeline(twitter_handles(), n=input$no_tweets))
    
    # compute summary statistics
    summary_stats <- reactive(compute_summary_stats(tweets()))
    
    # clean word corpus
    clean_tweets <- eventReactive(input$Submit, clean_tweets_text(tweets()))
      
    # generate sentiment circular plot
    sentiment_plot <- eventReactive(input$Submit, diversified_sentiment_analysis(clean_tweets(), nrc_lexicon, T))
    
    # generate sentiment time series plot
    #sent_time_series <- eventReactive(input$Submit, sentiment_time_series(tweets=tweets(), equity_dataset=stock_results()$data, 
     #                                                                     start=input$dateRange[1], end=input$dateRange[2]))
    
    sent_time_series <- eventReactive(input$Submit,{ 
                                      
      
                                      sentiment_time_series(tweets(), stock_results()$data)
                                      })

 
    

    
    
    
    
    # compute missing twitter users
    # missing_user_message <- eventReactive(input$Submit, {
    #   # users searched but not loaded
    #   print('-----------------------------')
    #   print(twitter_handles()[, !twitter_handles() %in% summary_stats()$screen_name])
    #   missing_users <- twitter_handles()[, !twitter_handles() %in% summary_stats()$screen_name]
    #   print("___________________________________ missing user_______________________")
    #   print(missing_users)
    #   # error message 
    #   error_message <- NULL
    #   if (!is_empty(missing_users)) {
    #     error_message <- paste0('We can\'t seem to find ', "Error_name", "? Are you sure thats the correct twitter handle?")
    #   }
    #   print(error_message)
    #   error_message
    # })
    #   

    # get profile pictures // use later
    profile_image_data <- reactive(store_image_data(summary_stats(), prod_image))
    # __________________________________ Compute: Twitter User Analysis  __________________________________
    
    

    
    
  
    # __________________________________ Render: Twitter Summary & Graph __________________________________
    output$user_stats <- renderTable(summary_stats()[, !names(summary_stats()) %in% c('profile_image_url')])
    
    #  output$user_not_found <- renderText(missing_user_message())
    
    
    # plot diversified sentiment circle 
    output$diversified_sentiment <- renderPlot(sentiment_plot()$plt_circular)
    
    # plot sentiment vs equities
    output$sent_time_series <- renderPlot({
      print('----here----')
      print(sent_time_series()$sentiment_data)
      sentiment_time_series()$plt})
  
    # __________________________________ Render: Twitter Summary & Graph __________________________________
    
    
    
    
    
    
    
    
    
    
    
    
    
    # __________________________________ Stock Data Dynamic Username Inputs __________________________________
    
    # _________ Plot Stock Data _________
    output$stockPlot <- renderPlot(stock_results()$plt_stocks)
    
    # _________ Plot Volume Data _________
    output$volumePlot <- renderPlot(stock_results()$plt_volume)
    

    # __________________________________ Stock Data Dynamic Username Inputs __________________________________
    
      
    
    
    
    


})
