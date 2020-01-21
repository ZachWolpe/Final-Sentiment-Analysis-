

# ________________________________________________________ Configure Workspace ________________________________________________________
configure_workspace <- function(download_lexicons=F) {
  
  # Configure Dependencies
  load_packages <- function() {
    library(tidytext)
    library(textdata)
    library(rtweet)
    library(dplyr)
    library(lubridate)
    library(ggplot2)
    library(stringr)
    library(tidyr)
    library(wordcloud)
    library(reshape2)
    library(tidyquant)
    library(purrr)
    library(imputeTS)
    library(readxl)
    library(jpeg) 
    library(readxl)
    library(randomcoloR)
    library(parallel)
  }
  
  load_packages()
  
  setwd("~/Desktop/Quanta AI/Final/Sentiment Analysis App")
  
  # JSE Equities Data 
  jse_tickers <<- read_excel('./data/jse_tickers.xls')
  jse_tickers$jse_code_jo <<- paste(jse_tickers$jse_code, ".JO", sep="")
  jse_tickers$jse_code_jo <<- sub("^\\s+", "", jse_tickers$jse_code_jo)
  
  
  # Sentiment Lexicons
  if (download_lexicons) {
    # download
    afinn_lexicon <- get_sentiments("afinn")
    nrc_lexicon <- get_sentiments("nrc")
    bing_lexicon <- get_sentiments("bing")
    
    # save lexicon
    write.csv(nrc_lexicon, 'data/lexicons/nrc_lexicon.csv')
    write.csv(afinn_lexicon, 'data/lexicons/afinn_lexicon.csv')
    write.csv(bing_lexicon, 'data/lexicons/bing_lexicon.csv')
  }
  
  # load lexicon
  nrc_lexicon <<- read.csv('data/lexicons/nrc_lexicon.csv')
  afinn_lexicon <<- read.csv('data/lexicons/afinn_lexicon.csv')
  bing_lexicon <<- read.csv('data/lexicons/bing_lexicon.csv')
  
}
# ________________________________________________________ Configure Workspace ________________________________________________________





# ____________________________________________________ Compute Influence Statistics ____________________________________________________
compute_summary_stats <- function(tweets) {

    user_summaries <- tweets %>% group_by(screen_name) %>% 
      summarise(n_tweets=n(), 
                followers=unique(followers_count),
                following=unique(friends_count),
                average_likes_per_tweet=mean(favorite_count),
                average_retweets=mean(retweet_count),
                profile_image_url = first(profile_image_url))
  
  return(user_summaries)
}
# ____________________________________________________ Compute Influence Statistics ____________________________________________________





# ______________________________________________________________ Get Stock Data ______________________________________________________________
# https://www.sharenet.co.za/free/jsenames.phtml?letter=all
call_stock_data <- function(tickers, start, end=Sys.Date()) {
  "Download & visualize stock data associated with specific tickers
   Return:
      - stock data
      - stock price visualization
      - stock volume visualization"
  
  
  # get data
  res <- tq_get(tickers, from=start, to=end, get="stock.prices")
  
  # add 'symbol' column if only 1 stock
  if (!'symbol' %in% names(res)) {res <- cbind(symbol=tickers, res)}
  
  # impute missing values with moving average
  res[,3:ncol(res)] <- na_ma(res[,3:ncol(res)], k=1, weighting='simple')
  
  
  # stock value
  plt_stocks <- res %>% group_by(symbol) %>%
    mutate(standardized_price = (adjusted-mean(adjusted, na.rm=T)) / sd(adjusted, na.rm=T)) %>% 
    ggplot(aes(x=date, y=standardized_price, col=factor(symbol))) + geom_line(alpha=0.4) + 
    labs(x='Date', y="", title="Stock Price Chart") + 
    theme(axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
    scale_colour_discrete("Tickers")
    
  
  # Trading Volume
  plt_volume <- res %>% group_by(symbol) %>%
    mutate(standardized_volume = (volume-mean(volume, na.rm=T)) / sd(volume, na.rm=T)) %>% 
    ggplot(aes(x=date, y=volume)) + 
    geom_segment(aes(xend=date, yend=0, color=volume)) + 
    theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.position='none')
  
  # save results
  results <- list()
  results[['data']] = res
  results[['plt_stocks']] = plt_stocks
  results[['plt_volume']] = plt_volume
  
  return(results)
}
# ______________________________________________________________ Get Stock Data ______________________________________________________________





# ____________________________________________________________ Clean Twitter Data ____________________________________________________________
clean_tweets_text <- function(twitter_dataset, plot=FALSE) {
  "Return: clean word tokens ranked by frequency of appearance"

  twitter_dataset <- twitter_dataset %>% unnest_tokens(word, text) %>% select(word)           # clean & tokenize words
  twitter_dataset <- twitter_dataset %>% anti_join(stop_words, by=c("word", "word"))          # remove stop words
  twitter_dataset <- twitter_dataset %>% count(word, sort=TRUE)                               # count word appearences
  twitter_dataset <- twitter_dataset[twitter_dataset$word != 'https',]                        # customly remove some unwanted terms        
  twitter_dataset <- twitter_dataset[is.na(as.numeric(twitter_dataset$word)),]                # remove numbers
  twitter_dataset <- twitter_dataset %>% mutate(word=reorder(word,n))                         # order (already done?)
  
  if (plot) {                                                                                 # Most Used Words
    plt <- twitter_dataset[1:15,] %>%
      ggplot(aes(word,n,fill=word)) +
      ggtitle("Most Common Words") + 
      geom_col() +
      xlab(NULL) + 
      coord_flip()
    print(plt)
  }
  return(twitter_dataset)
}
# ____________________________________________________________ Clean Twitter Data ____________________________________________________________





# ______________________________________________________ Compute Diversified Sentiment ______________________________________________________
diversified_sentiment_analysis <- function(word_list, nrc_lexicon=nrc_lexicon, create_plots=TRUE) {
  "Compute, plot & return the diversified sentiment"
  
  
  # compute diversified sentiment - compute the number of appearances of each sentiment type
  diversed_sentiment <- unique(word_list %>% inner_join(nrc_lexicon) %>% 
                                 group_by(sentiment) %>% mutate(sum_n = sum(n)) %>% select(sum_n))
  # compute proportion
  diversed_sentiment$proportion = diversed_sentiment$sum_n / sum(diversed_sentiment$sum_n)
  
  # compute ratio
  diversed_sentiment$ratio = diversed_sentiment$proportion / max(diversed_sentiment$proportion)
  
  # order nrc by 2 criteria: circular position (CLOCK)
  rank <- data.frame(sentiment=c('positive','joy','surprise',"sadness",'disgust','negative','anger','fear','anticipation','trust'),
                     position=c(0,1,2,3,4,5,6,7,8,9),
                     sent=c(1,1,0,-1,-1,-1,-1,-1,0,1)) 
  rank <- inner_join(diversed_sentiment, rank, by="sentiment")
  
  # order
  rank <- rank[order(rank$position),]
  
  # store results
  results <- list()
  results[['rank']] <- rank

  if (create_plots==T){
    # col plot
    plt_col <- ggplot(rank, aes(x=reorder(sentiment, position), y=proportion, fill=sent)) + geom_col() + ggtitle("Diversified Sentiment")
    
    # coordinate plot
    plt_circular <- ggplot(rank, aes(x=reorder(sentiment, position), y=ratio, fill=factor(sent))) + 
      geom_bar(stat='identity', show.legend=F) +
      coord_polar() + 
      ggtitle("Sentiment of Tweets")+ 
      xlab("") + ylab("") + 
      theme(legend.position = "None",
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour='#DCDCDC'), 
            panel.grid.minor  = element_line(colour='#DCDCDC')) + 
      scale_fill_manual(values=sample(colours(), 3)) 

    
    
    
    
    # save results 
    results[['plt_circular']] <- plt_circular
    results[['plt_col']] <- plt_col
  }
  
  return(results)
}
# ______________________________________________________ Compute Diversified Sentiment ______________________________________________________



  
  
# _________________________________________________________ Compute Bing Sentiment _________________________________________________________
compute_bing_sentiment <- function(word_list, lexicon=bing_lexicon, create_plots=FALSE, word_size=30, printt=F) {
  "
  Compute the Corpus Sentiment for a given Lexicon
  
  Fix: Cannot save Pie chart & word cloud to list 
  "
  
  # get bing sentiment
  word_list <- word_list %>% inner_join(lexicon, by=c('word','word')) %>% select(word, n, sentiment) 
  
  
  # compute for visualization
  word_list$pole = ifelse(word_list$sentiment == 'positive', 1, -1)
  word_list$score = word_list$pole * word_list$n
  
  # store results
  results <- list()
  results[['word_list']] <- word_list
  
  
  # visualize
  if (create_plots) {
    net_sent <- ggplot(word_list, aes(word, score, fill=pole)) + geom_col() + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = 'none')
    
    
    plt_pie_chart <- pie(x=c(mean(word_list$sentiment=="negative"),mean(word_list$sentiment=="positive")), 
                         labels=c("negative",'positive'),
                         col=c("darkblue", "lightblue"))
    
    
    # Visualize Most Common Positive vs Negative Words
    plt_most_common_words <- word_list %>% group_by(sentiment) %>% 
      top_n(n=10, wt=n) %>% mutate(word=reorder(word,n)) %>%
      ggplot(aes(reorder(word,n), n, fill=sentiment)) +
      geom_col(show.legend=FALSE) +
      facet_wrap(~sentiment, scales="free_y") +
      labs(y = "Contribution to Sentiment",
           x = NULL) +
      coord_flip()

    
    # generate wordcloud by sentiment colours  
    group = c(word_list$sentiment)
    basecolours = c('darkgreen','darkred')
    colourlist = basecolours[match(group,unique(group))]
    
    plt_word_cloud <- wordcloud(words=word_list$word, freq=word_list$n, 
                                colors=colourlist, 
                                ordered.colors=T,
                                max.words=word_size)
    
    
    # Structured sentiment word cloud
    plt_word_cloud_2 <- word_list %>% acast(word ~ sentiment, value.var="n", fill=0) %>% 
      comparison.cloud(colors=c("darkred",'darkgreen'), 
                       max.words=word_size)
    
    
    # store plots
    results[['word_list']] <- word_list
    results[['net_sent']] <- net_sent
    results[['plt_pie_chart']] <- plt_pie_chart                         # fix
    results[['plt_most_common_words']] <- plt_most_common_words
    results[['plt_word_cloud']] <- plt_word_cloud                       # fix 
    results[['plt_word_cloud_2']] <- plt_word_cloud_2                   # fix 
    
  }
  
  if (printt) {
    print(paste("Proportion of POSITIVE sentiment words: ", mean(word_list$sentiment=="positive")))
    print(paste("Proportion of NEGATIVE sentiment words: ", mean(word_list$sentiment=="negative")))
    print(paste("Proportion of NEUTRAL sentiment words: ", mean(word_list$sentiment=="neutral")))
  }
  
  return(results)
}
# _________________________________________________________ Compute Bing Sentiment _________________________________________________________





# ______________________________________________________ Compute Sentiment Time Series ______________________________________________________
sentiment_time_series <- function(tweets, equity_dataset, cleaning_function=clean_tweets_text, start=Sys.Date()-365, end=Sys.Date(), 
                                  colour_1=randomColor(1), colour_2=randomColor(1)) {
  "compute the sentiment overtime & contrast this with equity prices
  
  Return:
    - plt: visualization of equities & sentiment overtime (standardized)
    - sentiment_data: dataset contain sentiment averaged over weeks 
    - equity_dataset: original equity dataset returned  
    
  Parameters:
    - clean_twitter_dataset: unclean dataset
    - cleaning_function: function to clean dataset
    - equity_dataset: 
    - start & end: daterange
  "
  
  # _____________________ Sentiment per Week _____________________
  # create weekly grouping 
  weekly <- tweets %>% filter(created_at >= start, created_at<=end) %>% 
    mutate(date = as.Date(created_at),
           week = (as.numeric(date) %/% 7) - (as.numeric(min(date)) %/% 7)) %>% 
    select(text, week, created_at, screen_name)

  
  
  # create a list (1 item per week) and clean text
  clean_words_per_week <- mclapply(split(weekly, as.factor(weekly$week)), cleaning_function)
  
  
  # compute bing sentiment for each item in the list
  bing_sent_per_week <- mclapply(clean_words_per_week, compute_bing_sentiment)    
  
 
  
  
  net_bing_sent <- function(tibble) {mean(tibble$word_list$score)}                                # net sentiment function         
  ave_sent <- mclapply(bing_sent_per_week, net_bing_sent)                                         # apply net sent function
  ave_sent <- tibble(week=(1:length(ave_sent)), sent=unlist(ave_sent))                            # compile results 
  t <- weekly %>% group_by(week) %>% summarise(date = max(created_at))                            # correspond 'weeks' with dates (latest date per week)
  t$week <- t$week + 1                                                                           # align tibbles                
  sent_per_date <- inner_join(ave_sent, t, by=c('week', 'week'))                                  # combine to create dates                        
  sent_per_date$date <- as.Date(sent_per_date$date)                                               # convert date   
  sent_per_date$sent <- na_kalman(sent_per_date$sent)                                           # Impute Missing Data                      

  
  
  # _____________________ Visualize  _____________________
  # standardize datasets
  stnd <- function(data) (data-mean(data))/sd(data)
  
  plt <- ggplot(sent_per_date, aes(x=date, y=stnd(sent))) + 
    ggtitle('Equities & Weekly Sentiment') +
    geom_line(col=colour_1,linetype='dashed') + geom_point(aes(col=colour_1)) +
    geom_line(data=equity_dataset, aes(x=date, y=stnd(adjusted)), linetype='dashed', col=colour_2) + 
    geom_point(data=equity_dataset, aes(x=date, y=stnd(adjusted), col=colour_2)) +
    theme(axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(values=c(colour_2, colour_1), labels=c('equities', 'sentiment'))
  
  
  # store results
  results <- list()
  results[['sentiment_data']] <- sent_per_date
  results[['equity_dataset']] <- equity_dataset
  results[['plt']] <- plt
  
  print('---------------------------------------------------')
  print('sentiment_data')
  print(head(sent_per_date))
  print('---------------------------------------------------')
  print('equity_dataset')
  print(head(equity_dataset))
  print('---------------------------------------------------')
  print('plt')
  print((plt))
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  To Do  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  #  - compute metrics
  
  return(results)

}
# ______________________________________________________ Compute Sentiment Time Series ______________________________________________________













# # _____________________
# dummy data
# equity_dataset <- call_stock_data(tickers='AEG.jo', start=Sys.Date()-365)$data
# tweets <- get_timeline(c('nasa'), n=100)
# 
# # initial influencial users
# users <- c('WarrenIngram', 'TradersCorner', 'paul_vestact', 'SimonPB', 'Richards_Karin',
#            'JP_Verster', 'Nerina_Visser','AdrianSaville', 'chrishartZA', 'davidshapiro61')
# 
# # download their timelines
# jse_tweets_BIG <- get_timeline(users, n=50)
# 
# 
# 
# r <- sentiment_time_series(jse_tweets_BIG, equity_dataset=equity_dataset)
# 
# r$plt












































# _____________________________________________________________                _____________________________________________________________
# _____________________________________________________________ Call Functions _____________________________________________________________
# _____________________________________________________________                _____________________________________________________________
configure_workspace(F)
print('_______________________ workspace ready √ _______________________')



























# _____________________________________________________________ To Do _____________________________________________________________
# - 







# _____________________________________________________________ Get Tweets _____________________________________________________________
# # initial influencial users
# users <- c('WarrenIngram', 'TradersCorner', 'paul_vestact', 'SimonPB', 'Richards_Karin', 
#            'JP_Verster', 'Nerina_Visser','AdrianSaville', 'chrishartZA', 'davidshapiro61')
# 
# dummy_users <- 'WarrenIngram TradersCorner paul_vestact    davidshapiro61 ErrorName404234112'
# 
# dummy_users <- strsplit(dummy_users, "\\s+")[[1]]
# error_message <- "User Not Found"
# 
# 
# 
# get_user_tweets <- function(usernames, n_tweets) {
#   print("___________ go ___________")
# #  usernames <- trimws(usernames)
#   tweets <- get_timeline(usernames, n=n_tweets)
#   
#   return(tweets)
# }
# 
# 
# # download their timelines
# jse_influencers <- get_timeline(users, n=2500)

# _____________________________________________________________ Get Tweets _____________________________________________________________











# ____________________________________________________ Plot Profile Picture ____________________________________________________
# prod_image <- function(image_url) {
#   download.file('profile_image', url=image_url, mod='wb')
#   pp <- readJPEG('profile_image')
#   # plot.new() 
#   # rasterImage(pp,0,0,.5,.5)
#   pp
# }
# 
# store_image_data <- function(summary_stats, prod_image=prod_image){
#   "Return: a list of pixels representing image data"
#   
#   images <- summary_stats$profile_image_url      # urls
#   r <- lapply(images, prod_image)                # Store Image Data 
#   r
# }
# 
# 
# 
# # ______ example ______
# dummy_users <- c('zachcolinwolpe', 'realdonaldtrump', 'elonmusk')
# tweets <- get_timeline(dummy_users, n=5)
# summary_stats <- compute_summary_stats(tweets)
# 
# images <- summary_stats$profile_image_url
# 
# # Store Image Data 
# r <- lapply(images, prod_image)
# ____________________________________________________ Plot Profile Picture ____________________________________________________





