load.libraries <- c("shiny","tidytext","dplyr","rvest","shiny","tm","rpart","gridExtra","stringr","ggplot2")

install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)

#UI
ui <- fluidPage(
  headerPanel(title = "Rahul Reddy Muppidi_Natural Language Processing"),
  sidebarLayout(
    sidebarPanel(
      textInput("URL","Enter URL","https://www.cars.com/research/toyota-camry/")
    ),
    mainPanel(
      tableOutput("network")
      
      
      )
  )
)
#Server
server <- function(input,output){
  
  data <- reactive({
    site <- input$URL
    year<-read_html(site) %>%html_nodes("[class=cui-accordion-section__title]") %>%html_text()
    year<- year[-1]
    link<-read_html(site) %>%html_nodes("[class=star-rating-wrapper]") %>%html_attr("href")
    raw_data <- data.frame(year,link,stringsAsFactors = F)
    raw_data <- raw_data[c(2:7),]
    raw_data$link <- paste0("https://www.cars.com",raw_data$link)
    
    
    user_ratings <- data.frame()
    for (i in 1:nrow(raw_data)){
      count <- data.frame(user_ratings =((read_html(raw_data[i,2]) %>%html_nodes("[class=large]") %>%html_attr("reviews") )),stringsAsFactors = F)
      user_ratings <- rbind(user_ratings,count)
    }
    raw_data <- cbind(raw_data,user_ratings)
    
    final_data = data.frame()
    for(i in 1:nrow(raw_data)){
      rating <- data.frame(rating =((html(paste0(raw_data[i,2],"?nr=",max(raw_data$user_ratings))) %>%html_nodes("[class=cr-star-rating]") %>%html_attr("rating") )),stringsAsFactors = F)
      review <- data.frame(review =((html(paste0(raw_data[i,2],"?nr=",max(raw_data$user_ratings))) %>%html_nodes("[itemprop=description]") %>%html_text() )),stringsAsFactors = F)
      year <- rep(raw_data[i,1],raw_data[i,3])
      merged <- data.frame(year,rating,review)
      final_data <- rbind(final_data,merged)
    }
    final_data
    
  })
  
  
  output$training_data <- renderTable({                                   
    final_data<-data()
    training_data <- final_data[final_data$year != "2017",]
    training_data$year <- factor(training_data$year)
    training_data
  })
  
  output$test_data <- renderTable({
    final_data<-data()
    test_data <- final_data[final_data$year == "2017",]
    test_data$year<- factor(test_data$year)
    test_data
    
  })
  
  output$Norm_final_data <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    Norm_final_data
  })
  
  output$tagged_data <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data
  })
  
  output$affin_table <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data$normalized_review<- as.character(tagged_data$normalized_review)
    word_tb <- tagged_data %>%
      unnest_tokens(word,normalized_review)
    x<-get_sentiments("afinn")
    y <- word_tb %>%
      left_join(x)
    z <- data.frame(aggregate(score ~ review , y, mean))
    affin_table <- tagged_data %>%
      inner_join(z)
    affin_table
  })
  
  output$Q6_a <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data$normalized_review<- as.character(tagged_data$normalized_review)
    word_tb <- tagged_data %>%
      unnest_tokens(word,normalized_review)
    x<-get_sentiments("afinn")
    y <- word_tb %>%
      left_join(x)
    z <- data.frame(aggregate(score ~ review , y, mean))
    affin_table <- tagged_data %>%
      inner_join(z)
    Updated_training_data <- affin_table[affin_table$year != "2017",]
    Updated_training_data$year <- factor(Updated_training_data$year)
    mean_sentiment_score <- mean(Updated_training_data$score)
    mean_star_rating <- mean(as.numeric(Updated_training_data$rating))
    Q6_a<-cbind(mean_star_rating,mean_sentiment_score)
    Q6_a
    
  })
  
  output$agg <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data$normalized_review<- as.character(tagged_data$normalized_review)
    word_tb <- tagged_data %>%
      unnest_tokens(word,normalized_review)
    x<-get_sentiments("afinn")
    y <- word_tb %>%
      left_join(x)
    z <- data.frame(aggregate(score ~ review , y, mean))
    affin_table <- tagged_data %>%
      inner_join(z)
    Updated_training_data <- affin_table[affin_table$year != "2017",]
    Updated_training_data$year <- factor(Updated_training_data$year)
    mean_sentiment_score <- mean(Updated_training_data$score)
    mean_star_rating <- mean(as.numeric(Updated_training_data$rating))
    Q6_a<-cbind(mean_star_rating,mean_sentiment_score)
    refined<-Updated_training_data[Updated_training_data$tags != "",]
    refined$tags<-as.character(refined$tags)
    unnest_refined <- refined %>%
      unnest_tokens(word,tags)
    unnest_refined$rating<- as.numeric(unnest_refined$rating)
    agg_tags <- data.frame(aggregate(score ~ word , unnest_refined, mean))
    agg_rating <- data.frame(aggregate(rating ~ word , unnest_refined, mean))
    agg <- agg_tags %>%
      inner_join(agg_rating)
    agg
    
  })
  
  output$Accuracy <- renderTable({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data$normalized_review<- as.character(tagged_data$normalized_review)
    word_tb <- tagged_data %>%
      unnest_tokens(word,normalized_review)
    x<-get_sentiments("afinn")
    y <- word_tb %>%
      left_join(x)
    z <- data.frame(aggregate(score ~ review , y, mean))
    affin_table <- tagged_data %>%
      inner_join(z)
    Updated_training_data <- affin_table[affin_table$year != "2017",]
    Updated_training_data$year <- factor(Updated_training_data$year)
    mean_sentiment_score <- mean(Updated_training_data$score)
    mean_star_rating <- mean(as.numeric(Updated_training_data$rating))
    Q6_a<-cbind(mean_star_rating,mean_sentiment_score)
    refined<-Updated_training_data[Updated_training_data$tags != "",]
    refined$tags<-as.character(refined$tags)
    unnest_refined <- refined %>%
      unnest_tokens(word,tags)
    unnest_refined$rating<- as.numeric(unnest_refined$rating)
    agg_tags <- data.frame(aggregate(score ~ word , unnest_refined, mean))
    agg_rating <- data.frame(aggregate(rating ~ word , unnest_refined, mean))
    agg <- agg_tags %>%
      inner_join(agg_rating)
    ##Building the model using decision trees
    modeldata <- as.data.frame(c(Updated_training_data$rating,Updated_training_data$score))
    tree <- rpart(rating ~ score, data = Updated_training_data)
    ## Testing the model on the test data set
    Updated_test_data <- affin_table[affin_table$year == "2017",]
    Updated_test_data$year <- factor(Updated_test_data$year)
    tree.pred=predict(tree ,Updated_test_data , type="class")
    ##Table of the Actual and predicted values
    p<-table(tree.pred ,Updated_test_data$rating)
    #Overall Accuracy on the test data set
    Accuracy<-as.data.frame((p[5,5])/sum(p[]))
    names(Accuracy)<- c("Accuracy")
    Accuracy
    
  })
  output$finalplots <- renderPlot({
    final_data<-data()
    normalized_review <- removePunctuation(final_data$review)
    normalized_review <- tolower(normalized_review)
    Norm_final_data <- cbind(final_data,normalized_review)
    keywords<-c("service", "price", "handling", "interior")
    m = sapply(keywords, grepl, Norm_final_data$normalized_review)
    tags<-apply(m,1, function(y) paste0(colnames(m)[y], collapse=","))
    tagged_data <- cbind(Norm_final_data,tags)
    tagged_data$normalized_review<- as.character(tagged_data$normalized_review)
    word_tb <- tagged_data %>%
      unnest_tokens(word,normalized_review)
    x<-get_sentiments("afinn")
    y <- word_tb %>%
      left_join(x)
    z <- data.frame(aggregate(score ~ review , y, mean))
    affin_table <- tagged_data %>%
      inner_join(z)
    Updated_training_data <- affin_table[affin_table$year != "2017",]
    Updated_training_data$year <- factor(Updated_training_data$year)
    mean_sentiment_score <- mean(Updated_training_data$score)
    mean_star_rating <- mean(as.numeric(Updated_training_data$rating))
    Q6_a<-cbind(mean_star_rating,mean_sentiment_score)
    refined<-Updated_training_data[Updated_training_data$tags != "",]
    refined$tags<-as.character(refined$tags)
    unnest_refined <- refined %>%
      unnest_tokens(word,tags)
    unnest_refined$rating<- as.numeric(unnest_refined$rating)
    agg_tags <- data.frame(aggregate(score ~ word , unnest_refined, mean))
    agg_rating <- data.frame(aggregate(rating ~ word , unnest_refined, mean))
    agg <- agg_tags %>%
      inner_join(agg_rating)
    modeldata <- as.data.frame(c(Updated_training_data$rating,Updated_training_data$score))
    tree <- rpart(rating ~ score, data = Updated_training_data)
    Updated_test_data <- affin_table[affin_table$year == "2017",]
    Updated_test_data$year <- factor(Updated_test_data$year)
    tree.pred=predict(tree ,Updated_test_data , type="class")
    p<-table(tree.pred ,Updated_test_data$rating)
    Accuracy<-(p[5,5])/sum(p[])
    ##Computing the TF - IDF
    
    unnest_refined$normalized_review <- removeWords(unnest_refined$normalized_review, stopwords("english"))
    
    
    tag_words <- unnest_refined %>%
      unnest_tokens(words,normalized_review) %>%
      count(word,words,sort = TRUE) %>%
      ungroup()
    
    
    final_tag_words <- tag_words %>%
      bind_tf_idf(words,word, n) %>%
      arrange(desc(tf_idf))
    
    names(final_tag_words) <- c("tag","word","n","tf", "idf", "tf_idf")
    final_tag_words <- final_tag_words[order(-final_tag_words$tf_idf),]
    
    # Create plots of the tf-idf for each tag
    # interior
    interior_plot <- ggplot(final_tag_words %>% 
                              filter(tag == "interior") %>% 
                              top_n(10),
                            aes(x = word, y = tf_idf)) +
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#4169E1") +
      coord_flip() +
      #scale_y_continuous(limits = c(0, 0.02)) +
      labs(x = NULL, y = "tf-idf", title = "interior") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    # price
    price_plot <- ggplot(final_tag_words %>% 
                           filter(tag == "price") %>% 
                           top_n(10),
                         aes(x = word, y = tf_idf)) +
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#4169E1") +
      coord_flip() +
      #scale_y_continuous(limits = c(0, 0.02)) +
      labs(x = NULL, y = "tf-idf", title = "price") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    # service
    service_plot <- ggplot(final_tag_words %>% 
                             filter(tag == "service") %>% 
                             top_n(10),
                           aes(x = word, y = tf_idf)) +
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#4169E1") +
      coord_flip() +
      #scale_y_continuous(limits = c(0, 0.02)) +
      labs(x = NULL, y = "tf-idf", title = "service") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    #handling
    handling_plot <- ggplot(final_tag_words %>% 
                              filter(tag == "handling") %>% 
                              top_n(10),
                            aes(x = word, y = tf_idf)) +
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#4169E1") +
      coord_flip() +
      #scale_y_continuous(limits = c(0, 0.02)) +
      labs(x = NULL, y = "tf-idf", title = "handling") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    finalplots <- grid.arrange(interior_plot,price_plot,service_plot,handling_plot, nrow =2,ncol = 2)
    finalplots
    
  })
  
  output$network <- renderUI({
    tabsetPanel(tabPanel("Q1_Training data", tableOutput("training_data")),
                tabPanel("Q2_Test data", tableOutput("test_data")),
                tabPanel("Q3_Normalized review", h6("Note: Please scroll to right to view all the columns"),tableOutput("Norm_final_data")),
                tabPanel("Q4_Tagged data",h6("Note: Please scroll to right to view all the columns"), tableOutput("tagged_data")),
                tabPanel("Q5_Sentiment Analysis",h6("Note: Please scroll to right to view all the columns"), tableOutput("affin_table")),
                tabPanel("Q6_a",h6("Below is the table which shows the comparision between Average sentiment score and Average user rating for the training data set"), tableOutput("Q6_a")),
                tabPanel("Q6_b", h6("Below is the table which displays the Average score and Average user rating for each tag in the training dataset"),tableOutput("agg"),h6("The average sentiment score(for all the tags together) from the above table is 1.745 which is almost close to value from Q6_a i.e 1.78")),
                tabPanel("Q7 and 8",h6("Over here we are trying to predict the star rating given the sentiment score and we also know that the dependent variable(i.e star rating) is a discrete variable, so I have used decision trees to built the model using the training data set and predict on the test data set. Below is overall accuracy on the test data set"), tableOutput("Accuracy"),h6("NOTE: Brief comments has been provided in the Code on How the model is built, prediction using the test data set and Accuracy calculations  ")),
                tabPanel("Q9",h6("Below are the plots for each tag in the training data set for the words with Top 10 TF_IDF scores "),h6("Note: There are few plots where we can find more than 10 words because the TF_IDF scores for couple of words are same to that of others, in total we have Top 10 different TF-IDF scores "), plotOutput("finalplots"),h6("COMMENTS:Above plots which are based on top 10 TF-IDF score for respective tags reflects how important a word is to a document in a collection. From these top 10 words we can draw the hidden insights what each customer is looking for and where the management can focus upon in maximising the customer satisfaction"))
                
    )
  })
}
shinyApp(ui,server)