library(shiny)
library(ggplot2)
library(ggmap)
# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  autoUpdate <- reactiveTimer(7500, session)
  
  data <- reactive({
    choice <- switch(input$choice,
                     day = daily_mentions,
                     week = weekly_mentions)
    
    choice(input$n)
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot1 <- renderPlot({
    autoUpdate()
    choice <- get(input$choice)
    #will have the choice here .. 
    if(identical(choice, weekly_mentions))
    {
      for (i in 1:length(temp)){
        file_name = paste0(c('primary','_',i),collapse = "")
        day_name = paste0(c('day','_',i,'_mentions'),collapse = "")
        #assign(file_name, read.xls(temp[i],quote = NULL,header=TRUE ))
        #assign(file_name, parseTweets(temp[i], simplify = FALSE))
        assign(day_name, data.frame(candidate= factor(), num = factor()))
        temp2 <- get(file_name) 
        temp3 <- get(day_name)
        for(j in candidates){
          sub <- temp2[grep(j, temp2$text), ]
          temp3<- rbind(temp3, data.frame(day=i,candidate = j,num=nrow(sub)))
        }
        day_mentions <- rbind(day_mentions,temp3)
        primary_data <- rbind(primary_data, temp2)
      }
      
      #have all tweet data at this point.. 
      
      weekly_mentions <- data.frame(candidate = factor(), num = factor())
      # tweet mentions for each popular candidate
      for(i in candidates){
        sub2 <- primary_data[grep(i, primary_data$text), ]
        weekly_mentions<- rbind(weekly_mentions, data.frame(candidate = i,num=nrow(sub2)))
      }
    ggplot(choice, aes(choice$candidate,choice$num)) + geom_bar(stat= "identity")
    }
    else if(identical(choice, day_mentions)){
    
      ggplot(choice, aes(choice$day, choice$num, color= choice$candidate, fill= choice$candidate))+geom_bar(stat='identity')
    }
    
    else{
      Sys.sleep(10)  
      file.remove("new_tweets.json")  
      filterStream(file.name = "new_tweets.json", # Save tweets in a json file
                   track = c("primary election","us election","trump","donald trump",
                             "hilary clinton", "clinton","democrats",
                             "republicans","sanders","bernie sanders",
                             "caucuses result","nevada primary",
                             "south carolina primary","iowa primary","new hampshire result",
                             "presidential election us","republican result",
                             "democrat result","primaries delegate result",
                             "new york times bernie sanders","new york times hilary clinton","new york times donald trump",
                             "fox news bernie sabders","fox news hilary","fox news donald trump",
                             "huffington post bernie sanders","huffington post hilary clinton","huffington post donald trump",
                             "cnbc bernie sanders","cnbc donald trump","cnbc hilary clinton",
                             "bloomberg bernie sanders","bloomberg hilary clinton","bloomberg donald trump",
                             "bbc bernie sanders","bbc hsilary clinton","bbc donald trump",
                             "new york times primary elections","fox news primary elections", "cnbc primary elections","bloomberg primary elections"),
                   language = "en",
                   timeout = 1,
                   oauth = my_oauth) 
      
      new_tweets <- data.frame()
      new_tweets <- parseTweets("new_tweets.json", simplify = FALSE)
      new_tweets <- new_tweets [1:100,]
      new_mentions <- data.frame(candidate = factor(), num = factor())
      # tweet mentions for each popular candidate
      for(i in candidates){
        sub2 <- new_tweets[grep(i, new_tweets$text), ]
        new_mentions<- rbind(new_mentions, data.frame(candidate = i,num=nrow(sub2)))
        
      }
      
      ggplot(new_mentions, aes(new_mentions$candidate,new_mentions$num)) + geom_bar(stat= "identity")
      
      
      
      
      
    }
    
    })
  
  output$plot2 <- renderPlot({
    autoUpdate()
    choice <- get(input$choice)
    #will have the choice here .. 
    if(identical(choice, weekly_mentions))
    {
      
      ggplot() +    borders("world", colour="gray50", fill="gray50")  +geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=1.5) 
      
    }
    else if(identical(choice , day_mentions)){
      
      ggplot() +    borders("world", colour="gray50", fill="gray50")  + geom_point(aes(x=visit_1.x, y=visit_1.y) ,color="blue", size=1.5)+
        geom_point(aes(x=visit_2.x, y=visit_2.y) ,color="red", size=1.5) +
        geom_point(aes(x=visit_3.x, y=visit_3.y) ,color="green", size=1.5) +
        geom_point(aes(x=visit_4.x, y=visit_4.y) ,color="orange", size=1.5) +
        geom_point(aes(x=visit_5.x, y=visit_5.y) ,color="black", size=1.5) +
        geom_point(aes(x=visit_6.x, y=visit_6.y) ,color="yellow", size=1.5)  
        
     
      
    } 
    
  else {
    Sys.sleep(10)  
    file.remove("new_tweets.json")  
    filterStream(file.name = "new_tweets.json", # Save tweets in a json file
                 track = c("primary election","us election","trump","donald trump",
                           "hilary clinton", "clinton","democrats",
                           "republicans","sanders","bernie sanders",
                           "caucuses result","nevada primary",
                           "south carolina primary","iowa primary","new hampshire result",
                           "presidential election us","republican result",
                           "democrat result","primaries delegate result",
                           "new york times bernie sanders","new york times hilary clinton","new york times donald trump",
                           "fox news bernie sabders","fox news hilary","fox news donald trump",
                           "huffington post bernie sanders","huffington post hilary clinton","huffington post donald trump",
                           "cnbc bernie sanders","cnbc donald trump","cnbc hilary clinton",
                           "bloomberg bernie sanders","bloomberg hilary clinton","bloomberg donald trump",
                           "bbc bernie sanders","bbc hsilary clinton","bbc donald trump",
                           "new york times primary elections","fox news primary elections", "cnbc primary elections","bloomberg primary elections"),
                 language = "en",
                 timeout = 1,
                 oauth = my_oauth) 
    
    new_tweets <- data.frame()
    new_tweets <- parseTweets("new_tweets.json", simplify = FALSE)
    new_tweets <- new_tweets [1:100,]
    new_mentions <- data.frame(candidate = factor(), num = factor())
    # tweet mentions for each popular candidate
    for(i in candidates){
      sub2 <- new_tweets[grep(i, new_tweets$text), ]
      new_mentions<- rbind(new_mentions, data.frame(candidate = i,num=nrow(sub2)))
    }
    new_visited <- data.frame()
    new_ll.visited <- data.frame()
    new_visit.x<- data.frame()
    new_visit.y<- data.frame()
    new_visited <- subset(new_tweets,  !(is.na(new_tweets$location)))$location[1:10]
    new_ll.visited <- geocode(new_visited)
    new_visit.x <- new_ll.visited$lon
    new_visit.y <- new_ll.visited$lat
    
    
    
    ggplot() +   borders("world", colour="gray50", fill="gray50")  +
      geom_point(aes(x=new_visit.x, y=new_visit.y) ,color="white", size=1.5) 
    
    
    
    
    
  }
    
  })
  
})