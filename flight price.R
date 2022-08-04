#####Initialization
#check and download packages for the code
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")

if(!require(elasticnet)) install.packages("elasticnet")
if(!require(gam)) install.packages("gam")
if(!require(randomForest)) install.packages("randomForest")

if(!require(utils)) install.packages("utils")
if(!require(DescTools)) install.packages("DescTools")
if(!require(Metrics)) install.packages("Metrics")
if(!require(doParallel)) install.packages("doParallel")
if(!require(knitr)) install.packages("knitr")

library(tidyverse)
library(caret)

library(elasticnet)
library(gam)
library(randomForest)

library(utils)
library(DescTools)
library(Metrics)

#this is for using multiple cores, depending of your computer you can avoid running the code
#of this package or change the number of cores for training the models, also I made the models
#be available to download, so you don't need to train the models
library(doParallel)
library(knitr)

#check if the data is downloaded
if(!file.exists("archive.zip")) stop("Download archive.zip first")
#data from: 
#https://www.kaggle.com/datasets/shubhambathwal/flight-price-prediction?select=Clean_Dataset.csv
#data collected in June 22, 2022
#also available from the GitHub repository of this project


#Creates data folder where I save plots and models used in the .RMD file if not already present
data_folder = 'data'
if(!dir.exists(data_folder)){
  dir.create(data_folder)
}

#####Data downloading##### 
#read the data
data <- read.csv(unz("./archive.zip","Clean_Dataset.csv"))

#change the type of data of each variable
data <- data %>% mutate(airline = as.factor(airline),
                        flight = as.factor(flight),
                        source_city = as.factor(source_city),
                        departure_time = as.factor(departure_time),
                        arrival_time = as.factor(arrival_time),
                        destination_city = as.factor(destination_city),
                        stops = as.factor(stops),
                        class = as.factor(class),
                        days_left = as.numeric(days_left)) %>% rename(ticketId = X)

#change the levels of the factor variables, to make plotting easier
data$stops <- factor(data$stops, levels = c("zero","one","two_or_more"))

data$departure_time <- factor(data$departure_time, 
                              levels = c("Early_Morning","Morning","Afternoon",
                                         "Evening","Night","Late_Night"))

data$arrival_time <- factor(data$arrival_time, 
                              levels = c("Early_Morning","Morning","Afternoon",
                                         "Evening","Night","Late_Night"))

#####Analysis#####
#plots made with ggplot

#duration of flight vs price, by airline and class
{
  if(!file.exists(file.path(data_folder,"duration_vs_price_by_airline.png"))){
    #make the plot
    data %>% ggplot(aes(x=duration,y=price,color=class)) +
      geom_point(alpha=0.2) +
      ggtitle("Duration of flight (in hours) vs Price, per airline") +
      facet_wrap(~airline, scale="fixed") +
      guides(color = guide_legend(override.aes = list(alpha = 1,size=5)))
    
    #save the plot
    ggsave(filename = file.path(data_folder,"duration_vs_price_by_airline.png"))
  }
}

#days_left vs price, by class
{
  if(!file.exists(file.path(data_folder,"days_left_vs_price_by_class.png"))){
    #make the plot
    data %>% ggplot(aes(x=as.factor(days_left),y=price)) +
      geom_boxplot(aes(fill=class),outlier.alpha = 0.5,outlier.size = 0.7,width=0.5) +
      ggtitle("Days left vs Price, by class") +
      scale_x_discrete(breaks = levels(as.factor(data$days_left))[c(T, rep(F, 4))]) +
      facet_wrap(~class, scale="fixed") +
      theme(legend.position = "none") +
      xlab("days_left")
    
    #save the plot
    ggsave(filename = file.path(data_folder,"days_left_vs_price_by_class.png"))
  }
}

#amount of tickets and mean price per route
{
  if(!file.exists(file.path(data_folder,"number_flights_per_route.png"))){
    #get the mean values
    mean_values <- data %>% group_by(source_city,destination_city) %>%
      summarise(mean_price = mean(price),n=n()) %>% ungroup()
    
    #setting values for not existing flights, needed for plotting
    for(city  in levels(data$source_city)){
      mean_values <- mean_values %>% add_row(source_city = city,
                                             destination_city = city,
                                             mean_price = NA,
                                             n=NA)
    }
    rm(city)
    mean_values <- as.data.frame(mean_values)
    
    #make the plot
    mean_values %>% ggplot(aes(x=source_city,y=destination_city)) +
      geom_tile(aes(fill=mean_price)) +
      geom_text(aes(x=source_city,y=destination_city,label=n)) +
      scale_fill_gradient(low = "yellow", high = "red") +
      ggtitle("Amount of tickets sold and mean price per route") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"))
    
    #save the plot
    ggsave(filename = file.path(data_folder,"number_flights_per_route.png"))
    remove(mean_values)
  }
}

#departure and arrival time mean price
#for each category of stops
{
  if(!file.exists(file.path(data_folder,"departure_arrival_count_mean_price_zero.png"))){
    #grouping the data so I can make different plots for each factor of stops
    mean_values <- data %>% group_by(arrival_time,departure_time,stops) %>%
      summarise(mean_price = mean(price),n=n()) %>% ungroup()
    
    #make the plots
    for(stop in unique(mean_values$stops)){
      print(mean_values %>% filter(stops == stop) %>%
              ggplot(aes(x=departure_time,y=arrival_time)) +
              geom_tile(aes(fill=mean_price)) +
              geom_text(aes(x=departure_time,y=arrival_time,label=n) ) +
              scale_fill_gradient(low = "yellow", high = "red") +
              ggtitle(paste0("Number of tickets in each time combination\nfor ",stop," stops")) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              theme(panel.background = element_rect(fill = "gray"),
                    panel.grid.major = element_line(color = "black"),
                    axis.text.x = element_text(color = "black"),
                    axis.text.y = element_text(color = "black")) )
      
      #save the plot
      ggsave(filename = file.path(data_folder,
                                  paste0("departure_arrival_count_mean_price_",stop,".png")),
             height = 2.5)
      Sys.sleep(2)
    }
    remove(stop,mean_values)
  }
} 

#price by number of stops
{
  if(!file.exists(file.path(data_folder,"number_stops_vs_price_by_class.png"))){
    #make the plot
    data %>% ggplot(aes(x=stops,y=price)) +
      geom_boxplot(aes(fill=class),outlier.alpha = 0.5,outlier.size = 0.7) +
      ggtitle("Number of stops vs price, by class")
    
    #save the plot
    ggsave(filename = file.path(data_folder,"number_stops_vs_price_by_class.png"))
  }
}

#quantity of tickets by flight
{
  if(!file.exists(file.path(data_folder,"number_of_tickets_by_flight.png"))){
    #reorder the data and make the plot
    data %>% count(flight) %>% arrange(desc(n)) %>%
      ggplot(aes(x = reorder(flight, -n), y=n)) +
      geom_col() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      xlab("flight") +
      ggtitle("Number of tickets sold by flight")
    
    #save the plot
    ggsave(filename = file.path(data_folder,"number_of_tickets_by_flight.png"))
  }
}

#get the top 52 flights more bought
#the top 52 is because the random forest package/function has a limit of 53
#categories, so 52 are individual flights and the 53th is one called the "others"
#I could use dummy variables but I don't have a good enough pc for this approach
top_flights <- data %>% select(flight) %>% group_by(flight) %>% count() %>% 
  arrange(desc(n)) %>% head(52)

#to know how many of the tickets are in the top 52 flights
mean(data$flight %in% top_flights$flight)

####Data split for a final validation####
set.seed(20220622, sample.kind="Rounding")
test_index <- createDataPartition(y=data$price,times = 1, p=0.2, list=FALSE)

flight_tickets <- data[-test_index,]
temp <- data[test_index,]

#make sure that flight in validation set are also in the flight_tickets set
validation <- temp %>%
  semi_join(flight_tickets, by = "flight") %>%
  semi_join(flight_tickets, by = "duration")

#add rows removed from validation set back into flight_tickets set
removed <- anti_join(temp, validation)
flight_tickets <- rbind(flight_tickets, removed)

rm(test_index,removed,temp)

remove(data)

#####Methods#####

##split for training for training and testing
set.seed(20220627, sample.kind="Rounding")
test_index <- createDataPartition(y=flight_tickets$price,times = 1, p=0.2, list=FALSE)

flight_tickets <- flight_tickets %>%
  mutate(duration = round(duration))

temp <- flight_tickets[test_index,]
flight_train <- flight_tickets[-test_index,]

#make sure that flight in test set are also in the train set
flight_test <- temp %>%
  semi_join(flight_train, by = "flight") %>%
  semi_join(flight_train, by = "duration")

#add rows removed from test set back into train set
removed <- anti_join(temp, flight_test)
flight_train <- rbind(flight_train,removed)

remove(test_index,temp,removed)

####gamLoess model####
if(!file.exists(file.path(data_folder,"gamLoess.Rdata"))){
  #setting the parameters to search
  set.seed(1, sample.kind="Rounding")
  gamLoess_parameters <- expand.grid(span=seq(0.10,1,len=25), degree=1)
  
  #starting the multi-processing code
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  train_gamLoess <- train(price~.-ticketId-flight,method="gamLoess",data=flight_train,
                          tuneGrid = gamLoess_parameters,
                          trControl = trainControl(allowParallel = T))
  
  #ending the multi-processing code
  stopCluster(cl)
  
  #get results of the model
  predictions_gamLoess <- predict(train_gamLoess, flight_test)
  results_gamLoess <- postResample(predictions_gamLoess, flight_test$price)
  results_gamLoess
  
  #save the model
  save(train_gamLoess,predictions_gamLoess,results_gamLoess, file=file.path(data_folder,"gamLoess.RData"))
} else {
  load(file = file.path(data_folder,"gamLoess.Rdata"))
}

####random forest model####
if(!file.exists(file.path(data_folder,"tune_rf.Rdata"))){
  set.seed(1, sample.kind="Rounding")
  
  #for searching for the best mtry value
  tune_rf <- tuneRF(flight_train[,-c(1,3,12)], flight_train[,12],
                    ntreeTry = 50, stepFactor = 1.5)
  
  #get the best mtry value
  mtry_best <- tune_rf[which.min(tune_rf[,2]),1]
  save(tune_rf,mtry_best, file=file.path(data_folder,"tune_rf.RData"))
}else{
  load(file = file.path(data_folder,"tune_rf.RData"))
}

#for searching the best node size
nodesize <- seq(5,25,5)
set.seed(1, sample.kind="Rounding")

#due to hardware capability, I use 20% of the data
sample_train <- flight_train %>% sample_frac(0.20)

rf_rmses <- sapply(nodesize, function(ns){
  #starting the multi-processing code
  cl <- makeCluster(2)
  registerDoParallel(cl)
  
  rmse_rf <- train(price~.-ticketId-flight, method = "rf" ,data = sample_train,
                   tuneGrid = data.frame(mtry = mtry_best),
                   trControl = trainControl(allowParallel = T),
                   nodesize = ns,
                   ntree = 100)$results$RMSE
  
  #ending the multi-processing code
  stopCluster(cl)
  
  print(rmse_rf)
  return(rmse_rf)
})

#save the tuning values for the model
save(tune_rf,mtry_best,sample_train,nodesize,rf_rmses,
     file=file.path(data_folder,"tune_rf.RData"))

qplot(nodesize, rf_rmses)

###final model for rf
if(!file.exists(file.path(data_folder,"rf.RData"))){
  #load the best values
  load(file = file.path(data_folder,"tune_rf.RData"))
  
  #final model
  set.seed(1, sample.kind="Rounding")
  train_rf <- randomForest(price ~.-ticketId-flight, data=flight_train,ntree = 100, 
                           mtry= mtry_best,
                           nodesize = nodesize[which.min(rf_rmses)])
  
  
  #get results of the model
  predictions_rf <- predict(train_rf, flight_test)
  results_rf <- postResample(predictions_rf, flight_test$price)
  results_rf
  
  #save the model
  save(train_rf,predictions_rf,results_rf, file=file.path(data_folder,"rf.RData"))
}else{
  load(file = file.path(data_folder,"rf.RData"))
}

remove(cl,sample_train)

###adding a linear flight effect to the rf model###
if(!file.exists(file.path(data_folder,"flight_effect.RData"))){
  
  #get predicted values of price for train and test data
  price_prediction <- list()
  
  #values of price for train set with ticketId
  price_prediction$train <- data.frame(flight_train$ticketId,predict(train_rf, flight_train))
  colnames(price_prediction$train) <- c("ticketId","price_prediction")
  #values of price for test set with ticketId
  price_prediction$test <- data.frame(flight_test$ticketId,predict(train_rf, flight_test))
  colnames(price_prediction$test) <- c("ticketId","price_prediction")
  
  #search and get the best value of lambda for regularization
  lambdas <- seq(0,5,0.5)
  rmses_reg_flight <- sapply(lambdas, function(lambda){
    flight_effect <- flight_train %>%
      left_join(price_prediction$train, by="ticketId") %>%
      group_by(flight) %>%
      summarise(flight_effect = sum(price - price_prediction)/(n() + lambda))
    
    predicted <- flight_test %>%
      left_join(price_prediction$test, by="ticketId") %>%
      left_join(flight_effect, by="flight") %>%
      mutate(pred = price_prediction + flight_effect) %>%
      pull(pred)
    
    return(RMSE(flight_test$price, predicted))
  })
  
  best_lambda <- lambdas[which.min(rmses_reg_flight)]
  
  #get values of effects
  flight_effect <- flight_train %>%
    left_join(price_prediction$train, by="ticketId") %>%
    group_by(flight) %>%
    summarise(flight_effect = sum(price - price_prediction)/(n() + best_lambda))
  
  #get price prediction
  predictions_rf_flight_ef <- flight_test %>%
    left_join(price_prediction$test, by="ticketId") %>%
    left_join(flight_effect, by="flight") %>%
    mutate(pred = price_prediction + flight_effect) %>%
    pull(pred)
  
  #get results of the model
  results_rf_flight_ef <- postResample(predictions_rf_flight_ef, flight_test$price)
  results_rf_flight_ef
  
  #remove unnecessary predictions
  remove(predictions_gamLoess,predictions_rf,predictions_rf_flight_ef)
  
  #save the model
  save(flight_effect, results_rf_flight_ef, file = file.path(data_folder,"flight_effect.RData"))
}else{
  load(file = file.path(data_folder,"flight_effect.RData"))
}

####Validation set####
#save the actual value and predictions, for easy use
predictions_validation <- list()
predictions_validation$actual_price <- validation$price

#gamLoess model
#get predictions for this model
predictions_validation$gamLoess <- predict(train_gamLoess,validation)

#put results in table 
validation_results <- 
  tibble(model="gamLoess", 
         RMSE = RMSE(predictions_validation$actual_price,predictions_validation$gamLoess),
         negative_predictions = sum(predictions_validation$gamLoess<0),
         min = quantile(predictions_validation$gamLoess, names = F)[1],
         lower_quartile = quantile(predictions_validation$gamLoess, names = F)[2],
         median = quantile(predictions_validation$gamLoess, names = F)[3],
         upper_quartile = quantile(predictions_validation$gamLoess, names = F)[4],
         max = quantile(predictions_validation$gamLoess, names = F)[5])

#rf model
#get predictions for this model
predictions_validation$rf <- predict(train_rf,validation)

#put results in table 
validation_results <- validation_results %>%
  add_row(model="rf",
          RMSE=RMSE(predictions_validation$actual_price,predictions_validation$rf),
          negative_predictions =  sum(predictions_validation$rf<0),
          min = quantile(predictions_validation$rf, names = F)[1],
          lower_quartile = quantile(predictions_validation$rf, names = F)[2],
          median = quantile(predictions_validation$rf, names = F)[3],
          upper_quartile = quantile(predictions_validation$rf, names = F)[4],
          max = quantile(predictions_validation$rf, names = F)[5])

#rf model with linear flight effect
#get predictions for this model
#save data frame with ticketId and prediction for later use
validation_price_pred <- data.frame(validation$ticketId,predictions_validation$rf)
colnames(validation_price_pred) <- c("ticketId","price_prediction")

predictions_validation$rf_flight_ef <- validation %>%
  left_join(validation_price_pred, by="ticketId") %>%
  left_join(flight_effect, by= "flight") %>%
  mutate(pred = price_prediction + flight_effect) %>%
  pull(pred)

remove(validation_price_pred)

#put results in table 
validation_results <- validation_results %>%
  add_row(model="rf+flight effect",
          RMSE=RMSE(predictions_validation$actual_price,predictions_validation$rf_flight_ef),
          negative_predictions = sum(predictions_validation$rf_flight_ef<0),
          min = quantile(predictions_validation$rf_flight_ef, names = F)[1],
          lower_quartile = quantile(predictions_validation$rf_flight_ef, names = F)[2],
          median = quantile(predictions_validation$rf_flight_ef, names = F)[3],
          upper_quartile = quantile(predictions_validation$rf_flight_ef, names = F)[4],
          max = quantile(predictions_validation$rf_flight_ef, names = F)[5])

knitr::kable(validation_results, digits = 0)