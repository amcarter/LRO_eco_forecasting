#run cleaning data fall 2026 before this file
library(ggplot2)
library(dplyr)

data <- Logan_River_Updated_train[Logan_River_Updated_train$DO >= 0 
                                  & Logan_River_Updated_train$turbidity >= 0 
                                  & Logan_River_Updated_train$discharge >= 0 
                                  & Logan_River_Updated_train$pH >=0 
                                  #& Logan_River_Updated_train$watertemp >= 0
                                  ,]


#removing one data source as a "test set" 
data <- data[data$source != "Main St",]



# summary(Logan_River_Updated_train)
# 
# head(Logan_River_Updated_train)

ggplot(data, aes(x=turbidity, y=DO, color = source)) +
  geom_point(size = 1)


ggplot(data, aes(x=turbidity, y=pH, color = source)) +
  geom_point(size = 1)



#just at franklin basin
fb_data <- data[data$source == "Franklin",]

ggplot(fb_data, aes(x=turbidity, y=PAR, color = pH)) +
  geom_point(size = 1)

ggplot(fb_data[fb_data$DO != 0,], aes(x=PAR, y = DO)) +
  geom_point(size = 0.5)


ggplot(fb_data[fb_data$watertemp > 0,], aes(x=PAR, y = watertemp)) +
  geom_point(size = 0.5)

ggplot(fb_data[fb_data$pH > 0,], aes(x=PAR, y = pH)) +
  geom_point(size = 0.5)


