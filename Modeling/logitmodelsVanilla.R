library(tidyverse)
library(aod)
library(xtable)
library(pROC)
library(stargazer)
library(randomForest)

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

preds2 = function(model, df, type, threshold){
  if(type == "logit"){
    preds = predict(model, newdata = df, type = "response")
    pred_class = ifelse(preds >= threshold, 1, 0) 
    
    
    return(pred_class)
  }
}

predictions = function(model, df, type, threshold){
  if(type == "logit"){
    preds = predict(model, newdata = df, type = "response")
    pred_class = ifelse(preds >= threshold, 1, 0) 

    nas = sum(is.na(pred_class))
    count = nrow(df) - nas
    error = sum(na.omit(ifelse(pred_class == df$slum, 1, 0))/count)
    return(error)
  }
}


setwd("E:/Senior Thesis/# Final Work/Modeling")

citydf = read_csv("../kenyaCity_modelAll.csv")
head(citydf)

citydf$cken_pd_2013 = as.factor(citydf$cken_pd_2013)
citydf$clights_KenyaF182013 = as.factor(citydf$clights_KenyaF182013)
citydf$slum = as.factor(citydf$slum)

binsdf = read_csv("../kenyaCity_modelAll10.csv")
head(binsdf)

binsdf$cken_pd_2013 = as.factor(binsdf$cken_pd_2013)
binsdf$clights_KenyaF182013 = as.factor(binsdf$clights_KenyaF182013)
binsdf$slum = as.factor(binsdf$slum)

set.seed(10)

## Sampling and splitting datasets

spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(citydf)), 
  nrow(citydf)*cumsum(c(0,spec)),
  labels = names(spec)
))

res_norm = split(citydf, g)
res_bins = split(binsdf, g)

res_norm$train$cases = ifelse(res_norm$train$slum == 1, 2, 1)
res_bins$train$cases = ifelse(res_bins$train$slum == 1, 2, 1)

valid_slums_norm = subset(res_norm$validate, slum == 1)
valid_slums_bins = subset(res_bins$validate, slum == 1)

## Forming the models


logit_cityTRAIN = glm(slum ~ clights_KenyaF182013*cken_pd_2013, 
                      data = res_norm$train, 
                      family = "binomial")
summary(logit_cityTRAIN)
stargazer(logit_cityTRAIN)

binsdf_cityTRAIN = glm(slum ~ clights_KenyaF182013*cken_pd_2013, 
                       data = res_bins$train, 
                       family = "binomial")
summary(binsdf_cityTRAIN)
stargazer(binsdf_cityTRAIN)

city_weightedTRAIN = glm(slum ~ clights_KenyaF182013*cken_pd_2013, 
                         data = res_norm$train, 
                         family = "binomial",
                         weights = cases)
summary(city_weightedTRAIN)
stargazer(city_weightedTRAIN)




## Doing the Vanilla threshold cuttoff


total_error_van = c()
slum_error_van = c()
for (val in 1:1000){
  percent = val/1000
  
  total_error_van = append(total_error_van, predictions(logit_cityTRAIN, res_norm$validate, "logit", percent))
  slum_error_van = append(slum_error_van, predictions(logit_cityTRAIN, valid_slums_norm, "logit", percent))
}


accuracy_df_van = data.frame(pred_thresh = (1:1000)/1000,
                         `Total Accuracy` = total_error_van,
                         `Slum Restricted Accuracy` = slum_error_van)

pacc_df_van = pivot_longer(accuracy_df_van, cols = c(`Total.Accuracy`, `Slum.Restricted.Accuracy`) , names_to = "Acc. Type", values_to = "Value")

pacc_df_van %>%
  ggplot(aes(x = pred_thresh, y = Value, color = `Acc. Type`))+
  geom_line(size = 1.001)+
  scale_linetype_discrete(labels=c('Total Dataset', 'Slum Observation'))+
  labs(x = "Prediction Threshold", y = "Accuracy", title = "")+
  scale_color_manual(values=c("#386cb0","#fdb462"))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(.8, .9),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))


## Bins Treshold Cuttoff


total_error_bins = c()
slum_error_bins = c()
for (val in 1:1000){
  percent = val/1000
  
  total_error_bins = append(total_error_bins, predictions(binsdf_cityTRAIN, res_bins$validate, "logit", percent))
  slum_error_bins = append(slum_error_bins, predictions(binsdf_cityTRAIN, valid_slums_bins, "logit", percent))
}

accuracy_df_bins = data.frame(pred_thresh = (1:1000)/1000,
                         `Total Accuracy` = total_error_bins,
                         `Slum Restricted Accuracy` = slum_error_bins)

pacc_df_bins = pivot_longer(accuracy_df_bins, cols = c(`Total.Accuracy`, `Slum.Restricted.Accuracy`) , names_to = "Acc. Type", values_to = "Value")

pacc_df_bins %>%
  ggplot(aes(x = pred_thresh, y = Value, color = `Acc. Type`))+
  geom_line(size = 1.001)+
  scale_linetype_discrete(labels=c('Total Dataset', 'Slum Observation'))+
  labs(x = "Prediction Threshold", y = "Accuracy", title = "")+
  scale_color_manual(values=c("#386cb0","#fdb462"))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(.8, .9),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))


# Weighted Threshold Cuttoff

total_error_weight = c()
slum_error_weight = c()
for (val in 1:1000){
  percent = val/1000
  
  total_error_weight = append(total_error_weight, predictions(city_weightedTRAIN, res_norm$validate, "logit", percent))
  slum_error_weight = append(slum_error_weight, predictions(city_weightedTRAIN, valid_slums_norm, "logit", percent))
}


accuracy_df_weight = data.frame(pred_thresh = (1:1000)/1000,
                             `Total Accuracy` = total_error_weight,
                             `Slum Restricted Accuracy` = slum_error_weight)

pacc_df_weight = pivot_longer(accuracy_df_weight, cols = c(`Total.Accuracy`, `Slum.Restricted.Accuracy`) , names_to = "Acc. Type", values_to = "Value")

pacc_df_weight %>%
  ggplot(aes(x = pred_thresh, y = Value, color = `Acc. Type`))+
  geom_line(size = 1.001)+
  scale_linetype_discrete(labels=c('Total Dataset', 'Slum Observation'))+
  labs(x = "Prediction Threshold", y = "Accuracy", title = "")+
  scale_color_manual(values=c("#386cb0","#fdb462"))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(.8, .9),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))



## ROC Curve Vanilla

predslol_van = predict(logit_cityTRAIN, newdata = res_norm$train, type = "response")
(test_roc_van = roc(res_norm$train$slum ~ predslol_van, plot = TRUE, print.auc = TRUE))


## ROC Curve Bins

predslol_bins = predict(binsdf_cityTRAIN, newdata = res_bins$train, type = "response")
(test_roc_bins = roc(res_bins$train$slum ~ predslol_bins, plot = TRUE, print.auc = TRUE))


## ROC Curve Weighted

predslol_weight = predict(city_weightedTRAIN, newdata = res_norm$train, type = "response")
(test_roc_weight = roc(res_norm$train$slum ~ predslol_weight, plot = TRUE, print.auc = TRUE))


## Random Forest Modeling

rf = randomForest(data = res_norm$train, slum ~ lights_KenyaF182013 + ken_pd_2013 + lights_KenyaF182013*ken_pd_2013)
summary(rf)
rf




predslol_rf = predict(rf, newdata = res_norm$validate, type = "response")
(accuracy = sum(ifelse(predslol_weight == res_norm$validate$slum, 1, 0))/nrow(res_norm$validate))
sum(ifelse(res_norm$validate$slum == 1, 1, 0))


predslol_weight


slum_val  = subset(res_norm$validate, slum == 1)
predsslum_weight = predict(rf, newdata = slum_val, type = "response")
(accuracy = sum(ifelse(predsslum_weight == slum_val$slum, 1, 0))/nrow(slum_val))
sum(ifelse(slum_val$slum == 1, 1, 0))

sum(ifelse(predsslum_weight == 1, 1, 0))

plot(rf,
     font = 5)

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
MeanDecreaseGini

partialPlot(rf, citydf, ken_pd_2013, 1)

library(ROCR)

predslol_weightrf <- predict(rf, newdata=res_norm$validate)
(test_roc_weightrf = roc(res_norm$validate$slum ~ predslol_weightrf, plot = TRUE, print.auc = TRUE))


roc.test <- roc(test$outcome, test.predictions$votes[,2])
auc(test_roc_weightrf)


pred1=predict(rf,type = "prob")
perf = prediction(pred1[,2], res_norm$train$slum)

# 1. Area under curve
auc = performance(perf, "auc")

attributes(performance(perf, 'auc'))$y.values[[1]]

typeof(auc)
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

## Prepping for Map Predictions

citydf$weightedBins = preds2(city_weightedTRAIN, citydf, "logit", 0.6)
citydf$predVanilla = preds2(logit_cityTRAIN, citydf, "logit", 0.4)
citydf$predBins = preds2(binsdf_cityTRAIN, binsdf, "logit", 0.4)
citydf$predRF = predict(rf, newdata = citydf, type = "response")


write_csv(citydf, "../kenyaCity_mapping.csv")




#########################################################################
# Archive


#statedf = read_csv("../kenyaState_modelAll.csv")
#head(statedf)

#statedf$cken_pd_2013 = as.factor(statedf$cken_pd_2013)
#statedf$clights_KenyaF182013 = as.factor(statedf$clights_KenyaF182013)
#statedf$slum = as.factor(statedf$slum)

# Looking at train and test error for the state dataset, also has code to make
# the plots for the threshold changing tradeoffs


#statedf$cken_pd_2013 = as.factor(statedf$cken_pd_2013)
#statedf$clights_KenyaF182013 = as.factor(statedf$clights_KenyaF182013)
#statedf$slum = as.factor(statedf$slum)

#spec = c(train = .6, test = .2, validate = .2)

#g = sample(cut(
#  seq(nrow(citydf)), 
#  nrow(citydf)*cumsum(c(0,spec)),
#  labels = names(spec)
#))

#res_state = split(statedf, g)




#logit_stateTRAIN = glm(slum ~ clights_KenyaF182013*cken_pd_2013, 
#                       data = res_state$train, 
#                       family = "binomial")
#summary(logit_stateTRAIN)

#predictions(logit_stateTRAIN, state_traindf, "logit", 0.1)
#predictions(logit_stateTRAIN, state_testdf, "logit", 0.1)


#valslums = subset(res_state$val, slum == 1)
#predictions(logit_stateTRAIN, res_state$val, "logit", 0.1)


#total_error = c()
#slum_error = c()
#for (val in 1:1000){
#  percent = val/1000
#  
#  total_error = append(total_error, predictions(logit_stateTRAIN, res_state$val, "logit", percent))
#  slum_error = append(slum_error, predictions(logit_stateTRAIN, valslums, "logit", percent))
#}


#accuracy_df = data.frame(pred_thresh = (1:1000)/1000,
#                         `Total Accuracy` = total_error,
#                         `Slum Restricted Accuracy` = slum_error)

#pacc_df = pivot_longer(accuracy_df, cols = c(`Total.Accuracy`, `Slum.Restricted.Accuracy`) , names_to = "Acc. Type", values_to = "Value")

#pacc_df %>%
#  ggplot(aes(x = pred_thresh, y = Value, color = `Acc. Type`))+
#  geom_line(size = 1.001)+
#  scale_linetype_discrete(labels=c('Total Dataset', 'Slum Observation'))+
#  labs(x = "Prediction Threshold", y = "Accuracy", title = "Slums and Total Dataset Accuracy Tradeoff")+
#  scale_color_manual(values=c('red','darkblue'))+
#  theme_bw()+
#  theme(legend.title = element_blank(),
#        legend.position = c(.8, .6),
#        plot.subtitle = element_text(hjust = 0.5),
#        plot.caption = element_text(hjust = 0), 
#        text = element_text(family="serif"),
#        axis.line = element_line(colour = "black"), 
#        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
#        axis.text.x = element_text(angle = 0))



#statedf$predVanilla = preds2(logit_stateTRAIN, statedf, "logit", 0.1)



#write_csv(statedf, "../kenyaState_mapping.csv")

# creating the base full data models
#logit_city = glm(slum ~ clights_KenyaF182013*cken_pd_2013, data = citydf, 
#            family = "binomial")
#summary(logit_city)
#
#logit_state = glm(slum ~ clights_KenyaF182013*cken_pd_2013, data = statedf, 
#                 family = "binomial")
#summary(logit_state)

# Looking at training and test error for the city dataset

