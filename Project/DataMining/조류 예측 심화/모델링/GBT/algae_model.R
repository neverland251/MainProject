library('Boruta')
library("ggplot2")
library("scales")
library("forecast")
library("TSA")
library("ggplot2")
library("dplyr")
library("reshape2")
library("magrittr")
library("lubridate")
library("tseries")

rowdata_none_NA_temp <- na.omit(rowdata)

set.seed(777)
Boruta_result <-
  Boruta(cells_mean ~ ., 
         data = rowdata_none_NA_temp[1:(length(rowdata_none_NA_temp[, 1]) - 1), ], 
         maxRuns = 10000)

summary(Boruta_result)

Boruta_result$finalDecision[Boruta_result$finalDecision == "Confirmed"]
names(Boruta_result$finalDecision[Boruta_result$finalDecision == "Confirmed"])

summary(Boruta_result)

### 최종 변수 선택
data_anal <- rawdata %>% select(cells_mean,
                                do_max,
                                do_mean,
                                do_min,
                                hPa_max,
                                humidity_min,
                                inflow_max,
                                inflow_mean,
                                inflow_min,
                                outflow_max,
                                outflow_mean,
                                outflow_min,
                                temp_max,
                                temp_mean,
                                temp_min,
                                temp_sd,
                                tn_max,
                                tn_mean,
                                tn_min,
                                toc_mean,
                                week.iso,
                                wt_temp_max,
                                wt_temp_mean,
                                wt_temp_min) # %>% mutate(week.iso = as.factor(week.iso))

colSums(is.na(data_anal))

train_data <- data_anal[1:104,]
test_data <- data_anal[105:156,]
train_data_NA <- na.omit(train_data)

################################################################################## lm
lm_model <- lm(cells_mean ~ ., data = train_data_NA) %>% step(direction = "both")
summary(lm_model)

library("lmtest")

plot(lm_model)

dwtest(lm_model)

lm_result <- data.frame(week = test_data$week.iso)
lm_result <- lm_result %>% mutate(actual = test_data$cells_mean,
                                  preds = predict(lm_model, 
                                                  newdata = test_data %>%select(-cells_mean), 
                                                  type = 'response') %>% round(1)) %>% 
  filter(is.na(preds) == F) 

lm_result$preds <- ifelse(lm_result$preds < 0, 0, lm_result$preds)

ggplot(data = lm_result, aes(x = week, y = actual, group = 1)) + geom_line(size = 2) + 
  geom_line(aes(x = week , y = preds, group = 2), colour = "hotpink", size = 1.2) + 
  theme_bw() + theme(legend.position = "top",axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("pred Vs original")

lm_RMSE <- mean((lm_result$preds - lm_result$actual) ^ 2) ^ 0.5
lm_MAE <- mean(abs(lm_result$preds - lm_result$actual))
lm_MAPE <- mean(abs(lm_result$preds - lm_result$actual) / lm_result$preds) * 100

library(car)
vif(lm_model)

################################################################## RANDOM FOREST
library(randomForest)
set.seed(777)

### ntree = 10,000 NA
rf_model10000_NA <- randomForest(formula = cells_mean ~ ., 
                                 data = train_data_NA,
                                 mtry = (ncol(train_data) - 1)%/%3, ntree = 10000, importance = T)

rf_result1 <- as.data.frame(test_data$week.iso)
rf_result1 <- rf_result1 %>% mutate(actual = test_data$cells_mean,
                                    preds = predict(rf_model10000_NA, 
                                                    newdata = test_data %>%select(-cells_mean), 
                                                    type = 'response') %>% round(1)) %>% 
  filter(is.na(preds) == F) 

rf_result1$preds <- ifelse(rf_result1$preds < 0, 0, rf_result1$preds)
rf10000_NA_RMSE <- mean((rf_result1$preds - rf_result1$actual) ^ 2) ^ 0.5
rf10000_NA_MAE <- mean(abs(rf_result1$preds - rf_result1$actual))
rf10000_NA_MAPE <- mean(abs(rf_result1$preds - rf_result1$actual) / rf_result1$preds) * 100

### ntree = 10,000 action
rf_model10000_action <- randomForest(formula = log1p(cells_mean) ~ ., 
                                     data = train_data,
                                     mtry = (ncol(train_data) - 1)%/%3, ntree = 10000, importance = T,
                                     na.action = na.roughfix)

IMP_rf <- rf_model10000_action$importance

IMP_rf <-as.data.frame(IMP_rf[order(IMP_rf[,c("IncNodePurity")],decreasing = T),])

rf_result2 <- data.frame(week = test_data$week.iso)
rf_result2 <- rf_result2 %>% mutate(actual = test_data$cells_mean,
                                    preds = exp(predict(rf_model10000_action, 
                                                        newdata = test_data %>%select(-cells_mean), 
                                                        type = 'response') %>% round(1))) %>% 
  filter(is.na(preds) == F) 

rf_result2$preds <- ifelse(rf_result2$preds < 0, 0, rf_result2$preds)

rf_result2_temp <- abs((rf_result2$actual - rf_result2$preds)/rf_result2$actual)
sum(rf_result2_temp[is.infinite(rf_result2_temp) == F])/length(rf_result2_temp[is.infinite(rf_result2_temp) == F])


ggplot(data = rf_result2, aes(x = week, y = actual, group = 1)) + geom_line(size = 2) +
  geom_line(aes(x = week , y = preds, group = 2),
            colour = "red",
            size = 1.2) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("RandomForest 예측값 대 실제값")

ggplot(data = IMP_rf, aes(x = reorder(feature, IncNodePurity), y = IncNodePurity, fill=IncNodePurity)) + 
  geom_bar(stat='identity', show.legend = F) +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_blank())





rf10000_action_RMSE <- mean((rf_result2$preds - rf_result2$actual) ^ 2) ^ 0.5
rf10000_action_MAE <- mean(abs(rf_result2$preds - rf_result2$actual))
rf10000_action_MAPE <- mean(abs(rf_result2$preds - rf_result2$actual) / rf_result2$preds) * 100

varImpPlot(rf_model10000_NA)
varImpPlot(rf_model10000_action)


############################################################## xgboost
library(xgboost)
x_var <- train_data %>% select(-cells_mean) %>% as.matrix()
y_var <- train_data[,"cells_mean"] %>% log1p() %>% as.matrix()

test_x_var <- test_data %>% select(-cells_mean) %>% as.matrix()
test_y_var <- test_data$cells_mean

xgb_model <- xgboost(data = x_var, # the data
                     label = y_var,
                     nround = 100, # max number of boosting iterations
                     objective = "reg:squarederror", # the objective function
                     seed =777)  

options(scipen=999)

IMP_xgb <- xgb.importance(model = xgb_model)
IMP_xgb <- as.data.frame(IMP_xgb[order(IMP_xgb[,c("Gain")],decreasing = T),])
IMP_xgb$Gain <- exp(IMP_xgb$Gain)

ggplot(data = IMP_xgb, aes(x = reorder(Feature, Gain), y = Gain, fill=Gain)) + 
  geom_bar(stat='identity', show.legend = F) +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_blank())

xgb_preds <- predict(xgb_model, newdata = test_data %>% select(-cells_mean) %>% as.matrix())
xgb_preds <- exp(xgb_preds)
xgb_preds <- ifelse(xgb_preds < 0, 0, xgb_preds)

xgb_actual <- test_y_var
xgb_RMSE <- mean((xgb_preds-xgb_actual)^2)^(1/2)
xgb_MAE <- mean(abs(xgb_preds-xgb_actual))
xgb_MAPE <- mean(abs(xgb_actual - xgb_preds) / xgb_actual) * 100

xgb_result_temp <- abs((xgb_actual - xgb_preds)/xgb_actual)
sum(xgb_result_temp[is.infinite(xgb_result_temp) == F])/length(xgb_result_temp[is.infinite(xgb_result_temp) == F])



xgb_result1 <- data.frame(week = test_data$week.iso)
xgb_result1 <- xgb_result1 %>% mutate(
  actual = test_data$cells_mean,
  preds = exp(predict(xgb_model, newdata = test_data %>% select(-cells_mean) %>% as.matrix()))) %>%
  filter(is.na(preds) == F) 

ggplot(data = xgb_result1, aes(x = week, y = actual, group = 1)) + geom_line(size = 2) +
  geom_line(aes(x = week , y = preds, group = 2),
            colour = "purple",
            size = 1.2) +
  theme_bw() +   theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("XGBoost 예측값 대 실제값")

score_model <- data.frame(
  model = c(
    "lasso",
    "lasso",
    "RandomForest",
    "RandomForest",
    "XGboost",
    "XGboost"
  ),
  type = c("RMSE", "MAE", "RMSE", "MAE", "RMSE", "MAE"),
  정확도 = c(result[2,2], result[2,3], rf10000_action_RMSE, rf10000_action_MAE, xgb_RMSE, xgb_MAE))

ggplot(data = score_model, aes(x=model, y=정확도, fill=type)) + 
  geom_bar(stat='identity',position="dodge", colour="black", width=0.5) +
  #coord_cartesian(ylim = c(0.5, 0.9)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(angle = 0, vjust = 1),
        axis.title.y = element_text(angle = 0, hjust = 1)) +
  ggtitle("분류 모델 전체 결과")


