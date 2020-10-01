### Lasso Regression

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

train_data <- data_anal[1:104,]
test_data <- data_anal[105:156,]

train_data_na <- na.omit(train_data)
x_var <- train_data_na %>% select(-cells_mean) %>% as.matrix()
y_var <- train_data_na[,"cells_mean"] %>% log1p() %>% as.matrix()

test_x_var <- test_data %>% select(-cells_mean) %>% as.matrix()
test_y_var <- test_data$cells_mean

result <- matrix(nrow = 3, ncol = 3)
colnames(result) <- c("parameter", "RMSE", "MAE")
rownames(result) <- c("Ridge1", "Ridge2", "Ridge3")


require(glmnet)
# RIDGE : 0, LASSO : 1
set.seed(777)
alpha <- 1

lambda_seq <- 10 ^ seq(3, -4, by = -0.1)
get_lambda <- cv.glmnet(x_var, y_var, 
                        alpha = alpha, lambda = lambda_seq, type.measure = "mse") 

# lambda.1se
best_1se <- glmnet(x_var, y_var, 
                   alpha = alpha, lambda = get_lambda$lambda.1se ,type.measure = "mse")

preds <- predict(best_1se, newx = test_x_var)
actual_ridge <- test_y_var

result[1,1] <- best_1se$lambda
result[1,2] <- mean((exp(preds) - actual_ridge) ^ 2) ^ 0.5
result[1,3] <- mean(abs(exp(preds) - actual_ridge))


# lambda.min
best_min <- glmnet(
  x_var,
  y_var,
  alpha = alpha,
  lambda = get_lambda$lambda.min ,
  type.measure = "mse"
)



IMP_lasso <- as.data.frame(as.matrix(best_min$beta))
IMP_lasso$feature <- rownames(IMP_lasso)

ggplot(data = IMP_lasso, aes(x = reorder(feature, abs(s0)), y = abs(s0), fill=abs(s0))) + 
  geom_bar(stat='identity', show.legend = F) +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_blank())

preds <- predict(best_min, newx = test_x_var)

lasso_result1 <- data.frame(week = test_data$week.iso)
lasso_result1 <- lasso_result1 %>% mutate(
  actual = test_y_var,
  preds = exp(predict(best_min, newx = test_x_var %>% as.matrix()))) %>%
  filter(is.na(preds) == F) 

lasso_result1_temp <- abs((lasso_result1$actual - lasso_result1$preds)/lasso_result1$actual)
sum(lasso_result1_temp[is.infinite(lasso_result1_temp) != T])/length(lasso_result1_temp[is.infinite(lasso_result1_temp) != T])

ggplot(data = lasso_result1, aes(x = week, y = actual, group = 1)) + geom_line(size = 2) +
  geom_line(aes(x = week , y = preds, group = 2),
            colour = "blue",
            size = 1.2) +
  theme_bw() +   theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("Lasso 회귀분석 예측값 대 실제값")

result[2,1] <- best_min$lambda
result[2,2] <- mean((exp(preds) - actual_ridge) ^ 2) ^ 0.5
result[2,3] <- mean(abs(exp(preds) - actual_ridge))

Ridge2_MAPE <- mean(abs(preds - actual) / actual) * 100

# lambda.mean
best_mean <- glmnet(x_var, y_var, 
                    alpha = alpha, lambda = (get_lambda$lambda.min + get_lambda$lambda.1se) / 2,
                    type.measure = "mse")

preds <- predict(best_mean, newx = test_x_var)

result[3,1] <- best_mean$lambda
result[3,2] <- mean((exp(preds) - actual_ridge) ^ 2) ^ 0.5
result[3,3] <- mean(abs(exp(preds) - actual_ridge))

result

