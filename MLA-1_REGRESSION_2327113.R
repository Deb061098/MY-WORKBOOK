# Load the data
Salary_data <- read.csv("C:/Users/DEBANIK HAZRA/Documents/Latest_Data_Science_Salaries.csv", header = TRUE)

# Summary and structure
summary(Salary_data)
str(Salary_data)

# Check for null values
check_null_values <- function(dataframe) {
  null_counts <- colSums(is.na(dataframe))
  return(null_counts)
}
null_counts <- check_null_values(Salary_data)
print(null_counts)

# Convert categorical variables to factors
Salary_data$Experience.Level <- as.factor(Salary_data$Experience.Level)
Salary_data$Company.Size <- as.factor(Salary_data$Company.Size)
Salary_data$Expertise.Level <- as.factor(Salary_data$Expertise.Level)










# Example visualizations
library(ggplot2)

# Pie chart for average salary by Experience Level
average_salary <- aggregate(Salary_data$Salary.in.USD, by = list(Salary_data$Experience.Level), FUN = mean)
total_salary <- sum(average_salary$x)
ggplot(average_salary, aes(x = "", y = x, fill = Group.1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Experience Level Wise Avg Salary of Employees") +
  theme_void() +
  geom_text(aes(label = paste0(round(100 * x / total_salary), "%")), position = position_stack(vjust = 0.5))

# Bar graph for Company Size and Expertise Level
ggplot(Salary_data, aes(x = Company.Size, y = Salary.in.USD, fill = Expertise.Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Salary Distribution by Company Size and Expertise Level") +
  xlab("Company Size") +
  ylab("Salary")

# Scatter plot between Experience Level and Salary
ggplot(Salary_data, aes(x = Experience.Level, y = Salary.in.USD, color = Experience.Level)) +
  geom_point() +
  ggtitle("Scatter Plot between Experience Level and Salary")






###simple linear regression
model_simple <- lm(Salary.in.USD ~ Experience.Level, data = Salary_data)
summary(model_simple)

# Predictions and visualization
Salary_data$predicted_Salary_simple <- predict(model_simple)
ggplot(Salary_data, aes(x = predicted_Salary_simple, y = Salary.in.USD)) +
  geom_point() +
  ggtitle("Simple Linear Regression: Actual vs Predicted Salary")








model_multiple <- lm(Salary.in.USD ~ Experience.Level + Company.Size, data = Salary_data)
summary(model_multiple)

# Predictions and visualization
Salary_data$predicted_Salary_multiple <- predict(model_multiple)
ggplot(Salary_data, aes(x = predicted_Salary_multiple, y = Salary.in.USD)) +
  geom_point() +
  ggtitle("Multiple Linear Regression: Actual vs Predicted Salary")




library(glmnet)

# Prepare data
X <- model.matrix(Salary.in.USD ~ . - 1, data = Salary_data)
y <- Salary_data$Salary.in.USD

# Ridge Regression
ridge_model <- glmnet(X, y, alpha = 0)
cv_ridge <- cv.glmnet(X, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_ridge
ridge_model_best <- glmnet(X, y, alpha = 0, lambda = best_lambda_ridge)
coef(ridge_model_best)

# Predictions and visualization
ridge_predictions <- predict(ridge_model_best, newx = X)
Salary_data$predicted_ridge <- ridge_predictions
ggplot(Salary_data, aes(x = predicted_ridge, y = Salary.in.USD)) +
  geom_point() +
  ggtitle("Ridge Regression: Actual vs Predicted Salary")







# Lasso Regression
lasso_model <- glmnet(X, y, alpha = 1)
cv_lasso <- cv.glmnet(X, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_lasso
lasso_model_best <- glmnet(X, y, alpha = 1, lambda = best_lambda_lasso)
coef(lasso_model_best)

# Predictions and visualization
lasso_predictions <- predict(lasso_model_best, newx = X)
Salary_data$predicted_lasso <- lasso_predictions
ggplot(Salary_data, aes(x = predicted_lasso, y = Salary.in.USD)) +
  geom_point() +
  ggtitle("Lasso Regression: Actual vs Predicted Salary")







# Example for Mean Squared Error
mse_simple <- mean((Salary_data$Salary.in.USD - Salary_data$predicted_Salary_simple)^2)
mse_multiple <- mean((Salary_data$Salary.in.USD - Salary_data$predicted_Salary_multiple)^2)
mse_ridge <- mean((Salary_data$Salary.in.USD - Salary_data$predicted_ridge)^2)
mse_lasso <- mean((Salary_data$Salary.in.USD - Salary_data$predicted_lasso)^2)

print(paste("MSE Simple:", mse_simple))
print(paste("MSE Multiple:", mse_multiple))
print(paste("MSE Ridge:", mse_ridge))
print(paste("MSE Lasso:", mse_lasso))


#mae evaluation
mae_simple <- mean(abs(Salary_data$Salary.in.USD - Salary_data$predicted_Salary_simple))
mae_multiple <- mean(abs(Salary_data$Salary.in.USD - Salary_data$predicted_Salary_multiple))
mae_ridge <- mean(abs(Salary_data$Salary.in.USD - Salary_data$predicted_ridge))
mae_lasso <- mean(abs(Salary_data$Salary.in.USD - Salary_data$predicted_lasso))

print(paste("MAE Simple:", mae_simple))
print(paste("MAE Multiple:", mae_multiple))
print(paste("MAE Ridge:", mae_ridge))
print(paste("MAE Lasso:", mae_lasso))





#rmse evaluation

rmse_simple <- sqrt(mean((Salary_data$Salary.in.USD - Salary_data$predicted_Salary_simple)^2))
rmse_multiple <- sqrt(mean((Salary_data$Salary.in.USD - Salary_data$predicted_Salary_multiple)^2))
rmse_ridge <- sqrt(mean((Salary_data$Salary.in.USD - Salary_data$predicted_ridge)^2))
rmse_lasso <- sqrt(mean((Salary_data$Salary.in.USD - Salary_data$predicted_lasso)^2))

print(paste("RMSE Simple:", rmse_simple))
print(paste("RMSE Multiple:", rmse_multiple))
print(paste("RMSE Ridge:", rmse_ridge))
print(paste("RMSE Lasso:", rmse_lasso))


#r2 evaluation

r_squared_simple <- summary(model_simple)$r.squared
r_squared_multiple <- summary(model_multiple)$r.squared
# For Ridge and Lasso, use predictions to calculate R-squared
rss_ridge <- sum((Salary_data$Salary.in.USD - Salary_data$predicted_ridge)^2)
tss <- sum((Salary_data$Salary.in.USD - mean(Salary_data$Salary.in.USD))^2)
r_squared_ridge <- 1 - rss_ridge/tss

rss_lasso <- sum((Salary_data$Salary.in.USD - Salary_data$predicted_lasso)^2)
r_squared_lasso <- 1 - rss_lasso/tss

print(paste("R-squared Simple:", r_squared_simple))
print(paste("R-squared Multiple:", r_squared_multiple))
print(paste("R-squared Ridge:", r_squared_ridge))
print(paste("R-squared Lasso:", r_squared_lasso))







#plottings
# Residual plot for Simple Linear Regression
ggplot(Salary_data, aes(x = predicted_Salary_simple, y = Salary.in.USD - predicted_Salary_simple)) +
  geom_point() +
  ggtitle("Residual Plot for Simple Linear Regression") +
  xlab("Predicted Salary") +
  ylab("Residuals")

# Residual plot for Multiple Linear Regression
ggplot(Salary_data, aes(x = predicted_Salary_multiple, y = Salary.in.USD - predicted_Salary_multiple)) +
  geom_point() +
  ggtitle("Residual Plot for Multiple Linear Regression") +
  xlab("Predicted Salary") +
  ylab("Residuals")

# Residual plot for Ridge Regression
ggplot(Salary_data, aes(x = predicted_ridge, y = Salary.in.USD - predicted_ridge)) +
  geom_point() +
  ggtitle("Residual Plot for Ridge Regression") +
  xlab("Predicted Salary") +
  ylab("Residuals")

# Residual plot for Lasso Regression
ggplot(Salary_data, aes(x = predicted_lasso, y = Salary.in.USD - predicted_lasso)) +
  geom_point() +
  ggtitle("Residual Plot for Lasso Regression") +
  xlab("Predicted Salary") +
  ylab("Residuals")


