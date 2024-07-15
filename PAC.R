knitr::opts_chunk$set(fig.align='center', out.width='50%')

if (!require("ggplot2"))
{
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("tidyr"))
{
  install.packages("tidyr")
  library(tidyr)
}
if (!require("dplyr"))
{
  install.packages("dplyr")
  library(dplyr)
}
if (!require("ggcorrplot"))
{
  install.packages("ggcorrplot")
  library(ggcorrplot)
}
if (!require("corrplot"))
{
  install.packages("corrplot")
  library(corrplot)
}
if (!require("caret"))
{
  install.packages("caret")
  library(caret)
}
if (!require("glmnet"))
{
  install.packages("glmnet")
  library(glmnet)
}
if (!require("randomForest"))
{
  install.packages("randomForest")
  library(randomForest)
}
if (!require("xgboost"))
{
  install.packages("xgboost")
  library(xgboost)
}
if (!require("rpart"))
{
  install.packages("rpart")
  library(rpart)
}

train = read.csv('analysisData.csv')
test = read.csv('scoringData.csv')

test$price <- 0

train$dataset_type <- 'train'
test$dataset_type <- 'test'
idtest <- test[,1]
combine = rbind(train, test)

combine = combine[, -1]
combine <- subset(combine, select = -description)
combine <- subset(combine, select = -major_options)
combine <- subset(combine, select = -wheel_system_display)
combine <- subset(combine, select = -transmission_display)

colSums(is.na(combine) | combine == "")
nrow(combine)

boxplot(combine$price ~ combine$transmission, data = combine, main = "transmission vs price")
combine$transmission <- ifelse(combine$transmission == 'A', 'A', 'Other')
boxplot(combine$price ~ combine$transmission, data = combine, main = "transmission vs price")

combine$fuel_tank_volume_gallons[is.na(combine$fuel_tank_volume_gallons)] <- median(combine$fuel_tank_volume_gallons, na.rm = TRUE)

combine$owner_count[is.na(combine$owner_count)] = 0

non_missing <- combine[!is.na(combine$engine_displacement), ]
missing <- combine[is.na(combine$engine_displacement), ]
model <- lm(engine_displacement ~ engine_type + fuel_tank_volume_gallons + wheelbase_inches + length_inches, data = non_missing)
predicted_values <- predict(model, newdata = missing)
combine$engine_displacement[is.na(combine$engine_displacement)] <- predicted_values

non_missing <- combine[!is.na(combine$highway_fuel_economy), ]
missing <- combine[is.na(combine$highway_fuel_economy), ]
model <- lm(highway_fuel_economy ~ fuel_tank_volume_gallons + height_inches + engine_displacement, data = non_missing)
predicted_values <- predict(model, newdata = missing)
combine$highway_fuel_economy[is.na(combine$highway_fuel_economy)] <- predicted_values

non_missing <- combine[!is.na(combine$mileage), ]
missing <- combine[is.na(combine$mileage), ]
model <- lm(mileage ~ fuel_tank_volume_gallons + year + owner_count + height_inches + highway_fuel_economy + height_inches,data = non_missing)
predicted_values <- predict(model, newdata = missing)
combine$mileage[is.na(combine$mileage)] <- predicted_values

combine$seller_rating[is.na(combine$seller_rating)] <- median(combine$seller_rating, na.rm = TRUE)

non_missing <- combine[!is.na(combine$horsepower), ]
missing <- combine[is.na(combine$horsepower), ]
model <- lm(horsepower ~ fuel_tank_volume_gallons + highway_fuel_economy + wheelbase_inches + engine_displacement + power + torque + length_inches, data = non_missing)
predicted_values <- predict(model, newdata = missing)
combine$horsepower[is.na(combine$horsepower)] <- predicted_values

non_missing <- combine[!is.na(combine$transmission) & combine$transmission != "", ]
missing <- combine[is.na(combine$transmission)| combine$transmission == "", ]
model <- rpart(transmission ~ highway_fuel_economy + width_inches + height_inches + horsepower, data=non_missing, method="class")
predicted_values <- predict(model, newdata = missing, type = "class")
combine$transmission[is.na(combine$transmission) | combine$transmission == ""] <- predicted_values

combine$exterior_color[combine$exterior_color  == "" | is.na(combine$exterior_color)] <- "Black"
combine$trim_name[combine$trim_name == "" | is.na(combine$trim_name)] = "SE FWD"

non_missing <- combine[!is.na(combine$wheel_system) & combine$wheel_system != "",  ]
missing <- combine[is.na(combine$wheel_system)| combine$wheel_system == "", ]
model <- rpart(wheel_system ~ fuel_tank_volume_gallons  + wheelbase_inches   , data=non_missing, method="class", control=rpart.control(maxdepth = 20))
predicted_values <- predict(model, newdata = missing, type = "class")
combine$wheel_system[is.na(combine$wheel_system)| combine$wheel_system == ""] <- predicted_values

combine$fuel_type[is.na(combine$fuel_type) | combine$fuel_type == "" ] <- "Gasoline"
combine$is_cpo[combine$is_cpo == ""] <- FALSE

non_missing <- combine[!is.na(combine$engine_type) & combine$engine_type != "",  ]
missing <- combine[is.na(combine$engine_type) | combine$engine_type == "", ]
model <- rpart(engine_type ~ fuel_tank_volume_gallons  + highway_fuel_economy + wheelbase_inches  + length_inches + height_inches  + engine_displacement, data=non_missing, method="class",control=rpart.control(maxdepth = 10))
predicted_values <- predict(model, newdata = missing, type = "class")
combine$engine_type[is.na(combine$engine_type) | combine$engine_type == ""] <- predicted_values

non_missing <- combine[!is.na(combine$power) & combine$power != "", ]
missing <- combine[is.na(combine$power) | combine$power == "", ]
model <- rpart(power ~ fuel_tank_volume_gallons + highway_fuel_economy + horsepower + engine_displacement, 
               data = non_missing, 
               method = "class",
               control = rpart.control(maxdepth = 5))
predicted_values <- predict(model, newdata = missing, type = "class")
combine$power[is.na(combine$power) | combine$power == ""] <- predicted_values

non_missing <- combine[!is.na(combine$torque) & combine$torque != "", ]
missing <- combine[is.na(combine$torque) | combine$torque == "", ]
model <- rpart(torque ~ fuel_tank_volume_gallons + highway_fuel_economy + horsepower + engine_displacement, 
               data = non_missing, 
               method = "class",
               control = rpart.control(maxdepth = 5))
predicted_values <- predict(model, newdata = missing, type = "class")
combine$torque[is.na(combine$torque) | combine$torque == ""] <- predicted_values

combine$franchise_make[is.na(combine$franchise_make) | combine$franchise_make == "" ] <- "Chevrolet"
combine <- subset(combine, select = -has_accidents)
combine <- subset(combine, select = -salvage)
combine <- subset(combine, select = -isCab)
combine <- subset(combine, select = -frame_damaged)
combine <- subset(combine, select = -fleet)

Charcol <- names(combine[,sapply(combine, is.character)])
Charcol
combine$is_cpo <- ifelse(combine$is_cpo == TRUE | combine$is_cpo == "True", 1, 0)
combine$is_new <- ifelse(combine$is_new == TRUE| combine$is_new == "True", 1, 0)
combine[Charcol[-length(Charcol)]] <- lapply(combine[Charcol[-length(Charcol)]], as.factor)
combine[Charcol[-length(Charcol)]] <- lapply(combine[Charcol[-length(Charcol)]], function(x) as.integer(as.factor(x)))

#Correlation Matrix
train <- subset(combine, dataset_type == 'train')[, !(names(combine) %in% c('dataset_type'))]
numericVars <- which(sapply(combine, is.numeric))
numerical_data <- train[, numericVars]
cor_matrix <- cor(numerical_data, use = "pairwise.complete.obs")
cor_matrix

numerical_data |>
  pivot_longer(1:24,names_to = 'var',values_to = 'values')|>
  group_by(var)|>
  summarize(r = round(cor(train$price, values,use = "complete.obs"),2), p = round(cor.test(train$price, values,use = "complete.obs")$p.value, 4))|>
  arrange(desc(abs(r)))

combine <- subset(combine, select = -city_fuel_economy)
combine <- subset(combine, select = -length_inches)

combine <- subset(combine, select = -listed_date)

train <- subset(combine, dataset_type == 'train')[, !(names(combine) %in% c('dataset_type'))]
test <- subset(combine, dataset_type == 'test')[, !(names(combine) %in% c('dataset_type'))]

qqnorm(train$price)
qqline(train$price)
train$price <- log(train$price)
qqnorm(train$price)
qqline(train$price)

set.seed(3184)
dtrain <- xgb.DMatrix(data = as.matrix(train[, -ncol(train)]), label = train[, ncol(train)])
dtest <- xgb.DMatrix(data = as.matrix(test[, -ncol(test)]))

params <- list(
    objective = "reg:squarederror",
    eta = 0.3,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
)
nrounds <- 2000

cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = nrounds,
  nfold = 5, 
  metrics = "rmse",
  early_stopping_rounds = 50, 
  verbose = TRUE
)

optimal_nrounds <- which.min(cv_results$evaluation_log$test_rmse_mean)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 2000)
xgb_predict_test <- predict(xgb_model, dtest)
xgb_predict_test_transformed <- exp(xgb_predict_test)
submissionFile <- data.frame(id = idtest, price = xgb_predict_test_transformed)
write.csv(submissionFile, 'xgb_sample_submission.csv', row.names = FALSE)

knitr::purl(input = "PAC.Rmd", output = "PAC.R",documentation = 0)
