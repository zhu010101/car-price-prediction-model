# Used Car Price Prediction
## Introduction
The primary objective of this project is to develop a predictive model that can accurately estimate the sale price of used cars. The model is trained using various features and conditions of the cars provided in the dataset.

## Author 
Siyuan Zhu

## Dataset
The dataset used in this project is provided by the Kaggle competition [How much is your car worth?](https://www.kaggle.com/competitions/usedcars2023/leaderboard). It consists of a wide range of features including car specifications, mileage, condition, and more.

## Data Processing and Analysis
The data processing includes handling missing values, transforming categorical variables into factors, and feature selection. Key steps are:
- Handling Missing Values:
  1. Numerical Variables: Median imputation for minimal missing values and linear regression imputation for significant gaps.
  2. Categorical Variables: Mode imputation for minimal missing values and decision tree imputation for substantial missing values.
- Dropping Non-Essential Variables: Variables like id, description, and wheel_system_display are removed.
- Log-Transformation: Applied to skewed data (e.g., price).
## Model Building
Various models are trained and evaluated including:
- Linear Regression
- Lasso Regression
- Regression Tree
- XGBoost
- Random Forest (with ranger)
- SVM
## Best Model 
The best performing model in this project was XGBoost with the lowest RMSE of 1,293.21313, ranked 14th out of 549 entries for the accuracy on this Kaggle competition. 
