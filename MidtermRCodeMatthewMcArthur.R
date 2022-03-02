### Using Linear regression
### Predict the Price (Price) for used Toyota Corolla cars.
### Use the follwoing three predictors Age_08_04, KM, Fuel_Type
library(forecast)

car.df <- read.csv("ToyotaCorolla.csv")
View(car.df)
# partition data into Training (70%) and Validation (30%) datasets
set.seed(1)
train.rows <- sample(rownames(car.df), dim(car.df)[1]*0.7)
train.data <- car.df[train.rows, ]

valid.rows <- setdiff(rownames(car.df), train.rows) 
valid.data <- car.df[valid.rows, ]

# use lm() to run a linear regression of Price on the 
# three predictors in the training set. 
car.df$Fuel_TypeDummy <- ifelse(car.df$Fuel_Type == 'Diesel',1,0)
View(car.df)
reg <- lm(Price ~ Age_08_04+KM+Fuel_Type, data = train.data)
tr.res <- data.frame(train.data$Price, reg$fitted.values, reg$residuals)
head(tr.res)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)

# Display the results (summary()) of the linear regression
summary(reg)
# use predict() to make predictions on a new set. 
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$Price, pred, residuals = 
                       valid.data$Price - pred)
head(vl.res)
# use accuracy() to compute common accuracy measures.
# recall that accuracy is computed against the validation dataset
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$Price)

