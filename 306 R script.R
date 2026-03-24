library(car)
## EDA

## 1. Impute total_bedrooms with its median
bedrooms_median <- median(housing$total_bedrooms, na.rm = TRUE)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- bedrooms_median

## 2. Create derived variable
housing$rooms_per_household <- housing$total_rooms / housing$households
housing$bedrooms_per_room <- housing$total_bedrooms / housing$total_rooms
housing$population_per_household <- housing$population / housing$households

## 3. Check distributions of the response and predictors

# Distribution of the response and covariate
hist(housing$median_house_value)

hist(housing$median_income)

hist(housing$rooms_per_household)

summary(housing$rooms_per_household)

quantile(housing$rooms_per_household,
         probs = c(0.90, 0.95, 0.99, 0.999),
         na.rm = TRUE)

sort(housing$rooms_per_household, decreasing = TRUE)[1:10]

hist(housing$rooms_per_household,
     main = "Histogram of rooms_per_household",
     xlab = "rooms_per_household")

housing$log_rooms_per_household <- log(housing$rooms_per_household)

hist(housing$log_rooms_per_household,
     main = "Histogram of log(rooms_per_household)",
     xlab = "log(rooms_per_household)")

hist(housing$bedrooms_per_room)

summary(housing$bedrooms_per_room)

quantile(housing$bedrooms_per_room,
         probs = c(0.90, 0.95, 0.99, 0.999),
         na.rm = TRUE)

hist(housing$population_per_household)

housing$log_population_per_household <- log(housing$population_per_household)

hist(housing$log_population_per_household,
     main = "Histogram of log(population_per_household)",
     xlab = "log(population_per_household)")

hist(housing$housing_median_age,
     main = "Histogram of housing_median_age",
     xlab = "housing_median_age")

boxplot(median_house_value ~ ocean_proximity,
        data = housing,
        main = "Median House Value by Ocean Proximity",
        xlab = "ocean_proximity",
        ylab = "median_house_value")

# Make scatterplots with the response
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
pt_col <- rgb(0.2, 0.4, 0.6, 0.15)

plot(housing$median_income, housing$median_house_value,
     xlab = "median_income", ylab = "median_house_value",
     main = "Median Income", pch = 16, cex = 0.3, col = pt_col)

plot(housing$housing_median_age, housing$median_house_value,
     xlab = "housing_median_age", ylab = "median_house_value",
     main = "Housing Median Age", pch = 16, cex = 0.3, col = pt_col)

plot(housing$log_rooms_per_household, housing$median_house_value,
     xlab = "log_rooms_per_household", ylab = "median_house_value",
     main = "Log Rooms per Household", pch = 16, cex = 0.3, col = pt_col)

plot(housing$bedrooms_per_room, housing$median_house_value,
     xlab = "bedrooms_per_room", ylab = "median_house_value",
     main = "Bedrooms per Room", pch = 16, cex = 0.3, col = pt_col)

plot(housing$log_population_per_household, housing$median_house_value,
     xlab = "log_population_per_household", ylab = "median_house_value",
     main = "Log Population per Household", pch = 16, cex = 0.3, col = pt_col)

par(mfrow = c(1, 1))

## Initial Model Fitting
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

model1 <- lm(median_house_value ~ median_income +
               housing_median_age +
               log_rooms_per_household +
               bedrooms_per_room +
               log_population_per_household +
               latitude +
               longitude +
               ocean_proximity,
             data = housing)

summary(model1)

model2 <- lm(log(median_house_value) ~ median_income +
               housing_median_age +
               log_rooms_per_household +
               bedrooms_per_room +
               log_population_per_household +
               latitude +
               longitude +
               ocean_proximity,
             data = housing)

summary(model2)
vif(model2)

