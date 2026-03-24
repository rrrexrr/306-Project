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

# Model 1: core variables only
m1 <- lm(
  log(median_house_value) ~ median_income + ocean_proximity,
  data = housing
)

# Model 2: add housing age
m2 <- lm(
  log(median_house_value) ~ median_income + ocean_proximity +
    housing_median_age,
  data = housing
)

# Model 3: add engineered housing variables
m3 <- lm(
  log(median_house_value) ~ median_income + ocean_proximity +
    housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household,
  data = housing
)

# Model 4: add latitude and longitude
m4 <- lm(
  log(median_house_value) ~ median_income + ocean_proximity +
    housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household +
    latitude + longitude,
  data = housing
)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# Course-style Cp function

# Residual mean square error from the full model
sigma2_full <- summary(m4)$sigma^2

# Function to compute Mallows' Cp
mallows_cp <- function(model, sigma2_full, n) {
  rss <- sum(residuals(model)^2)
  p <- length(coef(model))   # includes intercept
  cp <- rss / sigma2_full - (n - 2 * p)
  return(cp)
}

# Sample size
n <- nrow(housing)

# Compute Cp for each model
cp_m1 <- mallows_cp(m1, sigma2_full, n)
cp_m2 <- mallows_cp(m2, sigma2_full, n)
cp_m3 <- mallows_cp(m3, sigma2_full, n)
cp_m4 <- mallows_cp(m4, sigma2_full, n)

# Put results in a table
cp_results <- data.frame(
  Model = c("m1", "m2", "m3", "m4"),
  Parameters_p = c(length(coef(m1)), length(coef(m2)),
                   length(coef(m3)), length(coef(m4))),
  Mallows_Cp = c(cp_m1, cp_m2, cp_m3, cp_m4)
)

cp_results

# VIF

reg_income <- lm(
  median_income ~ housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household +
    latitude + longitude,
  data = housing
)
VIF_income <- 1 / (1 - summary(reg_income)$r.squared)

reg_age <- lm(
  housing_median_age ~ median_income +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household +
    latitude + longitude,
  data = housing
)
VIF_age <- 1 / (1 - summary(reg_age)$r.squared)

reg_rooms <- lm(
  log_rooms_per_household ~ median_income +
    housing_median_age +
    bedrooms_per_room +
    log_population_per_household +
    latitude + longitude,
  data = housing
)
VIF_rooms <- 1 / (1 - summary(reg_rooms)$r.squared)

reg_bed <- lm(
  bedrooms_per_room ~ median_income +
    housing_median_age +
    log_rooms_per_household +
    log_population_per_household +
    latitude + longitude,
  data = housing
)
VIF_bed <- 1 / (1 - summary(reg_bed)$r.squared)

reg_pop <- lm(
  log_population_per_household ~ median_income +
    housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    latitude + longitude,
  data = housing
)
VIF_pop <- 1 / (1 - summary(reg_pop)$r.squared)

reg_lat <- lm(
  latitude ~ median_income +
    housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household +
    longitude,
  data = housing
)
VIF_lat <- 1 / (1 - summary(reg_lat)$r.squared)

reg_long <- lm(
  longitude ~ median_income +
    housing_median_age +
    log_rooms_per_household +
    bedrooms_per_room +
    log_population_per_household +
    latitude,
  data = housing
)
VIF_long <- 1 / (1 - summary(reg_long)$r.squared)

vif_table <- data.frame(
  Variable = c("median_income", "housing_median_age",
               "log_rooms_per_household", "bedrooms_per_room",
               "log_population_per_household", "latitude", "longitude"),
  VIF = c(VIF_income, VIF_age, VIF_rooms, VIF_bed, VIF_pop, VIF_lat, VIF_long)
)

vif_table
summary(m4)

# Standardized residuals
std_res <- rstandard(m4)
outlier_idx <- which(abs(std_res) > 3)
outlier_idx
length(outlier_idx)

# Cook's distance
cook <- cooks.distance(m4)
infl_idx <- which(cook > 1)
infl_idx
length(infl_idx)
