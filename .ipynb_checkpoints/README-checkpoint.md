# 306-Project
Statistical Inference for a Linear Regression Model of California Housing Prices

The dataset has 20,640 rows and 10 variables
only total_bedrooms has missing values
that missingness is 207 rows, about 1%
ocean_proximity is very unbalanced, with ISLAND = 5 rows only
median_house_value is capped at 500001, and that happens 965 times = 4.68%
INLAND clearly has much lower house values than the coastal categories

<head>simple interpretation</head>

We began by examining the structure and quality of the data. The dataset contains 20,640 census block groups and 10 variables. Most variables are complete, with missing values appearing only in total_bedrooms. Specifically, 207 observations, or about 1% of the sample, are missing this variable. This suggests that missingness is limited, though it will still need to be addressed before fitting regression models.

We also converted ocean_proximity to a categorical variable and examined its distribution. The largest categories are <1H OCEAN and INLAND, while ISLAND contains only 5 observations, so results for that category should be interpreted cautiously.

Preliminary summaries of median_house_value show a mean of 206,856 and a median of 179,700, suggesting a right-skewed distribution. In addition, 965 observations (4.68%) take the maximum recorded value of 500,001, indicating that the response variable is top-coded. This may affect interpretation at the upper end of the market and should be noted as a limitation.

Grouped summaries by ocean_proximity suggest substantial differences in house value across location categories, with inland districts having much lower house values than coastal districts. This provides initial evidence that proximity to the ocean may be associated with housing prices.