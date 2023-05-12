# Assignment: ASSIGNMENT_08_03(a)
# Name: POTHINENI, KALYAN
# Date: 2023-05-07


library(readxl)
library(writexl)

## Load the `data/Week-6-housing.xlsx` to
housing_df <- read_excel('data/week-6-housing.xlsx', sheet = 'Sheet2')
head(housing_df, n=5)

## Remove any columns that are not related to the analysis
housing_df <- housing_df[,c("Sale Date", "Sale Price", "building_grade", 
                            "square_feet_total_living", "bedrooms", 
                            "bath_full_count", "bath_half_count", 
                            "bath_3qtr_count", "year_built", 
                            "year_renovated", "sq_ft_lot")]

## Checking and removing any duplicate rows in the data set
housing_df <- housing_df[!duplicated(housing_df),]


## Checking and removing any missing values in the data set
housing_df <- na.omit(housing_df)

## To write the cleaned-up data to a new xlsx file, I am using the write.xlsx() function from the openxlsx package. 
## The cleaned-up data to a new file called "cleaned_housing.xlsx" in your working directory
clean_housting <- writexl::write_xlsx(housing_df, "data/week-8/cleaned_housing.xlsx", col_names = TRUE)

clean_housing_df <- read_excel('data/week-8/cleaned_housing.xlsx')
head(clean_housing_df, n=5)



housing_VAR_1 <- clean_housing_df[,c("Sale Price", "sq_ft_lot")]
housing_VAR_1


# Load necessary libraries
library(stats)

# Fit the linear regression model
model <- lm(Sale_Price ~ sq_ft_lot + square_feet_total_living + bedrooms + bath_full_count + year_built, data = clean_housing_df)

# Perform casewise diagnostics
influence_results <- influence(second_model)

# Store cook's distance in a dataframe
cooks_d <- data.frame(index = rownames(influence_results$infmat), cooks_d = influence_results$infmat[, "cook.d"])
cooks_d
# Store DFBETAS in a dataframe
dfbetas <- data.frame(dfb.1_ = influence_results$infmat[, "dfb.1_"],
                      dfb.sq__ = influence_results$infmat[, "dfb.sq__"],
                      dfb.s___ = influence_results$infmat[, "dfb.s___"],
                      dfb.bdrm = influence_results$infmat[, "dfb.bdrm"],
                      dfb.bt__ = influence_results$infmat[, "dfb.bt__"],
                      dfb.yr_b = influence_results$infmat[, "dfb.yr_b"])

# Store DFFITS in a dataframe
dffits <- data.frame(dffit = influence_results$infmat[, "dffit"])

# Store covariance ratios in a dataframe
cov_ratios <- data.frame(cov.r = influence_results$infmat[, "cov.r"])


# Get the influence measures
influential_cases <- influence.measures(second_model)

# Store Cook's distance in a dataframe
cooks_d <- data.frame(index = rownames(influence_results$infmat), cooks_d = influence_results$infmat[, "cook.d"])
cooks_d
# Store standardized residuals in a dataframe
std_resid <- data.frame(index = rownames(influential_cases), std_resid = influential_cases$standardized.residuals)

# Store dfbetas in a dataframe for each predictor variable
dfbetas <- data.frame(index = rownames(influential_cases), dfbetas_sq_ft_lot = influential_cases$dfbetas[,1],
                      dfbetas_square_feet_total_living = influential_cases$dfbetas[,2],
                      dfbetas_bedrooms = influential_cases$dfbetas[,3],
                      dfbetas_bath_full_count = influential_cases$dfbetas[,4],
                      dfbetas_year_built = influential_cases$dfbetas[,5])

dfbetas;


library(car)
# Calculate standardized residuals
std_resid <- studres(second_model)

# Identify large residuals
large_resid <- abs(std_resid) > 2

large_resid;

influences <- influence.measures(second_model)
influences
leverage <- influences$hat
leverage
cooks_d <- influences$cooks.distance
cooks_d
cov_ratio <- influences$covratio
cov_ratio
influential_cases <- data.frame(index = rownames(influences),
                                leverage = leverage,
                                cooks_d = cooks_d,
                                cov_ratio = cov_ratio)
influential_cases