library(foreign)
# Download and load the dataset
# Library Foregin
# ref.https://stat.ethz.ch/R-manual/R-devel/library/foreign/html/read.arff.html
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00277/ThoraricSurgery.arff"
download.file(url, "ThoracicSurgery.arff")
thoracic_df <- read.arff("ThoracicSurgery.arff")
head(df, n=5)

# Rename the columns
colnames(thoracic_df) <- c('DGN', 'PRE4', 'PRE5', 'PRE6', 'PRE7', 'PRE8', 'PRE9', 'PRE10',
                  'PRE11', 'PRE14', 'PRE17', 'PRE19', 'PRE25', 'PRE30', 'PRE32', 'AGE', 'Risk1Y')

# Convert non-numeric columns to factors
thoracic_df$DGN <- as.factor(thoracic_df$DGN)
thoracic_df$Risk1Y <- as.factor(thoracic_df$Risk1Y)
thoracic_df$Risk1Y

# Fit a logistic regression model using 'glm
model <- glm(Risk1Y ~ ., data = thoracic_df, family = binomial(link = "logit"))
summary(model)


# Compute accuracy
# Predict the outcome variable for the dataset
y_pred <- predict(model, type = "response")

# new vector, predicted probability in to binary values, with threshold of 0.5
y_pred <- ifelse(y_pred >= 0.5, 1, 0)
dy_pre
# Compute accuracy is caluclated by comparing predicted vs actual values
accuracy <- mean(y_pred == thoracic_df$Risk1Y) * 100
accuracy


binary_df <- read.csv('data/binary-classifier-data.csv')

# Fit the logistic regression model
model <- glm(label ~ x + y, data = binary_df, family = binomial)

# Print the summary of the model
summary(model)

binary_df$label <- as.factor(binary_df$label)


ref. 
Call: It shows the function call used to fit the model, including the formula, family, and data.

Deviance Residuals: These are the deviance residuals, which are a measure of the goodness of fit of the model. They represent the differences between the observed response values and the predicted probabilities from the model. The values range from the minimum (Min) to the maximum (Max).

Coefficients: This table displays the estimated coefficients for the predictor variables (x and y) in the logistic regression model. The Estimate column shows the estimated coefficients, the Std. Error column provides the standard errors of the estimates, the z value column gives the z-scores, and the Pr(>|z|) column shows the corresponding p-values. Significance codes (***, **, *, .) indicate the level of significance of the coefficients.

Dispersion parameter: It specifies the dispersion parameter assumed for the binomial family. In this case, it is set to 1.

Null deviance: It represents the deviance of a model with only an intercept term. It indicates the degree of lack of fit of the null model to the data.

Residual deviance: It represents the deviance of the fitted model. It measures the degree of lack of fit of the model to the data after accounting for the predictor variables.

AIC: The Akaike Information Criterion (AIC) is a measure of the models goodness of fit, considering the complexity of the model. Lower AIC values indicate better-fitting models.

Number of Fisher Scoring iterations: It shows the number of iterations performed by the fitting algorithm to estimate the coefficients.



predictions <- ifelse(predict(model, type = 'response') >= 0.5, 1, 0)

# Calculate the accuracy
accuracy <- sum(predictions == binary_df$label) / nrow(data)
accuracy