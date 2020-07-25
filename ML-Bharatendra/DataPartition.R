
# Data

vechile <- read_csv("vehicle.csv")

head(vehicle)


# Split dataset into "training" (80%) and "validation" (20%)

ind <- sample(2, nrow(vehicle), replace = TRUE, prob = c(0.8,0.2))

tdata <- vehicle[ind == 1,]

vdata <- vehicle[ind == 2,]

head(tdata)

head(vdata)



# Multiple linear regression model

result <- lm(lc~Mileage+lh, tdata)

summary(result)

result$coefficients

coef(results)


# Prediction

vdata

pred <- predict(result, vdata)

head(pred)

head(vdata)
