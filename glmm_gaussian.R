# Install and load required packages
install.packages("lme4")
library(lme4)

#upload data
your_data <- read.csv("your_data.csv")

# Fit the GLMM
model <- glmer(y ~ x + (1 | random_effect), data = your_data, family = gaussian(link = "log"))

# Check normality of residuals
shapiro.test(residuals(model))

#Check visually
plot(model)

# Check for overdispersion (when using a Poisson/Negative binomial distribution)
check_overdispersion(model)

# Display model summary
summary(model)

# check Variance inflation factor (VIF) when you have multiple predictors
vif(model)

# Calculate R-squared
rsq(model)

#Plot significant predictors
# Create ggplot for Gaussian GLMM
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red") +
  labs(x = "X", y = "Y") +
  ggtitle("Gaussian GLMM: Observed vs Predicted") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16, face = "bold"))