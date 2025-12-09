library(caret)
library(tidyverse)
library(earth)
library(ggbiplot)
library(ggplot2)
library(knitr)
options(digits = 4,
        scipen = 10)

data <- readRDS("data/combined-dataset.rds")
data.prep <- readRDS("data/data-prep.rds")
data.train <- readRDS("data/data-train.rds")
data.test <- readRDS("data/data-test.rds")

p <- predict(fit.mars, data.test)
caret::postResample(p,data.test$nsb)

pca_index <- createDataPartition(
  y = data.prep$nsb, 
  p = 0.05,
  list = FALSE
)
data.pca <-data.prep[pca_index, ]
pca <- prcomp(data.pca %>% select(-nsb), center = TRUE, scale = TRUE)
ggbiplot(pca,
         groups = data.pca$nsb)

df.pred <- data.frame(Actual = data.test$nsb, Predicted = p) %>%
  rename(Predicted = y)
ggplot(df.pred,
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values", x = "Actual Outcome", y = "Model Prediction") +
  theme_minimal()

residuals <- data.test$nsb - p
df.res <- data.frame(Predicted = p, Residuals = residuals) %>%
  rename(Predicted = y, Residuals = y.1)
ggplot(df.res,
       aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", y = "Residuals (Actual - Predicted)") +
  theme_minimal()

imp <- evimp(fit.mars$finalModel)
View(imp)
df.imp <- data.frame(nsubsets = imp[,"nsubsets"],
                     gcv = imp[,"gcv"],
                     rss = imp[,"rss"])
df.imp %>% kable()
