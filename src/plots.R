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
#fit.mars <- readRDS("mars-model.rds")

data.prep <- readRDS("data/data-prep_no-SR.rds")
data.test <- readRDS("data/data-test_no-SR.rds")
fit.mars <- readRDS("mars-model_no-SR.rds")

levels(data.test$site)

data.test$site <- fct_other(data.test$site, keep=c('SR','TSU'))


#sites = levels(data$site)
#site_i <- sites[[13]]

#data.prep <- readRDS(paste("data/data-prep-",site_i,".rds",sep=""))
#data.train <- readRDS(paste("data/data-train-",site_i,".rds",sep=""))
#data.test <- readRDS(paste("data/data-test-",site_i,".rds",sep=""))
#fit.mars <- readRDS(paste("mars-model-",site_i,".rds",sep=""))

p <- predict(fit.mars, data.test)
p_post <- ifelse(p < 1, 1, p)
caret::postResample(p_post,data.test$nsb)

pca_index <- createDataPartition(
  y = data.prep$nsb, 
  p = 0.05,
  list = FALSE
)
data.pca <-data.prep[pca_index, ]
pca <- prcomp(data.pca %>% select(-c(nsb,site)), center = TRUE, scale = TRUE)
plot.pca <- ggbiplot(pca,
                     groups = data.pca$nsb,
                     obs.scale = 0.8,
                     var.scale = 1,
                     var.axes = TRUE,
                     point.size = 1) +
  scale_alpha() +
  scale_color_continuous(name = "NSB") +
  theme_minimal()
plot(plot.pca)

ggsave(
  filename = "../doc/draft/pca.pdf",
  plot = plot.pca,
  width = 6,
  height = 4,
  units = "in"
)

df.pred <- data.frame(Actual = data.test$nsb, Predicted = p_post, Site=data.test$site) %>%
  rename(Predicted = y)
plot.pred_vs_actual <- ggplot(df.pred,
                              aes(y = Actual, x = Predicted, color=Site)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Values", y = "Actual Value", x = "Predicted") +
  #labs(title = paste("Predicted vs. Actual Values - ",site_i,sep=""), y = "Actual Value", x = "Predicted") +
  theme_minimal()

ggsave(
  filename = "../doc/draft/predicted-v-actual_highlight-SR-TSU.pdf",
  #filename = paste("../doc/draft/predicted-v-actual-",site_i,".pdf",sep=""),
  plot = plot.pred_vs_actual,
  width = 6,
  height = 4,
  units = "in"
)

residuals <- data.test$nsb - p_post
df.res <- data.frame(Predicted = p_post, Residuals = residuals, Site=data.test$site) %>%
  rename(Predicted = y, Residuals = y.1)
plot.residuals <- ggplot(df.res,
                         aes(x = Predicted, y = Residuals, color=Site)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", y = "Residuals (Actual - Predicted)") +
  #labs(title = paste("Residuals Plot - ",site_i,sep=""), y = "Residuals (Actual - Predicted)") +
  theme_minimal()

ggsave(
  filename = "../doc/draft/residuals_highlight-SR-TSU.pdf",
  #fiame = paste("../doc/draft/residuals-",site_i,".pdf",sep=""),
  plot = plot.residuals,
  width = 6,
  height = 4,
  units = "in"
)

imp <- evimp(fit.mars$finalModel)
View(imp)
df.imp <- data.frame(nsubsets = imp[,"nsubsets"],
                     gcv = imp[,"gcv"],
                     rss = imp[,"rss"])
df.imp %>% kable()

sort(coef(fit.mars$finalModel))
