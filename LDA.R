# LDA analysis on sample JSON data

## Initialization
rm(list=ls())
library("jsonlite")
library("MASS")
library("caret")  # Optional if too big
library("plotly")
library("DiscriMiner")
library("candisc")

options(warn = -1)
# Load data
json_data <- fromJSON(txt = "./Data/data_norm.json")

input_df <- json_data$data
predictor_list <- json_data$nameList
target <- "group"
input_df[[target]] <- as.factor(input_df[[target]])

# Optional (Normalize and scale data)
# input <- scale(input_df[predictor_list], center = TRUE, scale = TRUE)

f <- as.formula(paste(target, "~ ."))

lda_cv <- lda(f, data = input_df[, c(predictor_list, target)], CV = TRUE)
print(lda_cv)

#confusion matrix calculation
confmat_cv <- confusionMatrix(lda_cv$class, input_df[[target]])
print(confmat_cv)

# confusion matrix
## Count
confmat_cv$table
## Percentage
prop.table(confmat_cv$table, margin = 1)


## Or simply
table(lda_cv$class, input_df[[target]])
prop.table(table(lda_cv$class, input_df[[target]]), margin = 1)
## Modeling with LDA
lda_model <- lda(f, data = input_df[, c(predictor_list, target)])
print(lda_model)

print('Coefficients of linear discriminants')
print(lda_model$scaling)
print(lda_model$means)
plot(lda_model, dimen=3)
pairs(lda_model, dimen = 2)
# for 1st discriminant function
plot(lda_model, dimen=1, type="both") # fit from lda

## Pred plot
output = as.data.frame(predict(lda_model, input_df[predictor_list])$x)
output[[target]] = input_df[[target]]

centroids <- lda_model$means %*% lda_model$scaling
p <- ggplot() +
  geom_point(output, 
             mapping = aes_string(x = 'LD1', y='LD2', 
                                  group = target, 
                                  color = target,
                                  shape = target),size = 3,
             alpha = .5
             ) + 
  geom_point(data = as.data.frame(centroids),
             mapping = aes_string(x = 'LD1', y = 'LD2'), color="blue",size=5,alpha=.3)
  labs(title = "Typical Differentiation Function",
       x = "LD1",
       y = "LD2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(p)

lda_model <- linDA(input_df[predictor_list], input_df[[target]])
summary(lda_model)
print(lda_model$functions)
