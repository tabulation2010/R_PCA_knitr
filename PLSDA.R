# PLSDA analysis on sample JSON data

## Initialization
rm(list = ls())
library("jsonlite")
library("mixOmics")
library("plotly")

# Load data
json_data <- fromJSON(txt = "./Data/1589353276563.json")

input_df <- json_data$data
predictor_list <- json_data$nameList
target <- "group"
input_df[[target]] <- as.factor(input_df[[target]])

# Optional (Normalize and scale data)
# input_df[predictor_list] <- scale(input_df[predictor_list], center = TRUE, scale = TRUE)

plsda.model <-
  splsda(input_df[predictor_list], input_df[[target]], ncomp = 3)

# auroc(plsda.model)


# plotLoadings(plsda.model, contrib = 'max', method = 'mean', comp = 2)

plotIndiv(
  plsda.model,
  blocks = c(1, 2),
  # ind.names = TRUE,
  legend = TRUE,
  ellipse = TRUE,
  # star = TRUE,
  X.label = 'PLS-DA 1',
  Y.label = 'PLS-DA 2',
  title = 'PLSDA Sample Plot (t1 v.s. t2)'
)

plotVar(plsda.model)
selectVar(plsda.model, comp = 3)$name


plotIndiv(
  plsda.model,
  blocks = c(2, 3),
  group = input_df[[target]],
  ellipse = TRUE,
  legend = TRUE,
  title = 'PLSDA Sample Plot (t2 v.s. t3)'
)

plotIndiv(
  plsda.model,
  blocks = c(1, 3),
  group = input_df[[target]],
  ellipse = TRUE,
  legend = TRUE,
  title = 'PLSDA Sample Plot (t1 v.s. t3)'
)

output <-
  predict(plsda.model, input_df[predictor_list])$class$max.dist[, 2]


plsda_vali_loo <- perf(plsda.model, validation = "loo")
plsda_vali_loo$class$max.dist[, , 2]

plot(plsda_vali_loo)

pred <-
  cbind(Actual = as.character(input_df[[target]]),
        Prediction = plsda_vali_loo$class$max.dist[, , 3])
print(pred)
cat("LOO CV Accurary:\n")
print(paste0(round(mean(pred[, 'Actual'] == pred[, 'Prediction']) * 100, 2), "%"))

if (dim(plsda.model$variates$X)[2] >= 3) {
  # Scale factor for loadings
  scale.loads <- 5
  
  validates = as.data.frame(plsda.model$variates$X)
  # 3D plot
  p <- plot_ly(
    data = validates,
    x =  ~ comp1,
    y =  ~ comp2,
    z =  ~ comp3
  ) %>%
    add_trace(type = "scatter3d",
              opacity = 0.6,
              mode = "markers",
              color = input_df[[target]],
              marker = list(
                line = list(
                  width = 1
                )
              )) %>%
    layout(title = "PLSDA projection")
  p
}

# Score plot
f <- list(family = "Courier New, monospace",
          size = 18,
          color = "#7f7f7f")
x <- list(title = "ID",
          titlefont = f)
y <- list(title = "Score",
          titlefont = f)
p <- plot_ly(
  data = validates,
  x = as.integer(row.names(validates)),
  y = ~ comp1,
  name = "",
  type = "bar"
) %>% layout(xaxis = x, yaxis = y)
p

loading <- as.data.frame(rbind(plsda.model$loadings$X, plsda.model$loadings$Y))
loading['sep'] <- 'Y'
loading$sep[1:nrow(plsda.model$loadings$X)] <- "X"

# Loading Plot
p <- ggplot(loading, 
            aes_string(x = 'comp1', y='comp2', 
                       group = 'sep')) +
  geom_point(size = 3,
             alpha = .5, 
             aes_string(color = 'sep',
                        shape = 'sep')) +
  labs(title = "Loading Plot",
       x = "comp1",
       y = "comp2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(p)


plsda.model$loadings


# VIP
library("ropls")

json.plsda <- opls(input_df[predictor_list], input_df[[target]], predI=3, fig.pdfC=F)

VIP <- getVipVn(json.plsda)
barplot(sort(VIP, decreasing = T))
print(sort(VIP, decreasing = T))
