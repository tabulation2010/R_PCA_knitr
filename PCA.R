# PCA analysis on sample JSON data

## Initialization
rm(list=ls())
library("jsonlite")
library("ggbiplot")
library("factoextra")
library("plotly")
library("xlsx")

options(warn=-1)
# Load data
json_data <- fromJSON(txt = "./Data/data_norm.json")

input_df <- json_data$data

predictor_list <- json_data$nameList

# Optional (Normalize and scale data)
# input <- scale(input_df[predictor_list], center = TRUE, scale = TRUE)

target <- 'group'

# apply PCA (Assume no missing values!!!)
input_df.pca <- prcomp(input_df[, predictor_list],
                       center = TRUE,
                       scale. = TRUE)

# Access results
# Extract eigenvalues
eig.val <- get_eigenvalue(input_df.pca)

# Results for Variables (column)
res.var <- get_pca_var(input_df.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation

# Results for each individual Record (row)
res.ind <- get_pca_ind(input_df.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation

# Visualiza results
print(input_df.pca)

# Univariate variance
p <- fviz_eig(
  input_df.pca,
  xlab = "PC",
  main = "Variance of each PC",
  addlabels = TRUE,
  barfill = "white",
  barcolor = "darkblue",
  linecolor = "red"
) +
  labs(title = "Variances - PCA",
       x = "Principal Components") +
  theme_minimal()

print(p)

names(input_df.pca$sdev) <- paste0("PC", 1:length(input_df.pca$sdev))
barplot(input_df.pca$sdev^2)
# Cumulative variance
plot(
  eig.val$cumulative.variance.percent,
  xaxt = "n",
  type = "l",
  main = "Cumulative % of explained total variance",
  xlab = 'PC',
  ylab = '%'
)
axis(1, 1:length(predictor_list))

# Biplot of first 2 PC
ggbiplot(
  input_df.pca,
  choices = c(1, 2),
  groups = input_df[[target]],
  ellipse = TRUE,
  ellipse.prob = .95
)


# Extract results
loading_mat <- input_df.pca$rotation
projected_df <- input_df.pca$x

# Score plot
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "ID",
  titlefont = f
)
y <- list(
  title = "Score",
  titlefont = f
)
p <- plot_ly(data=as.data.frame(projected_df),
  x = as.integer(row.names(projected_df)),
  y = ~PC1,
  name = "",
  type = "bar"
) %>% layout(xaxis = x, yaxis = y)
p

# Loading plot
plot(input_df.pca$rotation[,1:2])
# Add vertical and horizontal lines at c(0,0)
abline(h =0, v = 0, lty = 2)

# Output to CSV
write.csv(projected_df, file = './PCA_projection.csv', row.names = FALSE)

# Output to Excel
write.xlsx(projected_df, file = './PCA_projection.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)