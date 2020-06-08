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

# Extract results
projected_df <- as.data.frame(input_df.pca$x)
projected_df[target] <- input_df[[target]]

# Visualization of Projected Records
if (dim(input_df.pca$rotation)[2] > 3) {
  # Scale factor for loadings
  scale.loads <- 5
  
  # 3D plot
  p <- plot_ly(data = projected_df,
               x =  ~ PC1,
               y =  ~ PC2,
               z =  ~ PC3) %>%
    add_trace(type = "scatter3d",
              mode = "markers",
              color = projected_df[[target]])
  
  for (k in 1:nrow(input_df.pca$rotation)) {
    x <- c(0, input_df.pca$rotation[k, 1]) * scale.loads
    y <- c(0, input_df.pca$rotation[k, 2]) * scale.loads
    z <- c(0, input_df.pca$rotation[k, 3]) * scale.loads
    p <- p %>% add_trace(
      x = x,
      y = y,
      z = z,
      name = paste0("PC", k),
      type = "scatter3d",
      mode = "lines",
      line = list(width = 8),
      opacity = 1
    )
  }
  p <- p %>% layout(title = "Written Premium and PIF", 
                    autosize = TRUE)
  p
  htmlwidgets::saveWidget(as.widget(p), "PCA1.html", selfcontained = FALSE)
}

# Construct output dataframe
DT::datatable(eig.val)

# Visualiza results
# print(input_df.pca)

# # Univariate variance
# p <- fviz_eig(
#   input_df.pca,
#   xlab = "PC",
#   main = "Variance of each PC",
#   addlabels = TRUE,
#   barfill = "white",
#   barcolor = "darkblue",
#   linecolor = "red"
# ) +
#   labs(title = "Variances - PCA",
#        x = "Principal Components") +
#   theme_minimal()
#   # theme_minimal() +
#   # theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# p
# ggsave("hca1.png", p)


# # Cumulative Expalined Variance
# p <- ggplot(data=eig.val, aes(x=as.numeric(gsub("^.*\\.","",row.names(eig.val))), y=cumulative.variance.percent), group=1) +
#   geom_line(color="red")+
#   geom_point()+
#   geom_text(aes(label=sprintf("%0.2f%%", round(cumulative.variance.percent, digits = 2))),hjust=0.5, vjust=-0.5) +
#   labs(title = "Cumulative % of explained total variance",
#        x = "Principal Component",
#        y = 'Cumulative Variance (%)') +
#   theme_minimal()
# p

# PCA
x <- paste0("PC", 1:length(input_df.pca$sdev))
y <- input_df.pca$sdev^2
p <- data.frame(x, y)
p <- plot_ly(p, x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers')
p
htmlwidgets::saveWidget(as.widget(p), "PCA2.html", selfcontained = FALSE)

# Biplot of first 2 PC
ggbiplot(
  input_df.pca,
  choices = c(1, 2),
  groups = input_df[[target]],
  ellipse = TRUE,
  ellipse.prob = .95
) +
  theme_minimal()
p <- ggplotly()
p
htmlwidgets::saveWidget(as.widget(p), "PCA3.html", selfcontained = FALSE)

# Loading plot
p <- input_df.pca$rotation[,1:2]
p <- plot_ly(data = as.data.frame(p), x = ~PC1,
             y = ~PC2, color = rownames(p),
             marker = list(size = 16), type = 'scatter', mode = 'markers')
p <- p %>% layout(title = 'Loading plot')
p
htmlwidgets::saveWidget(as.widget(p), "PCA4.html", selfcontained = FALSE)

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
p <- plot_ly(projected_df,
             x = as.integer(row.names(projected_df)),
             y = ~PC1,
             name = "",
             type = "bar"
) %>% layout(xaxis = x, yaxis = y)
p
htmlwidgets::saveWidget(as.widget(p), "PCA5.html", selfcontained = FALSE)

# names(input_df.pca$sdev) <- paste0("PC", 1:length(input_df.pca$sdev))
# barplot(input_df.pca$sdev^2)
# axis(1, 1:length(predictor_list))

# Cumulative variance
# plot(
#   eig.val$cumulative.variance.percent,
#   xaxt = "n",
#   type = "l",
#   main = "Cumulative % of explained total variance",
#   xlab = 'PC',
#   ylab = '%'
# )
# axis(1, 1:length(predictor_list))


# Extract results
# projected_df <- as.data.frame(input_df.pca$x)
# projected_df[target] <- input_df[[target]]

# Extract results
# loading_mat <- input_df.pca$rotation
# projected_df <- input_df.pca$x

# # Loading plot
# plot(input_df.pca$rotation[,1:2])
# # Add vertical and horizontal lines at c(0,0)
# abline(h =0, v = 0, lty = 2)




# # Output to CSV
# write.csv(projected_df, file = './PCA_projection.csv', row.names = FALSE)
# 
# # Output to Excel
# write.xlsx(projected_df, file = './PCA_projection.xlsx', sheetName = "Sheet1", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)