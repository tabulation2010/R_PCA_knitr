rm(list=ls())

library('cluster')
library('factoextra')
library('dendextend')
library("plotly")
library("ggplot2")
library("circlize")
library("corrplot")
library("gplots")
library("RColorBrewer")
library("heatmaply")
library("jsonlite")


source("./hm.R")
# Load data
rm(list=ls())
json_data <- fromJSON(txt = "./Data/data_norm.json")

input_df <- json_data$data
predictor_list <- json_data$nameList
target <- 'group'
n_cluster <- 5

# Assume data is scaled and normalized. Otherwise run the following command
# input <- scale(input_df[predictor_list], center = TRUE, scale = TRUE)

## Agglomerative Hierarchical Clustering

# methods to assess
m <-
  c("average", "single", "complete", "ward", "weighted", "gaverage")
names(m) <-
  c("average", "single", "complete", "ward", "weighted", "gaverage")

# function to compute agglomerative coefficient
ac_calc <- function(x) {
  agnes(input_df[predictor_list], method = x)$ac
}

dend_calc <- function(x) {
  agnes(input_df[predictor_list], method = x) %>%
    as.dendrogram %>%
    set("branches_k_color", k = n_cluster) %>%
    set("branches_lwd", c(1.2, 0.8, 0.5)) %>%
    set("branches_lty", c(1, 1, 3, 1, 1, 1)) %>%
    set("labels_colors") %>%
    set("labels_cex", c(.6, 1.5)) %>%
    set("nodes_pch", 19) %>%
    set("nodes_col", c("orange", "black", "plum", NA))
}

ac_list <- sapply(m, ac_calc)

opt_method <- names(which.max(ac_list))

hc <- agnes(input_df[predictor_list], method = opt_method)

dend <- hc %>%
  as.dendrogram %>%
  set("branches_k_color", k = n_cluster) %>%
  set("branches_lwd", c(1.2, 0.8, 0.5)) %>%
  set("branches_lty", c(1, 1, 3, 1, 1, 1)) %>%
  set("labels_colors") %>%
  set("labels_cex", c(.6, 1.5)) %>%
  set("nodes_pch", 19) %>%
  set("nodes_col", c("orange", "black", "plum", NA))

# Plot the dendrogram of the optimal hc method
ggd1 <- as.ggdend(dend)
p <- ggplot(ggd1) +
  labs(title = paste0(
    "Dendrogram of Agglomerative Hierarchical Clustering with ",
    opt_method
  )) +
  theme(plot.title = element_text(hjust = 0.5))
# p <- ggplotly(p)
# 
# for (i in 1:length(p$x$data)) {
#   p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "")
#   p$x$data[[i]]$showlegend <- FALSE
# }
p

# Create a radial plot and remove labels
circlize_dendrogram(dend)


# Cut tree into 3 groups
sub_grp <- cutree(hc, k = n_cluster)

# Number of members in each cluster
table(sub_grp)

# p <-
#   fviz_cluster(list(data = input_df[predictor_list], cluster = sub_grp), ellipse.type = "norm") +
#   labs(title = paste0(
#     "Cluster plot of Agglomerative Hierarchical Clustering with ",
#     opt_method
#   )) +
#   theme(plot.title = element_text(hjust = 0.5))
# p

## Divisive Hierarchical Clustering
# compute divisive hierarchical clustering
hc_div <- diana(input_df[predictor_list])

# Divise coefficient; amount of clustering structure found
hc_div$dc

# plot dendrogram
dend_diana <- hc_div %>%
  as.dendrogram %>%
  set("branches_k_color", k = n_cluster) %>%
  set("branches_lwd", c(1.2, 0.8, 0.5)) %>%
  set("branches_lty", c(1, 1, 3, 1, 1, 1)) %>%
  set("labels_colors") %>%
  set("labels_cex", c(.6, 1.5)) %>%
  set("nodes_pch", 19) %>%
  set("nodes_col", c("orange", "black", "plum", NA))

# Plot the dendrogram of the optimal hc method
ggd1 <- as.ggdend(dend_diana)
p <- ggplot(ggd1) +
  labs(title = "Dendrogram of Divisive Hierarchical Clustering") +
  theme(plot.title = element_text(hjust = 0.5))
# p <- ggplotly(p)
# 
# for (i in 1:length(p$x$data)) {
#   p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "")
#   p$x$data[[i]]$showlegend <- FALSE
# }
p


## Compare 2 dendrograms
hc_1 <- agnes(input_df[predictor_list], method = 'gaverage')

dend_list <- dendlist(dend, dend_diana)

tanglegram(
  dend,
  dend_diana,
  highlight_distinct_edges = FALSE,
  # Turn-off dashed lines
  common_subtrees_color_lines = FALSE,
  # Turn-off line colors
  main = paste(
    "entanglement ",
    opt_method,
    " v.s. divisive",
    " =",
    round(entanglement(dend_list), 2)
  )
)

full_dend_list <- as.dendlist(lapply(m, dend_calc))
names(full_dend_list) <- m
corrplot(cor.dendlist(full_dend_list), "pie", "lower")

## Heatmap 
# Method 1 with heatmap
Rowv  <- input_df[predictor_list] %>% dist %>% hclust(method="complete") %>% as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", 1) %>%
  ladderize
Colv  <- input_df[predictor_list] %>% t %>% dist %>% hclust(method="complete") %>% as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", 1) %>%
  ladderize

p <- heatmaply(input_df[predictor_list], Rowv = Rowv, Colv = Colv)
p
htmlwidgets::saveWidget(as.widget(p), "HCA2.html", selfcontained = FALSE)

heatmap.2(as.matrix(input_df[predictor_list]), Rowv = Rowv, Colv = Colv)

# Method 2 with updated heatmap
cols <- colorRampPalette(brewer.pal(10, "RdBu"))(256)

distCor <- function(x) as.dist(1-cor(t(x)))
hclustAvg <- function(x) hclust(x, method="average")
source("./hm.R")

heatmap.2.upd(as.matrix(input_df[predictor_list]), trace="none", scale="row", zlim=c(-3,3), reorder=FALSE,
          distfun=distCor, hclustfun=hclustAvg, col=rev(cols), symbreak=FALSE) 

