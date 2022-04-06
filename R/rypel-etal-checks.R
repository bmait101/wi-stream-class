
# Reproduce kmean cluster analysis from Rypel et al. 2019

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library()

# data from online supplement
data <- 
  readxl::read_excel("C:/Users/maitlb/Downloads/fsh10228-sup-0001-dataset1.xls")

# get p/a columns
df <- data %>% select(4:ncol(data))

# check levels
levels(as.factor(data$Muskie))

df <- df %>% 
  mutate(
    across(
      .cols = everything(), 
      ~ case_when(
        .x %in% c("A","C","P") ~ 1, 
        TRUE ~ 0))
  )

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df)


k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", 
     main="Optimal number of clusters")
