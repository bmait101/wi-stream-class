
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # panel plots

## Data

df <- readRDS(here("data", "df_fish_pa"))
df <- df %>% column_to_rownames("reach_id")

dfs <- readRDS(here("data", "df_fish_pa_sport"))
dfs <- dfs %>% column_to_rownames("reach_id")


## K-means cluster analysis: all species ---------------------------------------

# 2-5 clusters
k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


## K-means cluster analysis: sportfish -----------------------------------------

# 2-5 clusters
k2s <- kmeans(dfs, centers = 2, nstart = 25)
k3s <- kmeans(dfs, centers = 3, nstart = 25)
k4s <- kmeans(dfs, centers = 4, nstart = 25)
k5s <- kmeans(dfs, centers = 5, nstart = 25)

# plots to compare
p1s <- fviz_cluster(k2s, geom = "point", data = dfs) + ggtitle("k = 2")
p2s <- fviz_cluster(k3s, geom = "point",  data = dfs) + ggtitle("k = 3")
p3s <- fviz_cluster(k4s, geom = "point",  data = dfs) + ggtitle("k = 4")
p4s <- fviz_cluster(k5s, geom = "point",  data = dfs) + ggtitle("k = 5")

grid.arrange(p1s, p2s, p3s, p4s, nrow = 2)

