library(factoextra)
library(ggpubr)
library(FactoMineR)

data <- as.data.frame(read.table("coded.txt", header = T))
data$continent = as.factor(data$continent)
colors = rainbow(length(unique(data$continent)))
names(colors) = unique(data$continent)
pca = prcomp(data[, -1:-3])
summary(pca)
fviz_eig(pca, addlabels = T)
getplot =function(x, y, centers){
  transform = as.data.frame(pca$x[,c(x,y)])
  kmeans_h = kmeans(transform, centers = centers)
  clusterplot <- fviz_cluster(kmeans_h, data = transform, geom = "point")
  coord <- as.data.frame(get_pca_ind(pca)$coord)
  coord$cluster <- factor(kmeans_h$cluster)
  coord$continent <- data$continent
  eigenvalue <- get_eigenvalue(pca)
  variance.percent <- eigenvalue$variance.percent
  scatter <- ggscatter(
    coord, x = paste0("Dim.", x), y = paste0("Dim.", y), 
    color = "continent", shape="cluster", size = 1.5,  
    legend = "right", ggtheme = theme_bw(), palette=colors,
    xlab = paste0("Dim ", x ," (", variance.percent[x], "% )" ),
    ylab = paste0("Dim ", y ," (", variance.percent[y], "% )" )
  )
  return (list(clusterplot, scatter))
}
plots <- getplot(1,5,2)
plots[1]
plots[2]
plots <- getplot(1,2,3)
plots[1]
plots[2]
plots <- getplot(1,3,3)
plots[1]
plots[2]
plots <- getplot(2,3,3)
plots[1]
plots[2]
plots <- getplot(1,4,2)
plots[1]
plots[2]
plots <- getplot(1,6,2)
plots[1]
plots[2]
plots <- getplot(4,6,5)
plots[1]
plots[2]
var<-get_pca_var(pca)
contrib <- var$contrib
#change axes to get the top 10 contribution to each PC.
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
