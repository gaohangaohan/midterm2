library(Rtsne)

data <- as.data.frame(read.table("coded.txt", header = T))
data$continent = as.factor(data$continent)
colors = rainbow(length(unique(data$continent)))
names(colors) = unique(data$continent)
tsne <- Rtsne(data[,-1:-3], dims = 1, perplexity=30, verbose=TRUE, max_iter = 5000)
par(mar = c(5,4,4,15),xpd=T)
plot(tsne$Y, t='n', main="tsne")
points(tsne$Y, col=colors[data$continent], pch = 20)
legend("topright", inset = c(- 1, 0), legend = names(colors), col = colors[names(colors)], pch =20)

