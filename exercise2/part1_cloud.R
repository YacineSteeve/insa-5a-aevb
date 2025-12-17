library(ggplot2)

data = read.csv("dataCloud.csv")

ggplot(data, aes(x=x, y=y, color=cluster)) + 
  geom_point(size=2)

points = data[, c("x","y")]

clusters = kmeans(points, length(unique(data$cluster)))

ggplot(points, aes(x=x, y=y, color=clusters$cluster)) + 
  geom_point() +
  geom_point(
    data=as.data.frame(clusters$centers),
    aes(x=x, y=y),
    color="red",
    shape=4,
    size=3
  ) 
