install.packages("ggplot2")
library(ggplot2)

data = read.csv("prenoms.csv", sep=";")

a=aggregate(data$Nombre,list(data$Annee), sum)
x11()
barplot(a$x, names.arg=a$Group, xlab="Year", ylab="Birth Count")

male = aggregate(data$Nombre[data$Sexe=="M"], list(data$Annee[data$Sexe=="M"]), sum)
female = aggregate(data$Nombre[data$Sexe=="F"], list(data$Annee[data$Sexe=="F"]), sum)
values = matrix(c(male$x, female$x), nrow=2, ncol=length(male$Group), byrow=TRUE)
x11()
barplot(values, names.arg=male$Group, col=c("blue","pink"), beside=TRUE)
legend("topleft", c("Male","Female"), cex=0.7, fill=c("blue","pink"))


c("Mateo","Yacine") %in% data$Prenom    # TRUE TRUE


top_5 = data  %>% group_by(Annee,Sexe) %>% slice_min(Ordre, n=5)
ggplot(top_5, aes(x=Annee, y=Ordre, color = Prenom)) + geom_point() +geom_line() + facet_wrap(~Sexe)


total = aggregate(data$Nombre, list(data$Prenom), sum)
head(total[order(total$x, decreasing=TRUE),],10) 


