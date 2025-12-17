install.packages("ggplot2")  
library(ggplot2)
library(dplyr) 
library(patchwork)

data = read.csv("prenoms.csv", sep=";")

a=aggregate(data$Nombre,list(data$Annee), sum)
#x11()
barplot(a$x, names.arg=a$Group, xlab="Year", ylab="Birth Count")

male = aggregate(data$Nombre[data$Sexe=="M"], list(data$Annee[data$Sexe=="M"]), sum)
female = aggregate(data$Nombre[data$Sexe=="F"], list(data$Annee[data$Sexe=="F"]), sum)
values = matrix(c(male$x, female$x), nrow=2, ncol=length(male$Group), byrow=TRUE)
#x11()
barplot(values, names.arg=male$Group, col=c("blue","pink"), beside=TRUE)
legend("topleft", c("Male","Female"), cex=0.7, fill=c("blue","pink"))


c("Mateo","Yacine") %in% data$Prenom    # TRUE TRUE


top_5 = data  %>% group_by(Annee,Sexe) %>% slice_min(Ordre, n=5)
ggplot(top_5, aes(x=Annee, y=Ordre, color = Prenom)) + geom_point() +geom_line() + facet_wrap(~Sexe)


total = aggregate(data$Nombre, list(data$Prenom), sum)
head(total[order(total$x, decreasing=TRUE),],10) 

result = data %>%
  group_by(Annee) %>%
  mutate(NbLettres = nchar(Prenom)) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(result, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()

avg_vowel = data %>% group_by(Annee) %>%
  mutate(NbLettres=nchar(gsub("[^aeiouy]","",Prenom, ignore.case=TRUE))) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(avg_vowel, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()
  
avg_consonant = data %>% group_by(Annee) %>%
  mutate(NbLettres=nchar(gsub("[aeiouy]","",Prenom, ignore.case=TRUE))) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(avg_consonant, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()

nb_compose = data %>% group_by(Annee) %>% 
  mutate(Compose=nchar(gsub("[^-]","",Prenom, ignore.case=TRUE))*Nombre) %>%
  summarize(Nombre=sum(Compose))

births_by_year_T <- data %>% 
  group_by(Annee)

total_T <- aggregate(births_by_year_T$Nombre, list(births_by_year_T$Annee), sum)

p2 <-ggplot(nb_compose, aes(x = Annee, y = Nombre)) + geom_line() + geom_point()





#Paris

data= read.csv("prenomsParis.csv", sep=";")

a=aggregate(data$Nombre,list(data$Annee), sum)
#x11()
barplot(a$x, names.arg=a$Group, xlab="Year", ylab="Birth Count")

male = aggregate(data$Nombre[data$Sexe=="M"], list(data$Annee[data$Sexe=="M"]), sum)
female = aggregate(data$Nombre[data$Sexe=="F"], list(data$Annee[data$Sexe=="F"]), sum)
values = matrix(c(male$x, female$x), nrow=2, ncol=length(male$Group), byrow=TRUE)
#x11()
barplot(values, names.arg=male$Group, col=c("blue","pink"), beside=TRUE)
legend("topleft", c("Male","Female"), cex=0.7, fill=c("blue","pink"))


c("Mateo","Yacine") %in% data$Prenom    # TRUE TRUE


top_5 = data  %>% group_by(Annee,Sexe) %>% slice_max(Nombre, n=5)
ggplot(top_5, aes(x=Annee, y=Nombre, color = Prenom)) + geom_point() +geom_line() + facet_wrap(~Sexe)


total = aggregate(data$Nombre, list(data$Prenom), sum)
head(total[order(total$x, decreasing=TRUE),],10) 

result = data %>%
  group_by(Annee) %>%
  mutate(NbLettres = nchar(Prenom)) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(result, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()

avg_vowel = data %>% group_by(Annee) %>%
  mutate(NbLettres=nchar(gsub("[^aeiouy]","",Prenom, ignore.case=TRUE))) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(avg_vowel, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()

avg_consonant = data %>% group_by(Annee) %>%
  mutate(NbLettres=nchar(gsub("[aeiouy]","",Prenom, ignore.case=TRUE))) %>%
  summarize(Moyenne=mean(NbLettres))
ggplot(avg_consonant, aes(x = Annee, y = Moyenne)) + geom_line() + geom_point()

nb_compose = data %>% group_by(Annee) %>% 
  mutate(Compose=nchar(gsub("[^-]","",Prenom, ignore.case=TRUE))*Nombre) %>%
  summarize(Nombre=sum(Compose))

births_by_year_P <- data %>% 
  group_by(Annee)

total_P <- aggregate(births_by_year_P$Nombre, list(births_by_year_P$Annee), sum)

ggplot(births_by_year_P, aes(x = Annee, y = Total)) + geom_line() + geom_point()

p1 <- ggplot(nb_compose, aes(x = Annee, y = Nombre)) + geom_line() + geom_point()

p1 + p2

