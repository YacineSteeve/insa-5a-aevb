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


births_by_year_T <- data %>% group_by(Annee)

births_by_year_T_a = aggregate(births_by_year_T$Nombre, list(births_by_year_T$Annee), sum)
colnames(births_by_year_T_a)[1] <- "Annee"
colnames(births_by_year_T_a)[2] <- "Total"
births_by_year_T_a

nb_compose = data %>% 
  group_by(Annee) %>% 
  mutate(Compose=nchar(gsub("[^-]","",Prenom, ignore.case=TRUE))*Nombre) %>%
  summarize(Nombre=sum(Compose))
nb_compose

nb_compose2 = merge(nb_compose, births_by_year_T_a, by="Annee", all=TRUE)
nb_compose2

nb_compose3 = nb_compose2 %>% mutate(Avg=Nombre/Total)
nb_compose3

p1 <-ggplot(nb_compose3, aes(x = Annee, y = Avg)) + geom_line() + geom_point()





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

births_by_year_P <- data %>% group_by(Annee)

births_by_year_P_a = aggregate(births_by_year_P$Nombre, list(births_by_year_P$Annee), sum)
colnames(births_by_year_P_a)[1] <- "Annee"
colnames(births_by_year_P_a)[2] <- "Total"
births_by_year_P_a

nb_compose = data %>% 
  group_by(Annee) %>% 
  mutate(Compose=nchar(gsub("[^-]","",Prenom, ignore.case=TRUE))*Nombre) %>%
  summarize(Nombre=sum(Compose))
nb_compose

nb_compose2 = merge(nb_compose, births_by_year_P_a, by="Annee", all=TRUE)
nb_compose2

nb_compose3 = nb_compose2 %>% mutate(Avg=Nombre/Total)
nb_compose3

p2 <-ggplot(nb_compose3, aes(x = Annee, y = Avg)) + geom_line() + geom_point()

p1 + p2


###############################################################################

data_t = read.csv("prenoms.csv", sep=";")
data_p = read.csv("prenomsParis.csv", sep=";")

total_t = sum(data_t$Nombre)
total_p = sum(data_p$Nombre)

total_prenoms_t = aggregate(data_t$Nombre, list(data_t$Prenom), sum)
total_prenoms_p = aggregate(data_p$Nombre, list(data_p$Prenom), sum)

total_prenoms_t$Frequence <- total_prenoms_t$x / total_t
total_prenoms_p$Frequence <- total_prenoms_p$x / total_p

total_prenoms = merge(total_prenoms_t, total_prenoms_p, by="Group.1")

total_prenoms$Diff = abs(total_prenoms$Frequence.x - total_prenoms$Frequence.y)

most_unshared = total_prenoms %>% slice_max(Diff, n=5)






