# 2.1

df = data.frame(
  letter=letters,
  number=(1:length(letters)),
  is_vowel=as.numeric(letters %in% c('a', 'e', 'i', 'o', 'u', 'y'))
)
df[which(df$letter %in% strsplit("rstudio", split='')[[1]]),]

install.packages("ggplot2")
library(ggplot2)
data(msleep)
head(msleep)
str(msleep)
names(msleep)
summary(msleep)
msleep[msleep$sleep_total + msleep$awake != 24.,]
msleep[which.max(msleep$sleep_total),"name"]
length(msleep[msleep$sleep_total > 12.0 & msleep$bodywt > 0.1,"name"])
mean(na.omit(msleep$brainwt / msleep$bodywt))
msleep[which.max(na.omit(msleep$brainwt / msleep$bodywt)),"name"]

# 2.2

m2=data.frame(mlseep)
m2.f = factor(m2$conservation, levels=c("lc", "domesticated", "cd", "nt", "vu", "en"), ordered=TRUE)
mean1 = mean(m2$bodywt[m2$conservation %in% c("vu", "en")], na.rm=T)
mean2 = mean(m2$bodywt[!m2$conservation %in% c("vu", "en")], na.rm=T)
m2$threatened = m2$conservation %in% c("vu", "en")

# 2.3

get_letters_indices <- function(name) {
  if (nchar(name) == 0) return(NA)
  
  name_clean = gsub("[^a-z]", "", tolower(name))
  
  if (nchar(name_clean) == 0) return(NA)
  
  letters_numbers = match(strsplit(name_clean, split='')[[1]], letters)
  
  return(letters_numbers)
}

find_animal_genus <- function(name) {
  found = m2[tolower(m2$name) == tolower(name),]

  if (nrow(found) == 0) return("I don't know")

  return(paste("The", name, "is a", found$genus))
}
