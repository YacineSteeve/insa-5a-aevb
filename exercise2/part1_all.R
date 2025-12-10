data = read.csv("prenomsRennesStrassNantesToul.csv", sep=";")
head(data)

data %>% group_by(ANNAISS)
