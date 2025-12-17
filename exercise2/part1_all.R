library(dplyr)

DIRECTORY = "prenoms-par-dpts"

filenames = list.files(DIRECTORY, pattern="*.csv")

all_data = lapply(filenames, function(filename) read.csv(paste(DIRECTORY, filename, sep="/"), sep=";"))
all_data_2 = lapply(all_data, function(data) mutate(data, PrenomAnnee=paste0(preusuel, "_", annais)))

head(all_data_2[[1]])

data = read.csv("prenomsRennesStrassNantesToul.csv", sep=";")
head(data)

sort(unique(data$ANNAISS))

unique(data$LBCOM)

data %>% group_by(LBCOM)

cos_sim = function(a, b) {
  sum(a * b) / sqrt(sum(a**2) * sum(b**2))
}

data_by_city = split(data, data$LBCOM)

data_by_city$NANTES$Prn
