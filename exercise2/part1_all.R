library(dplyr)
library(tidyr)
library(ggplot2)

cos_sim = function(a, b) {
  sum(a * b) / sqrt(sum(a**2) * sum(b**2))
}

DIRECTORY = "prenoms-par-dpts"

filenames = list.files(DIRECTORY, pattern="*.csv")

all_data = lapply(filenames, function(filename) read.csv(paste(DIRECTORY, filename, sep="/"), sep=";"))

df = bind_rows(all_data[1:99])

matrix = df %>% 
  group_by(preusuel, dpt) %>%
  summarize(nombre=sum(nombre, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(
    names_from = dpt,
    values_from = nombre,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("preusuel") %>%
  as.matrix()

dpts = sort(colnames(matrix), decreasing=TRUE)

cosine_matrix = outer(
  dpts,
  dpts,
  Vectorize(function(i,j)
    cos_sim(matrix[,i], matrix[,j])
  )
)

cosine_df = reshape2::melt(cosine_matrix)

ggplot(
  cosine_df, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", mid="orange", high="darkgreen", midpoint=0.5) +
  coord_fixed() +
  labs(x="dpt", y="dpt", fill="Cosine\nSimilarity") +
  theme(axis.text.x=element_text(angle=90, hjust=1))



