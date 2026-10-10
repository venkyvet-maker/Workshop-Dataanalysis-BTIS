x<-read.csv('trainingset.csv')
library(ggstatsplot)
library(correlation)
ggcorrmat(x, type = "pearson", nbreaks = 22, title = "Correlation Heat Map") #heat map with values
ggsave("heatmap.png", plot = heatmap, width = 8, height = 6, dpi = 300)