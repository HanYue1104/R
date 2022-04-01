rm(list = ls()) 
#install.packages("pheatmap")
library(pheatmap) 
library(ggplot2)
data<-read.csv("./heatmap/heatmap.csv",header = T,row.names = 1)
group<-read.csv("./heatmap/grouplist.csv",header = T,row.names = 1)
head(data)
head(group)
#dev.off()
pheatmap(data,
         annotation_col=group,
         show_colnames = F,
         show_rownames = F)

p2<-pheatmap(
  mat=data,
  cluster_rows = T,
  cluster_columns = F,
  border_color      = NA,
  show_colnames     = FALSE,
  show_rownames     = FALSE,
  fontsize          = 20,
  annotation_col    = group,
  drop_levels       = TRUE,
  main              = "Heatmap",   
  filename = './heatmap_top4464_2.png')
