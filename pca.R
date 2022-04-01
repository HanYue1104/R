rm(list = ls()) 
data<-read.csv("PCA/PCA_FA.csv",header = T,row.names = 1)
dat=as.data.frame(t(data))
head(dat)
#二，自己生成（记得加引号，不加会找不到对象，这个用于比较有规律的分布）
group_list=c(rep("FA",times=6),rep("C",times=6))
group_list
#在讲PPT的时候讲了转置数据，要数据框。
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)#画主成分分析图需要加载这两个包
library(factoextra) 

# pca的统一操作走起
dat.pca <- PCA(dat, graph = FALSE)
head(dat)
?fviz_pca_ind
?fviz_pca_var
pca_plot <- fviz_pca_ind(dat.pca,
                         geom.ind = "point",
                         pointsize = 3,# show points only (nbut not "text")
                         col.ind = group_list, # color by groups
                         palette = c("#00AFBB", "#E7B800"),
                         addEllipses = TRUE, # Concentration ellipses
                         legend.title = "Groups"
)

pca_plot
