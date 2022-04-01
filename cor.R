rm(list = ls()) 
#library(RColorBrewer)
df<-read.csv("correlation/cor_FA.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE,header=TRUE,row.names = 1)#数据导入为数据框#
head(df)
df<-as.matrix(df)
write.csv(df,"./correlation/2.csv")
mat <- as.data.frame(round(cor(df,method="pearson"), 2))
mat1 <- as.data.frame(round(cor(df,method="spearman"), 2))
mat2 <- as.data.frame(round(cor(df,method="kendall"), 2))
?cor
?round
head(mat)
head(mat1)
mat<- as.matrix(mat)
mat1<- as.matrix(mat1)
mat2<- as.matrix(mat2)
head(mat2)
write.csv(mat2,"./correlation/1_2.csv")
library(corrplot)
corrplot(mat, method = "circle")

corrplot.mixed(matr, order = "AOE")
corrplot.mixed(mat,lower = "ellipse", order="AOE")
corrplot.mixed(mat,lower = "ellipse", upper = "number",order="AOE")
corrplot.mixed(mat2,lower = "number",upper = "ellipse",order="AOE")

display.brewer.all()
display.brewer.pal(11,"Set1")
brewer.pal.info
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(corr =mat,method = "number",
         type="lower",add = TRUE,
         tl.pos="n",cl.pos = "n",diag = FALSE,
         col = "black",number.cex = 0.7)
