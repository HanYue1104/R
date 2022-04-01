rm(list = ls()) 
#BiocManager::install("org.Hs.eg.db")
dev.off()
library(clusterProfiler)
library(DOSE)
library(org.Hs.eg.db)
library(ggplot2)
#TOTAL
gene_total<-read.csv("GO_2/go_up.csv",header= T)
keytypes(org.Hs.eg.db)
head(gene_total)
gene.total<-bitr(gene_total$Protein,fromType = "UNIPROT",toType = "SYMBOL",OrgDb = "org.Hs.eg.db")
total<-merge(gene.total,gene_total)
head(total)
#write.csv(total,"t1.csv")
go.all<- enrichGO(gene=total$SYMBOL, 
                  OrgDb = org.Hs.eg.db, 
                  ont='ALL', 
                  #ont='BP',
                  pAdjustMethod = 'BH',
                  pvalueCutoff = 0.05, 
                  #qvalueCutoff = 0.2,
                  keyType = 'SYMBOL')

dim(go.all[go.all$ONTOLOGY=='BP',]);dim(go.all[go.all$ONTOLOGY=='CC',]);dim(go.all[go.all$ONTOLOGY=='MF',])
write.csv(go.all@result,'./GO_2/go_up1.csv',row.names=F)
#write.csv(gene.total,'GOinput_FA.csv',row.names=F)
FA<-read.csv("./GO_2/go_up1.csv",header= T)
library(stringr)
gr1 <- as.numeric(str_split(FA$GeneRatio,"/",simplify = T)[,1])
gr2 <- as.numeric(str_split(FA$GeneRatio,"/",simplify = T)[,2])
bg1 <- as.numeric(str_split(FA$BgRatio,"/",simplify = T)[,1])
bg2 <- as.numeric(str_split(FA$BgRatio,"/",simplify = T)[,2])
FA$fold <- (gr1/gr2)/(bg1/bg2)
head(FA)
dim(FA[FA$ONTOLOGY=='BP',]);dim(FA[FA$ONTOLOGY=='CC',]);dim(FA[FA$ONTOLOGY=='MF',])
#FA$Description = factor(FA$Description,levels = FA$Description,ordered = T)
head(FA)
write.csv(FA,'./GO_2/go_up2.csv',row.names=F)

#作图
#输入文件之前需要先排序
FA2<-read.csv("./GO_2/go_up3.csv",header= T)
head(FA2)
FA2$Description = factor(FA2$Description,levels = FA2$Description,ordered = T)
head(FA2)
ggplot(FA2,aes(x = fold,y = Description))+
  geom_point(aes(size = Count,color = p.adjust))+
  #guides(fill = guide_legend(reverse = TRUE))+
  scale_color_gradient(low = "red", high = "blue")+
  ylab("")+
  xlab("Fold Enrichment")+
  theme_bw()+
  theme(axis.title=element_text(size=12,colour = 'black'), #坐标轴标题
        axis.text=element_text(size=12,colour = 'black'), #坐标轴标签
        #axis.line = element_line(size=0.5, colour = 'black'), #轴线
        #panel.background = element_rect(color='black'), #绘图区边框
        #legend.key = element_blank() #关闭图例边框
  )+
  facet_grid(ONTOLOGY ~ ., scales = "free",space = "free")+
#edit legends
guides(
#reverse color order (higher value on top)
color = guide_colorbar(reverse = TRUE))
#reverse size order (higher diameter on top) 
#size = guide_legend(reverse = TRUE))

#dev.off() 
#dotplot(go.all,showCategory = 8, size = NULL,font.size = 10, title = "GO enrichment", split = "ONTOLOGY") + facet_grid(ONTOLOGY ~ ., scales = "free")
#barplot(go.all,showCategory = 20, size = NULL,font.size = 10, title = "GO enrichment", split = "ONTOLOGY") + facet_grid(ONTOLOGY ~ ., scales = "free")
#dotplot(go.all,showCategory = 18)


