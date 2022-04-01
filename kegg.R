rm(list = ls())  ## 魔幻操作，一键清空~
library(clusterProfiler)
library(org.Hs.eg.db)
library(ggplot2)
gene<-read.csv("KEGG/24h.csv",header = T)
head(gene)
gene.total<-bitr(gene$uniprot,fromType = "UNIPROT",toType = "ENTREZID",OrgDb = "org.Hs.eg.db")
total<-merge(gene.total,gene)
kk.up <- enrichKEGG(gene         = total$ENTREZID,
                    organism     = 'hsa',
                    pvalueCutoff = 0.05,
                    pAdjustMethod ="BH",
                    qvalueCutoff =0.05)
head(kk.up)[,1:6]
write.csv(kk.up,"kegg.csv")
ggplot(data = kk.up)+
  geom_bar(aes(y=reorder(Description,Count),x=Count,fill=-log(p.adjust)),stat='identity')+
  # Y轴为Term，以Count数排列，X轴为Count数；绘图函数里的stat参数表示对样本点做统计的方式，默认为identity，表示一个x对应一个y  
  scale_fill_gradient(expression(-log["10"](p.adjust)),low="blue",high="red")+
  # 设置图例
  ylab("")+
  xlab("Gene count")+ # 设置坐标轴名称
  theme_bw() # 设置背景
dotplot(kk.up,showCategory=15)


kegg<-read.csv("./KEGG/kegg.csv",header= T)
library(stringr)
gr1 <- as.numeric(str_split(kegg$GeneRatio,"/",simplify = T)[,1])
gr2 <- as.numeric(str_split(kegg$GeneRatio,"/",simplify = T)[,2])
bg1 <- as.numeric(str_split(kegg$BgRatio,"/",simplify = T)[,1])
bg2 <- as.numeric(str_split(kegg$BgRatio,"/",simplify = T)[,2])
kegg$fold <- (gr1/gr2)/(bg1/bg2)
head(kegg)
dim(kegg[kegg$ONTOLOGY=='BP',]);dim(kegg[kegg$ONTOLOGY=='CC',]);dim(kegg[kegg$ONTOLOGY=='MF',])

head(kegg)
write.csv(kegg,'kegg2.csv',row.names=F)

kk.up<-read.csv("./KEGG/kegg3.csv",header= T)
kk.up$Description = factor(kk.up$Description,levels = kk.up$Description,ordered = T)
ggplot(kk.up,aes(x = fold,y = Description))+
  geom_point(aes(size = Count,color = p.adjust))+
  guides(fill = guide_legend(reverse = TRUE))+
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
  # facet_grid(ONTOLOGY ~ ., scales = "free")
  guides(
    #reverse color order (higher value on top)
    color = guide_colorbar(reverse = TRUE))
#reverse size order (higher diameter on top) 
#size = guide_legend(reverse = TRUE))
dotplot(kk.up,showCategory=15)

kk.up<-read.csv("./KEGG/KEGG_String.csv",header= T)
ggplot(data = kk.up)+
  geom_bar(aes(y=reorder(Description,Count),x=Count,fill=FDR),stat='identity')+
  # Y轴为Term，以Count数排列，X轴为Count数；绘图函数里的stat参数表示对样本点做统计的方式，默认为identity，表示一个x对应一个y  
  scale_fill_gradient(expression(FDR),low="#82E0AA" ,high="#EAFAF1")+
  # 设置图例
  ylab("")+
  xlab("Counts")+ # 设置坐标轴名称
  theme_bw()+ # 设置背景
  theme(axis.title=element_text(size=12,colour = 'black'), #坐标轴标题
        axis.text=element_text(size=12,colour = 'black'))+
  guides(
    #reverse color order (higher value on top)
    color = guide_colorbar(reverse = TRUE))
#reverse size order (higher diameter on top) 
#size = guide_legend(reverse = TRUE))
