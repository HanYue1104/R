rm(list = ls())
library(ggplot2)
package_version("ggplot")
#install.packages("Cairo")
library(Cairo)
data<-read.csv("./volcano/volcano_FA.csv",header= T)
head(data)
#data<-as.data.frame(data)
# 设置p_value和logFC的阈值
cut_off_pvalue = 0.05  #统计显著性
cut_off_logFC = 1           #差异倍数值

# 根据阈值参数，上调基因设置为‘up’，下调基因设置为‘Down’，无差异设置为‘Stable’，并保存到change列中

data$change = ifelse(data$Pvalue < cut_off_pvalue & abs(data$logFC) >= cut_off_logFC, 
                        ifelse(data$logFC> cut_off_logFC ,'Up','Down'),
                        'Stable')
head(data)
#write.csv(data,"./volcano_finall.csv")
?geom_point
p <- ggplot(
  # 数据、映射、颜色
  data, aes(x = logFC, y = -log10(Pvalue), colour=change)) +
  geom_point(alpha=1, size=1.5,stroke=0.05) +
  xlim(-5,5)+
  scale_color_manual(values=c("blue", "#d2dae2","red"))+
  # 辅助线
  geom_vline(xintercept=c(-1,1),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(cut_off_pvalue),lty=4,col="black",lwd=0.8) +
  # 坐标轴
  labs(x="Log2 (Fold Change)",
       y="-Log10 (P-value)")+
  theme_bw()+
  # 图例
  theme(plot.title = element_text(hjust = 0.5),legend.position = c(0.9,0.9))
  #theme(plot.title = element_text(hjust = 0.5), 
        #legend.position="right", 
        #legend.title = element_blank())
p
?theme_bw
?labs
?aes

windowsFonts(myFont = windowsFont("Times New Roman"))
p+theme(title=element_text(family="myFont",size=12,color="black",
                           face="bold",hjust=0.5),
        axis.text.x=element_text(family="myFont",size=12,color="black",face="bold"),
        axis.text.y=element_text(family="myFont",size=12,color="black",face="bold"))

library(DESeq2)
