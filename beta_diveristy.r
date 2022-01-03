library(vegan)

tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
tab <- tab[,-c(1:6)]

T_ID <- colnames(tab)[grep("T",colnames(tab))]
G_ID <- colnames(tab)[grep("G",colnames(tab))]

##用betadiver函数

# ##j得到的是jaccard index
# # cc得到的是jaccard distance
# jaccard_tab <- tab
# jaccard_tab[jaccard_tab>0] <- 1
# jaccard_tab <- t(jaccard_tab)
# res <- betadiver(jaccard_tab, "cc")
# jaccard_result <- as.matrix(res)
# 
# write.table(jaccard_result,file="jaccard_distance.txt",quote = F,sep = "\t")


##用vegdist函数

temp <- t(tab)

tab.ja <- vegdist(temp,"jac",binary = T)
ja_distance <- as.matrix(tab.ja)
tab.bc <- vegdist(temp,'bray')
bc_distance <- as.matrix(tab.bc)

write.table(ja_distance,file="beta_distance_jaccard_d5.txt",quote = F,sep = "\t")
write.table(bc_distance,file="beta_distance_bray_curtis_d5.txt",quote = F,sep = "\t")

library(ggplot2)
library(ggsignif)

for (dis in c("ja","bc")){
  eval(parse(text=paste("dis_tab <-", dis,"_distance",sep="")))
  dis_tab2 <- dis_tab[G_ID,T_ID]
  dis_same_sampe <- diag(dis_tab2)
  dis_diff_sample <- c(dis_tab2[lower.tri(dis_tab2)],dis_tab2[upper.tri(dis_tab2)])
  
  plot_data <- data.frame(dis=c(dis_same_sampe,dis_diff_sample),group=c(rep("within",length(dis_same_sampe)),rep("between",length(dis_diff_sample))))
  
  plot_data$p <- wilcox.test(dis_same_sampe,dis_diff_sample)$p.value
  
  plot_data$group <- factor(plot_data$group,levels = c("within","between"))
  
  write.table(plot_data,file = paste("beta_distance_",dis,"_plot.txt",sep=""),quote = F,sep = "\t")
  
  ggplot(plot_data,aes(group,dis))+
    geom_boxplot(aes(fill=group))+
    # theme_bw()+
    labs(title=paste(dis,"distance"))+
    # geom_signif(annotations = "***", y_position = 1.05, xmin=1, xmax=2)+
    geom_signif(comparisons = list(c("within","between")))+
    ylim(0.3,1.1)+
    ggsave(paste("beta_distance_",dis,".pdf",sep=""),width = 5,height = 4)
}


