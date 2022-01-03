library(xlsx)

pathology_tab <- read.xlsx2("../../报告结果整理35meta.xlsx",sheetName = "stage_ID",stringsAsFactors = F)
rownames(pathology_tab) <- pathology_tab[,"ID"]

hp_positive_id <- rownames(pathology_tab)[which(pathology_tab$幽门螺杆菌 %in% c("3+","2+","1+"))]
hp_negative_id <- rownames(pathology_tab)[which(pathology_tab$幽门螺杆菌=="-")]

alpha_tab <- read.table("../3diversity/alpha_diversity_tab.txt")
alpha_tab_t <- alpha_tab[,grep("T",colnames(alpha_tab))] 
colnames(alpha_tab_t) <- gsub("T","",colnames(alpha_tab_t))

alpha_tab_g <- alpha_tab[,grep("G",colnames(alpha_tab))]
colnames(alpha_tab_g) <- gsub("G","",colnames(alpha_tab_g))

hp_alpha_tab <- matrix(NA,4,2,dimnames = list(rownames(alpha_tab),c("tongue_p","juice_p")))

for(rows in rownames(alpha_tab)){
  hp_alpha_tab[rows,"tongue_p"] <- wilcox.test(as.numeric(alpha_tab_t[rows,hp_positive_id]),as.numeric(alpha_tab_t[rows,hp_negative_id]))$p.value
  hp_alpha_tab[rows,"juice_p"] <- wilcox.test(as.numeric(alpha_tab_g[rows,hp_positive_id]),as.numeric(alpha_tab_g[rows,hp_negative_id]))$p.value
}

write.table(hp_alpha_tab,file = "hp_diversity_alpha_tab.txt",quote = F)

library(reshape2)
plot_alpha <- melt(cbind(index=rownames(alpha_tab)[c(1,2,4)],alpha_tab[c(1,2,4),]))

plot_alpha$id <- substr(plot_alpha$variable,1,4)
plot_alpha$hp <- "positive"
plot_alpha$hp[which(plot_alpha$id %in% hp_negative_id )] <- "negative"

library(ggplot2)
library(ggsignif)
ggplot(plot_alpha[grep("G",plot_alpha$variable),],aes(hp,value))+
  geom_boxplot(aes(fill=hp))+
  geom_jitter()+
  facet_wrap(~index,scales = "free_y")+
  geom_signif(comparisons = list(c("positive","negative")))+
  labs(title="gastric")+
  theme(text = element_text(size=18))+
  ggsave("hp_diversity_alpha_gastric.pdf",width = 8,height = 6)

ggplot(plot_alpha[grep("T",plot_alpha$variable),],aes(hp,value))+
  geom_boxplot(aes(fill=hp))+
  geom_jitter()+
  facet_wrap(~index,scales = "free_y")+
  geom_signif(comparisons = list(c("positive","negative")))+
  labs(title="tongue")+
  theme(text = element_text(size=18))+
  ggsave("hp_diversity_alpha_tongue.pdf",width = 8,height = 6)


###########################################################
#beta

beta_bc_tab <- read.table("../3diversity/beta_distance_bray_curtis_d5.txt")
beta_ja_tab <- read.table("../3diversity/beta_distance_jaccard_d5.txt")

for (class in c("bc","ja")){
  for (site in c("T","G")){
    beta_tab <- get(paste0("beta_",class,"_tab"))
    
    beta_tab_site <- beta_tab[grep(site,rownames(beta_tab)),grep(site,colnames(beta_tab))]
    
    positive_tab <- beta_tab_site[paste0(hp_positive_id,site),paste0(hp_positive_id,site)]
    negative_tab <- beta_tab_site[paste0(hp_negative_id,site),paste0(hp_negative_id,site)]
    
    positive_data <- positive_tab[lower.tri(positive_tab)]
    negative_data <- negative_tab[lower.tri(negative_tab)]
    
    plot_beta_data <- data.frame(dis=c(positive_data,negative_data),group=c(rep("within_positive",length(positive_data)),rep("within_negative",length(negative_data))))
    
    
    plot_beta_data$p <- wilcox.test(positive_data,negative_data)$p.value
    
    plot_beta_data$group <- factor(plot_beta_data$group,levels = c("within_negative","within_positive"))
    
    write.table(plot_beta_data,file = paste0("hp_diversity_beta_distance_",site,"_",class,"_plot.txt"),quote = F,sep = "\t")
    
    ggplot(plot_beta_data,aes(group,dis))+
      geom_boxplot(aes(fill=group))+
      # theme_bw()+
      labs(title=paste(site,class,"distance"))+
      scale_fill_manual(values=c("within_positive"="#9370DB","within_negative"="#8DB6CD"))+
      # geom_signif(annotations = "***", y_position = 1.05, xmin=1, xmax=2)+
      geom_signif(comparisons = list(c("within_positive","within_negative")))+
      ggsave(paste("hp_diversity_beta_distance_",site,"_",class,".pdf",sep=""),width = 5,height = 4)
    
  }
  
  
}




                       