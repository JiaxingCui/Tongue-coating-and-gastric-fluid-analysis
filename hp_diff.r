species_tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
species_tab <- species_tab[,-c(1:6)]

T_ID <- colnames(species_tab)[grep("T",colnames(species_tab))]
G_ID <- colnames(species_tab)[grep("G",colnames(species_tab))]

# species_tab["s__Helicobacter_pylori",T_ID]
# species_tab["s__Helicobacter_pylori",G_ID]

library(xlsx)

pathology_tab <- read.xlsx2("../../报告结果整理35meta.xlsx",sheetName = "stage_ID",stringsAsFactors = F)
rownames(pathology_tab) <- pathology_tab[,"ID"]

hp_positive_id <- rownames(pathology_tab)[which(pathology_tab$幽门螺杆菌 %in% c("3+","2+","1+"))]
hp_negative_id <- rownames(pathology_tab)[which(pathology_tab$幽门螺杆菌=="-")]


#######################################################################
##分析两组微生物差异
i <- 1
for (level in c("Phylum","Class","Order","Family","Genus","Species")) {
  i <- i+1
  tab <- read.table(paste("../../d5/metaphlan2_unpair_merge_",level,"_d5.txt",sep = ""),sep="\t",header = T,row.names = i)
  
  mean_hp <- matrix(NA,nrow(tab),8,dimnames = list(rownames(tab),c("hp positive tongue coating","hp negative tongue coating","tongue_p","tongue_fdr","hp positive gastric fluid","hp negative gastric fluid","gastric_p","gastric_fdr")))
  
  mean_hp[,"hp positive tongue coating"] <- apply(tab[,paste(hp_positive_id,"T",sep = "")],1,mean)
  mean_hp[,"hp negative tongue coating"] <- apply(tab[,paste(hp_negative_id,"T",sep = "")],1,mean)
  mean_hp[,"tongue_p"] <- apply(tab[,i:69+i],1,function(x) wilcox.test(x[paste(hp_positive_id,"T",sep = "")],x[paste(hp_negative_id,"T",sep = "")])$p.value)
  mean_hp[,"tongue_fdr"] <- p.adjust(mean_hp[,"tongue_p"],method = "fdr")
  
  mean_hp[,"hp positive gastric fluid"] <- apply(tab[,paste(hp_positive_id,"G",sep = "")],1,mean)
  mean_hp[,"hp negative gastric fluid"] <- apply(tab[,paste(hp_negative_id,"G",sep = "")],1,mean)
  mean_hp[,"gastric_p"] <- apply(tab[,i:69+i],1,function(x) wilcox.test(x[paste(hp_positive_id,"G",sep = "")],x[paste(hp_negative_id,"G",sep = "")])$p.value)
  mean_hp[,"gastric_fdr"] <- p.adjust(mean_hp[,"gastric_p"],method = "fdr")
  
  assign(paste(level,"_tab",sep=""),tab)
  assign(paste(level,"_mean_hp",sep=""), mean_hp)
  write.table(mean_hp,file = paste("hp_diff_mean_",level,"_hp.txt",sep=""),sep = "\t",quote = F)
  
}

#########################################################################
##整理所有差异的微生物

sink("hp_diff_merge_fdr.txt")

for (level in c("Phylum","Class","Order","Family","Genus","Species")) {
  diff_tab <- read.table(paste0("hp_diff_mean_",level,"_hp.txt"),sep="\t")
  
  print(level,quote = FALSE)
  
  print("tongue",quote = FALSE)
  
  for(rows in rownames(diff_tab)){
    if(!is.na(diff_tab[rows,"tongue_fdr"]) && diff_tab[rows,"tongue_fdr"]<0.05){
      
      print(diff_tab[rows,c(1:4)],quote = FALSE)
      
    }
  }
  print("juice",quote = FALSE)
  
  for(rows in rownames(diff_tab)){
    if(!is.na(diff_tab[rows,"gastric_fdr"]) && diff_tab[rows,"gastric_fdr"]<0.05){
      
      print(diff_tab[rows,c(5:8)],quote = FALSE)
      
    }
  }
}

sink()

##############################################################
##差异微生物的pvalue表格

diff_micro <- c("c__Epsilonproteobacteria","o__Campylobacterales","f__Helicobacteraceae","g__Helicobacter","s__Helicobacter_pylori")
diff_micro_tab <- data.frame(hp.positive.gastric.fluid=NA,hp.negative.gastric.fluid=NA,gastric_p=NA,gastric_fdr=NA)
i <- 1
for (level in c("Class","Order","Family","Genus","Species")) {
  tab_temp <- read.table( paste0("hp_diff_mean_",level,"_hp.txt"),sep="\t",header = T)
  diff_micro_tab <- rbind(diff_micro_tab,tab_temp[diff_micro[i],c(5:8)])
  i <- i+1
}
diff_micro_tab <- diff_micro_tab[-1,]

write.table(diff_micro_tab,file="hp_diff_merge_p_tab.txt",quote = F,sep="\t")

###############################################################
###plot_diff

diff_micro <- c("c__Epsilonproteobacteria","o__Campylobacterales","f__Helicobacteraceae","g__Helicobacter","s__Helicobacter_pylori")
plot_diff_tab <- matrix(NA,5,35,dimnames = list(diff_micro,G_ID))
level<- "class"
i <- 1
for (level in c("Class","Order","Family","Genus","Species")) {
  tab <- read.table(paste("../../d5/metaphlan2_unpair_merge_",level,"_d5.txt",sep = ""),sep="\t",header = T,row.names = i+2)
  plot_diff_tab[i,] <- as.numeric(tab[diff_micro[i],G_ID])
  i <- i+1
}

library(reshape2)
library(ggplot2)
library(ggsignif)
plot_diff_data <- melt(plot_diff_tab)
names(plot_diff_data) <- c("microbe","sample_ID","abundance")
plot_diff_data$ID <- substr(plot_diff_data$sample_ID,1,4)
plot_diff_data$H.pylori <- "positive"
plot_diff_data[which(plot_diff_data$ID %in% hp_negative_id),"H.pylori"] <- "negative"
ggplot(plot_diff_data,aes(H.pylori,abundance))+
  geom_boxplot(aes(fill=H.pylori))+
  geom_jitter()+
  facet_wrap(~microbe,scales = "free_y")+
  geom_signif(comparisons = list(c("positive","negative")),y_position = 65)+
  theme_bw()+
  ylim(0,70)+
  theme(axis.text.x = element_text(size = 12),strip.text=element_text(size = 12))+
  labs(title="hp_diff_gastric fluid_p_need_varify")+
  ggsave("hp_diff_plot_p要改.pdf",width = 8,height = 6)

