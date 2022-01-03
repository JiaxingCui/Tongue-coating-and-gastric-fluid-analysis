correlation_tab <- read.table("../4correlation/correlation_tab_Species.txt")
correlate_species <- rownames(correlation_tab)[which(correlation_tab$spearman.fdr<0.05)]
correlate_species_pearson <- rownames(correlation_tab)[which(correlation_tab$pearson.fdr<0.05)]


abun_tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)

tongue_id <- colnames(abun_tab)[grep("T",colnames(abun_tab))]
juice_id <- colnames(abun_tab)[grep("G",colnames(abun_tab))]

tongue_tab <- abun_tab[,tongue_id]
juice_tab <-  abun_tab[,juice_id]

tongue_tab <- tongue_tab[-which(rowSums(tongue_tab)==0),]
juice_tab <-  juice_tab[-which(rowSums(juice_tab)==0),]

tongue_stat <- as.matrix(table(rowSums(tongue_tab>0)))
juice_stat <- as.matrix(table(rowSums(juice_tab>0)))

data_stat <- merge(data.frame(num=rownames(tongue_stat),tongue=tongue_stat[,1]),data.frame(num=rownames(juice_stat),juice=juice_stat[,1]),by="num",all=T)

write.table(data_stat,file = "insight_exist_num_species_num.txt",row.names = F,sep="\t",quote = F)

library(ggplot2)

temp_tongue <- rowSums(tongue_tab>0)
temp_juice <- rowSums(juice_tab>0)

data <- merge(data.frame(species=names(temp_tongue),tongue_num=temp_tongue),data.frame(species=names(temp_juice),juice_num=temp_juice),by="species",all = T)

write.table(data,file = "insight_species_exist_num.txt",row.names = F,sep = "\t",quote = F)

ggplot(data,aes(tongue_num))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0, 35, 2))+
  theme_bw()+
  labs(x="Frequence in all samples",y="Num of species",title = "Tongue coating")+
  ggsave("insight_species_count_tongue.pdf",width = 6,height = 4)
  

ggplot(data,aes(juice_num))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0, 35, 2))+
  theme_bw()+
  labs(x="Frequence in all samples",y="Num of species",title = "Gastric fluid")+
  ggsave("insight_species_count_juice.pdf",width = 6,height = 4)
  


name_spearman <- setdiff(correlate_species,correlate_species_pearson)  
name_pearson <- setdiff(correlate_species_pearson,correlate_species)  
name_both <- intersect(correlate_species,correlate_species_pearson)  


# pearson_t_tab <- abun_tab[name_pearson,tongue_id]
# rownames(pearson_t_tab) <- paste0(rownames(pearson_t_tab),"_t")
# colnames(pearson_t_tab) <- substr(colnames(pearson_t_tab),1,4)
# pearson_g_tab <- abun_tab[name_pearson,tongue_id]
# rownames(pearson_g_tab) <- paste0(rownames(pearson_g_tab),"_g")
# colnames(pearson_g_tab) <- substr(colnames(pearson_g_tab),1,4)
# 
# temp1 <- cbind(rownames(pearson_t_tab),pearson_t_tab)
# temp2 <- cbind(rownames(pearson_g_tab),pearson_g_tab)
# pearson_tab <- rbind(cbind(name=rownames(pearson_t_tab),pearson_t_tab),cbind(name=rownames(pearson_g_tab),pearson_g_tab),stringsAsFactors=F)


write.table(abun_tab[name_pearson,tongue_id],file = "insight_pearson_tongue.txt",sep="\t",quote = F)
write.table(abun_tab[name_spearman,tongue_id],file = "insight_spearman_tongue.txt",sep="\t",quote = F)

write.table(abun_tab[name_pearson,juice_id],file = "insight_pearson_juice.txt",sep="\t",quote = F)
write.table(abun_tab[name_spearman,juice_id],file = "insight_spearman_juice.txt",sep="\t",quote = F)

write.table(abun_tab[name_both,tongue_id],file = "insight_both_tongue.txt",sep="\t",quote = F)
write.table(abun_tab[name_both,juice_id],file = "insight_both_juice.txt",sep="\t",quote = F)
