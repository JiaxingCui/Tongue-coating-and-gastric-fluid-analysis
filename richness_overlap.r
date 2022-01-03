tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
tab <- tab[,-c(1:6)]

T_ID <- colnames(tab)[grep("T",colnames(tab))]
G_ID <- colnames(tab)[grep("G",colnames(tab))]

T_tab <- tab[,T_ID]
G_tab <- tab[,G_ID]

minus_tab <- (T_tab>0) - (G_tab>0)


plot_data <- data.frame(ID=gsub("T","",colnames(minus_tab)),Tongue=colSums(minus_tab==1),Fluid=colSums(minus_tab==-1),Both= colSums((T_tab>0) & (G_tab>0)))


library(ggplot2)
library(reshape2)

plot_data2 <- melt(plot_data)
colnames(plot_data2) <- c("ID","site","num")
plot_data2$site <- factor(plot_data2$site,levels = c("Tongue","Both","Fluid"))
ggplot(plot_data2,aes(ID,num))+
  geom_col(aes(fill=site), width = 0.75)+
  theme_bw()+
  scale_fill_manual(values=c("Tongue"="indianred1","Fluid"="cadetblue","Both"="darkolivegreen"))+
  theme(panel.grid=element_blank(),axis.text.x=element_text(angle = 90),text = element_text(size=20))+
  ggsave("richness_overlap_species_num_overlap_d5.pdf",width = 10,height = 6)
  

richness_overlap_tab <- plot_data
richness_overlap_tab$overlap_ratio_of_gastric <- plot_data$Both / (plot_data$Both + plot_data$Fluid)
write.table(richness_overlap_tab,file="richness_overlap_tab.txt",quote = F,sep = "\t",row.names = F)
