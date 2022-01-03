tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
tab <- tab[,-c(1:6)]

T_ID <- colnames(tab)[grep("T",colnames(tab))]
G_ID <- colnames(tab)[grep("G",colnames(tab))]


library(vegan)

diversity_tab <- matrix(NA,4,ncol(tab),dimnames = list(c("Shannon","Simpson","Invsimpson","Richness"),colnames(tab)))

##函数会自动归一化
diversity_tab["Shannon",] <- diversity(tab,index="shannon",MARGIN = 2)
diversity_tab["Simpson",] <- diversity(tab,index="simpson",MARGIN = 2)
diversity_tab["Invsimpson",] <- diversity(tab,index="invsimpson",MARGIN = 2)
diversity_tab["Richness",] <- specnumber(tab,MARGIN = 2)

correlation_tab <- matrix(NA,4,4,dimnames = list(c("Shannon","Simpson","Invsimpson","Richness"),c("pearson_p","pearson_rho","spearman_p","spearman_rho")))
correlation_tab[,1] <- apply(diversity_tab,1,function(x) cor.test(x[T_ID],x[G_ID],method = "pearson")$p.value)
correlation_tab[,2] <- apply(diversity_tab,1,function(x) cor.test(x[T_ID],x[G_ID],method = "pearson")$estimate)
correlation_tab[,3] <- apply(diversity_tab,1,function(x) cor.test(x[T_ID],x[G_ID],method = "spearman")$p.value)
correlation_tab[,4] <- apply(diversity_tab,1,function(x) cor.test(x[T_ID],x[G_ID],method = "spearman")$estimate)

write.table(diversity_tab,file = "alpha_diversity_tab.txt",sep = "\t",quote = F)
write.table(correlation_tab,file="alpha_correlation_tab.txt",sep="\t",quote = F)

library(ggplot2)

plot_data <- data.frame(sampleID=colnames(diversity_tab),t(diversity_tab))

plot_data$patientID <- gsub("G|T","",plot_data$sampleID)

plot_data$site <- "tongue coating"
plot_data$site[grep("G",plot_data$sampleID)] <- "gastric fluid"

index <- 2*as.numeric(plot_data$site=="tongue coating")-1

plot_data$shannon_plot <- plot_data$Shannon*index
plot_data$simpson_plot <- plot_data$Simpson*index
plot_data$richness_plot <- plot_data$Richness*index

ggplot(plot_data,aes(patientID,shannon_plot))+
  geom_col(aes(fill=site),width = 0.85)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_fill_manual(values=c("tongue coating"="indianred1","gastric fluid"="cadetblue"))+
  scale_y_continuous(breaks=seq(-4,4,1),labels = c(4,3,2,1,0,1,2,3,4))+
  # geom_text(aes(label = round(Shannon,2),y=shannon_plot+0.15*index))+
  ggsave("alpha_shannon_d5.pdf",width = 5,height = 5)

ggplot(plot_data,aes(patientID,simpson_plot))+
  geom_col(aes(fill=site),width = 0.85)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_fill_manual(values=c("tongue coating"="indianred1","gastric fluid"="cadetblue"))+
  scale_y_continuous(breaks=seq(-1,1,0.2),labels = c(1,0.8,0.6,0.4,0.2,0,0.2,0.4,0.6,0.8,1))+
  # geom_text(aes(label = round(Simpson,2),y=simpson_plot+0.04*index))+
  ggsave("alpha_simpson_d5.pdf",width = 5,height = 5)

ggplot(plot_data,aes(patientID,richness_plot))+
  geom_col(aes(fill=site),width = 0.85)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_fill_manual(values=c("tongue coating"="indianred1","gastric fluid"="cadetblue"))+
  scale_y_continuous(breaks=seq(-125,230,25),labels = c(125,100,75,50,25,0,25,50,75,100,125,150,175,200,225))+
  # geom_text(aes(label = Richness,y=richness_plot+2.5*index))+
  ggsave("alpha_richness_d5.pdf",width = 5,height = 5)



