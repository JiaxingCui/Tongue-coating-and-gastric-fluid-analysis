library(ggplot2)
library(reshape2)

phylum_tab <- read.table("../../d5/metaphlan2_unpair_merge_Phylum_d5.txt",sep="\t",header = T,row.names = 2)
rownames(phylum_tab) <- gsub("p__","",rownames(phylum_tab))

G_ID <- colnames(phylum_tab)[grep("G",colnames(phylum_tab))]
T_ID <- colnames(phylum_tab)[grep("T",colnames(phylum_tab))]


abundant_species_site <- read.table("phylum_d5_abundant_species_site_d5.txt",stringsAsFactors = F)[,1]
rare_species_site <- read.table("phylum_d5_rare_species_site_d5.txt",stringsAsFactors = F)[,1]  

phylum_tab_t <- phylum_tab[,T_ID]
phylum_tab_t <- phylum_tab_t[-which(rowSums(phylum_tab_t)==0),]
phylum_tab_t$mean_tongue <- apply(phylum_tab_t,1,mean)
level1 <- rownames(phylum_tab_t)[order(phylum_tab_t$mean_tongue,decreasing = T)]
plot_phylum_tab_t <- melt(cbind(phylum=rownames(phylum_tab_t),phylum_tab_t),id.vars = c("phylum","mean_tongue"))
plot_phylum_tab_t$phylum <- factor(plot_phylum_tab_t$phylum,levels = level1)
colnames(plot_phylum_tab_t)[3:4] <- c("ID","abundance")

plot_phylum_tab_t$log_abundance <- log10(plot_phylum_tab_t$abundance+1e-5)


plot_phylum_tab_t_abun <- plot_phylum_tab_t[which(plot_phylum_tab_t$phylum %in% abundant_species_site),]
plot_phylum_tab_t_rare <- plot_phylum_tab_t[which(plot_phylum_tab_t$phylum %in% rare_species_site),]

ggplot(plot_phylum_tab_t_abun,aes(phylum,abundance))+
  geom_boxplot(fill="#EEB4B4")+
  theme_bw()+
  # scale_colour_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  # scale_fill_manual(values=c("Tongue coating"="#EEB4B4","Gastric fluid"="#96CDCD"))+
  # scale_fill_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  theme(axis.text.x = element_text(angle = 45,hjust=0.9),text = element_text(size=18),axis.title.x=element_blank())+
  # geom_signif(annotations = c("***", "**","*"), y_position = c(90, 60,100), xmin=c(0.75,1.75,2.75), xmax=c(1.25,2.25,3.25))
  # geom_signif(annotations = c("***", "***"), y_position = c(88, 80), xmin=c(0.8,1.8), xmax=c(1.2,2.2))+
  ggsave("phylum_boxplot_tongue_abun.pdf",width = 5,height = 6)

ggplot(plot_phylum_tab_t_rare,aes(phylum,log_abundance))+
  geom_boxplot(fill="#EEB4B4")+
  theme_bw()+
  # scale_colour_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  # scale_fill_manual(values=c("Tongue coating"="#EEB4B4","Gastric fluid"="#96CDCD"))+
  # scale_fill_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  theme(axis.text.x = element_text(angle = 45,hjust=0.9),text = element_text(size=18),axis.title.x=element_blank())+
  # geom_signif(annotations = c("***", "**","*"), y_position = c(90, 60,100), xmin=c(0.75,1.75,2.75), xmax=c(1.25,2.25,3.25))
  # geom_signif(annotations = c("***", "***"), y_position = c(88, 80), xmin=c(0.8,1.8), xmax=c(1.2,2.2))+
  ggsave("phylum_boxplot_tongue_rare.pdf",width = 8,height = 6)

phylum_tab_g <- phylum_tab[,G_ID]
phylum_tab_g <- phylum_tab_g[-which(rowSums(phylum_tab_g)==0),]
phylum_tab_g$mean_juice <- apply(phylum_tab_g,1,mean)
level2 <- rownames(phylum_tab_g)[order(phylum_tab_g$mean_juice,decreasing = T)]
plot_phylum_tab_g <- melt(cbind(phylum=rownames(phylum_tab_g),phylum_tab_g),id.vars = c("phylum","mean_juice"))
plot_phylum_tab_g$phylum <- factor(plot_phylum_tab_g$phylum,levels = level2)
colnames(plot_phylum_tab_g)[3:4] <- c("ID","abundance")

plot_phylum_tab_g$log_abundance <- log10(plot_phylum_tab_g$abundance+1e-5)


plot_phylum_tab_g_abun <- plot_phylum_tab_g[which(plot_phylum_tab_g$phylum %in% abundant_species_site),]
plot_phylum_tab_g_rare <- plot_phylum_tab_g[which(plot_phylum_tab_g$phylum %in% rare_species_site),]

ggplot(plot_phylum_tab_g_abun,aes(phylum,abundance))+
  geom_boxplot(fill="#96CDCD")+
  theme_bw()+
  # scale_colour_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  # scale_fill_manual(values=c("Tongue coating"="#EEB4B4","Gastric fluid"="#96CDCD"))+
  # scale_fill_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  theme(axis.text.x = element_text(angle = 45,hjust=0.9),text = element_text(size=18),axis.title.x=element_blank())+
  # geom_signif(annotations = c("***", "**","*"), y_position = c(90, 60,100), xmin=c(0.75,1.75,2.75), xmax=c(1.25,2.25,3.25))
  # geom_signif(annotations = c("***", "***"), y_position = c(88, 80), xmin=c(0.8,1.8), xmax=c(1.2,2.2))+
  ggsave("phylum_boxplot_juice_abun.pdf",width = 5,height = 6)

ggplot(plot_phylum_tab_g_rare,aes(phylum,log_abundance))+
  geom_boxplot(fill="#96CDCD")+
  theme_bw()+
  # scale_colour_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  # scale_fill_manual(values=c("Tongue coating"="#EEB4B4","Gastric fluid"="#96CDCD"))+
  # scale_fill_manual(values=c("Tongue coating"="indianred1","Gastric fluid"="cadetblue"))+
  theme(axis.text.x = element_text(angle = 45,hjust=0.9),text = element_text(size=18),axis.title.x=element_blank())+
  # geom_signif(annotations = c("***", "**","*"), y_position = c(90, 60,100), xmin=c(0.75,1.75,2.75), xmax=c(1.25,2.25,3.25))
  # geom_signif(annotations = c("***", "***"), y_position = c(88, 80), xmin=c(0.8,1.8), xmax=c(1.2,2.2))+
  ggsave("phylum_boxplot_juice_rare.pdf",width = 8,height = 6)




