class_tab <- read.table("../../d5/metaphlan2_unpair_merge_Class_d5.txt",sep="\t",header = T,row.names = 3)

species_tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)


T_ID <- colnames(class_tab)[grep("T",colnames(class_tab))]
G_ID <- colnames(class_tab)[grep("G",colnames(class_tab))]

library(xlsx)

pathology_tab <- read.xlsx2("../../±¨¸æ½á¹ûÕûÀí35meta.xlsx",sheetName = "stage_ID",stringsAsFactors = F)
rownames(pathology_tab) <- pathology_tab[,"ID"]

hp_positive_id <- rownames(pathology_tab)[which(pathology_tab$ÓÄÃÅÂÝ¸Ë¾ú %in% c("3+","2+","1+"))]
hp_negative_id <- rownames(pathology_tab)[which(pathology_tab$ÓÄÃÅÂÝ¸Ë¾ú=="-")]


##########################################################################
library(pheatmap)
heat_tab <- class_tab[,T_ID]
colnames(heat_tab) <- substr(colnames(heat_tab),1,4)

annotation_cl <- as.data.frame(matrix(NA,35,1,dimnames = list(rownames(pathology_tab),"hp")))
annotation_cl[,"hp"] <- pathology_tab$ÓÄÃÅÂÝ¸Ë¾ú
annotation_cl_01 <- annotation_cl
annotation_cl_01[which(annotation_cl_01[,1]=="-"),"hp"] <- "negative"
annotation_cl_01[which(annotation_cl_01[,1] %in% c("3+","2+","1+")),"hp"] <- "positive"

# pheatmap(log10(heat_tab+1e-5),color = colorRampPalette(c("darkgreen","white", "red"))(100),annotation_col = annotation_cl_01,scale = "row",clustering_distance_cols = "correlation",annotation_colors = ann_colors,annotation_names_row = F,show_colnames = F,filename = "heatmap_simpfy_red_green.pdf",width = 8,height = 5)
# 
# pheatmap(log10(heat_tab+1e-5),color = colorRampPalette(c("darkgreen","white", "red"))(100),annotation_col = annotation_cl_01,scale = "row",clustering_distance_cols = "correlation",annotation_names_row = F,show_colnames = F)
# 
# pheatmap(log10(heat_tab+1e-5),annotation_col = annotation_cl_01,scale = "row",clustering_distance_cols = "correlation")
pheatmap(heat_tab,annotation_col = annotation_cl_01,fontsize = 13,filename = "heatmap_class_tongue.pdf",width = 12,height = 8)

#########################################################################
library(reshape2)
library(ggplot2)
pro_tab <- class_tab[which(class_tab$X.1=="p__Proteobacteria"),G_ID]
plot_pro_tab <- melt(cbind(class=rownames(pro_tab),pro_tab))
names(plot_pro_tab)[2:3] <- c("sample_id","abundance")

plot_pro_tab$id <- substr(plot_pro_tab$sample_id,1,4)

plot_pro_tab <- rbind(plot_pro_tab,data.frame(class="c__Epsilonproteobacteria",sample_id="null",abundance=0,id="null"))

order_id <- c( hp_positive_id[order(class_tab["c__Epsilonproteobacteria",paste(hp_positive_id,"G",sep="")],decreasing = T)], "null",hp_negative_id[order(class_tab["c__Epsilonproteobacteria",paste(hp_negative_id,"G",sep="")],decreasing = T)] )

plot_pro_tab$id <- factor(plot_pro_tab$id,levels = order_id)

# plot_pro_tab$hp <- pathology_tab[plot_pro_tab$id,"ÓÄÃÅÂÝ¸Ë¾ú"]
# plot_pro_tab$hp[which(plot_pro_tab$hp=="-")] <- "negative"
# plot_pro_tab$hp[which(plot_pro_tab$hp %in% c("3+","2+","1+"))] <- "positive"

plot_pro_tab$class <- factor(plot_pro_tab$class,levels = c("c__Alphaproteobacteria","c__Betaproteobacteria","c__Gammaproteobacteria","c__Epsilonproteobacteria"))

ggplot(plot_pro_tab,aes(id,abundance))+
  geom_col(aes(fill=class))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),panel.grid=element_blank(),text = element_text(size=15))+
  scale_fill_manual(values=c("c__Alphaproteobacteria"="#8B008B","c__Betaproteobacteria"="indianred1","c__Gammaproteobacteria"="#A2CD5A","c__Epsilonproteobacteria"="#6495ED"))+
  ggsave("hp_class.pdf",width=10,height = 7)

