tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
tab <- tab[,-c(1:6)]

T_ID <- colnames(tab)[grep("T",colnames(tab))]
G_ID <- colnames(tab)[grep("G",colnames(tab))]

tongue_tab <- tab[,T_ID]
gastric_tab <- tab[,G_ID]

sort(rowSums(tongue_tab))

tongue_tab <- tongue_tab[-which(rowSums(tongue_tab)==0),]
gastric_tab <- gastric_tab[-which(rowSums(gastric_tab)==0),]

rowSums(gastric_tab>0)
library(pheatmap)
pheatmap(t(gastric_tab))
pheatmap(t(tongue_tab))

library(gplots)
library(VennDiagram)

# input_site <- list(tongue=rownames(tongue_tab),gastric_fluid=rownames(gastric_tab))
# tmp <- venn(input_site,intersection=T)
# isect_site <- attr(tmp,"intersection")
# save(isect_site,file = "isect_site_species.Rdata")
# venn.diagram(input_site,filename = "venn_site.tiff",fontface="bold")


#########################################################################################
##all level

library(gplots)
library(VennDiagram)


i <- 1
for (level in c("Phylum","Class","Order","Family","Genus","Species")) {
  i <- i+1
  tab <- read.table(paste("../../d5/metaphlan2_unpair_merge_",level,"_d5.txt",sep = ""),sep="\t",header = T,row.names = i)
  
  T_ID <- colnames(tab)[grep("T",colnames(tab))]
  G_ID <- colnames(tab)[grep("G",colnames(tab))]
  
  tongue_tab <- tab[,T_ID]
  gastric_tab <- tab[,G_ID]
  
  tongue_tab <- tongue_tab[-which(rowSums(tongue_tab)==0),]
  gastric_tab <- gastric_tab[-which(rowSums(gastric_tab)==0),]

  
  input_site <- list(tongue=rownames(tongue_tab),gastric_fluid=rownames(gastric_tab))
  tmp <- venn(input_site,intersection=T)
  isect_site <- attr(tmp,"intersection")
  save(isect_site,file = paste("venn_isect_site_",level,".Rdata",sep=""))
  venn.diagram(input_site,filename = paste("venn_site_",level,"_d5.tiff",sep = ""),fontface="bold")
}


