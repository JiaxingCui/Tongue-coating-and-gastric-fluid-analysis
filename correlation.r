###correlation in species levels

tab <- read.table("../../d5/metaphlan2_unpair_merge_Species_d5.txt",sep="\t",header = T,row.names = 7)
tab <- tab[,-c(1:6)]

T_ID <- colnames(tab)[grep("T",colnames(tab))]
G_ID <- colnames(tab)[grep("G",colnames(tab))]


##correlation in all level

i <- 1
for (level in c("Phylum","Class","Order","Family","Genus","Species")) {
  i <- i+1
  level_tab <- read.table(paste("../../d5/metaphlan2_unpair_merge_",level,"_d5.txt",sep = ""),sep="\t",header = T,row.names = i)

  tongue_level_tab <- level_tab[,T_ID]
  gastric_level_tab <- level_tab[,G_ID]
  
  tongue_level_tab <- tongue_level_tab[-which(rowSums(tongue_level_tab)==0),]
  gastric_level_tab <- gastric_level_tab[-which(rowSums(gastric_level_tab)==0),]
  
  overlap_species <- intersect(rownames(tongue_level_tab),rownames(gastric_level_tab))
  tongue_species_only <- setdiff(rownames(tongue_level_tab),rownames(gastric_level_tab))
  gastric_species_only <- setdiff(rownames(gastric_level_tab),rownames(tongue_level_tab))
  
  correlation_tab <- matrix(NA,length(overlap_species),8,dimnames = list(overlap_species,c("spearman","spearman.p","spearman.fdr","pearson","pearson.p","pearson.fdr","tongueMoreThan0","juiceMoreThan0")))
  overlap_tab <- level_tab[overlap_species,]
  
  ##spearman，Exact=F与exact=T结果一致
  ##pearson 带连结时，也不会有warning：无法精确计算带连结的P值。只有spearman会有warning
  correlation_tab[,1] <- apply(overlap_tab,1,function(x) cor(as.numeric(x[T_ID]),as.numeric(x[G_ID]),method = "spearman"))
  correlation_tab[,2] <- apply(overlap_tab,1,function(x) cor.test(as.numeric(x[T_ID]),as.numeric(x[G_ID]),method = "spearman")$p.value)
  
  correlation_tab[,3] <- p.adjust(correlation_tab[,2],method = "fdr")
  
  correlation_tab[,4] <- apply(overlap_tab,1,function(x) cor(as.numeric(x[T_ID]),as.numeric(x[G_ID]),method = "pearson"))
  correlation_tab[,5] <- apply(overlap_tab,1,function(x) cor.test(as.numeric(x[T_ID]),as.numeric(x[G_ID]),method = "pearson")$p.value)
  
  correlation_tab[,6] <- p.adjust(correlation_tab[,5],method = "fdr")
  
  correlation_tab[,7] <- rowSums(overlap_tab[,T_ID]>0)
  correlation_tab[,8] <- rowSums(overlap_tab[,G_ID]>0)
  
  correlation_tab <- correlation_tab[order(correlation_tab[,2]),]
  write.table(correlation_tab,file=paste("correlation_tab_",level,".txt",sep = ""),quote = F,sep = "\t")
  assign(paste("correlation_tab_",level,sep = ""),correlation_tab)
}


####与舌苔中21个species比较
tongue21 <- c("Veillonella parvula","Corynebacterium matruchotii","Streptococcus infantis","Kingella oralis","Atopobium rimae","Treponema vincentii","Leptotrichia unclassified","Campylobacter rectus","Aggregatibacter aphrophilus","Campylobacter showae","Streptococcus sanguinis","Capnocytophaga gingivalis","Leptotrichia buccalis","Acinetobacter lwoffii","Campylobacter concisus","Prevotella amnii","Prevotella bivia","Selenomonas flueggei","Leptotrichia hofstadii","Cardiobacterium hominis","Oribacterium sinus")
tongue21 <- gsub(" ","_",tongue21)
tongue21 <- paste("s__",tongue21,sep = "")

m <- rownames(correlation_tab_Species)[which(correlation_tab_Species[,"spearman.p"]<0.05)]
n <- rownames(correlation_tab_Species)[which(correlation_tab_Species[,"spearman.fdr"]<0.05)]
t <- rownames(correlation_tab_Species)[which(correlation_tab_Species[,"spearman.fdr"]<0.1)]

intersect(tongue21,m)
intersect(tongue21,n)
intersect(tongue21,t)




doubtful_species <- c("s__Porphyromonas_gingivalis","s__Porphyromonas_asaccharolytica","s__Dialister_invisus","s__Haemophilus_parahaemolyticus","s__Streptococcus_phage_EJ_1")
write.table(tab[doubtful_species,G_ID],file = "doubt_species_G.txt",sep ="\t",quote = F)
write.table(tab[doubtful_species,T_ID],file = "doubt_species_T.txt",sep ="\t",quote = F)
