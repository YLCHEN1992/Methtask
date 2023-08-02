
#语言包
library(circlize) 
#真诚感谢作者Zuguang Gu 大神 说明书地址 https://jokergoo.github.io/circlize_book/book/
library(ComplexHeatmap)
library(gridExtra)
library(ggplot2)
library(devtools)
install_github("JanCoUnchained/ggunchained")
#谨慎使用半小提琴图，未安装存在更新风险
library(ggunchained)
library(org.Hs.eg.db)

#甲基化表格读取（不熟悉的测序公司）
hpdmc=read.csv("HP_Treat_vs_Ctrl_Annotation.xls",sep="\t")
hpdmr=read.csv("HP_Treat_vs_Ctrl_DMR_Filt_Annotation.xls",sep="\t")
hwdmc=read.csv("HW_Treat_vs_Ctrl_Annotation.xls",sep="\t")
hwdmr=read.csv("HW_Treat_vs_Ctrl_DMR_Filt_Annotation.xls",sep="\t")
#分类整列（个性化分析选择使用，无需整列）
hpdmc_up2k=hpdmc[hpdmc$up2k!="--",]
hpdmc_promoter=hpdmc[hpdmc$promoter!="--",]
hpdmc_5utr=hpdmc[hpdmc$X5utr!="--",]
hpdmc_cds=hpdmc[hpdmc$cds!="--",]
hpdmc_3utr=hpdmc[hpdmc$X3utr!="--",]
hpdmc_down2k=hpdmc[hpdmc$down2k!="--",]
hpdmc_exon=hpdmc[hpdmc$exon!="--",]
hpdmc_intron=hpdmc[hpdmc$intron!="--",]
hpdmc_genebody=hpdmc[hpdmc$genebody!="--",]
hpdmc_intergenic=hpdmc[hpdmc$intergenic!="--",]
hpdmc_enhancer=hpdmc[hpdmc$enhancer!="--",]
hpdmr_up2k=hpdmr[hpdmr$up2k!="--",]
hpdmr_promoter=hpdmr[hpdmr$promoter!="--",]
hpdmr_5utr=hpdmr[hpdmr$X5utr!="--",]
hpdmr_cds=hpdmr[hpdmr$cds!="--",]
hpdmr_3utr=hpdmr[hpdmr$X3utr!="--",]
hpdmr_down2k=hpdmr[hpdmr$down2k!="--",]
hpdmr_exon=hpdmr[hpdmr$exon!="--",]
hpdmr_intron=hpdmr[hpdmr$intron!="--",]
hpdmr_genebody=hpdmr[hpdmr$genebody!="--",]
hpdmr_intergenic=hpdmr[hpdmr$intergenic!="--",]
hpdmr_enhancer=hpdmr[hpdmr$enhancer!="--",]
hwdmc_up2k=hwdmc[hwdmc$up2k!="--",]
hwdmc_promoter=hwdmc[hwdmc$promoter!="--",]
hwdmc_5utr=hwdmc[hwdmc$X5utr!="--",]
hwdmc_cds=hwdmc[hwdmc$cds!="--",]
hwdmc_3utr=hwdmc[hwdmc$X3utr!="--",]
hwdmc_down2k=hwdmc[hwdmc$down2k!="--",]
hwdmc_exon=hwdmc[hwdmc$exon!="--",]
hwdmc_intron=hwdmc[hwdmc$intron!="--",]
hwdmc_genebody=hwdmc[hwdmc$genebody!="--",]
hwdmc_intergenic=hwdmc[hwdmc$intergenic!="--",]
hwdmc_enhancer=hwdmc[hwdmc$enhancer!="--",]
hwdmr_up2k=hwdmr[hwdmr$up2k!="--",]
hwdmr_promoter=hwdmr[hwdmr$promoter!="--",]
hwdmr_5utr=hwdmr[hwdmr$X5utr!="--",]
hwdmr_cds=hwdmr[hwdmr$cds!="--",]
hwdmr_3utr=hwdmr[hwdmr$X3utr!="--",]
hwdmr_down2k=hwdmr[hwdmr$down2k!="--",]
hwdmr_exon=hwdmr[hwdmr$exon!="--",]
hwdmr_intron=hwdmr[hwdmr$intron!="--",]
hwdmr_genebody=hwdmr[hwdmr$genebody!="--",]
hwdmr_intergenic=hwdmr[hwdmr$intergenic!="--",]
hwdmr_enhancer=hwdmr[hwdmr$enhancer!="--",]
hpdmc_up2k_control__mean=mean(as.vector(as.matrix(hpdmc_up2k[,8:10])))
hpdmc_promoter_control__mean=mean(as.vector(as.matrix(hpdmc_promoter[,8:10])))
hpdmc_5utr_control__mean=mean(as.vector(as.matrix(hpdmc_5utr[,8:10])))
hpdmc_cds_control__mean=mean(as.vector(as.matrix(hpdmc_cds[,8:10])))
hpdmc_3utr_control__mean=mean(as.vector(as.matrix(hpdmc_3utr[,8:10])))
hpdmc_down2k_control__mean=mean(as.vector(as.matrix(hpdmc_down2k[,8:10])))
hpdmc_exon_control__mean=mean(as.vector(as.matrix(hpdmc_exon[,8:10])))
hpdmc_intron_control__mean=mean(as.vector(as.matrix(hpdmc_intron[,8:10])))
hpdmc_genebody_control__mean=mean(as.vector(as.matrix(hpdmc_genebody[,8:10])))
hpdmc_intergenic_control__mean=mean(as.vector(as.matrix(hpdmc_intergenic[,8:10])))
hpdmc_enhancer_control__mean=mean(as.vector(as.matrix(hpdmc_enhancer[,8:10])))
hwdmc_up2k_control__mean=mean(as.vector(as.matrix(hwdmc_up2k[,8:10])))
hwdmc_promoter_control__mean=mean(as.vector(as.matrix(hwdmc_promoter[,8:10])))
hwdmc_5utr_control__mean=mean(as.vector(as.matrix(hwdmc_5utr[,8:10])))
hwdmc_cds_control__mean=mean(as.vector(as.matrix(hwdmc_cds[,8:10])))
hwdmc_3utr_control__mean=mean(as.vector(as.matrix(hwdmc_3utr[,8:10])))
hwdmc_down2k_control__mean=mean(as.vector(as.matrix(hwdmc_down2k[,8:10])))
hwdmc_exon_control__mean=mean(as.vector(as.matrix(hwdmc_exon[,8:10])))
hwdmc_intron_control__mean=mean(as.vector(as.matrix(hwdmc_intron[,8:10])))
hwdmc_genebody_control__mean=mean(as.vector(as.matrix(hwdmc_genebody[,8:10])))
hwdmc_intergenic_control__mean=mean(as.vector(as.matrix(hwdmc_intergenic[,8:10])))
hwdmc_enhancer_control__mean=mean(as.vector(as.matrix(hwdmc_enhancer[,8:10])))
hpdmc_up2k_tumor__mean=mean(as.vector(as.matrix(hpdmc_up2k[,11:13])))
hpdmc_promoter_tumor__mean=mean(as.vector(as.matrix(hpdmc_promoter[,11:13])))
hpdmc_5utr_tumor__mean=mean(as.vector(as.matrix(hpdmc_5utr[,11:13])))
hpdmc_cds_tumor__mean=mean(as.vector(as.matrix(hpdmc_cds[,11:13])))
hpdmc_3utr_tumor__mean=mean(as.vector(as.matrix(hpdmc_3utr[,11:13])))
hpdmc_down2k_tumor__mean=mean(as.vector(as.matrix(hpdmc_down2k[,11:13])))
hpdmc_exon_tumor__mean=mean(as.vector(as.matrix(hpdmc_exon[,11:13])))
hpdmc_intron_tumor__mean=mean(as.vector(as.matrix(hpdmc_intron[,11:13])))
hpdmc_genebody_tumor__mean=mean(as.vector(as.matrix(hpdmc_genebody[,11:13])))
hpdmc_intergenic_tumor__mean=mean(as.vector(as.matrix(hpdmc_intergenic[,11:13])))
hpdmc_enhancer_tumor__mean=mean(as.vector(as.matrix(hpdmc_enhancer[,11:13])))
hwdmc_up2k_tumor__mean=mean(as.vector(as.matrix(hwdmc_up2k[,11:13])))
hwdmc_promoter_tumor__mean=mean(as.vector(as.matrix(hwdmc_promoter[,11:13])))
hwdmc_5utr_tumor__mean=mean(as.vector(as.matrix(hwdmc_5utr[,11:13])))
hwdmc_cds_tumor__mean=mean(as.vector(as.matrix(hwdmc_cds[,11:13])))
hwdmc_3utr_tumor__mean=mean(as.vector(as.matrix(hwdmc_3utr[,11:13])))
hwdmc_down2k_tumor__mean=mean(as.vector(as.matrix(hwdmc_down2k[,11:13])))
hwdmc_exon_tumor__mean=mean(as.vector(as.matrix(hwdmc_exon[,11:13])))
hwdmc_intron_tumor__mean=mean(as.vector(as.matrix(hwdmc_intron[,11:13])))
hwdmc_genebody_tumor__mean=mean(as.vector(as.matrix(hwdmc_genebody[,11:13])))
hwdmc_intergenic_tumor__mean=mean(as.vector(as.matrix(hwdmc_intergenic[,11:13])))
hwdmc_enhancer_tumor__mean=mean(as.vector(as.matrix(hwdmc_enhancer[,11:13])))
hpdmc_up2k_control__sd=sd(as.vector(as.matrix(hpdmc_up2k[,8:10])))
hpdmc_promoter_control__sd=sd(as.vector(as.matrix(hpdmc_promoter[,8:10])))
hpdmc_5utr_control__sd=sd(as.vector(as.matrix(hpdmc_5utr[,8:10])))
hpdmc_cds_control__sd=sd(as.vector(as.matrix(hpdmc_cds[,8:10])))
hpdmc_3utr_control__sd=sd(as.vector(as.matrix(hpdmc_3utr[,8:10])))
hpdmc_down2k_control__sd=sd(as.vector(as.matrix(hpdmc_down2k[,8:10])))
hpdmc_exon_control__sd=sd(as.vector(as.matrix(hpdmc_exon[,8:10])))
hpdmc_intron_control__sd=sd(as.vector(as.matrix(hpdmc_intron[,8:10])))
hpdmc_genebody_control__sd=sd(as.vector(as.matrix(hpdmc_genebody[,8:10])))
hpdmc_intergenic_control__sd=sd(as.vector(as.matrix(hpdmc_intergenic[,8:10])))
hpdmc_enhancer_control__sd=sd(as.vector(as.matrix(hpdmc_enhancer[,8:10])))
hwdmc_up2k_control__sd=sd(as.vector(as.matrix(hwdmc_up2k[,8:10])))
hwdmc_promoter_control__sd=sd(as.vector(as.matrix(hwdmc_promoter[,8:10])))
hwdmc_5utr_control__sd=sd(as.vector(as.matrix(hwdmc_5utr[,8:10])))
hwdmc_cds_control__sd=sd(as.vector(as.matrix(hwdmc_cds[,8:10])))
hwdmc_3utr_control__sd=sd(as.vector(as.matrix(hwdmc_3utr[,8:10])))
hwdmc_down2k_control__sd=sd(as.vector(as.matrix(hwdmc_down2k[,8:10])))
hwdmc_exon_control__sd=sd(as.vector(as.matrix(hwdmc_exon[,8:10])))
hwdmc_intron_control__sd=sd(as.vector(as.matrix(hwdmc_intron[,8:10])))
hwdmc_genebody_control__sd=sd(as.vector(as.matrix(hwdmc_genebody[,8:10])))
hwdmc_intergenic_control__sd=sd(as.vector(as.matrix(hwdmc_intergenic[,8:10])))
hwdmc_enhancer_control__sd=sd(as.vector(as.matrix(hwdmc_enhancer[,8:10])))
hpdmc_up2k_tumor__sd=sd(as.vector(as.matrix(hpdmc_up2k[,11:13])))
hpdmc_promoter_tumor__sd=sd(as.vector(as.matrix(hpdmc_promoter[,11:13])))
hpdmc_5utr_tumor__sd=sd(as.vector(as.matrix(hpdmc_5utr[,11:13])))
hpdmc_cds_tumor__sd=sd(as.vector(as.matrix(hpdmc_cds[,11:13])))
hpdmc_3utr_tumor__sd=sd(as.vector(as.matrix(hpdmc_3utr[,11:13])))
hpdmc_down2k_tumor__sd=sd(as.vector(as.matrix(hpdmc_down2k[,11:13])))
hpdmc_exon_tumor__sd=sd(as.vector(as.matrix(hpdmc_exon[,11:13])))
hpdmc_intron_tumor__sd=sd(as.vector(as.matrix(hpdmc_intron[,11:13])))
hpdmc_genebody_tumor__sd=sd(as.vector(as.matrix(hpdmc_genebody[,11:13])))
hpdmc_intergenic_tumor__sd=sd(as.vector(as.matrix(hpdmc_intergenic[,11:13])))
hpdmc_enhancer_tumor__sd=sd(as.vector(as.matrix(hpdmc_enhancer[,11:13])))
hwdmc_up2k_tumor__sd=sd(as.vector(as.matrix(hwdmc_up2k[,11:13])))
hwdmc_promoter_tumor__sd=sd(as.vector(as.matrix(hwdmc_promoter[,11:13])))
hwdmc_5utr_tumor__sd=sd(as.vector(as.matrix(hwdmc_5utr[,11:13])))
hwdmc_cds_tumor__sd=sd(as.vector(as.matrix(hwdmc_cds[,11:13])))
hwdmc_3utr_tumor__sd=sd(as.vector(as.matrix(hwdmc_3utr[,11:13])))
hwdmc_down2k_tumor__sd=sd(as.vector(as.matrix(hwdmc_down2k[,11:13])))
hwdmc_exon_tumor__sd=sd(as.vector(as.matrix(hwdmc_exon[,11:13])))
hwdmc_intron_tumor__sd=sd(as.vector(as.matrix(hwdmc_intron[,11:13])))
hwdmc_genebody_tumor__sd=sd(as.vector(as.matrix(hwdmc_genebody[,11:13])))
hwdmc_intergenic_tumor__sd=sd(as.vector(as.matrix(hwdmc_intergenic[,11:13])))
hwdmc_enhancer_tumor__sd=sd(as.vector(as.matrix(hwdmc_enhancer[,11:13])))
data=data.frame(hpdmc_up2k_control__mean	=	hpdmc_up2k_control__mean	,
hpdmc_promoter_control__mean	=	hpdmc_promoter_control__mean	,
hpdmc_5utr_control__mean	=	hpdmc_5utr_control__mean	,
hpdmc_cds_control__mean	=	hpdmc_cds_control__mean	,
hpdmc_3utr_control__mean	=	hpdmc_3utr_control__mean	,
hpdmc_down2k_control__mean	=	hpdmc_down2k_control__mean	,
hpdmc_exon_control__mean	=	hpdmc_exon_control__mean	,
hpdmc_intron_control__mean	=	hpdmc_intron_control__mean	,
hpdmc_genebody_control__mean	=	hpdmc_genebody_control__mean	,
hpdmc_intergenic_control__mean	=	hpdmc_intergenic_control__mean	,
hpdmc_enhancer_control__mean	=	hpdmc_enhancer_control__mean	,		
hwdmc_up2k_control__mean	=	hwdmc_up2k_control__mean	,
hwdmc_promoter_control__mean	=	hwdmc_promoter_control__mean	,
hwdmc_5utr_control__mean	=	hwdmc_5utr_control__mean	,
hwdmc_cds_control__mean	=	hwdmc_cds_control__mean	,
hwdmc_3utr_control__mean	=	hwdmc_3utr_control__mean	,
hwdmc_down2k_control__mean	=	hwdmc_down2k_control__mean	,
hwdmc_exon_control__mean	=	hwdmc_exon_control__mean	,
hwdmc_intron_control__mean	=	hwdmc_intron_control__mean	,
hwdmc_genebody_control__mean	=	hwdmc_genebody_control__mean	,
hwdmc_intergenic_control__mean	=	hwdmc_intergenic_control__mean	,
hwdmc_enhancer_control__mean	=	hwdmc_enhancer_control__mean	,		
hpdmc_up2k_tumor__mean	=	hpdmc_up2k_tumor__mean	,
hpdmc_promoter_tumor__mean	=	hpdmc_promoter_tumor__mean	,
hpdmc_5utr_tumor__mean	=	hpdmc_5utr_tumor__mean	,
hpdmc_cds_tumor__mean	=	hpdmc_cds_tumor__mean	,
hpdmc_3utr_tumor__mean	=	hpdmc_3utr_tumor__mean	,
hpdmc_down2k_tumor__mean	=	hpdmc_down2k_tumor__mean	,
hpdmc_exon_tumor__mean	=	hpdmc_exon_tumor__mean	,
hpdmc_intron_tumor__mean	=	hpdmc_intron_tumor__mean	,
hpdmc_genebody_tumor__mean	=	hpdmc_genebody_tumor__mean	,
hpdmc_intergenic_tumor__mean	=	hpdmc_intergenic_tumor__mean	,
hpdmc_enhancer_tumor__mean	=	hpdmc_enhancer_tumor__mean	,		
hwdmc_up2k_tumor__mean	=	hwdmc_up2k_tumor__mean	,
hwdmc_promoter_tumor__mean	=	hwdmc_promoter_tumor__mean	,
hwdmc_5utr_tumor__mean	=	hwdmc_5utr_tumor__mean	,
hwdmc_cds_tumor__mean	=	hwdmc_cds_tumor__mean	,
hwdmc_3utr_tumor__mean	=	hwdmc_3utr_tumor__mean	,
hwdmc_down2k_tumor__mean	=	hwdmc_down2k_tumor__mean	,
hwdmc_exon_tumor__mean	=	hwdmc_exon_tumor__mean	,
hwdmc_intron_tumor__mean	=	hwdmc_intron_tumor__mean	,
hwdmc_genebody_tumor__mean	=	hwdmc_genebody_tumor__mean	,
hwdmc_intergenic_tumor__mean	=	hwdmc_intergenic_tumor__mean	,
hwdmc_enhancer_tumor__mean	=	hwdmc_enhancer_tumor__mean	,		
hpdmc_up2k_control__sd	=	hpdmc_up2k_control__sd	,
hpdmc_promoter_control__sd	=	hpdmc_promoter_control__sd	,
hpdmc_5utr_control__sd	=	hpdmc_5utr_control__sd	,
hpdmc_cds_control__sd	=	hpdmc_cds_control__sd	,
hpdmc_3utr_control__sd	=	hpdmc_3utr_control__sd	,
hpdmc_down2k_control__sd	=	hpdmc_down2k_control__sd	,
hpdmc_exon_control__sd	=	hpdmc_exon_control__sd	,
hpdmc_intron_control__sd	=	hpdmc_intron_control__sd	,
hpdmc_genebody_control__sd	=	hpdmc_genebody_control__sd	,
hpdmc_intergenic_control__sd	=	hpdmc_intergenic_control__sd	,
hpdmc_enhancer_control__sd	=	hpdmc_enhancer_control__sd	,	
hwdmc_up2k_control__sd	=	hwdmc_up2k_control__sd	,
hwdmc_promoter_control__sd	=	hwdmc_promoter_control__sd	,
hwdmc_5utr_control__sd	=	hwdmc_5utr_control__sd	,
hwdmc_cds_control__sd	=	hwdmc_cds_control__sd	,
hwdmc_3utr_control__sd	=	hwdmc_3utr_control__sd	,
hwdmc_down2k_control__sd	=	hwdmc_down2k_control__sd	,
hwdmc_exon_control__sd	=	hwdmc_exon_control__sd	,
hwdmc_intron_control__sd	=	hwdmc_intron_control__sd	,
hwdmc_genebody_control__sd	=	hwdmc_genebody_control__sd	,
hwdmc_intergenic_control__sd	=	hwdmc_intergenic_control__sd	,
hwdmc_enhancer_control__sd	=	hwdmc_enhancer_control__sd	,		
hpdmc_up2k_tumor__sd	=	hpdmc_up2k_tumor__sd	,
hpdmc_promoter_tumor__sd	=	hpdmc_promoter_tumor__sd	,
hpdmc_5utr_tumor__sd	=	hpdmc_5utr_tumor__sd	,
hpdmc_cds_tumor__sd	=	hpdmc_cds_tumor__sd	,
hpdmc_3utr_tumor__sd	=	hpdmc_3utr_tumor__sd	,
hpdmc_down2k_tumor__sd	=	hpdmc_down2k_tumor__sd	,
hpdmc_exon_tumor__sd	=	hpdmc_exon_tumor__sd	,
hpdmc_intron_tumor__sd	=	hpdmc_intron_tumor__sd	,
hpdmc_genebody_tumor__sd	=	hpdmc_genebody_tumor__sd	,
hpdmc_intergenic_tumor__sd	=	hpdmc_intergenic_tumor__sd	,
hpdmc_enhancer_tumor__sd	=	hpdmc_enhancer_tumor__sd	,	
hwdmc_up2k_tumor__sd	=	hwdmc_up2k_tumor__sd	,
hwdmc_promoter_tumor__sd	=	hwdmc_promoter_tumor__sd	,
hwdmc_5utr_tumor__sd	=	hwdmc_5utr_tumor__sd	,
hwdmc_cds_tumor__sd	=	hwdmc_cds_tumor__sd	,
hwdmc_3utr_tumor__sd	=	hwdmc_3utr_tumor__sd	,
hwdmc_down2k_tumor__sd	=	hwdmc_down2k_tumor__sd	,
hwdmc_exon_tumor__sd	=	hwdmc_exon_tumor__sd	,
hwdmc_intron_tumor__sd	=	hwdmc_intron_tumor__sd	,
hwdmc_genebody_tumor__sd	=	hwdmc_genebody_tumor__sd	,
hwdmc_intergenic_tumor__sd	=	hwdmc_intergenic_tumor__sd	,
hwdmc_enhancer_tumor__sd	=	hwdmc_enhancer_tumor__sd	)
data=t(data)

#频率分布图
frq=function(x,n=0.01,title="Tumor1",col="white"){
categories=cut(x, breaks = seq(0, 1, by = n), labels = FALSE)*n
p=ggplot()+geom_histogram(aes(x=categories),fill=col,col="black")+
scale_y_continuous(labels = function(x) x/100000)+
labs(x = "Methylation", y = "Frequency(x10e5)") + ggtitle(title)+
theme(axis.title= element_text(face = "bold", color = "black",size=10),
axis.text = element_text(face = "bold", color = "black",size=10), 
title=element_text(face = "bold", color = "black",size=10),
legend.position = "none")
p}

o=grid.arrange(frq(apply(hpdmc[,8:10],1,mean),title="Control",col="blue3"),
frq(apply(hpdmc[,11:13],1,mean),title="Tumor",col="red3"),ncol = 2,nrow=1)
grid.arrange(frq(apply(hwdmc[,8:10],1,mean),title="Control",col="blue3"),
frq(apply(hpdmw[,11:13],1,mean),title="Tumor",col="red3"),ncol = 2,nrow=1)

mean(as.vector(as.matrix(hpdmc[,8:10])))
sd(as.vector(as.matrix(hpdmc[,8:10])))
mean(as.vector(as.matrix(hwdmc[,8:10])))
sd(as.vector(as.matrix(hwdmc[,8:10])))
mean(as.vector(as.matrix(hpdmc[,11:13])))
sd(as.vector(as.matrix(hpdmc[,11:13])))
mean(as.vector(as.matrix(hwdmc[,11:13])))
sd(as.vector(as.matrix(hwdmc[,11:13])))
png("fig2.png",unit="in",width = 16,height=8,res =1200)

o=grid.arrange(frq(as.vector(hpdmc[,11]),title="Tumor1",col="red3"),
frq(as.vector(hpdmc[,12]),title="Tumor2",col="red3"),
frq(as.vector(hpdmc[,13]),title="Tumor3",col="red3"),
frq(as.vector(hpdmc[,8]),title="Control1",col="blue3"),
frq(as.vector(hpdmc[,9]),title="Control2",col="blue3"),
frq(as.vector(hpdmc[,10]),title="Control3",col="blue3"),
ncol = 3,nrow=2)

#关键筛选参数
x=hpdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
x=x[abs(x$meth.diff)>0.2&x$qvalue<0.05,]

#染色体整列图
chrp=function(x,t="chr1"){
ggplot()+geom_point(aes(x=as.numeric(x$start[x$type==t]),y=x$treat[x$type==t]),col="red3")+
geom_point(aes(x=as.numeric(x$start[x$type==t]),y=x$ctrl[x$type==t]),col="blue3")+
labs(x = "", y = "Methylation frequency (%)") +ylim(0,1)+ggtitle(t)+
theme(axis.title= element_text(face = "bold", color = "black",size=10),
axis.text = element_text(face = "bold", color = "black",size=10), 
title=element_text(face = "bold", color = "black",size=10))+
xlim(min(as.numeric(x$start[x$type==t])),max(as.numeric(x$start[x$type==t])))+
scale_x_continuous(labels = scales::comma_format())
}
f=data.frame(type=rep(c("Tumor","Control"),each=nrow(x)),values=c(x$treat,x$ctrl),class=x$type)

l=grid.arrange(
chrp(x,"chr1"),
chrp(x,"chr2"),
chrp(x,"chr3"),
chrp(x,"chr4"),
chrp(x,"chr5"),
chrp(x,"chr6"),
chrp(x,"chr7"),
chrp(x,"chr8"),
chrp(x,"chr9"),
chrp(x,"chr10"),
chrp(x,"chr11"),
chrp(x,"chr12"),
chrp(x,"chr13"),
chrp(x,"chr14"),
chrp(x,"chr15"),
chrp(x,"chr16"),
chrp(x,"chr17"),
chrp(x,"chr18"),
chrp(x,"chr19"),
chrp(x,"chr20"),
chrp(x,"chr21"),
chrp(x,"chr22"),
chrp(x,"chrX"),
chrp(x,"chrY"),
nrow=6,ncol=4)
ggsave("fig5-hpmax1.jpg",l,unit="in",width=21,heigh=14,dpi=1200)

chrpx=function(x,t="chr1",p=1){
ggplot()+geom_point(aes(x=as.numeric(x$start[x$type==t]),y=(x$meth.diff[x$type==t])^p),col="purple3",size=1)+
labs(x = "", y = "Distance of Methylation frequency")+ggtitle(t)+
theme(axis.title= element_text(face = "bold", color = "black",size=10),
axis.text = element_text(face = "bold", color = "black",size=10), 
title=element_text(face = "bold", color = "black",size=10))+
xlim(min(as.numeric(x$start[x$type==t])),max(as.numeric(x$start[x$type==t])))+
scale_x_continuous(labels = scales::comma_format())
}

chrpy=function(x,t="chr1",p=7){
ggplot()+geom_line(aes(x=as.numeric(x$start[x$type==t]),y=(x$meth.diff[x$type==t])^p),col="purple3",size=1)+
labs(x = "", y = paste("Meth.diff power=",p,""))+ggtitle(t)+
theme(axis.title= element_text(face = "bold", color = "black",size=10),
axis.text = element_text(face = "bold", color = "black",size=10), 
title=element_text(face = "bold", color = "black",size=10))+
xlim(min(as.numeric(x$start[x$type==t])),max(as.numeric(x$start[x$type==t])))+
scale_x_continuous(labels = scales::comma_format())
}

l=grid.arrange(
chrpx(x,"chr1"),
chrpx(x,"chr2"),
chrpx(x,"chr3"),
chrpx(x,"chr4"),
chrpx(x,"chr5"),
chrpx(x,"chr6"),
chrpx(x,"chr7"),
chrpx(x,"chr8"),
chrpx(x,"chr9"),
chrpx(x,"chr10"),
chrpx(x,"chr11"),
chrpx(x,"chr12"),
chrpx(x,"chr13"),
chrpx(x,"chr14"),
chrpx(x,"chr15"),
chrpx(x,"chr16"),
chrpx(x,"chr17"),
chrpx(x,"chr18"),
chrpx(x,"chr19"),
chrpx(x,"chr20"),
chrpx(x,"chr21"),
chrpx(x,"chr22"),
chrpx(x,"chrX"),
chrpx(x,"chrY"),
nrow=6,ncol=4)
ggsave("fig6-hpmax2.jpg",l,unit="in",width=21,heigh=14,dpi=1200)

#染色体环化呈现
x=hpdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
x=x[abs(x$meth.diff)>0.2&x$qvalue<0.001,]
y=hpdmr
y$type=substr(y$chr,1,5)
y$type=gsub("_","",y$type)
y=y[!y$type%in%c("chrM","chrUn"),]
y$treat=y$Treat
y$ctrl=y$Ctrl
y$meth.diff=y$methyl_diff
y=y[abs(y$methyl_diff)>=0.2,]

png(paste(gsub(":","_",Sys.time()),"fig10-hpci.png",
sep="_"),units="in",width=8,height=8,res=2400)

circos.clear()
circos.par(gap.after=c(rep(2,23),30),start.degree=90,clock.wise=T,track.margin=c(0,0.02),cell.padding=c(0,0,0,0))
circos.initializeWithIdeogram(species = "hg19")
col_fun = colorRamp2(c(0, 1), c("white","blue4"))
circos.track(factors=x$type,x=x$start,y=x$treat,ylim=c(0,1),bg.border="white",track.height=0.01,
panel.fun=function(x,y){
circos.points(x,0,col=col_fun(y),cex = 0.4,pch=19)})
circos.track(factors=x$type,x=x$start,y=x$ctrl,ylim=c(0,1),bg.border="white",track.height=0.01,
panel.fun=function(x,y){
col_fun = colorRamp2(c(0, 1), c("white","blue4"))
circos.points(x,0,col=col_fun(y),cex = 0.4,pch=19)})
circos.track(factors=x$type,x=x$start,y=x$meth.diff,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.points(x,y,col="red3",cex=0.05)})
circos.yaxis(side="left",at=c(-1,0,1),labels.cex=0.5,sector.index ="chr1")
circos.track(factors=y$type,x=y$start,y=y$meth.diff,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.points(x,y,col="blue3",cex=0.05)})
circos.yaxis(side="left",at=c(-1,0,1),labels.cex=0.5,sector.index ="chr1")
circos.track(factors=x$type,x=x$start,y=(x$meth.diff)^3,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.lines(x, y, type="l",col="brown4")})
circos.track(factors=y$type,x=y$start,y=(y$meth.diff)^3,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.lines(x, y,type="l",col="green4")})
lgd_points = Legend(at = c("Meth.diff of DMCs", "Meth.diff of DMRs","Freq of DMC","Freq of DMR"), type = "points", 
legend_gp = gpar(col =c("red3", "blue3","brown4","green4")), title_position = "topleft", title = "Notes")
lgd_links = Legend(at = c(0, 0.25, 0.5, 0.75, 1), col_fun = col_fun,title_position = "topleft", title = "MetFrq(%)")
lgd_list_vertical = packLegend(lgd_links,lgd_points)
draw(lgd_list_vertical, x = unit(4, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
circos.text(0, 0, "Tumor                   ",cex=0.6, sector.index="chr1", track.index=3)
circos.text(0, 0, "Control                     ",cex=0.6, sector.index="chr1", track.index=4)
circos.text(0, 0, "DMCs\n\n\n\n\n",cex=0.3, sector.index="chr1", track.index=5,facing="clockwise")
circos.text(0, 0, "DMRs\n\n\n\n\n",cex=0.3, sector.index="chr1", track.index=6,facing="clockwise")
circos.text(0, 0, "DMC               ",cex=0.6, sector.index="chr1", track.index=7)
circos.text(0, 0, "DMR               ",cex=0.6, sector.index="chr1", track.index=8)
dev.off()

z=hwdmc_5utr
z$type=substr(z$chr,1,5)
z$type=gsub("_","",z$type)
z=z[!z$type%in%c("chrM","chrUn"),]
z=z[abs(z$meth.diff)>0.2 & z$qvalue<0.001,]

x=hwdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
x=x[abs(x$meth.diff)>0.2&x$qvalue<0.001,]

y=hwdmr
y$type=substr(y$chr,1,5)
y$type=gsub("_","",y$type)
y=y[!y$type%in%c("chrM","chrUn"),]
y$treat=y$Treat
y$ctrl=y$Ctrl
y$meth.diff=y$methyl_diff
y=y[abs(y$methyl_diff)>=0.2,]

png(paste(gsub(":","_",Sys.time()),"fig11-hwci.png",
sep="_"),units="in",width=8,height=8,res=2400)

circos.clear()
circos.par(gap.after=c(rep(2,23),30),start.degree=90,clock.wise=T,track.margin=c(0,0.02),cell.padding=c(0,0,0,0))
circos.initializeWithIdeogram(species = "hg19")
col_fun = colorRamp2(c(0, 1), c("white","blue4"))
circos.track(factors=x$type,x=x$start,y=x$treat,ylim=c(0,1),bg.border="white",track.height=0.01,
panel.fun=function(x,y){
circos.points(x,0,col=col_fun(y),cex = 0.4,pch=19)})
circos.track(factors=x$type,x=x$start,y=x$ctrl,ylim=c(0,1),bg.border="white",track.height=0.01,
panel.fun=function(x,y){
col_fun = colorRamp2(c(0, 1), c("white","blue4"))
circos.points(x,0,col=col_fun(y),cex = 0.4,pch=19)})
circos.track(factors=x$type,x=x$start,y=x$meth.diff,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.points(x,y,col="red3",cex=0.05)})
circos.yaxis(side="left",at=c(-1,0,1),labels.cex=0.5,sector.index ="chr1")
circos.track(factors=y$type,x=y$start,y=y$meth.diff,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.points(x,y,col="blue3",cex=0.05)})
circos.yaxis(side="left",at=c(-1,0,1),labels.cex=0.5,sector.index ="chr1")
circos.track(factors=x$type,x=x$start,y=(x$meth.diff)^3,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.lines(x, y, type="l",col="brown4")})
circos.track(factors=y$type,x=y$start,y=(y$meth.diff)^3,track.height=0.05,ylim=c(-1,1),
panel.fun=function(x,y){circos.lines(x, y,type="l",col="green4")})
col_fun = colorRamp2(c(0, 1), c("white","purple3"))
circos.track(factors=z$type,x=z$start,y=abs(z$meth.diff),ylim=c(0,1),bg.border="white",track.height=0.01,
panel.fun=function(x,y){
circos.points(x,0,col="purple3",cex = 0.4,pch=19)})

lgd_points = Legend(at = c("Meth.diff of DMCs", "Meth.diff of DMRs","Freq of DMC","Freq of DMR","5' UTR"), type = "points",legend_gp = gpar(col =c("red3", "blue3","brown4","green4","purple3")), title_position = "topleft", title = "Notes")
lgd_links = Legend(at = c(0, 0.25, 0.5, 0.75, 1), col_fun = col_fun,title_position = "topleft", title = "MetFrq(%)")
lgd_list_vertical = packLegend(lgd_links,lgd_points)
draw(lgd_list_vertical, x = unit(4, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
circos.text(0, 0, "Tumor                   ",cex=0.6, sector.index="chr1", track.index=3)
circos.text(0, 0, "Control                     ",cex=0.6, sector.index="chr1", track.index=4)
circos.text(0, 0, "DMCs\n\n\n\n\n",cex=0.3, sector.index="chr1", track.index=5,facing="clockwise")
circos.text(0, 0, "DMRs\n\n\n\n\n",cex=0.3, sector.index="chr1", track.index=6,facing="clockwise")
circos.text(0, 0, "DMC               ",cex=0.6, sector.index="chr1", track.index=7)
circos.text(0, 0, "DMR               ",cex=0.6, sector.index="chr1", track.index=8)
circos.text(0, 0, "5'-UTR               ",cex=0.6, sector.index="chr1", track.index=9)
dev.off()

#部分信息筛选
n=hwdmc$X5utr[hwdmc$meth.diff>0.2&hwdmc$qvalue<0.001]
m=strsplit(n,",")
o=sapply(m, function(x) x[1])
o=unique(o)
o=o[o!="--"]
YY=select(org.Hs.eg.db, keys=o, columns=c("ENSEMBL"),keytype="SYMBOL")

x=hwdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
 x=x[x$ctrl==0&x$treat>=0.5,]

x=hwdmr
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
 x=x[x$treat==0&x$ctrl>=0.5,]

#通路富集
kk=read.csv("5utr-reactom.csv")
kk=read.csv("5utr-gad.csv")
d=data.frame(Pathway=kk$Term,Ratio=kk$Ratio,PvalueJ=kk$PValue,Counts=kk$Count)
pp=ggplot(d,aes(Ratio,Pathway))+
labs(title="Reactome Pathway",x ="Ratio%", y = "Pathway",fontface = "bold")+
geom_point(aes(size=Counts,color= PvalueJ))+
scale_colour_gradient(high="Green3",low="Red2")+
theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
theme(axis.text.x = element_text(size = 14,color="black"),
axis.text.y = element_text(size = 14,color="black"))+
theme(legend.title=element_text(size = 14),legend.text=element_text(size = 14))
pp

gsav(pp,"newup2kXpromoterkegg.png",10,8)

#信息计算
std=function(x){
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$METH=apply(x[,8:13],1,mean)
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
xh=x[x$METH>=0.75,]
xl=x[x$METH<=0.25,]
data=data.frame(
xhMETH=mean(xh$METH),
xhtreat=mean(xh$treat),
xhctrl=mean(xh$ctrl),
xlMETH=mean(xl$METH),
xltreat=mean(xl$treat),
xlctrl=mean(xl$ctrl))
data}

std=function(x){
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$METH=apply(x[,8:13],1,mean)
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
xh=x[x$METH>=0.75,]
xl=x[x$METH<=0.25,]
data=data.frame(
xhMETH=mean(xh$METH),
xhtreat=mean(xh$treat),
xhctrl=mean(xh$ctrl),
xlMETH=mean(xl$METH),
xltreat=mean(xl$treat),
xlctrl=mean(xl$ctrl))
data}
data=rbind(std(hpdmc_up2k),std(hpdmc_promoter),std(hpdmc_5utr),std(hpdmc_cds),std(hpdmc_3utr),std(hpdmc_exon),std(hpdmc_intron),std(hpdmc_genebody),std(hpdmc_intergenic))
data=rbind(std(hwdmc_up2k),std(hwdmc_promoter),std(hwdmc_5utr),std(hwdmc_cds),std(hwdmc_3utr),std(hwdmc_exon),std(hwdmc_intron),std(hwdmc_genebody),std(hwdmc_intergenic))

#高频率点power显示最高点
chrpz=function(x,t="chr1",p=7){
ggplot()+geom_line(aes(x=as.numeric(x$start[x$type==t]),y=(x$meth.diff[x$type==t])^p),col="orange3",size=1)+
labs(x = "", y = paste("Diff.Meth.Frq power=",p,""))+ggtitle(t)+
theme(axis.title= element_text(face = "bold", color = "black",size=10),
axis.text = element_text(face = "bold", color = "black",size=10), 
title=element_text(face = "bold", color = "black",size=10))+
xlim(min(as.numeric(x$start[x$type==t])),max(as.numeric(x$start[x$type==t])))+
scale_x_continuous(labels = scales::comma_format())
}

x=hwdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
x=x[abs(x$meth.diff)>0.2&x$qvalue<0.001,]

library(gridExtra)
l=grid.arrange(
chrpz(x,"chr1"),
chrpz(x,"chr2"),
chrpz(x,"chr3"),
chrpz(x,"chr4"),
chrpz(x,"chr5"),
chrpz(x,"chr6"),
chrpz(x,"chr7"),
chrpz(x,"chr8"),
chrpz(x,"chr9"),
chrpz(x,"chr10"),
chrpz(x,"chr11"),
chrpz(x,"chr12"),
chrpz(x,"chr13"),
chrpz(x,"chr14"),
chrpz(x,"chr15"),
chrpz(x,"chr16"),
chrpz(x,"chr17"),
chrpz(x,"chr18"),
chrpz(x,"chr19"),
chrpz(x,"chr20"),
chrpz(x,"chr21"),
chrpz(x,"chr22"),
chrpz(x,"chrX"),
chrpz(x,"chrY"),
nrow=6,ncol=4)

ggsave("fig6-df2.jpg",l,unit="in",width=21,heigh=14,dpi=1200)


x=hpdmc
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
f=data.frame(type=rep(c("Tumor","Control"),each=nrow(x)),values=c(x$treat,x$ctrl),class=x$type)

#小提琴图
pic=ggplot(f,aes(x=type,y=values,fill=type))+
geom_violin(alpha=0.7,trim=F,col="white")+
geom_boxplot(width=0.1,col="white",position=position_dodge(0.9))+
scale_fill_manual(values=c("blue3","red3"))+ylim(0,1)+
theme_bw()+
theme(axis.text.x=element_text(angle=15,hjust = 1,colour="black",family="Times",size=14,face="bold"), 
        axis.text.y=element_text(family="Times",size=12,face="bold"),
        axis.title.y=element_text(family="Times",size = 12,face="bold"), 
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
        legend.text=element_text(face="italic", family="Times", colour="black", size=14),
        legend.title=element_text(face="bold", family="Times", colour="black", size=16),
        title=element_text(face="bold", family="Times", colour="black", size=14),
        panel.grid.major = element_blank(),  panel.grid.minor = element_blank())+
  ylab("AvgMethFrq(%)")+xlab("") +labs(title="Overall methylation level",fill="Types")

#半小提琴图
pic=ggplot(f,aes(x=class,y=values,fill=type))+
geom_split_violin(alpha=0.7,trim=T,col="white")+
geom_boxplot(width=0.1,col="white",position=position_dodge(0.2))+
scale_fill_manual(values=c("blue3","red3"))+ylim(0,1)+
theme_bw()+
theme(axis.text.x=element_text(angle=15,hjust = 1,colour="black",family="Times",size=14,face="bold"), 
        axis.text.y=element_text(family="Times",size=12,face="bold"),
        axis.title.y=element_text(family="Times",size = 12,face="bold"), 
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
        legend.text=element_text(face="italic", family="Times", colour="black", size=14),
        legend.title=element_text(face="bold", family="Times", colour="black", size=16),
        title=element_text(face="bold", family="Times", colour="black", size=14),
        panel.grid.major = element_blank(),  panel.grid.minor = element_blank())+
  ylab("AvgMethFrq(%)")+xlab("") +labs(title="Chromosome methylation level",fill="Types")
gsav(pic,"l.jpg",21,7,dpi=1200)

colorz=function(x,y){
n=length(levels(factor(x)))
for (i in 1:n){x[x==levels(factor(x))[i]]=y[i]}
x}


vilof=function(x,main="Overall methylation level"){
x$type=substr(x$chr,1,5)
x$type=gsub("_","",x$type)
x=x[!x$type%in%c("chrM","chrUn"),]
x$treat=apply(x[,11:13],1,mean)
x$ctrl=apply(x[,8:10],1,mean)
f=data.frame(type=rep(c("Tumor","Control"),each=nrow(x)),values=c(x$treat,x$ctrl),class=x$type)
pic=ggplot(f,aes(x=type,y=values,fill=type))+
geom_violin(alpha=0.7,trim=F,col="white")+
geom_boxplot(width=0.1,col="white",position=position_dodge(0.9))+
scale_fill_manual(values=c("blue3","red3"))+ylim(0,1)+
theme_bw()+
theme(axis.text.x=element_text(angle=15,hjust = 1,colour="black",family="Times",size=14,face="bold"), 
        axis.text.y=element_text(family="Times",size=12,face="bold"),
        axis.title.y=element_text(family="Times",size = 12,face="bold"), 
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
        legend.text=element_text(face="italic", family="Times", colour="black", size=14),
        legend.title=element_text(face="bold", family="Times", colour="black", size=16),
        title=element_text(face="bold", family="Times", colour="black", size=14),
        panel.grid.major = element_blank(),  panel.grid.minor = element_blank())+
  ylab("AvgMethFrq(%)")+xlab("") +labs(title=main,fill="Types")
 gsav(pic,paste(main,".jpg",sep=""),dpi=1200);pic}

vilof(hpdmc,main="Overall methylation level")
vilof(hpdmc_up2k,main="Overall methylation level for up2k")
vilof(hpdmc_promoter,main="Overall methylation level for promoter")
vilof(hpdmc_5utr,main="Overall methylation level for 5utr")
vilof(hpdmc_cds,main="Overall methylation level for cds")
vilof(hpdmc_3utr,main="Overall methylation level for 3utr")
vilof(hpdmc_down2k,main="Overall methylation level for down2k")
vilof(hpdmc_exon,main="Overall methylation level for exon")
vilof(hpdmc_intron,main="Overall methylation level for intron")
vilof(hpdmc_genebody,main="Overall methylation level for genebody")
vilof(hpdmc_intergenic,main="Overall methylation level for intergenic")

vilof(hwdmc,main="Overall methylation level")
vilof(hwdmc_up2k,main="Overall methylation level for up2k")
vilof(hwdmc_promoter,main="Overall methylation level for promoter")
vilof(hwdmc_5utr,main="Overall methylation level for 5utr")
vilof(hwdmc_cds,main="Overall methylation level for cds")
vilof(hwdmc_3utr,main="Overall methylation level for 3utr")
vilof(hwdmc_down2k,main="Overall methylation level for down2k")
vilof(hwdmc_exon,main="Overall methylation level for exon")
vilof(hwdmc_intron,main="Overall methylation level for intron")
vilof(hwdmc_genebody,main="Overall methylation level for genebody")
vilof(hwdmc_intergenic,main="Overall methylation level for intergenic")

