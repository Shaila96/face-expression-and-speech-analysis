library(car)
library(MASS)
library(gplots)
library(agricolae)


#Setting Directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)


tmpDb<-read.csv("B_facs.csv",header=T,sep=",")		### use the file that I attach
tmpDc<-read.csv("C_facs.csv",header=T,sep=",")		### use the file that I attach
tmpD<-rbind(tmpDb,tmpDc)
Subject<-tmpD[,1]
table(Subject)
Group<-tmpD[,2]
levels(Group)[1:2]<-"B"
levels(Group)[2:3]<-"C"
table(Group)
table(Group, Subject)
FP<-tmpD[14:20]


MFEb<-matrix(NA, 13 ,8) ### Mean Facial Emotions for Batch (col 1 is # of frames and then 7 emotion %)
MFEc<-matrix(NA, 13,8)### Mean Facial Emotions for Continuous (col 1 is # of frames and then 7 emotion %)
tmplb<-1
tmplc<-1
for (i in unique(Subject)){
	if (unique(Group[Subject==i])=="B"){
		MFEb[tmplb,1]<-sum(!is.na(FP[Subject==i,1]))
		MFEb[tmplb,2:8]<-colMeans(FP[Subject==i,],na.rm=T)
		tmplb<-tmplb+1
	} else {
		MFEc[tmplc,1]<-sum(!is.na(FP[Subject==i,1]))
		MFEc[tmplc,2:8]<-colMeans(FP[Subject==i,],na.rm=T)
	tmplc<-tmplc+1
	}
}

### Number of Frames per experiment
NF<-rbind(cbind(MFEb[,1],rep(0,13)),cbind(MFEc[,1],rep(1,13)))
quartz()
boxplot(NF[,1]~NF[,2],col=c("lightblue", "pink"),names=c("B","C"),xlab="Group",ylab="Number of available frames")
points(rep(1,13), NF[1:13,1],pch=16,col="black")
points(rep(2,13), NF[14:26,1],pch=16,col="black")
title("Length of available frames per Group")

B<-cbind(c(MFEb[,2:8]),rep(1:7,each=13),rep(0,7*13))
C<-cbind(c(MFEc[,2:8]),rep(1:7,each=13),rep(1,7*13))
MFE<-rbind(B,C)
boxplot(MFE[,1]~MFE[,3]*MFE[,2],col=c("green","red"))

TFEb<-matrix(NA, 13 ,8) ### Total Facial Emotions for Batch (col 1 is # of frames and then 7 emotion in # of frames)
TFEc<-matrix(NA, 13,8)	### Total Facial Emotions for Continuous (col 1 is # of frames and then 7 emotion in # of frames)
TFEb[,1]<-MFEb[,1]
TFEb[,2:8]<-MFEb[,2:8]*MFEb[,1]
TFEc[,1]<-MFEc[,1]
TFEc[,2:8]<-MFEc[,2:8]*MFEc[,1]

TB<-cbind(c(TFEb[,2:8]),rep(1:7,each=13),rep(0,7*13))
TC<-cbind(c(TFEc[,2:8]),rep(1:7,each=13),rep(1,7*13))
TFE<-rbind(TB,TC)
boxplot(TFE[,1]~TFE[,3]*TFE[,2],col=c("green","red"))



TFEper<-rbind(colSums(TFEb[,2:8]), colSums(TFEc[,2:8]))
chisq.test(TFEper)


quartz()
interaction.plot(TFE[,3],TFE[,2],TFE[,1], type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotion",xlab="Group", ylab="Mean of Frames", main="Interaction plot of mean X per levels of factors A and B")

fit0<-aov(TFE[,1]~ factor(TFE[,2])*factor(TFE[,3]))
fit0
summary(fit0)	### anova table

### BOX-COX
quartz()
boxcox(fit0)

fit<-aov(log(TFE[,1])~ factor (TFE[,2])* factor(TFE[,3]))
fit
summary(fit)	### anova table

### Test the normality assumption in the residuals
quartz()
qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit$residuals)


### Test the assumption of homogeneity of variances
bartlett.test(log(TFE[,1])~factor(TFE[,2]))
bartlett.test(log(TFE[,1])~factor(TFE[,3]))
fligner.test(log(TFE[,1])~factor(TFE[,2]))
fligner.test(log(TFE[,1])~factor(TFE[,3]))
leveneTest(log(TFE[,1])~factor(TFE[,2])*factor(TFE[,3]))


### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit)



fit1<-aov(log(TFE[,1])~ factor (TFE[,2])+factor(TFE[,3]))
fit1
summary(fit1)	### anova table


### Test the normality assumption in the residuals
quartz()
qqnorm(fit1$residuals,main="NPP for residuals")
qqline(fit1$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit1$residuals)


### Test the assumption of homogeneity of variances
bartlett.test(log(TFE[,1])~factor(TFE[,2]))
bartlett.test(log(TFE[,1])~factor(TFE[,3]))
fligner.test(log(TFE[,1])~factor(TFE[,2]))
fligner.test(log(TFE[,1])~factor(TFE[,3]))


### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit1)



#########################################
### MULTIPLE COMPARISONS ad-hoc to ANOVA
#########################################

### Error Plot (CI of means)
#############################

fit1<-aov(log(TFE[,1])~ factor(TFE[,2])+factor(TFE[,3]))
fit1
summary(fit1)	### anova table

# Plot Means with Error Bars
library(gplots)
quartz()
plotmeans(log(TFE[,1])~factor(TFE[,2]),xlab="", ylab="", main="Mean plot with 95% CI")
quartz()
plotmeans(log(TFE[,1])~factor(TFE[,3]),xlab="", ylab="", main="Mean plot with 95% CI")

### Tukey HSD method
TukeyHSD(fit1)


### p-value adjusted pairwise t-tests
pairwise.t.test(log(TFE[,1]),factor(TFE[,2]),p.adjust.method="bonferroni")
pairwise.t.test(log(TFE[,1]),factor(TFE[,3]),p.adjust.method="bonferroni")
pairwise.t.test(log(TFE[,1]),factor(TFE[,2]),p.adjust.method="holm")
pairwise.t.test(log(TFE[,1]),factor(TFE[,3]),p.adjust.method="holm")


###Least Significant Differences Method
DFE<-fit1$df.residual
MSE<-deviance(fit1)/DFE
library(agricolae)
print(LSD.test(log(TFE[,1]),factor(TFE[,2]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni"))
LSD.test(log(TFE[,1]),factor(TFE[,2]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni")$groups
print(LSD.test(log(TFE[,1]),factor(TFE[,3]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni"))
LSD.test(log(TFE[,1]),factor(TFE[,3]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni")$groups

### Scheffe's Method
DFE<-fit1$df.residual
MSE<-deviance(fit1)/DFE
library(agricolae)
print(scheffe.test(log(TFE[,1]),factor(TFE[,2]), DFerror=DFE, MSerror=MSE))
scheffe.test(log(TFE[,1]),factor(TFE[,2]), DFerror=DFE, MSerror=MSE)$groups
print(scheffe.test(log(TFE[,1]),factor(TFE[,3]), DFerror=DFE, MSerror=MSE))
scheffe.test(log(TFE[,1]),factor(TFE[,3]), DFerror=DFE, MSerror=MSE)$groups

