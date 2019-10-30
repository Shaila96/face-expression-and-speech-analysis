### This is the code used for the CHI-2020 paper on the dual task where we examine
### the two groups (Batch versus Continuous) email interruption of their work.
### The recorded response are seven emotions, where the algorithm provides
### the percent of certainty that the face has a particular emotion. These are:
### {1=Angry, 2=Disgusted, 3=Afraid, 4=Happy, 5=Sad, 6=Surprised, 7=Neutral}

setwd("/Users/pt/CPL/CHI2020_email_interruption/DATA/")

library(car)
library(MASS)

tmpDb<-read.csv("B_facs.csv",header=T,sep=",")		### The "Batch" group data
tmpDc<-read.csv("C_facs.csv",header=T,sep=",")		### The "Continuous" group data
tmpGl<-read.csv("Glasses.csv",header=T,sep=",")		### The data of whether subjects wear glasses or not
tmpD<-rbind(tmpDb,tmpDc)
Subject<-tmpD[,1]
table(Subject)
Group<-tmpD[,2]
levels(Group)[1:2]<-"B"
levels(Group)[2:3]<-"C"
table(Group)
table(Group, Subject)
FP<-tmpD[14:20]

SubB<-c("T016","T021","T051","T077","T079","T083","T085","T106","T139","T144","T166","T175","T178")
SubC<-c("T005","T063","T064","T068","T092","T094","T098","T099","T121","T124","T132","T151","T157")
SubID<-c(SubB,SubC)

### Here we estimate the mean emotion per subject and the numbers of frames we have for this video
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


### Graphical Interpretation of the weighted LS (WLS) for educational use.
### See the differences it makes in the line when we move from OLD to a 
### WLS where data point at x=9 receives a very small or a vary big weight.)
tmpx<-1:10
tmpy<-c(1:8,6,12)
tmpw9l<-c(rep(1,8),0.01,1)
tmpw9h<-c(rep(1,8),100,1)
plot(tmpx,tmpy)
abline(lm(tmpy~tmpx),lty=1)
abline(lm(tmpy~tmpx,weights=tmpw9l),col="blue",lty=2)
abline(lm(tmpy~tmpx,weights=tmpw9h),col="red",lty=3)
legend("topleft",legend=c("OLS","WLS with low weight for observation at x=9","WLS with high weight for observation at x=9"), lty=c(1,2,3), col=c("black","blue","red"))

### Number of Frames per experiment
NF<-rbind(cbind(MFEb[,1],rep(0,13)),cbind(MFEc[,1],rep(1,13)))
quartz()
boxplot(NF[,1]~NF[,2],col=c("lightblue", "pink"),names=c("B","C"),xlab="Group",ylab="Number of available frames")
points(rep(1,13), NF[1:13,1],pch=16,col="black")
points(rep(2,13), NF[14:26,1],pch=16,col="black")
title("Length of available frames per Group")
tableNF<-rbind(MFEb[,1],MFEc[,1])
tableNF



### We will create the MFE matrix which will have
### Column 1 = Mean Percent of Emotion in the entire video clip of Dual Task (DT). This is the response variable of interest
### Column 2 = The emotion we refer to for the current line (emotions 1-7) 
### Column 3 = Group of subjects (0=Batch and 1=Continuous)
### Column 4 = Subject number (running from 1-26)
### Column 5 = Number of frames used for this mean % of emotion calculation (we will use this as weights in regression) 
### Column 6 = Indicator of whether the subject wears glasses on not (0=no glasses and 1=with glasses)


B<-cbind(c(MFEb[,2:8]),rep(1:7,each=13),rep(0,7*13),rep(1:13,7),rep(MFEb[,1],7),rep(tmpGl[1:13,3],7))
C<-cbind(c(MFEc[,2:8]),rep(1:7,each=13),rep(1,7*13),rep(14:26,7),rep(MFEc[,1],7),rep(tmpGl[14:26,3],7))

MFE<-rbind(B,C)

summary(aov(log(MFE[,1])~MFE[,6]*MFE[,2]))

boxplot(MFE[,1]~MFE[,3]*MFE[,2],col=c("green","red"))

MP<-MFE[,1]				### Response: Mean Percent of Emotion
E<-factor(MFE[,2])		### Emotion: categorical with values 1,2,...,7
G<-factor(MFE[,3])		### Group: categorical with 0=Batch and 1=Continuous
Gl<-factor(MFE[,6])		### Glasses: categorical with 0=No and 1=Yes
W<-MFE[,5]					### Weights used in the weighted linear regression (total number of frames per subject)
NamesE<-c("Angry","Disgusted","Afraid","Happy","Sad", "Surprised","Neutral")
### Emotions: (1=Angry, 2=Disgusted, 3=Afraid 4=Happy 5=Sad 6=Surprised 7=Neutral)

### Here we will do the t-tests per emotion to examine whether there are significant groups versus the explanatory variable
### We will consider as explanatory variable the Group (B vs C)

quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MP[E==emot])
	tmpX<-G[E==emot]
	tmptest<-t.test(tmpY~tmpX)
	print(paste(NamesE[emot]))
	print(tmptest)	
	boxplot(tmpY~tmpX,names=c("B","C"),col=c("green","red"),main=paste(NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Log of Mean % of Emotions versus the Group (B/C)",col.main="blue",outer=T,line=-1)


### Repeat the above analysis when we exclude subjects 19 (T094) and 23 (T124) from the continuous group
### along with subject 9 (T139) from Batch group as they have too few frames (less than 200).
quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MFE[((MFE[,4]!=9)&(MFE[,4]!=19)&(MFE[,4]!=23)&(E==emot)),1])
	tmpX<-MFE[((MFE[,4]!=9)&(MFE[,4]!=19)&(MFE[,4]!=23)&(E==emot)),3]
	tmptest<-t.test(tmpY~tmpX)
	print(paste(NamesE[emot]))
	print(tmptest)	
	boxplot(tmpY~tmpX,names=c("B[12]","C[11]"),col=c("green","red"),main=paste(NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Log of Mean % of Emotions versus the Group (B/C) excluding T094 and T124 from C and T139 form B", col.main="blue", outer=T,line=-1)


### Here we will do the t-tests per emotion to examine whether there are significant groups versus the explanatory variable
### We will consider as explanatory variable the wearing glasses variable (No/Yes).

quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MP[E==emot])
	tmpX<-Gl[E==emot]
	tmptest<-t.test(tmpY~tmpX)	
	print(paste(NamesE[emot]))
	print(tmptest)	
	boxplot(tmpY~tmpX,names=c("No G","Yes G"),col=c("blue","pink"),main=paste(NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Log of Mean % of Emotions versus the Glasses (No/Yes)",col.main="blue",outer=T,line=-1)



### Repeat the above analysis when we exclude subjects 19 (T094) and 23 (T124) from the continuous group
### along with subject 9 (T139) from Batch group as they have too few frames (less than 200).
quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MFE[((MFE[,4]!=9)&(MFE[,4]!=19)&(MFE[,4]!=23)&(E==emot)),1])
	tmpX<-MFE[((MFE[,4]!=9)&(MFE[,4]!=19)&(MFE[,4]!=23)&(E==emot)),6]
	tmptest<-t.test(tmpY~tmpX)
	print(paste(NamesE[emot]))
	print(tmptest)	
	boxplot(tmpY~tmpX,names=c("No G","Yes G"),col=c("blue","pink"),main=paste(NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Log of Mean % of Emotions versus the Group (B/C) excluding T094 and T124 from C and T139 form B", col.main="blue", outer=T,line=-1)




### Do the analysis for the of % mean emotion versus glasses for each group separetely
### BATCH GROUP
quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MP[(E==emot)&(G==0)])
	tmpX<-Gl[(E==emot)&(G==0)]
	tmptest<-t.test(tmpY~tmpX)
	print(paste(NamesE[emot]))
	print(tmptest)		
	boxplot(tmpY~tmpX,names=c("No G","Yes G"),col=c("blue","pink"),main=paste("Batch", NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Batch Group: Log of Mean % of Emotions versus the Glasses (No/Yes)",col.main="blue",outer=T,line=-1)
### CONTINUOUS GROUP
quartz()
par(mfrow=c(3,3))
for (emot in 1:7){
	tmpY<-log(MP[(E==emot)&(G==1)])
	tmpX<-Gl[(E==emot)&(G==1)]
	tmptest<-t.test(tmpY~tmpX)	
	print(paste(NamesE[emot]))
	print(tmptest)	
	boxplot(tmpY~tmpX,names=c("No G","Yes G"),col=c("blue","pink"),main=paste("Continuous", NamesE[emot], " \n p-value =", round(tmptest$p.value,3)))
	}
title("Continuous Group: Log of Mean % of Emotions versus the Glasses (No/Yes)",col.main="blue",outer=T,line=-1)




### Here we do the interaction plot

quartz()
interaction.plot(G,E,log(MP), type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotion",xlab="Group", ylab="Mean of Frames", main="Interaction plot of log Mean % Emotion with Group (7 emotions)")

### Here we will fit the weighted Least Squares Model for Percent Mean Emotion on Group

fit<-lm(MP~G+E+G:E,weights=W)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit)
### BOX-COX
quartz()
boxcox(fit)

fit<-lm(log(MP)~G+E+G:E,weights=W)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit)

summary(fit)
anova(fit)


### Here we will remove emotions 2 (Disgusted) 4 (Happy) and 6 (Suprised) that we did not see often
E246ind<-(E==2)|(E==4)|(E==6)
MP1<-MP[E246ind==0]
E1<-E[E246ind==0]
G1<-G[E246ind==0]
W1<-W[E246ind==0]
Gl1<-Gl[E246ind==0]


### Here is the interaction plot
quartz()
interaction.plot(G1,E1,log(MP1), type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotion",xlab="Group", ylab="Mean of Frames", main="Interaction plot of log Mean % Emotion with Group (4 emotions)")



fit1<-lm(MP1~G1+E1+G1:E1,weights=W1)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit1)
### BOX-COX
quartz()
boxcox(fit1)

fit1<-lm(log(MP1)~G1+E1+G1:E1,weights=W1)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit1)

summary(fit1)
anova(fit1)






#### REPLACE THE VARIABLE GROUP BY VARIABLE GLASSES

quartz()
interaction.plot(Gl,E,log(MP), type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotion",xlab="Glasses", ylab="Mean of Frames", main="Interaction plot of log Mean % Emotion with Glasses (7 emotions)")

### Here we will fit the weighted Least Squares Model for Percent Mean Emotion on Glasses

fit<-lm(MP~Gl+E+Gl:E,weights=W)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit)
### BOX-COX
quartz()
boxcox(fit)

fit<-lm(log(MP)~Gl+E+Gl:E,weights=W)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit)

summary(fit)
anova(fit)


### Here we will remove emotions 2 (Disgusted) 4 (Happy) and 6 (Suprised) that we did not see often
E246ind<-(E==2)|(E==4)|(E==6)
MP1<-MP[E246ind==0]
E1<-E[E246ind==0]
G1<-G[E246ind==0]
W1<-W[E246ind==0]
Gl1<-Gl[E246ind==0]

quartz()
interaction.plot(Gl1,E1,log(MP1), type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotion",xlab="Glasses", ylab="Mean of Frames", main="Interaction plot of log Mean % Emotion with Glasses (4 emotions)")


fit1<-lm(MP1~Gl1+E1+Gl1:E1,weights=W1)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit1)
### BOX-COX
quartz()
boxcox(fit1)

fit1<-lm(log(MP1)~Gl1+E1+Gl1:E1,weights=W1)
### Diagnostic Plots
quartz()
layout(matrix(1:4,2,2))
plot(fit1)

summary(fit1)
anova(fit1)




