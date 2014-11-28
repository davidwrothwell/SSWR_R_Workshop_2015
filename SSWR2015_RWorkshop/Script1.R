


setwd("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop")
library(xtable)
library(ggplot2)
library(reshape2)
require(grid)


# Example 1: table of proportions
## using latex to print table
TrocmeTab2 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/TrocmeTab2.txt")
colnames(TrocmeTab2) <- c("Placement status", "Aboriginal (%)", "Caucasian (%)")
Table1 <- xtable(TrocmeTab2, align="llcc")
print(Table1, include.rownames=FALSE, floating=FALSE,hline.after=c(-1,0,4,nrow(Table1)))

## mosaic plot
cis <- c(82,93,32,623,164,121,86,3192)
dim(cis)<-c(4,2)
dimnames(cis)<- list(Placement= c("p", "inf", "con", "no"), Ethnicity = c("Aboriginal", "Caucasian"))
mosaicplot(cis, col=hcl(240, 120), main="Placement by Ethnicity")

# Example 2: simple descriptive table
## using latex to print basic table
library(xtable)
HollandTab3 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/HollandTab3.txt")
colnames(HollandTab3) <- c("Subscale and Range", "Abbreviated item and Content", "M(+-SD)", "Item-total correlation")
Table2 <- xtable(HollandTab3, align="lllcc")
print(Table2, include.rownames=FALSE, floating=FALSE)

## ggplot example
library(ggplot2)
df10 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/df10.txt")
#df10<-data.frame(name=var.names.order,mean=mean.vec.order,sd=sd.vec.order) #data of interest
#df10<-df10[order(nrow(df10):1),] #Invert the rows of dataset
df10$Variable<-seq(1,nrow(df10)) # Numbering index
###### Now ggplot displays the rows in this order: 7,6,5,4,3,2,1
ggplot(df10,aes(x=Variable))+
  geom_pointrange(aes(y=mean,ymin=mean-sd,ymax=mean+sd),size=1)+
  scale_x_discrete(labels=df10$name)+coord_flip()+
  theme_bw()+ylab("Score")+ theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())

# Example 3: regression table
trocmetab6 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/trocmetab6.txt")
Table3 <- xtable(trocmetab6, align="llccc")
print(Table3, include.rownames=FALSE, floating=FALSE)

## dotplot regression
tr6 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/tr6.txt")
library(lattice)
dotplot(ord.variable ~ OR, data = tr6, main= "Predictors of Child Welfare Placement (N = 2891)",
        aspect = .9,
        xlab = list("Odds ratios",
                    cex = .75),
        scales = list(cex = .75),
        panel = function(x, y) {
          panel.abline(h = as.numeric(y), col="gray", lty=2)
          panel.xyplot(x[tr6$pvalue < .05],
                       y[tr6$pvalue < .05],
                       cex = 1.25, pch = 16, col = "black")
          panel.xyplot(x[tr6$pvalue >= .05],
                       y[tr6$pvalue >= .05],
                       cex = 1.25, pch = 1, col = "black")
          panel.abline(v = 1, lty = 2, col = "gray")
        },
        key = list(text = list(c(" Significant, p < 0.05:",
                                 "Not significant, p < .05 level:"), cex = .75),
                   points = list(pch = c(16, 1), col = "black", cex = .75),
                   space = "top", border = T) )
###### dotplot regression sorted by magnitude

###### Create significance variable
tr6$signif<-factor(ifelse(tr6$pvalue<=.05,1,0))
tr6<-tr6[order(-tr6$OR),] # dataset arranged from highest OR to lowest OR
######See if we index from 1 to n, the highest OR is the first on the x/y axis
######If we want the lowest OR on the first tick for x/y axis, reverse the order
tr6<-tr6[order(tr6$OR),] #Now data displays lowest to highest OR
tr6$Variable<-seq(1,nrow(tr6)) #Index 
ggplot(tr6,aes(x=OR,y=Variable))+theme_bw()+
  geom_point(aes(pch=signif))+
  geom_vline(x=1,color="grey30",linetype='dashed')+
  scale_y_discrete(labels=tr6$ord.variable)+
  scale_shape_manual(breaks=c(1,0),values=c(16,1),name="Significance",
                     labels=c("Significant: p < .05","Not significant: p >.05"))+
  theme(legend.direction="vertical",legend.position="top",legend.key=element_blank(),
        legend.key.height=unit(.5,"line"),
        panel.grid.major=element_line(color="grey50",linetype='dashed'))


# Example 4: comparing multiple models
## first the table
mergeshio3 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/mergeshio3.txt", header=FALSE)
Table4 <- xtable(mergeshio3)
print(Table4, include.rownames=FALSE, floating=FALSE)

## then the coefplot
################## load ggplot
library(ggplot2)
########### 1. step 1 load data
########### 2. step 2 clean data and order factors
########### 3. Specify the width of your confidence intervals based on normal distribution
interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier
########### save a new datafile
###########rm(allModelFrame1,allModelFrame2)
allModelFrame1 <- read.delim("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/allModelFrame1.txt", header=TRUE)
allModelFrame2 <- allModelFrame1
########### reorder the data
allModelFrame2$ord.model <- factor(allModelFrame2$model, levels=c("m3", "m2", "m1"), ordered =TRUE)
zp2 <- ggplot(allModelFrame2, aes(colour = ord.model)) + 
  scale_colour_discrete(name  ="Model",
                        breaks=c("m1", "m2", "m3"),
                        labels=c("Model1", "Model2", "Model3")) +
  scale_shape_discrete(name  ="Model",
                       breaks=c("m1", "m2", "m3"),
                       labels=c("Model1", "Model2", "Model3")) 
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp2 <- zp2 + geom_linerange(aes(x = ord.variable, ymin = Coef - SE*interval1,
                                ymax = Coef + SE*interval1),
                            lwd = 1,position = position_dodge(width = 1/2))
zp2 <- zp2 + geom_pointrange(aes(x = ord.variable, y = Coef, ymin = Coef - SE*interval2,
                                 ymax = Coef + SE*interval2),
                             lwd = .8, position = position_dodge(width = 1/2),
                             shape = 21, fill = "BLACK")
zp2 <- zp2  + coord_flip() +theme_bw()
zp2 <- zp2 + ggtitle("Replication Shiovitz et al 2010, Table 3") +  xlab("Variables") +
  ylab("Odds Ratios") # The trick to these is position_dodge().
print(zp2)

# save the workspace
save.image("~/My R Files/RA017_SWDataVisual/Presentation/SSWR2015_RWorkshop/graphreplications.RData")

