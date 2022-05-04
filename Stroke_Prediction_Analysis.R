data = read.csv("stroke.csv")
attach(data)
#variables processing
data$sex = ifelse(data$gender == 'Female',1,0)
data$marriage = ifelse(data$ever_married == 'Yes',1,0)
data$private_work = ifelse(data$work_type == 'Private',1,0)
data$self_employed = ifelse(data$work_type == 'Self-employed',1,0)
data$Govt_job = ifelse(data$work_type == 'Govt_job',1,0)
data$children = ifelse(data$work_type == 'children',1,0)
data$urban = ifelse(data$Residence_type == 'Urban',1,0)
data$non_smoking = ifelse(data$smoking_status == 'never smoked',1,0)
data$smokes = ifelse(data$smoking_status == 'smokes',1,0)
data$formerly_smoked = ifelse(data$smoking_status == 'formerly smoked',1,0)
data$outcome = ifelse(data$stroke == '1', 'stroke','not_stroke')
attach(data)

#########################################data analysis#########################################
quantvars=data[, c(3,4,5,9,10,12,13,14,15,16,17,18,19,20,21,22)]   ##column numbers
corr_matrix=cor(quantvars)
round(corr_matrix,2)

#figure1
library(ggplot2) 
box1=ggplot(data, aes(x=stroke, y=age))+geom_boxplot()
box2=ggplot(data, aes(x=stroke, y=avg_glucose_level))+geom_boxplot()
box3=ggplot(data, aes(x=stroke, y=bmi))+geom_boxplot()

library(gridExtra)
grid.arrange(box1,box2,box3,ncol=3, nrow = 1)

#figure2
# Grouped Bar Plot
counts <- table(data$stroke, data$smoking_status)
bar1=barplot(counts, main="stroke distribution",
             xlab="Number of smoking_status", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$work_type)
bar2=barplot(counts, main="stroke distribution",
             xlab="Number of work_type", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$Residence_type)
bar3=barplot(counts, main="stroke distribution",
             xlab="Number of Residence_type", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$gender)
bar4=barplot(counts, main="stroke distribution",
             xlab="Number of gender", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$ever_married)
bar5=barplot(counts, main="stroke distribution",
             xlab="Number of ever_married", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$heart_disease)
bar5=barplot(counts, main="stroke distribution",
             xlab="Number of heart_disease", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)

counts <- table(data$stroke, data$hypertension)
bar5=barplot(counts, main="stroke distribution",
             xlab="Number of hypertension", col=c("darkblue","red"),
             legend = rownames(counts), beside=TRUE)


library(corrplot)
library(reshape2)
#figure3
#reference:
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_matrix, method="color", col=col(200),  
         type="upper", order="hclust", 
         #addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", #tl.srt=45, #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#########################################model building#########################################
##############supervised learning
###grow a tree
library(tree)
library(rpart)
library(rpart.plot)
myoverfittedtree=rpart(outcome~age+hypertension+heart_disease+ever_married+avg_glucose_level+bmi+gender+work_type
                       +Residence_type+smoking_status,cp=0.000001, na.action=na.omit)
printcp(myoverfittedtree)
plotcp(myoverfittedtree)
opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,'xerror']),"CP"]
mybesttree=rpart(outcome~age+hypertension+heart_disease+ever_married+avg_glucose_level+bmi+gender+work_type
                 +Residence_type+smoking_status,cp=0.0047, na.action=na.omit)
printcp(mybesttree)
rpart.plot(mybesttree)
summary(mybesttree)

###random forest
library(randomForest)
set.seed(1)
myforest=randomForest(as.factor(outcome)~age+hypertension+heart_disease+ever_married+avg_glucose_level+bmi+gender+work_type
                      +Residence_type+smoking_status,
                      ntree=10000, data=data, importance=TRUE,  na.action = na.omit, do.trace=1000)
#oob error rate = 4.36%
myforest
varImpPlot(myforest)
importance(myforest)

predict(myforest,data.frame(age=30,hypertension=1,heart_disease=1,ever_married='Yes' ,avg_glucose_level=105.92,bmi=31, gender='Male', work_type='Private',
                            Residence_type='Urban', smoking_status='smokes'))

predict(myforest,data.frame(age=8,hypertension=0,heart_disease=0,ever_married='Yes' ,avg_glucose_level=105.92,bmi=25, gender='Male', work_type='Private',
                            Residence_type='Urban', smoking_status='smokes'))


################unsupervised learning
#Principal Component Analysis
data_vars=data[,c(3,4,5,9,10,13,14,15,16,17,18,19,20,21,22)]
library(ggplot2)
library(GGally)
ggpairs(data_vars)
pca=prcomp(data_vars, scale=TRUE)
pca
data_labels = data[,c(12)] 
library(ggfortify)
autoplot(pca, data =data_vars, loadings = TRUE, 
         col=ifelse(data_labels == '1' ,"green","blue"), 
         loadings.label = TRUE)





