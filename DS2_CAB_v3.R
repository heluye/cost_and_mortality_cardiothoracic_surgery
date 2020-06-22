# DS2_CAB_v3
# In cooperate changes in defining pre-op and post-op variables.
# Merge with dataset to get VUC.



############################## load data #####################

###check path
getwd()
setwd('C:/Users/heluy/OneDrive/Documents/Class/ISYE 6478 Practicum')
getwd()

###load as csv
mydata = read.csv("STS_Dataset_DEIDENTIFIED_v2.csv")

data.input2= read.csv("PPRRVU17_V1219.csv",skip=9)[,c(1,2,3,6)]
data.input2$CPT=paste(data.input2$HCPCS,data.input2$MOD,sep="")
dim(data.input2)

str(data.CPT)
str(test)

###### install all packages used

library(dplyr)
library(glmnet)
library(plotmo) # for plotres




# names(mydata)
# str(mydata)
# 
# nrow(mydata)
# ncol(mydata)
# nrow(mydata)
# 
# unique(mydata$Procedure.Type)
# summary(mydata$Procedure.Type)
# str(mydata$Procedure.Type)
# 
# 
# str(df,list.len=ncol(df))
# test=which(mydata$Procedure.Type==2)
# test=which(mydata[mydata$Procedure.Type==2,])


######select for CAB ######

#don't know why this is not working
#data=mydata[mydata$Procedure.Type==2,] 
data=mydata[which(mydata$Procedure.Type==2),]

# unique(data$Intraop.Blood.Products)
# unique(data$Intraop.Blood.Products...Cryo.Units)
# unique(data$Intraop.Blood.Products...FFP.Units)
# unique(data$Intraop.Blood.Products...Platelet.Units)
# str(data$Intraop.Blood.Products...Platelet.Units)
# sum((data$Intraop.Blood.Products...Platelet.Units==2)==TRUE)
# data$Intraop.Blood.Products...Platelet.Units[7]==1
# unique(data$Intraop.Blood.Products...RBC.Units)
# unique(data$Intraop.Blood.Products.Refused)
# unique(data$Intraop.Blood.Products)
#data3=subset(mydata, Procedure.Type==2)

# unique(data$Procedure.Type)
# unique(data$op.Type)
# 
# #some stats on null values
# a=apply(data, 2, function(x) 1-sum(is.na(x))/nrow(data))
# a
# hist(a)
# length(a)
# 
# 
# b=apply(data, 1, function(x) 1-sum(is.na(x))/ncol(data))
# b
# hist(b)
# length(b)


############ most conservative ##########

#### select only full 





# ###revmove columns that are completely empty
# 
df = data[,colSums(is.na(data))/nrow(data)!=1]

str(df,list.len=ncol(df))
# #df1 = data[,which(colSums(is.na(data))/nrow(data)!=1)]
# str(df)
# 
# c=apply(df, 2, function(x) 1-sum(is.na(x))/nrow(df))
# c
# hist(c)
# length(c)
# 
# 
# d=apply(df, 1, function(x) 1-sum(is.na(x))/ncol(df))
# d
# hist(d)
# length(d)



# ###revmove columns that less than 75 complete
# df75 = data[,1-colSums(is.na(data))/nrow(data)>0.75]
# str(df75)
# 
# c75=apply(df75, 2, function(x) 1-sum(is.na(x))/nrow(df75))
# c75
# hist(c75)
# length(c75)
# 
# d75=apply(df75, 1, function(x) 1-sum(is.na(x))/ncol(df75))
# d
# hist(d75)
# length(d75)
# 
# str(df75)


### data conversion

#some tests
# unique(df75$Hispanic.or.Latino.or.Spanish.Ethnicity)
# test=factor(df75$Hispanic.or.Latino.or.Spanish.Ethnicity)
# as.character(test)
# as.numeric(test)
# test2=as.numeric(as.character(df75$Hispanic.or.Latino.or.Spanish.Ethnicity))
# 
# test[is.na(test)]=0
# df75.copy=df75
# df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity[is.na(df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity)]=0
# 
# df75.copy
# unique(df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity)
# test3=factor(df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity)
# as.character(test3)
# as.numeric(test3)
# test4=as.numeric(as.character(df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity))




# sapply(df75, class)


# ### change type to factor
# #### don't use this, otherwise can't remove NA value in next section
# df75.copy=df75
# typeSet=c()
# for(i in names(df75)) {
#   # i-th element of `u1` squared into `i`-th position of `usq`
#   feature=df75[[i]]
#   type=class(feature)
#   typeSet=c(typeSet,type)
#   
#   print(i)
#   print(type)
#   print(length(unique(feature)))
#   print('')
#   
#   if (type=='integer'&& length(unique(feature))>10 ){
#     df75.copy[[i]]=as.numeric(feature)
#   }else if (length(unique(feature))<=10 ){
#     df75.copy[[i]]=as.factor(feature)
#   }
# }


########### change data type to character and numeric

df.copy=df
typeSet=c()
for(i in names(df)) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  feature=df[[i]]
  type=class(feature)
  typeSet=c(typeSet,type)

  print(i)
  print(type)
  print(length(unique(feature)))
  print('----------------------------------')

  if (type=='integer'&& length(unique(feature))>10 ){
    df.copy[[i]]=as.numeric(feature)
  }else if (length(unique(feature))<=10 ){
    df.copy[[i]]=as.character(feature)
  }
}

#####check the original feature data type
str(df,list.len=ncol(df))
#####check the new feature data type
str(df.copy,list.len=ncol(df.copy))



# length(unique(df75$ Other.Replacement.Approaches))
# length(unique(df75$ Other.Card.Other.Describe))
# 
# unique(df75.copy$Other.Card.Other.Describe)
# unique(df75.copy$X..Hospital.Deaths)
# unique(df75.copy$Hispanic.or.Latino.or.Spanish.Ethnicity)
# unique(df75.copy$Deidentified.Record.ID)
# unique(df75.copy$Surgeon)
# unique(df75.copy$ RF.Hemoglobin)
# unique(df75.copy$ CPT.1.Code...2)

##################### manually change some features







###############change some features to character
unique(df.copy$CPT.1.Code...1)
unique(df.copy$CPT.1.Code...2)
unique(df.copy$CPT.1.Code...3)

df.copy$CPT.1.Code...1=as.character(df$CPT.1.Code...1)
df.copy$CPT.1.Code...2=as.character(df$CPT.1.Code...2)
df.copy$CPT.1.Code...3=as.character(df$CPT.1.Code...3)
df.copy$CPT.1.Code...4=as.character(df$CPT.1.Code...4)
df.copy$CPT.1.Code...5=as.character(df$CPT.1.Code...5)
df.copy$CPT.1.Code...6=as.character(df$CPT.1.Code...6)
df.copy$Surgeon=as.character(df$Surgeon)

unique(df.clean$CPT.1.Code...1)
###############change some features to numeric

df.copy$Deidentified.Patient.Age=as.numeric(df$Deidentified.Patient.Age)
df.copy$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP=as.numeric(df$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)


# str(df.copy$Deidentified.Patient.Age)
# str(df.copy$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)

############## merge two data frame

data.CPT=df.copy[c('CPT.1.Code...1','CPT.1.Code...2','CPT.1.Code...3','CPT.1.Code...4','CPT.1.Code...5','CPT.1.Code...6')]
data.input2$CPT=as.character(data.input2$CPT)
dim(data.input2)

str(data.CPT)

dim(data.CPT)
dim(df.copy)
dim(df.copy)

str(data.CPT)
str(data.RVU)

dim(data.CPT)
dim(data.input2)
data.join=left_join(data.CPT, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...1"="CPT"))
dim(data.join)
data.join=left_join(data.join, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...2"="CPT"))
dim(data.join)
data.join=left_join(data.join, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...3"="CPT"))
dim(data.join)
data.join=left_join(data.join, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...4"="CPT"))
dim(data.join)
data.join=left_join(data.join, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...5"="CPT"))
dim(data.join)
data.join=left_join(data.join, data.input2[c('CPT','RVU','DESCRIPTION')], by = c("CPT.1.Code...6"="CPT"))
dim(data.join)

variables.RVU=c(grep("RVU", names(data.join), value=TRUE))
variables.RVU

# data.join2=merge(data.join, data.input2, all.x=TRUE,by.x = "CPT.1.Code...6", by.y = "HCPCS",no.dups = TRUE)
# dim(data.join2)
# 
# data.join2[which(data.join2$CPT.1.Code...6=="93631"),]
# 
# 
# unique(data.CPT$CPT.1.Code...5)
# 
# dim(data.RVU)
data.RVU=data.join[variables.RVU]
data.RVU[is.na(data.RVU)] = 0
sumRVU=rowSums (data.RVU, na.rm = FALSE, dims = 1)

#add to data set
df.copy$sumRVU=sumRVU

# variables.description=c(grep("DESCRIPTION", names(data.join), value=TRUE))
# variables.description
# 
# data.description=data.join[variables.description]
# colnames(data.description) = c("CPT.DESCRIPTION.1.", "CPT.DESCRIPTION.2.","CPT.DESCRIPTION.3.",
#                                "CPT.DESCRIPTION.4.","CPT.DESCRIPTION.5.","CPT.DESCRIPTION.6.")
# 
# ###
# df.copy$CPT.DESCRIPTION.1.=as.factor(data.description$CPT.DESCRIPTION.1.)

###############remove some features
df.copy$DRG..=NULL
df.copy$Op.Category=NULL
df.copy$Other.Card.Other.Describe=NULL

df.copy$CPT.1.Code...1=NULL
df.copy$CPT.1.Code...2=NULL
df.copy$CPT.1.Code...3=NULL
df.copy$CPT.1.Code...4=NULL
df.copy$CPT.1.Code...5=NULL
df.copy$CPT.1.Code...6=NULL





########################################  remove NA values
df.clean=df.copy
for(i in names(df.copy)) {

  feature=df.copy[[i]]
  type=class(feature)
  feature=df.clean[[i]]
  
  print(type)
  #print(unique(feature))
  
  if (type=='numeric'){
    df.clean[[i]][is.na(df.copy[[i]])]=mean(df.copy[[i]][!is.na(df.copy[[i]])])
  }else {
    df.clean[[i]][is.na(df.copy[[i]])]='NA'
    df.clean[[i]]=as.factor(df.clean[[i]])
  }
}


###########check again the feature tyes
#orignal 
str(df.copy,list.len=ncol(df.copy))
#cleaned set
str(df.clean,list.len=ncol(df.clean))
#make sure all NA are removed
sum(is.na(df.clean))


###################check and remove factors that only has 1 level
l=sapply(df.clean, function(x) is.factor(x))
one=sapply(df.clean[l], function(x) length(levels(x)))
sum(one==1)
one
names(which(one==1))

length(unique(df$Procedure.Type))
length(unique(df$Other.Replacement.Approaches))
length(unique(df$Valve))

for(i in names(which(one==1))) {
  feature=df.clean[[i]]
  df.clean[[i]]=NULL
}

sum(names(df.clean) %in% names(which(one==1)))

str(df.clean,list.len=ncol(df.clean))

#set up print 
getOption("max.print")
options(max.print=1000000)


####### predicted variables
#contains key word "predicted"
variables.predicted=grep("Predicted", names(df.clean), value=TRUE)
variables.predicted

###### blood products
# in-op blood products contain "intraop"
# post-op blood products contain "blood prod"
variables.bloodProd=grep("Blood.Prod", names(df.clean), value=TRUE)
variables.bloodProd

###### ventilation 
#cotains key word "Ventilation"
# post-op
variables.ventilation=grep("Ventilation", names(df.clean), value=TRUE)
variables.ventilation

##### length of stay
# contain key word "LOS"
# post-op
variables.LOS=grep("LOS", names(df.clean), value=TRUE)
variables.LOS

##### cost variables
# contain key word "cost"
variables.cost=grep("Cost", names(df.clean), value=TRUE)
variables.cost

#### mortality variables
variables.mortality=grep("Mort", names(df.clean), value=TRUE)
variables.mortality=variables.mortality[variables.mortality!='Avg.APR.Risk.of.Mortality']
variables.mortality


unique(df.clean$Mort.Location)
unique(df.clean$Mort.Prim.Cause)
unique(df.clean$Mort.30d.Status)
unique(df.clean$Mort.DC.Status)
unique(df.clean$Mort.Op.Death)



#### readmission variables
variables.readmission=grep("Readm", names(df.clean), value=TRUE)
variables.readmission
variables.readmission[1:2]
unique(df.clean$Readmit...30.Days.from.DOP)
unique(df.clean$X..30.day.Readmissions)

#### post-op variables
variables.postOp=grep("Post", names(df.clean), value=TRUE)
variables.postOp
variables.postOp
unique(df.clean$In.Hospital.Post.Op.Events)


####### action taken on Aortic
#### contains key word "Aortic
#INTRA-OP
# c(grep("Aort", names(df.clean), value=TRUE))
# "Assessment.of.Aorta.Plaque" is not an action
# "Aortic.Occlusion" is an action

# variables.aortic=c(grep("Aortic.Procedure", names(df.clean), value=TRUE),'Aorta.Procedure.Performed','Aortic.Cross.Clamp...Partial','Aortic.Occlusion')
# variables.aortic


variables.aortic=c(grep("Aortic", names(df.clean), value=TRUE))
variables.aortic

variables.VD=c(grep("VD", names(df.clean), value=TRUE))
variables.VD

variables.RF=c(grep("RF", names(df.clean), value=TRUE))
variables.RF

variables.procedure=c(grep("Procedure", names(df.clean), value=TRUE))
variables.procedure

variables.approach=c(grep("Approach", names(df.clean), value=TRUE))
variables.approach

variables.operative=c(grep("Operative", names(df.clean), value=TRUE))
variables.operative

####### action taken on Aortic
#### contains key word "Cardioplegia"
#INTRA-op
variables.cardioplegia=c(grep("Cardioplegia", names(df.clean), value=TRUE))
variables.cardioplegia

####INTRA-op
variables.cerebral=c(grep("Cerebral", names(df.clean), value=TRUE))
variables.cerebral


#### INTRA-op
variables.circulatory=c(grep("Circulatory", names(df.clean), value=TRUE))
variables.circulatory


### INTRA-op
variables.cannulation=grep("Cannulation", names(df.clean), value=TRUE)
variables.cannulation

### medication
variables.meds=grep("Meds", names(df.clean), value=TRUE)
variables.meds

### CPB, action taken by surgeon
variables.CPB=grep("CPB", names(df.clean), value=TRUE)
variables.CPB

### ction taken by surgeon
variables.clamp=grep("Clamp", names(df.clean), value=TRUE)
variables.clamp


variables.dist=grep("Dist", names(df.clean), value=TRUE)
variables.dist

variables.IABP=grep("IABP", names(df.clean), value=TRUE)
variables.IABP

variables.chest=grep("Chest", names(df.clean), value=TRUE)
variables.chest

variables.discharge=grep("Discharge", names(df.clean), value=TRUE)
variables.discharge


variables.cardiopulmonary=grep("Cardiopulmonary", names(df.clean), value=TRUE)
variables.cardiopulmonary

variables.death=grep("Death", names(df.clean), value=TRUE)
variables.death

variables.otherCard=grep("Other.Card", names(df.clean), value=TRUE)
variables.otherCard=variables.otherCard[variables.otherCard != "Prev.Other.Card.PPM"]
variables.otherCard

variables.combined=grep("Combined", names(df.clean), value=TRUE)
variables.combined



variables.intra=grep("Intra", names(df.clean), value=TRUE)
variables.intra

variables.prev=grep("Prev", names(df.clean), value=TRUE)
variables.prev

variables.pre=grep("Pre", names(df.clean), value=TRUE)
variables.pre

grep("Death", names(df.clean), value=TRUE)
grep("Comps", names(df.clean), value=TRUE)

grep("Other.Card", names(df.clean), value=TRUE)
grep("Non.Card", names(df.clean), value=TRUE)
grep("Death", names(df.clean), value=TRUE)
grep("Comps", names(df.clean), value=TRUE)

str(df.clean,list.len=ncol(df.clean))

#response variables to regress on or irelevant
#post-op: 'Total.Hrs.ICU'
#irelavent: 
variables.other1=c("STS.Data.Version",'Case.Mix.Index','Deidentified.Record.ID','Surgeon','Total.Hrs.ICU',
                   'Comps.Pulm.ARDS','Total.OR.Time','Cardiac.Symptoms...At.Time.Of.Surgery','Hospital.Name')
variables.other1


variables.other2=c("STS.Data.Version",'Case.Mix.Index','Deidentified.Record.ID')
variables.other2


variables.otherAction=c('CAB','Robot.Used',"VAD.Device","Other.Non.Card","IMA.Artery.Used")
# 
# variables.combined
# 
# variables.aortic

# variables.exclude1=union(variables.predicted, variables.bloodProd, variables.ventilation, variables.LOS, 
#                             variables.cost, variables.mortality, variables.readmission, variables.postOp, 
#                             variables.aortic, variables.cardioplegia, variables.cerebral, variables.circulatory, 
#                             variables.cannulation, variables.CPB, variables.clamp, variables.dist,
#                             variables.IABP, variables.chest, variables.discharge, variables.cardiopulmonary,
#                             variables.combined,variables.death,variables.otherCard,variables.intra,
#                             variables.otherAction, variables.other1)
# 
# variables.exclude1

variables.exclude1=unique(c(variables.predicted, variables.bloodProd, variables.ventilation, variables.LOS, 
                           variables.cost, variables.mortality, variables.readmission, variables.postOp, 
                           variables.aortic, variables.cardioplegia, variables.cerebral, variables.circulatory, 
                           variables.cannulation, variables.CPB, variables.clamp, variables.dist,
                           variables.IABP, variables.chest, variables.discharge, variables.cardiopulmonary,
                           variables.combined,variables.death,variables.otherCard,variables.intra,
                           variables.procedure,variables.approach,
                           variables.otherAction, variables.other1))

variables.include1=unique(c(variables.VD, variables.RF, variables.prev))
variables.include1

variables.exclude.preOp=setdiff(variables.exclude1,variables.include1)
variables.exclude.preOp

X.preOp=df.clean[!(names(df.clean) %in% variables.exclude1)]
names(X.preOp)
variables.discharge

str(X.preOp,list.len=ncol(X.preOp))

# 
# variables.intraOpBlood=grep("Intraop.Blood", names(df.clean), value=TRUE)
# variables.intraOpBlood
# 
# variables.CPT=grep("CPT", names(df.clean), value=TRUE)
# variables.CPT



test=unique()
variables.ventilation
variables.postOp
variables.death
variables.discharge

variables.exclude2=unique(c(variables.predicted, variables.LOS, 
                            variables.cost, variables.mortality, variables.readmission, 
                            variables.death,variables.other2))



X.postOp=df.clean[!(names(df.clean) %in% variables.exclude2)]
names(X.postOp)

str(X.postOp,list.len=ncol(X.postOp))

####
# discharge
# Cardiogenic.Shock2 
# MI.When2
# Prior.MI2
# Resuscitation2  
# Meds.Aspirin.Within.Five.Days2
# Meds.Beta.Blocker.Therapy.For.More.Than.2.Weeks.Prior.To.Surgery2 
# Meds.Beta.Blockers.Within.24.Hours2 
# Meds
# Dominance
# Num.Dis.Vessels
# Hemo.Data.EF
# Num.Dis.Vessels2 
# VD
# Appropriate.
# CPB.Utilization2  
# Cross.Clamp.Time..min.
# Incidence
# Lowest.Hematocrit                                                   .           
# Lowest.Intra.op.Hemoglobin  
# Op.Category
unique(data$Op.Category)
# Operative.Approach
# Other.Card
unique(data$Other.Card)
# Other.Non.Card
unique(data$Other.Non.Card)
# Status
# Urgent.Or.Emergent.Reason
unique(data$Urgent.Or.Emergent.Reason)
# Dist.Anast...Vein
# Dist.Anast
# IMA.Dist.Anast
# IABP
# IABP.When.Inserted
# VAD.Device
# Other.Card.
# Chest.Tube.Loss
# Comps.Pulm.ARDS
# Total.OR.Time 
# Discharge



################ variable selection #######################





######################################### Pre-Op Direct Variable Cost ########################################

y.DVC=df.clean$Average.Variable.Direct.Cost
length(y.DVC)

y.DVC=y.DVC[df.clean$Mort.Mortality!=1]
length(y.DVC) 
  
  


dim(X.preOp)
X.DVC.preOp=X.preOp[df.clean$Mort.Mortality!=1,]
dim(X.DVC.preOp)
#only keep live person
str(df.clean$Mort.Mortality)
sum(df.clean$Mort.Mortality==1)

y2.DVC=log(y.DVC)
length(y2.DVC)

mm.X.DVC.preOp = model.matrix(~ ., data=X.DVC.preOp)

########## histogram distribution
library(scales)
df.summary.DVC=data.frame(as.integer(y.DVC),y2.DVC)
df.summary.DVC
names(df.summary.DVC)=c('DVC','logDVC')

# Draw with black outline, white fill
ggplot(df.summary.DVC, aes(x=DVC)) +
  geom_histogram(colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(DVC)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(labels = comma)+
  geom_text(data = df.summary.DVC, aes(label = round(mean(DVC)), y=1000, x=mean(DVC)),hjust=-0.5, color="blue", size=3.5)+
  ggtitle("Histogram of DVC of CABG")+
  labs(x="DVC in $",y="Counts") 


ggplot(df.summary.DVC, aes(x=logDVC)) +
  geom_histogram(colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(logDVC)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(labels = comma)+
  geom_text(data = df.summary.DVC, aes(label = round(mean(logDVC),2), y=400, x=mean(logDVC)),hjust=-0.5, color="blue", size=3.5)+
  ggtitle("Histogram of Log-transformed DVC of CABG")+
  labs(x="Log-transformed DVC",y="Counts") 

##lm: no y transform
lm.VDC = lm(y.DVC ~.,data=X.DVC.preOp)
summary(lm.VDC)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.VDC)

##lm: y transform
lm2.VDC = lm(y2.DVC ~.,data=X.DVC.preOp)
summary(lm2.VDC)
layout(matrix(c(1,2,3,4),2,2))
plot(lm2.VDC)





###### log transform



elastNet2.DVC.cv=cv.glmnet(mm.X.DVC.preOp,y2.DVC, alpha=0.5,nfolds=10)

elastNet2.DVC = glmnet(mm.X.DVC.preOp, y2.DVC, alpha = 0.5, nlambda = 100)

coef.elastNet2.DVC=coef(elastNet2.DVC,s=elastNet2.DVC.cv$lambda.min)

plot(elastNet2.DVC,xvar="lambda")
abline(v=log(elastNet2.DVC.cv$lambda.min),col='black',lty = 2)

sig.elastNet2.DVC=coef.elastNet2.DVC[which(coef.elastNet2.DVC[,1]!=0),1]
round(sig.elastNet2.DVC,2)

r2.elastNet2.DVC.cv <- elastNet2.DVC.cv$glmnet.fit$dev.ratio[which(elastNet2.DVC.cv$glmnet.fit$lambda == elastNet2.DVC.cv$lambda.min)]
r2.elastNet2.DVC.cv



pos.elastNet2.DVC=exp(coef.elastNet2.DVC[which(coef.elastNet2.DVC[,1]>0),])
pos.elastNet2.DVC=round(pos.elastNet2.DVC[order(-pos.elastNet2.DVC)],2)
pos.elastNet2.DVC
names(pos.elastNet2.DVC)
values(pos.elastNet2.DVC)
typeof(names(pos.elastNet2.DVC))
typeof((pos.elastNet2.DVC))

neg.elastNet2.DVC=exp(coef.elastNet2.DVC[which(coef.elastNet2.DVC[,1]<0),])
neg.elastNet2.DVC=round(neg.elastNet2.DVC[order(neg.elastNet2.DVC)],2)
neg.elastNet2.DVC

# newdata.coef.elastNet2.DVC=data.frame(names(pos.elastNet2.DVC)[2:21],pos.elastNet2.DVC[2:21],
#                                       names(neg.elastNet2.DVC)[1:20],neg.elastNet2.DVC[1:20])
# names(newdata.coef.elastNet2.DVC)=c('Positive','Value1','Negative','Value2')

# str(X.postOp)
# 
library(ggplot2)
# typeof(pos.elastNet2.DVC)
# qplot(1,pos.elastNet2.DVC, geom="boxplot")
# g <- ggplot(newdata.coef.elastNet2.DVC,x='Positive',y='Value1')
# # Number of cars in each class:
# g + geom_bar()
# 
# p<-ggplot(data=newdata.coef.elastNet2.DVC, aes(x= reorder(Positive, Value1), y=Value1)) +
#   geom_bar(stat="identity", fill="steelblue")+ 
#   coord_flip()+geom_text(aes(label=Value1), hjust=0.3,vjust=0, color="blue", size=3.5)
# p
unique(surgeon)
unique(as.character(surgeon))
unique(as.factor(surgeon))
unique(as.numeric(surgeon))

fitted.elastNet2.DVC=predict(elastNet2.DVC, newx = mm.X.DVC.preOp, s = elastNet2.DVC.cv$lambda.min)
residual.elastNet2.DVC=y2.DVC-fitted.elastNet2.DVC
surgeon=as.character(as.numeric(df.clean$Surgeon))[df.clean$Mort.Mortality!=1]
hospital=as.factor(df.clean$Hospital.Name)[df.clean$Mort.Mortality!=1]
robot=as.factor(df.clean$Robot.Used)[df.clean$Mort.Mortality!=1]
newdata.elastNet2.DVC=data.frame(fitted.elastNet2.DVC,residual.elastNet2.DVC,surgeon,hospital,robot)

#residual plot
p=ggplot(data=newdata.elastNet2.DVC, aes(x=fitted.elastNet2.DVC, y=residual.elastNet2.DVC)) + geom_point()
p +labs(title="Residual plot of log transformed Pre-Op DVC model for CABG", x ="Fitted log(DVC)", y = "Residual")



#ANOVA
anova.elastNet2.DVC=aov(residual.elastNet2.DVC~surgeon)
summary(anova.elastNet2.DVC)


library(gplots)
par(mfrow=c(1,1))
par(mar=c(9, 3.5, 3, 1), mgp=c(2.4, 0.8, 0), las=3)
plotmeans(exp(residual.elastNet2.DVC)~surgeon,xlab='',mean.labels = TRUE,barwidth=1,barcol='blue',
          ylab="Risk Adjusted Average Value in $", main="Risk Adjusted Average Variable Direct Cost"
          ,connect=FALSE)

#Visualization

#violin plot of the Direct Variable Cost Index by Surgeons
p = ggplot(newdata.elastNet2.DVC, aes(x=surgeon, y=exp(residual.elastNet2.DVC),color=robot)) + 
  geom_violin(trim=TRUE) + coord_flip()+
  labs(title="Ratio of Actual DVC / Expected DVC by Surgeons",y="Ratio", x = "Surgeons")+
  geom_hline(yintercept = 1,  color = "red", size=1.5)+
  ylim(0.5, 3)
 
p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')


#violin plot of the absolute Direct Variable Cost by Surgeons
p = ggplot(newdata.elastNet2.DVC, aes(x=surgeon, y=exp(y2.DVC)-exp(fitted.elastNet2.DVC),color=robot)) + 
  geom_violin(trim=TRUE) + coord_flip()+
  labs(title="Cost Difference from Expected DVC by Surgeons",y="Cost Difference from Expected DVC in $", x = "Surgeons")+
  geom_hline(yintercept = 0,  color = "red", size=1.5)+
  ylim(-25000, 60000)

p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')


#violin plot of the Direct Variable Cost Index by Surgeons and hospital
p = ggplot(newdata.elastNet2.DVC, aes(x=hospital, y=exp(residual.elastNet2.DVC),color=hospital)) + 
  geom_violin(trim=TRUE) + coord_flip()+
  labs(title="Direct Variable Cost Index by Surgeons in Various hospitals",x="Direct Variable Cost Index", y = "Surgeons")

p + geom_boxplot(width=0.3,color='black')

# library(ggplot2)
# 
# newdata=data.frame(riskAd.elastNet.DVC,surgeon)
# 
# 
# ggplot(data=newdata, aes(x=surgeon, y=riskAd.elastNet.DVC, fill=surgeon)) +
#   geom_bar(stat="identity")

#violin plot
# p <- ggplot(newdata, aes(x=surgeon, y=log(riskAd.elastNet.DVC),color=surgeon)) + 
#   geom_violin(trim=TRUE) + coord_flip()
# 
# p + stat_summary(fun.y=median, geom="point", size=2, color="red")
# 
# p + geom_boxplot(width=0.1)
# 
# p + stat_summary(fun.data=mean_sdl, mult=1, 
#                  geom="pointrange", color="red")
# 
# p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.01,bins=1)
# 
# p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')+ theme(legend.position="none")


######################################### Post-Op Direct Variable Cost ########################################

y.DVC=df.clean$Average.Variable.Direct.Cost

y.DVC=y.DVC[df.clean$Mort.Mortality!=1]
length(y.DVC) 

y2.DVC=log(y.DVC)
length(y2.DVC) 

X.DVC.postOp=X.postOp[df.clean$Mort.Mortality!=1,]
dim(X.DVC.postOp)

mm.X.DVC.postOp = model.matrix(~ ., data=X.DVC.postOp)
dim(mm.X.DVC.postOp)

str(X.DVC.postOp,list.len=ncol(df.clean))
###################check and remove factors that only has 1 level

one.DVC.postOp=sapply(X.DVC.postOp, function(x) length(unique(x)))
sum(one.DVC.postOp==1)
one.DVC.postOp
names(which(one.DVC.postOp==1))

length(unique(df$Procedure.Type))
length(unique(df$Other.Replacement.Approaches))
length(unique(df$Valve))

for(i in names(which(one.DVC.postOp==1))) {
  feature=X.DVC.postOp[[i]]
  X.DVC.postOp[[i]]=NULL
}

sum(names(X.DVC.postOp) %in% names(which(one.DVC.postOp==1)))

str(df.clean,list.len=ncol(df.clean))



##lm model prior to transformation
lm.VDC.postOp = lm(y.DVC ~.,data=X.DVC.postOp)
summary(lm.VDC.postOp)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.VDC.postOp)

##lm model after transformation
lm2.VDC.postOp = lm(y2.DVC ~.,data=X.DVC.postOp)
summary(lm2.VDC.postOp)
layout(matrix(c(1,2,3,4),2,2))
plot(lm2.VDC.postOp)




### elasticNet



#### post-transformation

elastNet2.DVC.postOp.cv=cv.glmnet(mm.X.DVC.postOp,y2.DVC, alpha=0.5,nfolds=10)

elastNet2.DVC.postOp = glmnet(mm.X.DVC.postOp, y2.DVC, alpha = 0.5, nlambda = 100)

coef.elastNet2.DVC.postOp=coef(elastNet2.DVC.postOp,s=elastNet2.DVC.postOp.cv$lambda.min)

plot(elastNet2.DVC.postOp,xvar="lambda")
abline(v=log(elastNet2.DVC.postOp.cv$lambda.min),col='black',lty = 2)



#R square
r2.elastNet2.DVC.postOp.cv <- elastNet2.DVC.postOp.cv$glmnet.fit$dev.ratio[which(elastNet2.DVC.postOp.cv$glmnet.fit$lambda == elastNet2.DVC.postOp.cv$lambda.min)]
r2.elastNet2.DVC.postOp.cv

#significant variables
sig.elastNet2.DVC.postOp=coef.elastNet2.DVC.postOp[which(coef.elastNet2.DVC.postOp[,1]!=0),1]
round(sig.elastNet2.DVC.postOp,2)

pos.elastNet2.DVC.postOp=exp(coef.elastNet2.DVC.postOp[which(coef.elastNet2.DVC.postOp[,1]>0),])
pos.elastNet2.DVC.postOp=round(pos.elastNet2.DVC.postOp[order(-pos.elastNet2.DVC.postOp)],2)
pos.elastNet2.DVC.postOp

neg.elastNet2.DVC.postOp=exp(coef.elastNet2.DVC.postOp[which(coef.elastNet2.DVC.postOp[,1]<0),])
neg.elastNet2.DVC.postOp=round(neg.elastNet2.DVC.postOp[order(neg.elastNet2.DVC.postOp)],2)
neg.elastNet2.DVC.postOp


# newdata.coefCompare.elastNet2.DVC=data.frame(names(pos.elastNet2.DVC)[2:13],pos.elastNet2.DVC[2:13],
#                                       names(pos.elastNet2.DVC.postOp)[2:13],pos.elastNet2.DVC.postOp[2:13])

# type=c(rep("Pre-Op", 12), rep("Post-Op", 12))
# name=c(names(pos.elastNet2.DVC)[2:13],names(pos.elastNet2.DVC.postOp)[2:13])
# value=c(pos.elastNet2.DVC[2:13],pos.elastNet2.DVC.postOp[2:13])
# df.pos.coef.compare=data.frame(name,value,type)
# 
# p <-ggplot(df.pos.coef.compare, aes(x= reorder(name, value), y=value))
# p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
#   geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
#   labs(title="Compounding Rate of Positive Factors on DVC Index",x="Factors", y = "Compounding Rate")
type
neg.elastNet2.DVC




#top of all pre-op factors
type=c(rep("Positive", 6), rep("Negative", 6))
name=c(names(pos.elastNet2.DVC)[2:7],names(neg.elastNet2.DVC)[1:6])
value=c(pos.elastNet2.DVC[2:7],neg.elastNet2.DVC[1:6])
df.coef.compare1=data.frame(name,value,type)
df.coef.compare1=df.coef.compare1[order(-value),]

p <-ggplot(df.coef.compare1, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Pre-Op Factors on DVC Index",x="Factors", y = "Compounding Rate")


#top of all post-op factors
type=c(rep("Positive", 6), rep("Negative", 6))
name=c(names(pos.elastNet2.DVC.postOp)[2:7],names(neg.elastNet2.DVC.postOp)[1:6])
value=c(pos.elastNet2.DVC.postOp[2:7],neg.elastNet2.DVC.postOp[1:6])
df.coef.compare2=data.frame(name,value,type)

df.coef.compare2=df.coef.compare2[order(-value),]
df.coef.compare2

p <-ggplot(df.coef.compare2, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Reds")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Post-Op Factors on DVC Index",x="Factors", y = "Compounding Rate")


#Compounding rate of top 12 positive factors on DVC index
type=c(rep("Pre-Op Model", length(pos.elastNet2.DVC)), rep("Full Model", length(pos.elastNet2.DVC.postOp)))
name=c(names(pos.elastNet2.DVC),names(pos.elastNet2.DVC.postOp))
value=c(pos.elastNet2.DVC,pos.elastNet2.DVC.postOp)
df.coef.compare3=data.frame(name,value,type)
df.coef.compare3=df.coef.compare3[order(-value),]
df.coef.compare3=df.coef.compare3[which(df.coef.compare3$name!='(Intercept)'),]

p <-ggplot(df.coef.compare3[1:12,], aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive Factors on DVC of CABG",x="Factors", y = "Compounding Rate")


#Compounding rate of top 12 neg factors on DVC index
type=c(rep("Pre-Op", length(neg.elastNet2.DVC)), rep("Post-Op", length(neg.elastNet2.DVC.postOp)))
name=c(names(neg.elastNet2.DVC),names(neg.elastNet2.DVC.postOp))
value=c(neg.elastNet2.DVC,neg.elastNet2.DVC.postOp)
df.coef.compare4=data.frame(name,value,type)
df.coef.compare4=df.coef.compare4[order(value),]
df.coef.compare4

p <-ggplot(df.coef.compare4[1:12,], aes(x= reorder(name, -value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Negative Factors on DVC of CABG",x="Factors", y = "Compounding Rate")


# type=c(rep("Pre-Op", 13), rep("Post-Op", 13))
# name=c(names(neg.elastNet2.DVC)[1:13],names(neg.elastNet2.DVC.postOp)[1:13])
# value=c(neg.elastNet2.DVC[1:13],neg.elastNet2.DVC.postOp[1:13])
# data.neg.elastNet2.DVC=data.frame(name,value,type)
# 
# p <-ggplot(data.neg.elastNet2.DVC, aes(x= reorder(name, -value), y=value))
# p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
#   geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)
# 
# 
# 
# 
# ggplot(data=data.neg.elastNet2.DVC, aes(x= reorder(name, value), y=value,fill=type)) +
#   geom_bar(stat="identity", fill="steelblue")+ 
#   coord_flip()+geom_text(aes(label=value), hjust=0.3,vjust=0, color="blue", size=3.5)
# 
# 
# unique(df.clean$Intraop.Blood.Products...Platelet.Units)
# 
# names(newdata.coef.elastNet2.DVC)=c('PreOp','Value1','PostOp','Value2')
# 
# ggplot(data=data.test, aes(x= reorder(name, value), y=value,fill=type)) +
#   geom_bar(stat="identity", fill="steelblue")+ 
#   coord_flip()+geom_text(aes(label=value), hjust=0.3,vjust=0, color="blue", size=3.5)
# 
# 
# ggplot(data=newdata.coef.elastNet2.DVC, aes(x= PostOp, y=Value2)) +
#   geom_bar(stat="identity", fill="steelblue", position=position_dodge())+ 
#   coord_flip()+geom_text(aes(label=Value2), hjust=0.3,vjust=0, color="blue", size=3.5)
# p


fitted.elastNet2.DVC.postOp=predict(elastNet2.DVC.postOp, newx = mm.X.DVC.postOp, s = elastNet2.DVC.postOp.cv$lambda.min)
residual.elastNet2.DVC.postOp=y2.DVC-fitted.elastNet2.DVC.postOp
surgeon=as.factor(df.clean$Surgeon)[df.clean$Mort.Mortality!=1]
hospital=as.factor(df.clean$Hospital.Name)[df.clean$Mort.Mortality!=1]
newdata.elastNet2.DVC.postOp=data.frame(fitted.elastNet2.DVC.postOp,residual.elastNet2.DVC.postOp,surgeon,hospital)

#residual analysis
p=ggplot(data=newdata.elastNet2.DVC.postOp, aes(x=fitted.elastNet2.DVC.postOp, y=residual.elastNet2.DVC.postOp)) + geom_point()
p +labs(title="Residual plot of log transformed DVC model Post-Op for CABG", x ="Fitted log(DVC)", y = "Residual")

p = ggplot(newdata.elastNet2.DVC.postOp, aes(x=surgeon, y=exp(residual.elastNet2.DVC.postOp),color=surgeon)) + 
  geom_violin(trim=TRUE) + coord_flip()
# 
# anova.elastNet2.DVC.postOp=aov(residual.elastNet2.DVC.postOp~surgeon)
# summary(anova.elastNet2.DVC.postOp)
# 
# library(gplots)
# par(mfrow=c(1,1))
# par(mar=c(9, 3.5, 3, 1), mgp=c(2.4, 0.8, 0), las=3)
# plotmeans(residual.elastNet2.DVC.postOp~surgeon,xlab='',mean.labels = TRUE,barwidth=1,barcol='blue',
#           ylab="Risk Adjusted Average Value in $", main="Risk Adjusted Average Variable Direct Cost"
#           ,connect=FALSE)


# #violin plot of the Direct Variable Cost Index by Surgeons
# p = ggplot(newdata.elastNet2.DVC.postOp, aes(x=surgeon, y=exp(residual.elastNet2.DVC.postOp),color=surgeon)) + 
#   geom_violin(trim=TRUE) + coord_flip()+
#   labs(title="Direct Variable Cost Index by Surgeons",x="Direct Variable Cost Index", y = "Surgeons")
# 
# p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')+
#   theme(legend.position="none")
# 
# 
#violin plot of the Direct Variable Cost Index by hospital
# p = ggplot(newdata.elastNet2.DVC.postOp, aes(x=hospital, y=exp(residual.elastNet2.DVC.postOp),color=hospital)) +
#   geom_violin(trim=TRUE) + coord_flip()+
#   labs(title="Direct Variable Cost Index by Hospitals",x="Direct Variable Cost Index", y = "Hospitals")
# 
# p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')+
#   theme(legend.position="none")
# 
# 
# #violin plot of the Direct Variable Cost Index by Surgeons and hospital
# p = ggplot(newdata.elastNet2.DVC.postOp, aes(x=surgeon, y=exp(residual.elastNet2.DVC.postOp),color=hospital)) + 
#   geom_violin(trim=TRUE) + coord_flip()+
#   labs(title="Direct Variable Cost Index by Surgeons in Various hospitals",x="Direct Variable Cost Index", y = "Surgeons")
# 
# p + geom_boxplot(width=0.3,color='black')




########################################### LOS Models ########################################

################################################## Pre-Op LOS ################################
variables.LOS

y.LOS
hist(y.LOS)
hist(y2.LOS)
hist(y.DVC)

hist(y2.DVC)
y.LOS=df.clean$LOS.Admit.Discharge
length(y.LOS)
y.LOS= y.LOS[df.clean$Mort.Mortality!=1]
length(y.LOS)

y2.LOS= log(y.LOS)
length(y2.LOS)

sum(y.LOS=='NA')

X.LOS.preOp=X.preOp[df.clean$Mort.Mortality!=1,]
dim(X.LOS.preOp)

mm.X.LOS.preOp = model.matrix(~ ., data=X.LOS.preOp)

########## histogram distribution
library(scales)
df.summary.LOS=data.frame(y.LOS,y2.LOS)
df.summary.LOS
names(df.summary.LOS)=c('LOS','logLOS')


# Draw with black outline, white fill
ggplot(df.summary.LOS, aes(x=LOS)) +
  geom_histogram(colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(LOS)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(labels = comma)+
  geom_text(data = df.summary.LOS, aes(label = round(mean(LOS)), y=1200, x=mean(LOS)),hjust=-0.5, color="blue", size=3.5)+
  ggtitle("Histogram of LOS for CABG")


ggplot(df.summary.LOS, aes(x=logLOS)) +
  geom_histogram(colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(logLOS)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(labels = comma)+
  geom_text(data = df.summary.LOS, aes(label = round(mean(logLOS),2), y=550, x=mean(logLOS)),hjust=-0.5, color="blue", size=3.5)+
  ggtitle("Histogram of Log-transformed LOS for CABG")


############ linear regression model

##lm model prior to transformation
lm.LOS = lm(y.LOS ~.,data=X.LOS.preOp)
summary(lm.LOS)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.LOS)

##lm model after transformation
lm2.LOS = lm(y2.LOS ~.,data=X.LOS.preOp)
summary(lm2.LOS)
layout(matrix(c(1,2,3,4),2,2))
plot(lm2.LOS)


### transform 

elastNet2.LOS.cv=cv.glmnet(mm.X.LOS.preOp,y2.LOS, alpha = 0.5,nfolds=10)

elastNet2.LOS = glmnet(mm.X.LOS.preOp, y2.LOS, alpha = 0.5, nlambda = 100)

coef.elastNet2.LOS=coef(elastNet2.LOS,s=elastNet2.LOS.cv$lambda.min)
coef.elastNet2.LOS

plot(elastNet2.LOS,xvar="lambda")
abline(v=log(elastNet2.LOS.cv$lambda.min),col='black',lty = 2)

sig.elastNet2.LOS=coef.elastNet2.LOS[which(coef.elastNet2.LOS[,1]!=0),1]
round(sig.elastNet2.LOS,2)

r2.elastNet2.LOS <- elastNet2.LOS.cv$glmnet.fit$dev.ratio[which(elastNet2.LOS.cv$glmnet.fit$lambda == elastNet2.LOS.cv$lambda.min)]
r2.elastNet2.LOS


pos.elastNet2.LOS=exp(coef.elastNet2.LOS[which(coef.elastNet2.LOS[,1]>0),])
pos.elastNet2.LOS=round(pos.elastNet2.LOS[order(-pos.elastNet2.LOS)],2)
pos.elastNet2.LOS

neg.elastNet2.LOS=exp(coef.elastNet2.LOS[which(coef.elastNet2.LOS[,1]<0),])
neg.elastNet2.LOS=round(neg.elastNet2.LOS[order(neg.elastNet2.LOS)],2)
neg.elastNet2.LOS


# Residual Plot
fitted.elastNet2.LOS=predict(elastNet2.LOS, newx = mm.X.LOS.preOp, s = elastNet2.LOS.cv$lambda.min)
residual.elastNet2.LOS=y2.LOS-fitted.elastNet2.LOS
surgeon=as.factor(df.clean$Surgeon)[df.clean$Mort.Mortality!=1]
hospital=as.factor(df.clean$Hospital.Name)[df.clean$Mort.Mortality!=1]
robot=as.factor(df.clean$Robot.Used)[df.clean$Mort.Mortality!=1]
newdata.elastNet2.LOS=data.frame(fitted.elastNet2.LOS,residual.elastNet2.LOS,surgeon,hospital,robot)

p=ggplot(data=newdata.elastNet2.LOS, aes(x=fitted.elastNet2.LOS, y=residual.elastNet2.LOS)) + geom_point()
p +labs(title="Residual plot of log transformed Pre-Op LOS model of CABG", x ="Fitted log(LOS)", y = "Residual")


#Anova Test
anova.elastNet2.LOS=aov(residual.elastNet2.LOS~surgeon)
summary(anova.elastNet2.LOS)

#Mean plot
library(gplots)
par(mfrow=c(1,1))
par(mar=c(9, 3.5, 3, 1), mgp=c(2.4, 0.8, 0), las=3)
plotmeans(exp(residual.elastNet2.LOS)~surgeon,xlab='',mean.labels = TRUE,barwidth=1,barcol='blue',
          ylab="Risk Adjusted LOS in hour", main="Risk Adjusted LOS"
          ,connect=FALSE)


#violin plot of the absolute LOS by Surgeons and Robot
p = ggplot(newdata.elastNet2.LOS, aes(x=surgeon, y=exp(y2.LOS)-exp(fitted.elastNet2.LOS),color=robot)) + 
  geom_violin(trim=TRUE) + coord_flip()+
  labs(title="Hour Difference from Adjusted Length Of Stay by Surgeons",y="Hour Difference from Adjusted LOS in hour", x = "Surgeons")+
  geom_hline(yintercept = 0,  color = "red", size=1.5)+
  ylim(-10, 40)

p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')



#violin plot of the LOS Index by Surgeons and Robot
p = ggplot(newdata.elastNet2.LOS, aes(x=surgeon, y=exp(residual.elastNet2.LOS),color=robot)) + 
  geom_violin(trim=TRUE) + coord_flip()+
  labs(title="LOS Index by Surgeons and Robots",y="LOS Index", x = "Surgeons")+
  geom_hline(yintercept = 1,  color = "red", size=1.5)+
  ylim(0, 3.5)

p + geom_jitter(position=position_jitter(0.3))+ geom_boxplot(width=0.3,color='black')



################################################## Post-Op LOS ################################
variables.LOS

X.LOS.postOp=X.postOp[df.clean$Mort.Mortality!=1,]
dim(X.LOS.postOp)

mm.X.LOS.postOp = model.matrix(~ ., data=X.LOS.postOp)



############# try linear models first

##lm model prior to transformation
lm.LOS.postOp = lm(y.LOS ~.,data=X.LOS.postOp)
summary(lm.LOS.postOp)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.LOS.postOp)

##lm model after transformation
lm2.LOS.postOp = lm(y2.LOS ~.,data=X.LOS.postOp)
summary(lm2.LOS.postOp)
layout(matrix(c(1,2,3,4),2,2))
plot(lm2.LOS.postOp)



### transform 

elastNet2.LOS.postOp.cv=cv.glmnet(mm.X.LOS.postOp,y2.LOS, alpha = 0.5,nfolds=10)

elastNet2.LOS.postOp = glmnet(mm.X.LOS.postOp, y2.LOS, alpha = 0.5, nlambda = 100)

coef.elastNet2.LOS.postOp=coef(elastNet2.LOS.postOp,s=elastNet2.LOS.postOp.cv$lambda.min)
coef.elastNet2.LOS.postOp

plot(elastNet2.LOS.postOp,xvar="lambda")
abline(v=log(elastNet2.LOS.postOp.cv$lambda.min),col='black',lty = 2)

sig.elastNet2.LOS.postOp=coef.elastNet2.LOS.postOp[which(coef.elastNet2.LOS.postOp[,1]!=0),1]
round(sig.elastNet2.LOS.postOp,2)

#R square
r2.elastNet2.LOS.postOp <- elastNet2.LOS.postOp.cv$glmnet.fit$dev.ratio[which(elastNet2.LOS.postOp.cv$glmnet.fit$lambda == elastNet2.LOS.postOp.cv$lambda.min)]
r2.elastNet2.LOS.postOp


pos.elastNet2.LOS.postOp=exp(coef.elastNet2.LOS.postOp[which(coef.elastNet2.LOS.postOp[,1]>0),])
pos.elastNet2.LOS.postOp=round(pos.elastNet2.LOS.postOp[order(-pos.elastNet2.LOS.postOp)],2)
pos.elastNet2.LOS.postOp

neg.elastNet2.LOS.postOp=exp(coef.elastNet2.LOS.postOp[which(coef.elastNet2.LOS.postOp[,1]<0),])
neg.elastNet2.LOS.postOp=round(neg.elastNet2.LOS.postOp[order(neg.elastNet2.LOS.postOp)],2)
neg.elastNet2.LOS.postOp


# Residual Plot
fitted.elastNet2.LOS.postOp=predict(elastNet2.LOS.postOp, newx = mm.X.LOS.postOp, s = elastNet2.LOS.postOp.cv$lambda.min)
residual.elastNet2.LOS.postOp=y2.LOS-fitted.elastNet2.LOS.postOp
surgeon=as.factor(df.clean$Surgeon)[df.clean$Mort.Mortality!=1]
hospital=as.factor(df.clean$Hospital.Name)[df.clean$Mort.Mortality!=1]
newdata.elastNet2.LOS.postOp=data.frame(fitted.elastNet2.LOS.postOp,residual.elastNet2.LOS.postOp,surgeon,hospital)

p=ggplot(data=newdata.elastNet2.LOS.postOp, aes(x=fitted.elastNet2.LOS.postOp, y=residual.elastNet2.LOS.postOp)) + geom_point()
p +labs(title="Residual plot of log transformed LOS model", x ="Fitted log(LOS)", y = "Residual")


#top of all pre-op factors
type=c(rep("Positive", 6), rep("Negative", 6))
name=c(names(pos.elastNet2.LOS)[2:7],names(neg.elastNet2.LOS)[1:6])
value=c(pos.elastNet2.LOS[2:7],neg.elastNet2.LOS[1:6])
df.coef.LOS.compare1=data.frame(name,value,type)
df.coef.LOS.compare1=df.coef.LOS.compare1[order(-value),]

p <-ggplot(df.coef.LOS.compare1, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Pre-Op Factors on LOS Index",x="Factors", y = "Compounding Rate")


#top of all post-op factors
type=c(rep("Positive", 6), rep("Negative", 6))
name=c(names(pos.elastNet2.LOS.postOp)[2:7],names(neg.elastNet2.LOS.postOp)[1:6])
value=c(pos.elastNet2.LOS.postOp[2:7],neg.elastNet2.LOS.postOp[1:6])
df.coef.LOS.compare2=data.frame(name,value,type)
df.coef.LOS.compare2=df.coef.LOS.compare2[order(-value),]

p <-ggplot(df.coef.LOS.compare2, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Reds")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Post-Op Factors on LOS Index",x="Factors", y = "Compounding Rate")



#top 12 positive factors
type=c(rep("Pre-Op", length(pos.elastNet2.LOS)), rep("Post-Op", length(pos.elastNet2.LOS.postOp)))
name=c(names(pos.elastNet2.LOS),names(pos.elastNet2.LOS.postOp))
value=c(pos.elastNet2.LOS,pos.elastNet2.LOS.postOp)
df.coef.LOS.compare3=data.frame(name,value,type)
df.coef.LOS.compare3=df.coef.LOS.compare3[order(-value),]
df.coef.LOS.compare3=df.coef.LOS.compare3[which(df.coef.LOS.compare3$name!='(Intercept)'),]

p <-ggplot(df.coef.LOS.compare3[1:12,], aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top 12 Positive Factors on LOS Index",x="Factors", y = "Compounding Rate")

unique(df.clean$Urgent.Or.Emergent.Reason)


#top 12 negative factors
type=c(rep("Pre-Op", length(neg.elastNet2.LOS)), rep("Post-Op", length(neg.elastNet2.LOS.postOp)))
name=c(names(neg.elastNet2.LOS),names(neg.elastNet2.LOS.postOp))
value=c(neg.elastNet2.LOS,neg.elastNet2.LOS.postOp)
df.coef.LOS.compare4=data.frame(name,value,type)
df.coef.LOS.compare4=df.coef.LOS.compare4[order(value),]
df.coef.LOS.compare4=df.coef.LOS.compare4[which(df.coef.LOS.compare4$name!='(Intercept)'),]

p <-ggplot(df.coef.LOS.compare4[1:12,], aes(x= reorder(name, -value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top 12 Negative Factors on LOS Index",x="Factors", y = "Compounding Rate")








###################################################################################################################

################################################## Classificaition ####################################################

####################################################################################################################


############################# mortality #############################



###########################3 use of tree based method


######################################### Pre-Op Mortality


variables.mortality
y=as.factor(df.clean$Mort.Mortality)
# y=as.factor(df.clean$Mort.30d.Status)
# y=as.factor(df.clean$Mort.DC.Status)
unique(y)


###remove the single 'NA' value much improve the classificaiton speed
y=as.character(y)
y.mortality=y[which(y!='NA')]
y.mortality=1*(y.mortality=='1')
unique(y.mortality)
length(y.mortality)
# X.mortality=X.exclude1.copy[which(y!='NA'),]
X.mortality.preOp=X.preOp[which(y!='NA'),]
dim(X.mortality.preOp)



#check dimention
length(y.mortality)
dim(X.mortality)
sum(y.mortality==0)

# construct model matrix
mm.X.mortality = model.matrix(~ ., data=X.mortality.preOp)
dim(mm.X.mortality)




########################## ElasticNet

# elastNet.mortality.cv=cv.glmnet(mm.X.mortality,y.mortality,family=c("binomial"),alpha=0.5,type="class",nfolds=10)


elastNet2.mortality.cv=cv.glmnet(mm.X.mortality,y.mortality,family=c("binomial"),alpha=0.5,type="auc",nfolds=10)

elastNet.mortality = glmnet(mm.X.mortality, y.mortality,family=c("binomial"), alpha = 0.5, nlambda = 100)


plot(elastNet2.mortality.cv)
title(main = "ROC curve for Pre-Op model of MORT in CABG")

# elastNet3.mortality.cv=cv.glmnet(mm.X.mortality,y.mortality,family=c("binomial"),alpha=0.5,type="mae",nfolds=10)
# plot(elastNet3.mortality.cv)
# 
# elastNet4.mortality.cv=cv.glmnet(mm.X.mortality,y.mortality,family=c("binomial"),alpha=0.5,type="deviance",nfolds=10)
# plot(elastNet4.mortality.cv)

#extract coefficient at optimal model
# coef.elastNet.mortality=coef(elastNet.mortality,s=elastNet.mortality.cv$lambda.min)
# coef.elastNet.mortality

coef.elastNet2.mortality=coef(elastNet.mortality,s=elastNet2.mortality.cv$lambda.min)
coef.elastNet2.mortality

# coef.elastNet3.mortality=coef(elastNet.mortality,s=elastNet3.mortality.cv$lambda.min)
# coef.elastNet3.mortality
# 
# coef.elastNet4.mortality=coef(elastNet.mortality,s=elastNet4.mortality.cv$lambda.min)
# coef.elastNet4.mortality

predict(elastNet.mortality, newx = mm.X.mortality, type = "coefficients", 
        s = c(elastNet.mortality.cv$lambda.min, elastNet4.mortality.cv$lambda.min,
              elastNet2.mortality.cv$lambda.min ,elastNet3.mortality.cv$lambda.min))

#plot
plot(elastNet.mortality,xvar="lambda",lwd=2)
# abline(v=log(elastNet.mortality.cv$lambda.min),col='black',lty = 2,lwd=2)
abline(v=log(elastNet2.mortality.cv$lambda.min),col='blue',lty = 2,lwd=2)
# abline(v=log(elastNet3.mortality.cv$lambda.min),col='red',lty = 2,lwd=2)
# abline(v=log(elastNet4.mortality.cv$lambda.min),col='green',lty = 2,lwd=2)
legend(1, 95, legend=c("class", "auc"))
# legend(x=1, legend=c("class", "auc",'mae','deviance'))

#reduce factors
neg.elastNet2.mortality=exp(coef.elastNet2.mortality[which(coef.elastNet2.mortality[,1]<0),])
neg.elastNet2.mortality=round(neg.elastNet2.mortality[order(neg.elastNet2.mortality)],2)
neg.elastNet2.mortality
names(neg.elastNet2.mortality)

pos.elastNet2.mortality=exp(coef.elastNet2.mortality[which(coef.elastNet2.mortality[,1]>0),])
pos.elastNet2.mortality=round(pos.elastNet2.mortality[order(-pos.elastNet2.mortality)],2)
pos.elastNet2.mortality
names(pos.elastNet2.mortality)


#plot
plot(elastNet.mortality,xvar="lambda",lwd=2)
abline(v=log(elastNet.mortality.cv$lambda.min),col='black',lty = 2,lwd=2)


test=predict(elastNet.mortality, newx = mm.X.mortality, type = "class", s = elastNet.mortality.cv$lambda.min)
test[,1]

predict(elastNet.mortality, newx = mm.X.mortality, type = "response", s = elastNet.mortality.cv$lambda.min)

predict(elastNet.mortality, newx = mm.X.mortality, type = "coefficients", s = elastNet.mortality.cv$lambda.min)

predict(elastNet.mortality, newx = mm.X.mortality, type = "nonzero", s = elastNet.mortality.cv$lambda.min)

sum(test[,1]=='1')

#### significant variables
round(exp(coef.elastNet.mortality[which(abs(coef.elastNet.mortality[,1])!=0),]),2)
str(df.clean$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
str(df$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
unique(df$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
str(df.clean)
#check the length
length(y.mortality)
dim(X.mortality)

###################### decision tree

library(rpart)
#train the model 
rpart.mortality <- rpart(y.mortality ~ .,data=X.mortality, method="class", parms=list(split="gini"))
# rpart.mortality <- rpart(y.mortality ~ .,data=X.mortality, method="class", parms=list(split="information"))


table(predict(rpart.mortality, X.mortality, type="class"), y.mortality)

print(rpart.mortality)
post(rpart.mortality,filename="")


#check surgeon
y.surgeon=df.clean$Surgeon[which(y!='NA')]
length(y.surgeon)
table(y.surgeon[yhat!=y])


####################################################### Post-Op Mortality ######################################################

X.postOp

variables.mortality
y=as.factor(df.clean$Mort.Mortality)
# y=as.factor(df.clean$Mort.30d.Status)
# y=as.factor(df.clean$Mort.DC.Status)
unique(y)

###remove the single 'NA' value much improve the classificaiton speed
y=as.character(y)
y.mortality=y[which(y!='NA')]
y.mortality=I(y.mortality=='1')
unique(y.mortality)
sum(y.mortality)

variables.discharge
X.mortality.postOp=X.postOp[!(names(X.postOp) %in% variables.discharge)]



# X.mortality=X.exclude1.copy[which(y!='NA'),]
X.mortality.postOp=X.mortality.postOp[which(y!='NA'),]

# construct model matrix
mm.X.mortality.postOp = model.matrix(~ ., data=X.mortality.postOp)

########################## ElasticNet

# elastNet.mortality.postOp.cv=cv.glmnet(mm.X.mortality.postOp,y.mortality,family=c("binomial"),alpha=0.5,type="class",nfolds=10)
# plot(elastNet.mortality.postOp.cv)

elastNet.mortality.postOp = glmnet(mm.X.mortality.postOp, y.mortality,family=c("binomial"), alpha = 0.5, nlambda = 100)

elastNet2.mortality.postOp.cv=cv.glmnet(mm.X.mortality.postOp,y.mortality,family=c("binomial"),alpha=0.5,type="auc",nfolds=10)
plot(elastNet2.mortality.postOp.cv)
title(main = "ROC curve for Post-Op model of MORT in CABG")

# elastNet3.mortality.postOp.cv=cv.glmnet(mm.X.mortality.postOp,y.mortality,family=c("binomial"),alpha=0.5,type="mae",nfolds=10)
# plot(elastNet3.mortality.postOp.cv)
# 
# elastNet4.mortality.postOp.cv=cv.glmnet(mm.X.mortality.postOp,y.mortality,family=c("binomial"),alpha=0.5,type="deviance",nfolds=10)
# plot(elastNet4.mortality.postOp.cv)

#extract coefficient at optimal model
# coef.elastNet.mortality.postOp=coef(elastNet.mortality.postOp,s=elastNet.mortality.postOp.cv$lambda.min)
# coef.elastNet.mortality.postOp

coef2.elastNet.mortality.postOp=coef(elastNet.mortality.postOp,s=elastNet2.mortality.postOp.cv$lambda.min)
coef2.elastNet.mortality.postOp

# coef3.elastNet.mortality.postOp=coef(elastNet.mortality.postOp,s=elastNet3.mortality.postOp.cv$lambda.min)
# 
# coef4.elastNet.mortality.postOp=coef(elastNet.mortality.postOp,s=elastNet4.mortality.postOp.cv$lambda.min)

round(exp(coef2.elastNet.mortality.postOp[which(abs(coef2.elastNet.mortality.postOp[,1])!=0),]),2)
# 
# round(exp(coef3.elastNet.mortality.postOp[which(abs(coef3.elastNet.mortality.postOp[,1])!=0),]),2)

#reduce factors
neg.elastNet2.mortality.postOp=exp(coef2.elastNet.mortality.postOp[which(coef2.elastNet.mortality.postOp[,1]<0),])
neg.elastNet2.mortality.postOp=round(neg.elastNet2.mortality.postOp[order(neg.elastNet2.mortality.postOp)],2)
neg.elastNet2.mortality.postOp
names(neg.elastNet2.mortality.postOp)

pos.elastNet2.mortality.postOp=exp(coef2.elastNet.mortality.postOp[which(coef2.elastNet.mortality.postOp[,1]>0),])
pos.elastNet2.mortality.postOp=round(pos.elastNet2.mortality.postOp[order(-pos.elastNet2.mortality.postOp)],2)
pos.elastNet2.mortality.postOp
names(pos.elastNet2.mortality.postOp)


#top of all pre-op factors
type=c(rep("Positive", 5), rep("Negative", 1))
name=c(names(pos.elastNet2.mortality),names(neg.elastNet2.mortality)[2])
value=c(pos.elastNet2.mortality,neg.elastNet2.mortality[2])
df.coef.mortality.compare1=data.frame(name,value,type)
df.coef.mortality.compare1=df.coef.mortality.compare1[order(-value),]

p <-ggplot(df.coef.mortality.compare1, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Pre-Op Factors on Odds Ratio of Mortality",x="Factors", y = "Compounding Rate")


#top of all post-op factors
type=c(rep("Positive", 6), rep("Negative", 6))
name=c(names(pos.elastNet2.mortality.postOp)[1:6],names(neg.elastNet2.mortality.postOp)[1:6])
value=c(pos.elastNet2.mortality.postOp[1:6],neg.elastNet2.mortality.postOp[1:6])
df.coef.mortality.compare2=data.frame(name,value,type)
df.coef.mortality.compare2=df.coef.mortality.compare2[order(-value),]

p <-ggplot(df.coef.mortality.compare2, aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  scale_fill_brewer(palette="Reds")+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Compounding Rate of Top Positive and Negative Post-Op Factors on Odds Ratio of Mortality",x="Factors", y = "Compounding Rate")




#top 12 positive of both pre-Op and post-Op
type=c(rep("Pre-Op Model", length(pos.elastNet2.mortality)), rep("Full Model", length(pos.elastNet2.mortality.postOp)))
name=c(names(pos.elastNet2.mortality),names(pos.elastNet2.mortality.postOp))
value=c(pos.elastNet2.mortality,pos.elastNet2.mortality.postOp)
df.coef.mortality.compare3=data.frame(name,value,type)
df.coef.mortality.compare3=df.coef.mortality.compare3[order(-value),]
df.coef.mortality.compare3=df.coef.mortality.compare3[which(df.coef.mortality.compare3$name!='(Intercept)'),]

p <-ggplot(df.coef.mortality.compare3[1:12,], aes(x= reorder(name, value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Odds Ratio of Top Positive Factors on Mortality of CABG",x="Factors", y = "Odds Ratio")



#top 12 negative of both pre-Op and post-Op
type=c(rep("Pre-Op Model", length(neg.elastNet2.mortality)), rep("Full Model", length(neg.elastNet2.mortality.postOp)))
name=c(names(neg.elastNet2.mortality),names(neg.elastNet2.mortality.postOp))
value=c(neg.elastNet2.mortality,neg.elastNet2.mortality.postOp)
df.coef.mortality.compare4=data.frame(name,value,type)
df.coef.mortality.compare4=df.coef.mortality.compare4[order(value),]
df.coef.mortality.compare4=df.coef.mortality.compare4[which(df.coef.mortality.compare4$name!='(Intercept)'),]

p <-ggplot(df.coef.mortality.compare4[1:12,], aes(x= reorder(name, -value), y=value))
p +geom_bar(stat = "identity", aes(fill = type), position = "dodge")+coord_flip()+
  geom_text(aes(label=value), hjust=1.5,vjust=0, color="black", size=3.5)+
  labs(title="Odds Ratio of Top Negative Factors on Mortality of CABG",x="Factors", y = "Odds Ratio")



# predict(elastNet.mortality.postOp, newx = mm.X.mortality.postOp, type = "coefficients", 
#         s = c(elastNet.mortality.postOp.cv$lambda.min, elastNet4.mortality.postOp.cv$lambda.min,
#               elastNet2.mortality.postOp.cv$lambda.min ,elastNet3.mortality.postOp.cv$lambda.min))
# 
# 
# predict(elastNet.mortality.postOp, newx = mm.X.mortality.postOp, type = "nonzero", 
#         s = c(elastNet.mortality.postOp.cv$lambda.min, elastNet4.mortality.postOp.cv$lambda.min,
#               elastNet2.mortality.postOp.cv$lambda.min ,elastNet3.mortality.postOp.cv$lambda.min))
# 
# 
# test3=predict(elastNet.mortality.postOp, newx = mm.X.mortality.postOp, type = "class", 
#         s = c(elastNet.mortality.postOp.cv$lambda.min, elastNet4.mortality.postOp.cv$lambda.min,
#               elastNet2.mortality.postOp.cv$lambda.min ,elastNet3.mortality.postOp.cv$lambda.min))

sum(test3==TRUE)


dim(mm.X.mortality.postOp)
length(y.mortality)

elastNet.mortality.postOp.cv$lambda.min


#plot
plot(elastNet.mortality.postOp,xvar="lambda",lwd=2)
abline(v=log(elastNet.mortality.postOp.cv$lambda.min),col='black',lty = 2,lwd=2)
abline(v=log(elastNet2.mortality.postOp.cv$lambda.min),col='blue',lty = 2,lwd=2)
abline(v=log(elastNet3.mortality.postOp.cv$lambda.min),col='red',lty = 2,lwd=2)
abline(v=log(elastNet4.mortality.postOp.cv$lambda.min),col='green',lty = 2,lwd=2)
legend(1, 95, legend=c("class", "auc"))
legend(x=1, legend=c("class", "auc",'mae','deviance'))

plot(elastNet.mortality.postOp.cv)

test2=predict(elastNet.mortality.postOp, newx = mm.X.mortality.postOp, type = "class", s = elastNet.mortality.postOp.cv$lambda.min)

sum(test2==TRUE)
#### significant variables
round(exp(coef.elastNet.mortality.postOp[which(abs(coef.elastNet.mortality.postOp[,1])!=0),]),2)
str(df.clean$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
str(df$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
unique(df$RF.High.Sensitivity.CRP.or.Ultra.sensitive.CRP)
str(df.clean)
#check the length
length(y.mortality)
dim(X.mortality)





############################### post-operation model ############################################
variables.exclude.postOpMort=unique(c(variables.mortality[-c(9)], 
                                      variables.discharge, variables.death, 'Deidentified.Record.ID'))
variables.exclude.postOpMort
variables.mortality

X.mortality.postOp=df.clean[!(names(df.clean) %in% variables.exclude.postOpMort)]

unique(df.clean$Mort.Mortality)

library(rpart)

y=as.factor(df.clean$Mort.Mortality)

unique(y)
sum(y=='NA')

y=as.character(y)
y.mortality.postOp=y[which(y!='NA')]
y.mortality.postOp=as.factor(y.mortality.postOp)
X.mortality.postOp=X.mortality.postOp[which(y!='NA'),]

unique(y.mortality.postOp)
length(y.mortality.postOp)
dim(X.mortality.postOp)

#train the model 
rpart.mortality.postOp = rpart(y.mortality.postOp ~ .,data=X.mortality.postOp, method="class", parms=list(split="gini"))
rpart.mortality.postOp = rpart(y.mortality.postOp ~ .,data=X.mortality.postOp, method="class", parms=list(split="information"))

table(predict(rpart.mortality.postOp, X.mortality.postOp, type="class"), y.mortality.postOp)

## Initial Tree T0, we have 6 terminal nodes
print(rpart.mortality.postOp)
post(rpart.mortality.postOp,filename="")

## Or simplified plot
plot(rpart.mortality.postOp,compress=TRUE)
text(rpart.mortality.postOp)

##
unique(df.clean$Mort.Location)
unique(df.clean$Mort.Prim.Cause)

## Training & Test Errors for Tree T0
predict(rpart.mortality.postOp,X.mortality.postOp)
yhat.mortality.postOp = ifelse(predict(rpart.mortality.postOp,X.mortality.postOp)[,2] < 0.5, 1, 2)
yhat.mortality.postOp

sum(yhat.mortality.postOp != y.mortality.postOp)/length(y.mortality.postOp)
#training error 0.01500766

sum(y.mortality.postOp==1)/length(y.mortality.postOp)
sum(y.mortality.postOp==2)



