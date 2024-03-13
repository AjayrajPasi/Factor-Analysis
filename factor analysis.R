library(psych)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(GPArotation)

############################################################## Data import

# data loading
data<-read.csv('C:/Users/Apse360/Desktop/factor/Stat career.csv')
head(data)

# Removing dependent variable
df<-subset(data,select = -career)
colnames(df)<-c('q1','q2','q3','q4','q5','q6','q7','q8','q9','q10','q11','q12','q13','q14','q15','q16','q17','q18','q19','q20','q21','q22','q23')

############################################################# Factor tests

# Checking significance for factor analysis
KMO(df)
bartlett.test(df)
library(corrplot)
corrplot(round(cor(df),2),method='number',type = 'upper')

# Principal component analysis
pca<-princomp(df,cor = T)
summary(pca)
screeplot(pca)
plot(pca,type='l')
vss(df)

# Selecting no of factors
r<-cor(df)
eval<-eigen(r)
eval$values
nfactors=sum((eval$values>=1))
nfactors

############################################################### Rotations
# Applying factor analysis
par(mfrow=c(1,2))

# No rotation
library(psych)
fac<-fa(df,nfactors = 4,rotate = 'none')
load<-fac$loadings
fa.diagram(load,main = 'No Rotation')

# Varimax Rotation
fac1<-fa(df,nfactors = 4,rotate = 'varimax')
load1<-fac1$loadings
fa.diagram(load1,main = 'Varimax Rotation ')

# Equamax Rotation
fac2<-fa(df,nfactors = 4,rotate = 'quartimax')
load2<-fac2$loadings
fa.diagram(load2,main = 'Quartimax Rotation')

# Quartimax Rotation
fac3<-fa(df,nfactors = 4,rotate = 'equamax')
load3<-fac3$loadings
fa.diagram(load3,main = 'Equamax Rotation')

# Promax Rotation
fac4<-fa(df,nfactors = 4,rotate = 'promax')
load4<-fac4$loadings
fa.diagram(load4,main = 'Promax Rotation')

# Oblimin Rotation
fac5<-fa(df,nfactors = 4,rotate = 'oblimin')
load5<-fac5$loadings
fa.diagram(load5,main = 'Oblimin Rotation')

############################################################### Final rotation

# Varimax Rotation
fac1<-fa(df,nfactors = 4,rotate = 'varimax')
load1<-fac1$loadings
fa.diagram(load1,main = 'Varimax Rotation 4')

fac1.2<-fa(df,nfactors = 5,rotate = 'varimax')
load1.2<-fac1.2$loadings
fa.diagram(load1.2,main = 'Varimax Rotation 5')

# Equamax Rotation
fac2<-fa(df,nfactors = 4,rotate = 'equamax')
load2<-fac2$loadings
fa.diagram(load2,main = 'Equamax Rotation 4')
load2

fac2.1<-fa(df,nfactors = 5,rotate = 'equamax')
load2.1<-fac2.1$loadings
fa.diagram(load2.1,main = 'Equamax Rotation 5')

############################################################ Model plotting

############################################################### model1
# Equamax Rotation 4 factor
d1<-fac2$scores
head(d1)
colnames(d1)<-c('comp_fear','maths_fear','Stat_fear','peer_pressure')
a<-data[24]
head(a)
fdata1<-data.frame(d1,a)
fdata1$career<-as.factor(fdata1$career)
class(fdata1$career)
head(fdata1)

# splitting data
library(tidyverse) # to use %>% operator
set.seed(100)
ndata<-fdata1$career %>%
  createDataPartition(p=0.7, list=FALSE)
train_data1<-fdata1[ndata,]
test_data1<-fdata1[-ndata,]

# Regression model
model1<-glm(career~comp_fear+maths_fear+Stat_fear+peer_pressure,family='binomial',train_data1)
summary(model1)

# model accuracy checking
library(caret)
class(test_data1$career)
pred1<-predict(model1,test_data1,type = 'response')
predicted1<-ifelse(pred1 >0.50,1,0)
predicted1<-as.factor(predicted1)
class(predicted1)
# confusion matrix using formula
cm1<-confusionMatrix(data = predicted1, reference = test_data1$career)
cm1$table
# confusion matrix table 
table(predicted1,test_data1$career)
# Accuracy
accuracy1<-(484+243)/(484+24+19+243)
accuracy1

################################################################ model2
# Equamax Rotation 5 factor
d2<-fac2.1$scores
head(d2)
colnames(d2)<-c('comp_fear','maths_fear','desc_stat','stat_application','peer_pressure')
b<-data[24]
head(b)
fdata2<-data.frame(d2,b)
fdata2$career<-as.factor(fdata2$career)
class(fdata2$career)
head(fdata2)

# splitting data
library(tidyverse) # to use %>% operator
library(caret)
set.seed(101)
ndata<-fdata2$career %>%
  createDataPartition(p=0.7, list=FALSE)
train_data2<-fdata2[ndata,]
test_data2<-fdata2[-ndata,]

# Regression model (equamax-5)
model2<-glm(career~comp_fear+maths_fear+desc_stat+stat_application+peer_pressure,family='binomial',train_data2)
summary(model2)

# model accuracy checking
library(caret)
class(test_data2$career)
pred2<-predict(model2,test_data2,type = 'response')
predicted2<-ifelse(pred2 >0.50,1,0)
predicted2<-as.factor(predicted2)
class(predicted2)
# confusion matrix using formula
cm2<-confusionMatrix(data = predicted2, reference = test_data2$career)
cm2$table
# confusion matrix table 
table(predicted2,test_data2$career)
# Accuracy
accuracy2<-(483+248)/(483+20+19+248)
accuracy2

################################################################ model3
# Varimax rotation 4
d3<-fac1$scores
head(d3)
colnames(d3)<-c('Stat_fear','comp_fear','maths_fear','peer_pressure')
c<-data[24]
head(c)
fdata3<-data.frame(d3,c)
fdata3$career<-as.factor(fdata3$career)
class(fdata3$career)
head(fdata3)

# splitting data
library(tidyverse) # to use %>% operator
library(caret)
set.seed(102)
ndata<-fdata3$career %>%
  createDataPartition(p=0.7, list=FALSE)
train_data3<-fdata3[ndata,]
test_data3<-fdata3[-ndata,]

# Regression model
model3<-glm(career~Stat_fear+comp_fear+maths_fear+peer_pressure,family='binomial',train_data3)
summary(model3)

# model accuracy checking
library(caret)
class(test_data3$career)
pred3<-predict(model3,test_data3,type = 'response')
predicted3<-ifelse(pred3 >0.50,1,0)
predicted3<-as.factor(predicted3)
class(predicted3)
# confusion matrix using formula
cm3<-confusionMatrix(data = predicted3, reference = test_data3$career)
cm3$table
# confusion matrix table 
table(predicted3,test_data3$career)
# Accuracy
accuracy3<-(479+241)/(479+26+24+241)
accuracy3

############################################################### model4
# Varimax Rotation 5
d4<-fac1.2$scores
head(d4)
colnames(d4)<-c('comp_fear','stat_application','maths_fear','desc_stat','peer_pressure')
d<-data[24]
head(d)
fdata4<-data.frame(d4,d)
fdata4$career<-as.factor(fdata4$career)
class(fdata4$career)
head(fdata4)

# splitting data
library(tidyverse) # to use %>% operator
library(caret)
set.seed(103)
ndata<-fdata4$career %>%
  createDataPartition(p=0.7, list=FALSE)
train_data4<-fdata4[ndata,]
test_data4<-fdata4[-ndata,]

# Regression model
model4<-glm(career~comp_fear+maths_fear+desc_stat+stat_application+peer_pressure,family='binomial',train_data4)
summary(model4)

# model accuracy checking
library(caret)
class(test_data4$career)
pred4<-predict(model4,test_data4,type = 'response')
predicted4<-ifelse(pred4 >0.50,1,0)
predicted4<-as.factor(predicted4)
class(predicted4)
# confusion matrix using formula
cm4<-confusionMatrix(data = predicted4, reference = test_data4$career)
cm4$table
# confusion matrix table 
table(predicted4,test_data4$career)
# Accuracy
accuracy4<-(479+244)/(479+23+24+244)
accuracy4

# Best model
model1$aic;accuracy1
model2$aic;accuracy2
model3$aic;accuracy3
model4$aic;accuracy4


