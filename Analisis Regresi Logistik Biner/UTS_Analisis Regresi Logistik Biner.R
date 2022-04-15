damin <- read.csv2("C:/Users/ASUS/Downloads/daminfix.csv", na = "?", stringsAsFactors=TRUE)
data <- damin 
View(data)
attach(data)
dim(data)
str(damin)
#int/num = age, trestbps, chol, thalac, oldpeak
#factor  = Sex, CP, fbs, restecg, exang, slope, ca, thal, num
data$Sex <- factor(data$Sex)
data$CP <- factor(data$CP)
data$fbs <- factor(data$fbs)
data$restecg <- factor(data$restecg)
data$exang <- factor(data$exang)
data$slope <- factor(data$slope)
data$ca <- factor(data$ca)
data$thal <- factor(data$thal)
data$num <- factor(data$num)
str(data) #cek tipe data lagi

##DATA CLEANING
#Cek Missing Value
colSums(is.na(as.data.frame(data))) 

#Mengatasi Missing Value
#karena nilai variabel ca dan thal bersifat kategori maka
#penanganannya menggunakan modus
mode <- function(data){
  uqx <- unique(data)
  tab <- table(data)
  sort(uqx)[tab == max(tab)]
}
data$ca[is.na(data$ca)] <- mode(data$ca)
data$thal[is.na(data$thal)] <- mode(data$thal)
colSums(is.na(as.data.frame(data))) #cek ulang
  
#Noisy Data
#outlier
#variabel bukan numerik: sex, cp, fbs, restecg, exang, slope, ca, thal, num
#variabel tersebut tidak perlu dicek outlier
boxplot(data)
par(mfrow=c(2,3))
bp1 = boxplot(data$Age,col="yellow",main="Age") ; bp1$out
bp2 = boxplot(data$trestbps, col = "yellow", main = "trestbps") ; bp2$out
bp3 = boxplot(data$chol, col="yellow",main="chol") ; bp3$out
bp4 = boxplot(data$thalac, col="yellow",main="thalac") ; bp4$out
bp5 = boxplot(data$oldpeak, col="yellow",main="oldpeak") ; bp5$out
#terdapat outlier pada variabel trestbps, chol,thalac, oldpeak,
#removeoutlier
which(data$trestbps %in% c(bp2$out))
which(data$chol %in% c(bp3$out))
which(data$thalac %in% c(bp4$out))
which(data$oldpeak %in% c(bp5$out))
data = data[-c(15, 84, 127, 173, 184, 189, 202, 214,
                      232, 49, 122, 153, 174, 182, 246, 92, 124, 192, 286),]
colnames(data)[colnames(data)=="num"] = "Disease"
View(data)

#===============================
levels(data$Sex) <- c("Female", "Male")
levels(data$CP) <- c("Typical angina", "Atypical angina", "No angina", "Asymptomatic")
levels(data$fbs) <- c("No", "Yes")
levels(data$restecg) <- c("Normal", "Abnormalities","Hypertrophy")
levels(data$exang) <- c("No", "Yes")
levels(data$slope) <- c("Upsloping", "Flat", "Downsloping")
levels(data$thal) <- c("Normal flow", "Fixed defect", "Reversible defect")
levels(data$Disease) <- c("No", "Yes")
head(data)

# STATISTIKA DESKRIPTIF =============================
# QUANTITATIF ( stat desk, hist, boxplot)
# AGE
summary(data$Age)
hist(Age,data = data, main="Age of Patient", xlab="years", col="pink")
ggplot(data, aes(Age, fill=Disease)) + 
  geom_histogram(binwidth=5) +
  labs(fill="Disease", x="Age", y="Number of patients")
boxplot(Age,data = data, main="Age of Patient", col="grey")
ggplot(data , aes(x = Disease, y = Age)) +
  geom_boxplot()

# TRESTBPS
summary(data$trestbps)
hist(trestbps,data = data, main="Resting Blood Pressure", xlab="Blood Pressure(mmHg)", col="pink")
ggplot(data, aes(trestbps, fill=Disease)) + 
  geom_histogram(binwidth=5) +
  labs(fill="Disease", x="Resting Blood Pressure", y="Number of patients")
boxplot(trestbps,data = data, main="Resting Blood Pressure", col="grey")
ggplot(data , aes(x = Disease, y = trestbps)) +
  geom_boxplot()

# CHOL
summary(data$chol)
hist(chol,data = data, main="Cholestrol", xlab="Cholestrol(mg/dL", col="pink")
ggplot(data, aes(chol, fill=Disease)) + 
  geom_histogram(binwidth=5) +
  labs(fill="Disease", x="Cholestrol", y="Number of patients")
boxplot(chol,data = data, main="Cholestrol", col="grey")
ggplot(data , aes(x = Disease, y = chol)) +
  geom_boxplot()

# THALAC
summary(data$thalac)
hist(thalac,data = data, main="Maximum Heart Rate", xlab="Max Heart Rate (bpm)", col="pink")
ggplot(data, aes(thalac, fill=Disease)) + 
  geom_histogram(binwidth=5) +
  labs(fill="Disease", x="Max Heart Rate", y="Number of patients")
boxplot(thalac,data = data, main="Maximum Heart Rate", col="grey")
ggplot(data , aes(x = Disease, y = thalac)) +
  geom_boxplot()

# OLDPEAK
summary(data$oldpeak)
hist(oldpeak,data = data, main="ST depression induced by exercise", xlab="ST depression", col="pink")
ggplot(data, aes(oldpeak, fill=Disease)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease", x="ST depression", y="Number of patients")
boxplot(oldpeak,data = data, main="ST depression induced by exercise", col="grey")
ggplot(data , aes(x = Disease, y = oldpeak)) +
  geom_boxplot()

# QUALITATIF ( bar chart, )
# Sex
counts <- table(data$Sex)
barplot(counts, main="Gender of Patient",
        xlab="Sex")
ggplot(data, aes(Sex, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Sex", y="Number of patients")
  
# CP
counts <- table(data$CP)
barplot(counts, main="Type of Chest Pain",
        xlab="Type")
ggplot(data, aes(CP, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Type", y="Number of patients")

# Fbs
counts <- table(data$fbs)
barplot(counts, main="Fasting Blood Sugar",
        xlab="FBS")
ggplot(data, aes(fbs, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="FBS", y="Number of patients")

# restecg
counts <- table(data$restecg)
barplot(counts, main="Resting Electrocardiographic Results",
        xlab="Restecg")
ggplot(data, aes(restecg, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Restecg", y="Number of patients")

# exang
counts <- table(data$exang)
barplot(counts, main="Exercise Induced Angina ",
        xlab="Exang")
ggplot(data, aes(exang, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Exang", y="Number of patients")

# slope
counts <- table(data$slope)
barplot(counts, main="The slope of peak exercise at segment",
        xlab="Slope")
ggplot(data, aes(slope, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Slope", y="Number of patients")

# ca
counts <- table(data$ca)
barplot(counts, main="Number of Major Vessels",
        xlab="Ca")
ggplot(data, aes(ca, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Ca", y="Number of patients")

# thal
counts <- table(data$thal)
barplot(counts, main="Thalassemia",
        xlab="Thalassemia")
ggplot(data, aes(thal, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Thalassemia", y="Number of patients")

# Disease
counts <- table(data$Disease)
barplot(counts, main="Angiographic disease status ",
        xlab="Disease")
ggplot(data, aes(Disease, fill=Disease)) + 
  geom_bar() +
  labs(fill="Disease", x="Disease", y="Number of patients")

# ANALISIS REGRESI LOGISTIK ===================
# Stratifikasi Data
library(dplyr)
hn <- filter(data,Disease=="No")
hd <- filter(data,Disease=="Yes")

# Split Data
set.seed(104)
acak.hn <- sample(1:nrow(hn), 0.7*nrow(hn))
acak.hd <- sample(1:nrow(hd), 0.7*nrow(hd))
disease.train <- rbind(hn[acak.hn,],hd[acak.hd,])
disease.test <- rbind(hn[-acak.hn,],hd[-acak.hd,])

#Pembentukan Model
logit1 <- glm(Disease ~ Age+Sex+CP+trestbps+chol+fbs+restecg+thalac+exang+oldpeak+slope+ca+thal, data=disease.train,family = binomial(link="logit"))
summary(logit1)

logit2 <- step(logit1,direction = "backward")
summary(logit2)

# Memilih model terbaik dengan aic (model 2)
AIC <- data.frame(logit1$aic,logit2$aic) ; AIC

# Prediksi
prob.prediksi<-predict(logit2, disease.test, type="response")
prediksi<-ifelse(prob.prediksi>0.5,"Yes","No")
pred.aktual<-data.frame("Prediksi"=prediksi,"Aktual"=disease.test$Disease)
head(pred.aktual)

# Akurasi dan Presisi
library(caret)
akurasi = confusionMatrix(as.factor(prediksi), disease.test$Disease) ; akurasi



exp(1.2575)

exp(1.6299)

exp(1.1936)

exp(-1.1857)

exp(-0.5660)

exp(-1.0792)

exp(-1.4473)
exp(-2.3332)
exp(-1.5207)

exp(1.6698)
