### JAWABAN NOMOR 1 ### =====================================================
library(tm)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)

berita<-read.csv("D:/THALITA/Materi Kuliah/SMT 5/DATA MNING/Data Train Berita.csv", header=TRUE, sep = ",",quote ="",
                 stringsAsFactors=FALSE)
attach(berita)
str(berita)
berita$Articles
berita$Text
berita$Category

## kemudian buat seperti database kamus untuk berita yg tsb
beritacorpus<-VCorpus(VectorSource(berita$Text))
beritacorpus
str(beritacorpus)

# buat mariks kata dokumen atau Document Term Matrix dengan perintah sbb
DTMberita<-DocumentTermMatrix(beritacorpus, control=list(tolower=TRUE,
                                                         removeNumbers=TRUE, stopwords=TRUE, removePunctuation=TRUE,
                                                         stemming=TRUE))
DTMberita
inspect(DTMberita)
str(DTMberita)
DTMberita$dimnames$Terms

# Membuat matrix
dokDTM<-TermDocumentMatrix(beritacorpus) ; dokDTM
ma<-as.matrix(dokDTM)
vec<-sort(rowSums(ma),decreasing = TRUE)
## melihat 6 data terbanyak yg digunakan ##
dec<-data.frame(word=names(vec),freq=vec)
head(dec)
str(dec)
dec$word
dec$freq

# WORDCLOUD
wordcloud(words = dec$word, freq = dec$freq,
          min.freq = 1, max.words=50, random.order=FALSE,
          rot.per=0.35, colors=brewer.pal(8, "Dark2")) 

# ASOSIASI
veec<-as.list(findAssocs(dokDTM, terms =c("the", "and", "for"), 
                         corlimit =c(0.15,0.15,0.15,0.15,0.15,0.15)))
head(veec$the) 
head(veec$and) 
head(veec$for) 

### JAWABAN NOMOR 2 ### =====================================================
library(NLP)
library(tm)
library(e1071)
library(naivebayes)
berita<-read.csv("D:/THALITA/Materi Kuliah/SMT 5/DATA MNING/Data Train Berita.csv", header=TRUE, sep = ",",quote ="",
                 stringsAsFactors=FALSE)
attach(berita)
str(berita)
berita$Articles
berita$Text
berita$Category

# Untuk Category dijadikan factor
berita$Category<-factor(berita$Category)
## kemudian buat seperti database kamus untuk berita yg tsb
beritacorpus<-VCorpus(VectorSource(berita$Text))
beritacorpus
str(beritacorpus)
beritacorpus[[4]]$content

# buat mariks kata dokumen atau Document Term Matrix dengan perintah sbb
DTMberita<-DocumentTermMatrix(beritacorpus, control=list(tolower=TRUE,
                                                         removeNumbers=TRUE, stopwords=TRUE, removePunctuation=TRUE,
                                                         stemming=TRUE))
DTMberita
inspect(DTMberita)

str(DTMberita)
DTMberita$dimnames$Terms
beritafreq<-findFreqTerms(DTMberita,1) ; beritafreq
DTMberita.freq<-DTMberita[,beritafreq] ; DTMberita.freq

# Kemudian dengan sedikit penambahan sebuah fyngsi (convert_counts) yang akan mengubah nilai 0
# jadi "no" dan 1 jadi "yes" untuk mempersiapkan data di training dalam Naive Bayes,
convert_counts<- function(x) {x<-ifelse(x>0, "yes","no")}
berita.train<-apply(DTMberita.freq, MARGIN=2, convert_counts)
berita.train


# Kemudian, untuk kategorisasinya / Category kita buat sebagai faktor
berita.Category <- factor(berita$Category)
berita.klas <- naiveBayes(berita.train,berita.Category,laplace=1) ; berita.klas

# DATA TEST
tanya<-read.csv("D:/THALITA/Materi Kuliah/SMT 5/DATA MNING/Data Test Berita.csv", header=TRUE,quote ="",
               stringsAsFactors=FALSE)
tanya
tanyaa<-read.csv("D:/THALITA/Materi Kuliah/SMT 5/DATA MNING/Data Test Beritaa.csv", header=TRUE, sep = "," ,quote ="",
                stringsAsFactors=FALSE)
tanyacorpus <- VCorpus(VectorSource(tanya$Text)) ; tanyacorpus
tanyaDTM <-DocumentTermMatrix(tanyacorpus,control=list(tolower=TRUE,
                                                       removeNumbers=TRUE, stopwords=TRUE, removePunctuation=TRUE,
                                                       stemming=TRUE))
tanyaDTM

tanyafreq<-findFreqTerms(tanyaDTM,1) ; tanyafreq
tanyaDTMfreq <- tanyaDTM[,tanyafreq] ; tanyaDTMfreq
convert_counts<-function(y){y<-ifelse(y>0,"yes","no")}
tanyatest<-apply(tanyaDTMfreq,MARGIN=2,convert_counts) ; tanyatest
hasilc<-predict(berita.klas,tanyatest,type="class") ; hasilc
hasilr<-predict(berita.klas,tanyatest,type="raw") ; hasilr

hasil=confusionMatrix(table(hasilc,tanyaa$Category)) ; hasil

### JAWABAN NOMOR 3 ### =====================================================
library(RTextTools)

# Configure the training data
container <- create_container(DTMberita,berita$Category, trainSize=1:40, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)
summary(model)

# new data ( anggap gapakai data test dulu biar tagnya udah ada )
predictionData <- tanya$Text

# create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix=DTMberita)
# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
# predict
results <- classify_model(predictionContainer, model)
results
hasil=confusionMatrix(table(results$SVM_LABEL,tanyaa$Category))
hasil

####
dtMatrix <- create_matrix(berita["Text"])
dtMatrix
container <- create_container(dtMatrix,berita$Category, trainSize=1:40, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)
summary(model)

# create a prediction document term matrix
predMatrix <- create_matrix(tanya, originalMatrix=dtMatrix)
# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
# predict
results <- classify_model(predictionContainer, model)
results
hasil=confusionMatrix(table(results$SVM_LABEL,tanyaa$Category))
hasil
