# Katherine Grisanzio

##---------Load packages---------

library(qdap)
library(dplyr)
library(tm)
library(textstem)
library(textdata)   
library(tidyverse)
library(textreg)
library(readr)  
library(wordcloud)
library(anacor)
library(tidytext) 
library(ldatuning)
library(topicmodels)   
library(Hmisc)
library(caret)
library(Matrix)
library(glmnet)
library(ggthemes)
library(e1071)
library(pROC)
require(neuralnet)  
require(NeuralNetTools)  
require(keras)  
library(devtools)
if (!require(Rstem)) install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if (!require(sentiment)) {install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz"); require(sentiment)}


##---------Load data---------

jealousy_data <- read.csv("jealousy_survey_data.csv")
emotion_data <- read.csv("emotion_data.csv")

##---------Process data---------

# Calculating a summary "jealousy" score for the jealousy data that will be used later for prediction

jealousy_data$interference_thought <- factor(jealousy_data$interference_thought, levels = c("None", "Slight interference with social or other activities, but overall performance not impaired", "Definite interference with social or occupational performance, but still manageable", "Causes substantial impairment in social or occupational performance", "Incapacitating"))
jealousy_data$interference_thought <- as.numeric(jealousy_data$interference_thought)
jealousy_data$interference_thought <- jealousy_data$interference_thought-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$time_thought <- factor(jealousy_data$time_thought, levels = c("None", "Less than 1 hr/day or occasional occurrence", "1 to 3 hrs/day or frequent", "Greater than 3 and up to 8 hrs/day or very frequent occurrence", "Greater than 8 hrs/day or nearly constant occurrence"))
jealousy_data$time_thought <- as.numeric(jealousy_data$time_thought)
jealousy_data$time_thought <- jealousy_data$time_thought-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$distress_thoughts <- factor(jealousy_data$distress_thoughts, levels = c("None", "Not too disturbing", "Disturbing, but still manageable", "Very disturbing", "Near constant and disabling distress"))
jealousy_data$distress_thoughts <- as.numeric(jealousy_data$distress_thoughts)
jealousy_data$distress_thoughts <- jealousy_data$distress_thoughts-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$resistance_thoughts <- factor(jealousy_data$resistance_thoughts, levels = c("Try to resist all the time", "Try to resist most of the time", "Make some effort to resist", "Yield to all retroactive jealousy-intrusive thoughts without attempting to control them, but with some reluctance", "Completely and willingly yield to all retroactive jealousy-intrusive thoughts"))
jealousy_data$resistance_thoughts <- as.numeric(jealousy_data$resistance_thoughts)
jealousy_data$resistance_thoughts <- jealousy_data$resistance_thoughts-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$control_thoughts <- factor(jealousy_data$control_thoughts, levels = c("Complete control", "Usually able to stop or divert retroactive jealousy-intrusive thoughts with some effort and concentration", "Sometimes able to stop or divert retroactive jealousy-intrusive thoughts", "Rarely successful in stopping or dismissing retroactive jealousy-intrusive thoughts, can only divert attention with difficulty", "Retroactive jealousy-intrusive thoughts are completely involuntary, rarely able to even momentarily alter obsessive thinking"))
jealousy_data$control_thoughts <- as.numeric(jealousy_data$control_thoughts)
jealousy_data$control_thoughts <- jealousy_data$control_thoughts-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$time_behav <- factor(jealousy_data$time_behav, levels = c("None", "Less than 1 hr/day or occasionally", "From 1 to 3 hrs/day, or frequently", "More than 3 and up to 8 hrs/day, or very frequently", "More than 8 hrs/day, or nearly constantly"))
jealousy_data$time_behav <- as.numeric(jealousy_data$time_behav)
jealousy_data$time_behav <- jealousy_data$time_behav-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$interference_behav <- factor(jealousy_data$interference_behav, levels = c("None", "Slight interference with social or other activities, but overall performance not impaired", "Definite interference with social or occupational performance, but still manageable", "Causes substantial impairment in social or occupational performance", "Incapacitating"))
jealousy_data$interference_behav <- as.numeric(jealousy_data$interference_behav)
jealousy_data$interference_behav <- jealousy_data$interference_behav-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$distress_behav <- factor(jealousy_data$distress_behav, levels = c("None", "Only slightly anxious if behaviors prevented", "Anxiety would mount but remain manageable if behaviors prevented", "Prominent and very disturbing increase in anxiety if behaviors interrupted", "Incapacitating anxiety from any intervention aimed at modifying activity"))
jealousy_data$distress_behav <- as.numeric(jealousy_data$distress_behav)
jealousy_data$distress_behav <- jealousy_data$distress_behav-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$resist_behav <- factor(jealousy_data$resist_behav, levels = c("Always try to resist", "Try to resist most of the time", "Make some effort to resist", "Yield to almost all retroactive jealousy-behaviors without attempting to control them, but with some reluctance", "Completely and willingly yield to all retroactive jealousy-behaviors"))
jealousy_data$resist_behav <- as.numeric(jealousy_data$resist_behav)
jealousy_data$resist_behav <- jealousy_data$resist_behav-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

jealousy_data$control_behav <- factor(jealousy_data$control_behav, levels = c("Complete control", "Pressure to perform the behavior but usually able to exercise voluntary control over it", "Strong pressure to perform behavior, can control it only with difficulty", "Very strong drive to perform behavior, can only delay with difficulty", "Drive to perform behavior experienced as completely involuntary and overpowering, rarely able to even momentarily delay activity"))
jealousy_data$control_behav <- as.numeric(jealousy_data$control_behav)
jealousy_data$control_behav <- jealousy_data$control_behav-1 # subtracting 1 so that the values are c(0,1,2,3,4) rather than c(1,2,3,4,5)

# Creating summary variables for the behavior and thoughts subscales total scores
jealousy_data$ybocs_tho_total <- rowSums(jealousy_data[,c("interference_thought", "time_thought", "distress_thoughts", "resistance_thoughts", "control_thoughts")])
jealousy_data$ybocs_bhv_total <- rowSums(jealousy_data[,c("time_behav", "interference_behav", "distress_behav", "resist_behav", "control_behav")])

# creating a total score for the entire adapted Y-BOCS
jealousy_data$ybocs_total <- rowSums(jealousy_data[,c("interference_thought", "time_thought", "distress_thoughts", "resistance_thoughts", "control_thoughts", "time_behav", "interference_behav", "distress_behav", "resist_behav", "control_behav")])


##---------Support vector machines---------

# We'll use the emotion_data to try out SVMs
head(emotion_data)

# Make the numeric DV (perceived_rejection) a binary variable, with 1 representing "high" levels of perceived rejection and 0 "low" levels
emotion_data$perceived_rejection_binary <- as.numeric(cut2(emotion_data$perceived_rejection, g=2)) 
emotion_data$perceived_rejection_binary <- as.factor(emotion_data$perceived_rejection_binary)
levels(emotion_data$perceived_rejection_binary)[levels(emotion_data$perceived_rejection_binary) == "1"]  <- "Low_perc_rejection"
levels(emotion_data$perceived_rejection_binary)[levels(emotion_data$perceived_rejection_binary) == "2"] <- "High_perc_rejection"

# Split into training a test data
set.seed(123)
split <- createDataPartition(emotion_data$perceived_rejection_binary, p = 0.8, list = FALSE) # 80% training, 20% test
emotion_train <- emotion_data[split, ]
Xtrain <- emotion_train[,5:32]
emotion_test <- emotion_data[-split, ]
dim(emotion_train)
dim(emotion_test)

# Linear SVM 
fitControl <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
Cgrid <- expand.grid(C = 2^seq(-4, 4))
Cgrid

set.seed(123)
emotion_svmlin <- caret::train(Xtrain, emotion_train$perceived_rejection_binary, method = "svmLinear", metric = "ROC", 
                               preProc = c("center", "scale"), tuneGrid = Cgrid, trControl = fitControl)
emotion_svmlin


# Predictive evaluation using test data 
linclass <- predict(emotion_svmlin, newdata = emotion_test[,5:32])
confusionMatrix(data = emotion_test$perceived_rejection_binary, reference = linclass, positive = "High_perc_rejection")
plin <- predict(emotion_svmlin, newdata = emotion_test[,5:32], type = "prob")[,"High_perc_rejection"]
roclin <- roc(response = emotion_test$perceived_rejection_binary, predictor = plin, levels = rev(levels(emotion_test$perceived_rejection_binary)))
plot(roclin, legacy.axes = TRUE, asp = NULL, print.auc = TRUE)
# Here we see the tradeoff between sensitivity and specificity


##---------Neural networks---------

scalenn <- function(data) {
  max <- apply(data, 2, max)
  min <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = min, scale = max - min))
}

emotion_train <- emotion_train[,c(2:3, 5:42)]
emotion_test <- emotion_test[,c(2:3, 5:42)]
train <- scalenn(model.matrix(~., emotion_train)[,-1]) # create design matrix and standardize for training
head(train)
predind <- c(1:30) # predictor index
test <- scalenn(model.matrix(~., emotion_test)[,-1]) # create design matrix and standardize for test
head(test)

# Predict perceived_rejection_binary
f <- as.formula(c("perceived_rejection_binaryHigh_perc_rejection~", paste(colnames(train)[predind], collapse = " + ")))
f

# One hidden layer with 20 neurons, and 5 replications
set.seed(123)
fit1 <- neuralnet(f, hidden = 20, linear.output = FALSE, act.fct = "logistic", rep = 5, data = train)
garson(fit1) # variable importance
dev.new()
plot(fit1, rep = "best")

# Test data evaluation
pred1 <- neuralnet::compute(fit1, test[, predind])$net.result # predicted probabilities test data
head(pred1)
# predicted classes
pred1r <- round(pred1, 0) 
head(pred1r)
# confusion matrix of test data
confmat <- table(observed = test$perceived_rejection_binaryHigh_perc_rejection, predicted = pred1r) 
confmat  
round(sum(diag(confmat))/sum(confmat)*100, 2)


##---------Deep neural networks---------

# Standardize
mean <- apply(emotion_train[3:30], 2, mean)
std <- apply(emotion_train[3:30], 2, sd)
train_data <- scale(emotion_train[3:30], center = mean, scale = std)
test_data <- scale(emotion_test[3:30], center = mean, scale = std) # standardize test data with training mean
train_y <- emotion_train$perceived_rejection
test_y <- emotion_test$perceived_rejection

# Build the network
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", input_shape = dim(train_data)[[2]]) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1) 
  model %>% compile(optimizer = "rmsprop", loss = "mse", metrics = c("mae"))
}

# k-fold cross-validation
set.seed(123)
k <- 3
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
folds
num_epochs <- 100
all_mae_history <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_y <- train_y[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_y <- train_y[-val_indices]
  
  model <- build_model()
  
  history <- model %>% keras::fit(partial_train_data, partial_train_y, epochs = num_epochs, 
                                  validation_data = list(val_data, val_y), batch_size = 1, verbose = 0)
  
  mae_history <- history$metrics$val_mae
  all_mae_history <- rbind(all_mae_history, mae_history)
}  

# Plot CV results
average_mae_history <- data.frame(epoch = seq(1:ncol(all_mae_history)), 
                                  validation_mae = apply(all_mae_history, 2, mean))
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()
# We determine the optimal number of epochs through cross validation, and can see here
# that the optimal number is around 30

model <- build_model()

# Fit the model with 30 epochs
model %>% keras::fit(train_data, train_y, epochs = 30, batch_size = 16)
# evaluate on test data
result <- model %>% keras::evaluate(test_data, test_y)
result


##---------Natural language processing---------

# We'll use the jealousy_data to try out NLP
head(jealousy_data)

# The jealousy_data contains several free response questions that 
# we'll analyze:
#  "trigger" - asks what triggered the participant's worst retroactive jealousy experience
#  "intrusive.thoughts" - asks participants to describe the content of the intrusive thoughts that are related to their retroactive jealousy
#  "partner_response"- asks participants to describe how their partner typically responds to their retroactive jealousy

# Take a look at the variables
head(jealousy_data$trigger)
head(jealousy_data$intrusive.thoughts)
head(jealousy_data$partner_response)
str(jealousy_data$trigger)
str(jealousy_data$intrusive.thoughts)
str(jealousy_data$partner_response)

# Switch from factor to character
jealousy_data$trigger <- as.character(jealousy_data$trigger)
jealousy_data$intrusive.thoughts <- as.character(jealousy_data$intrusive.thoughts)
jealousy_data$partner_response <- as.character(jealousy_data$partner_response)


# Word clouds for first visualization, before pre-processing
dev.new()
set.seed(123)
wordcloud(jealousy_data$trigger, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()

dev.new()
wordcloud(jealousy_data$intrusive.thoughts, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()

dev.new()
wordcloud(jealousy_data$partner_response, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()


# Remove punctuation, switch to lower case, remove stop words, strip white space, lemmatize, and remove numbers

myStopwords <- c("importidqid24text", "angerfrustration", "din", "boyfriendgirlfriend", "triadsopen", "themwhether", 
                 "partener", "imaginationintrusive", "importidqid25text", "romanticsexual", "attractivesexyinteresting",
                 "flirtingconnecting", "hás")

jealousy_data$trigger_clean <- jealousy_data$trigger %>% 
  removePunctuation %>% 
  tolower %>% 
  removeWords(c(stopwords("en"), myStopwords)) %>% 
  stripWhitespace %>% 
  lemmatize_strings %>% 
  removeNumbers
jealousy_data$intrusive.thoughts_clean <- jealousy_data$intrusive.thoughts %>% 
  removePunctuation %>% 
  tolower %>% 
  removeWords(c(stopwords("en"), myStopwords)) %>% 
  stripWhitespace %>% 
  lemmatize_strings %>% 
  removeNumbers
jealousy_data$partner_response_clean <- jealousy_data$partner_response %>% 
  removePunctuation %>%
  tolower %>% 
  removeWords(c(stopwords("en"), myStopwords)) %>% 
  stripWhitespace %>% 
  lemmatize_strings %>% 
  removeNumbers


# More word clouds after processing
dev.new()
set.seed(123)
wordcloud(jealousy_data$trigger_clean, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()

dev.new()
wordcloud(jealousy_data$intrusive.thoughts_clean, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()

dev.new()
wordcloud(jealousy_data$partner_response_clean, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE, main = "Original Words")
dev.off()


# Convert into tm text corpus
trigger_corp <- Corpus(VectorSource(jealousy_data$trigger_clean)) 
inspect(trigger_corp)

intrus_thoughts_corp <- Corpus(VectorSource(jealousy_data$intrusive.thoughts_clean)) 
inspect(intrus_thoughts_corp)

partner_resp_corp <- Corpus(VectorSource(jealousy_data$partner_response_clean)) 
inspect(partner_resp_corp)


# Compute tf-idf weighted term-document matrix
trigger_dtm <- DocumentTermMatrix(trigger_corp, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
dim(trigger_dtm) # 162 participants, 1022 words
as.matrix(trigger_dtm)[, 1:10] # show the first 10 words    

intrus_thoughts_dtm <- DocumentTermMatrix(intrus_thoughts_corp, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
dim(intrus_thoughts_dtm) # 162 participants, 818 words
as.matrix(intrus_thoughts_dtm)[, 1:10] # show the first 10 words  

partner_resp_dtm <- DocumentTermMatrix(partner_resp_corp, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
dim(partner_resp_dtm) # 162 participants, 688 words
as.matrix(partner_resp_dtm)[, 1:10] # show the first 10 words  


# Check whether empty documents were created and remove them
ind <- which(rowSums(as.matrix(trigger_dtm)) > 0)   
length(ind) # 120 are non-empty
trigger_dtm <- trigger_dtm[ind, ]
dim(trigger_dtm)

ind2 <- which(rowSums(as.matrix(intrus_thoughts_dtm)) > 0)   
length(ind2) # 116 are non-empty
intrus_thoughts_dtm <- intrus_thoughts_dtm[ind2, ]
dim(intrus_thoughts_dtm)

ind3 <- which(rowSums(as.matrix(partner_resp_dtm)) > 0)   
length(ind3) # 115 are non-empty
partner_resp_dtm <- partner_resp_dtm[ind3, ]
dim(partner_resp_dtm)

# Fit 2D CA solution and plot the columns (terms) using textplot()
fitca_trigger <- anacor(trigger_dtm) 
fitca_trigger


# Classifying emotions 
emotions_trigger <- as.data.frame(classify_emotion(jealousy_data$trigger, algorithm = "bayes"))
head(emotions_trigger, 20) # we see some responses cannot be classified

best_trigger <- emotions_trigger$BEST_FIT %>% as.factor() # extract emotions
length(best_trigger)
barplot(table(best_trigger), main = "Jealousy Triggers Emotions", las = 2)
# Apparently lots of anger and joy

# prepare data to be shown in a word cloud
em_trigger <- split(jealousy_data$trigger, best_trigger)
str(em_trigger) # list of length 4, according to emotion categories
names(em_trigger)
em_trigger <- lapply(em_trigger, paste, collapse = " ")
str(em_trigger)
em_trigger <- do.call(c, em_trigger)
em_corp_trigger <- VCorpus(VectorSource(em_trigger))
emTDM_trigger <- TermDocumentMatrix(em_corp_trigger, control = list(weighting = weightTf, removePunctuation = TRUE, 
                                                                    stopwords = stopwords("en")))
emTDM_trigger2 <- as.matrix(emTDM_trigger)
colnames(emTDM_trigger2) <- levels(best_trigger)

dev.new()
set.seed(123)
comparison.cloud(emTDM_trigger2, scale = c(3, 0.45))


# Topic models

# create tf weighted DTMs
trigger_dtm_tf <- DocumentTermMatrix(trigger_corp)
trigger_dtm_tf
intrus_dtm_tf <- DocumentTermMatrix(intrus_thoughts_corp)
intrus_dtm_tf
partner_dtm_tf <- DocumentTermMatrix(partner_resp_corp)
partner_dtm_tf

# tf-idf filtering
tfidf_trigger <- tapply(trigger_dtm_tf$v/row_sums(trigger_dtm_tf)[trigger_dtm_tf$i], trigger_dtm_tf$j, mean) * log2(nDocs(trigger_dtm_tf)/col_sums(trigger_dtm_tf > 0))
trigger_dtm_tf2 <- trigger_dtm_tf[, tfidf_trigger >= median(tfidf_trigger)] # median cut
ind_trigger <- which(rowSums(as.matrix(trigger_dtm_tf2)) > 0)
trigger_dtm_tf2 <- trigger_dtm_tf2[ind_trigger, ] # keep 50% of the most important words
trigger_dtm_tf2

tfidf_intrus <- tapply(intrus_dtm_tf$v/row_sums(intrus_dtm_tf)[intrus_dtm_tf$i], intrus_dtm_tf$j, mean) * log2(nDocs(intrus_dtm_tf)/col_sums(intrus_dtm_tf > 0))
intrus_dtm_tf2 <- intrus_dtm_tf[, tfidf_intrus >= median(tfidf_intrus)] # median cut
ind_intrus <- which(rowSums(as.matrix(intrus_dtm_tf2)) > 0)
intrus_dtm_tf2 <- intrus_dtm_tf2[ind_intrus, ] # keep 50% of the most important words
intrus_dtm_tf2

tfidf_partner <- tapply(partner_dtm_tf$v/row_sums(partner_dtm_tf)[partner_dtm_tf$i], partner_dtm_tf$j, mean) * log2(nDocs(partner_dtm_tf)/col_sums(partner_dtm_tf > 0))
partner_dtm_tf2 <- partner_dtm_tf[, tfidf_partner >= median(tfidf_partner)] # median cut
ind_partner <- which(rowSums(as.matrix(partner_dtm_tf2)) > 0)
partner_dtm_tf2 <- partner_dtm_tf2[ind_partner, ] # keep 50% of the most important words
partner_dtm_tf2


# find a good number of topics
set.seed(123)
topics_trigger <- FindTopicsNumber(trigger_dtm_tf2, topics = 2:30, metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014"))
FindTopicsNumber_plot(topics_trigger)
# we could try 6 topics

intrus_trigger <- FindTopicsNumber(intrus_dtm_tf2, topics = 2:30, metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014"))
FindTopicsNumber_plot(intrus_trigger)
# we could try 6 topics here too

partner_trigger <- FindTopicsNumber(partner_dtm_tf2, topics = 2:30, metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014"))
FindTopicsNumber_plot(partner_trigger)
# we could try 5 topics here

# fit topic model
set.seed(123)
trigger_lda <- LDA(trigger_dtm_tf2, k = 6)
trigger_lda

intrus_lda <- LDA(intrus_dtm_tf2, k = 6) 
intrus_lda

partner_lda <- LDA(partner_dtm_tf2, k = 5) 
partner_lda


postprob_trigger <- posterior(trigger_lda)
pterms_trigger <- as.data.frame(t(postprob_trigger$terms))
round(head(pterms_trigger, 10), 4) # probabilistic assignments of words to clusters
terms(trigger_lda, 5) # top 5 terms in each topic

postprob_intrus <- posterior(intrus_lda)
pterms_intrus <- as.data.frame(t(postprob_intrus$terms))
round(head(pterms_intrus, 10), 4) 
terms(intrus_lda, 5) 

postprob_partner <- posterior(partner_lda)
pterms_partner <- as.data.frame(t(postprob_partner$terms))
round(head(pterms_partner, 10), 4) 
terms(partner_lda, 5)


# visualization using word clouds
set.seed(123)
w2_trigger <- pterms_trigger %>% mutate(word = rownames(pterms_trigger)) %>% gather(topic, weight, -word)
n <- 50
pal <- rep(brewer.pal(9, "Greys"), each = ceiling(n/9))[n:1]
dev.new()
op <- par(mfrow = c(3,2), mar = c(3,0,2,0))
K <- 6
for (i in 1:K) {
  w3_trigger <- w2_trigger %>% dplyr::filter(topic == i) %>% arrange(desc(weight))
  with(w3_trigger[1:n, ], wordcloud(word, freq = weight, scale = c(2, 0.5), random.order = FALSE, ordered.colors = TRUE, 
                                    colors = pal))
  title(paste("Jealousy Trigger", i))
}
par(op)

w2_intrus <- pterms_intrus %>% mutate(word = rownames(pterms_intrus)) %>% gather(topic, weight, -word)
n <- 50
pal <- rep(brewer.pal(9, "Greys"), each = ceiling(n/9))[n:1]
dev.new()
op <- par(mfrow = c(3,2), mar = c(3,0,2,0))
K <- 6
for (i in 1:K) {
  w3_intrus <- w2_intrus %>% dplyr::filter(topic == i) %>% arrange(desc(weight))
  with(w3_intrus[1:n, ], wordcloud(word, freq = weight, scale = c(2, 0.5), random.order = FALSE, ordered.colors = TRUE, 
                                   colors = pal))
  title(paste("Intrusive Thought Content", i))
}
par(op)

w2_partner <- pterms_partner %>% mutate(word = rownames(pterms_partner)) %>% gather(topic, weight, -word)
n <- 50
pal <- rep(brewer.pal(9, "Greys"), each = ceiling(n/9))[n:1]
dev.new()
op <- par(mfrow = c(3,2), mar = c(3,0,2,0))
K <- 5
for (i in 1:K) {
  w3_partner <- w2_partner %>% dplyr::filter(topic == i) %>% arrange(desc(weight))
  with(w3_partner[1:n, ], wordcloud(word, freq = weight, scale = c(2, 0.5), random.order = FALSE, ordered.colors = TRUE, 
                                    colors = pal))
  title(paste("Partner Response to Jealousy", i))
}
par(op)


# Prediction

# jealousy_data$ybocs_total isour response variable, which is a summary measure of 
# retroactive jealousy severity. The intrusive thought content free response variable
# is our predictor, as we expect this to relate most strongly to overall jealousy scores. 

# Make the numeric variable a binary variable, with 1 representing "high" levels of jealousy and 0 "low" levels
jealousy_data$ybocs_total_binary <- as.numeric(cut2(jealousy_data$ybocs_total, g=2)) 
table(jealousy_data$ybocs_total_binary) # low jealousy (1) vs high jealousy (2)

# Remove rows that have missing data for the DV
jealousy_data_narm <- jealousy_data[!is.na(jealousy_data$ybocs_total_binary), ]  

# Split  into training and test data (50-50 split)
set.seed(111)
train <- createDataPartition(jealousy_data_narm$ybocs_total_binary, p = 0.5, list = FALSE)
train_jealousy <- jealousy_data_narm[train, ] # training data
test_jealousy <- jealousy_data_narm[-train, ] # test data

# Cleanup pipeline on training data
clean_train <- train_jealousy$intrusive.thoughts %>% removePunctuation %>% tolower %>% removeWords(stopwords("en")) %>% stripWhitespace
y <- as.factor(train_jealousy$ybocs_total_binary) # response (as factor, no empty documents)

# Create training DTM using the RTextTools package
train_dtm <-  RTextTools::create_matrix(clean_train, weighting = tm::weightTfIdf)  
train_dtm
train_matrix <- train_dtm %>% as.matrix %>% Matrix(sparse = TRUE) # convert into sparse matrix
dim(train_matrix) # 57 observations, 647 predictors (words)


# Fit lasso with 10-fold CV
set.seed(123)
cv_lasso_jealousy <- cv.glmnet(x = train_matrix, y = y, family = "binomial", nfolds = 10, intercept = FALSE, type.measure = "class")
plot(cv_lasso_jealousy)

log(cv_lasso_jealousy$lambda.min)
log(cv_lasso_jealousy$lambda.1se)

# Fit model and predict on training data
fit_lasso <- glmnet(train_matrix, y, alpha = 1, lambda = cv_lasso_jealousy$lambda.1se, family = "binomial") 
preds <- predict(fit_lasso, train_matrix, type = "class") 
caret::confusionMatrix(as.factor(preds), reference = y, positive = "1")
# 77% accuracy


# Evaluate lasso on test data, do the same pre-processing that we did for the training data
clean_test <- test_jealousy$intrusive.thoughts %>% removePunctuation %>% tolower %>% removeWords(stopwords("en")) %>% stripWhitespace
ytest <- as.factor(test_jealousy$ybocs_total_binary) # response, as a factor

# Create the test DTM, tf-idf weighting (make it so it has the same words as in the training DTM)
test_dtm <- RTextTools::create_matrix(clean_test, weighting = tm::weightTfIdf, originalMatrix = train_dtm)  
test_matrix <- test_dtm %>% as.matrix %>% Matrix(sparse = TRUE)
dim(test_matrix)
dim(train_matrix)

# Predictions on test data
predtest <- predict(fit_lasso, test_matrix, type = "class") 
caret::confusionMatrix(as.factor(predtest), reference = ytest, positive = "1")


# Visualization of most important words in the regression model
glmnet_coef <- as.matrix(coef(fit_lasso))
ind <- order(abs(glmnet_coef[,1]), decreasing = TRUE)
best30 <- glmnet_coef[ind[1:30], ] # top 30 predictors
sort(best30)

# Odds scale
best30odds <- exp(best30)
sort(best30odds)

# Probability scale
best30probs <- best30odds/(1 + best30odds)
sort(best30probs)

# ggplot for top 30 words
impact <- ifelse(as.vector(best30) > 0, "positive", "negative") 
top.coef <- data.frame(words = as.character(names(best30)), coefs = as.vector(best30), impact = impact, stringsAsFactors = FALSE)
top.coef$words <- factor(top.coef$words, levels = unique(top.coef$words))
str(top.coef)

ggplot(top.coef, aes (x = coefs, y = words)) + 
  geom_segment(aes(yend = words), xend = 0, colour = "grey50") + 
  geom_point(size = 3, aes(colour = impact)) + theme_few()
# The word "jealousy" had the highest positive impact in our regression. "Thought" had a slight negative impact, indicating the more
# someone wrote "thought", the smaller jealousy score they had. 







