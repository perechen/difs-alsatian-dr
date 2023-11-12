
sample_vectors <- function(x=pool,char,n_size) {

## char vs. other pools

  c_target <- x %>% filter(char_id==char)
  others <- x %>% anti_join(c_target,by="char_id")

## index words to samples

  chr_samples <-  paste(char, sample_bag(nrow(c_target),sample_words = n_size),sep="_")
  other_samples <- paste("others", sample_bag(nrow(others),sample_words = n_size),sep="_")

## join together

  sampled <- c_target %>%
    mutate(sample_id=chr_samples,isTarget=TRUE) %>%
    bind_rows(others %>% mutate(sample_id=other_samples)) %>% 
    filter(!str_detect(sample_id, "NA$"))

## turn to frequency vectors

  v <- sampled %>%
#  filter(sample_id == selection_id$sample_id) %>% 
  group_by(sample_id) %>% 
  count(word) %>%
  pivot_wider(names_from = word,values_from = n, values_fill =0)

  return(v)


}



### LOO

loo_brute <- function(x=v,v_expected,char) {

v_predicted <- v_expected

for(i in 1:nrow(v)) {
train=v[-i,]
test=v[i,]

m <- svm(as.factor(sample_id) ~.,
         kernel="linear",
         data = train,
         scale = T)

pr <- predict(m,test[,-1]) %>% table()
v_predicted[i] <- names(which(pr == 1))
}


#err = v$sample_id[m$accuracies == 0]

#if(length(err) == 0) {
#  v_predicted <- v_expected
#} else {
#  v_predicted[m$accuracies == 0] <- ifelse(err=="others",as.character(char),"others")
#}


return(v_predicted)


}

### LOO caret
loo_caret <- function(x=v) {

library(caret)
  loocv = trainControl(method="LOOCV",savePred = T)
  

  svm1 <- train(sample_id ~., 
                data = x,
                method = "svmLinear",
                preProcess=c("center","scale"),
                trControl=loocv) %>% suppressWarnings()
  


cm <- caret::confusionMatrix(svm1$pred[,1], svm1$pred[,2])

return(cm)

}

### top feature extraction


extract_top <- function(x=v) {

## "full" model
m <- svm(as.factor(sample_id) ~., kernel="linear",data = x,scale = T)
## support vector slopes
w = t(m$coefs) %*% m$SV

## top feature
top_f = which(w==max(w))
top_f
if(length(top_f) > 1) {
 top_f <- top_f[1]
}

if(w[top_f] == 0) {
  print("Features are shit!")
}

f <- colnames(w)[top_f]

## words that are also system words...
if(startsWith(f,"X.")) {
  f <- str_remove(f,"^X\\.")
  f <- str_remove(f, "\\.$")
}

return(f)

}



#### Weighted log-odds

logodds_curve <- function(x=pool,oversampling=F,char=chr_pool$char_id[c],features=50) {
  

current_target = char

long_df <- x %>% mutate(char_id=ifelse(char_id==current_target,char_id,"others"))

if(oversampling) {
  other_w <- table(long_df$char_id)["others"]
  sp <- long_df %>% group_by(char_id) %>% group_split()
  char_oversample <- sp[[1]] %>%ungroup() %>%  sample_n(other_w,replace = T)
  
  sp[[1]] <- char_oversample
  long_df <- bind_rows(sp)
}


lo <- long_df %>% count(char_id,word) %>% bind_log_odds(char_id,word,n) 

top_curve <- lo %>% filter(char_id==current_target) %>% top_n(features,log_odds_weighted) %>%   arrange(-log_odds_weighted) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank <= features)

return(top_curve)
}

calculate_auc <- function(df=top_curve) {

x  <- df$rank
y <- df$log_odds_weighted
id <- order(x)
AUC <- sum(diff(x[id])*zoo::rollmean(y[id],2))

return(AUC)
}


# lo %>% 
#   group_by(char_id) %>% 
#   top_n(50) %>%
#   arrange(char_id,-log_odds_weighted) %>% 
#   mutate(rank=row_number()) %>% 
#   ungroup() %>% 
#   ggplot(aes(rank,log_odds_weighted,color=char_id)) + geom_path() + facet_wrap(~char_id,scales="free_x")
