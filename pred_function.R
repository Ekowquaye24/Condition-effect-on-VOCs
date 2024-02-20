
pred_function <- function(test =""){
  
  # REMOVE NEW VOCs FOUND IN THE TEST DATA
  VOC.test <- names(test)[-1]
  vname.rm <- VOC.test[!is.element(VOC.test, VOC.train)]
  #length(vname.rm)  
  test1 <- test %>% dplyr::select(-all_of(vname.rm))
  
  # ADD ADDITIONAL COLUMNS FROM THE TRAINING DATA
  vname0 <- VOC.train[!is.element(VOC.train, VOC.test)]
  #length(vname0)
  dat.test <- cbind(test1, matrix(0, NROW(test), length(vname0)))
  names(dat.test) <- c(names(test1), vname0) 
  # setequal(names(dat.test)[-1], vnames)
  dat.test %>% dplyr::select(all_of(c("ID", VOC.train))) -> dat.test  # RE-ARRANGE VOCs
  #names(dat.test) <- as.character(c("ID", paste("X", 1:(NCOL(dat.test)-1), sep="")))
  dim(dat.test)
  
  
  # PREDICTION WITH ncvreg MODEL DIRECTLY
  cutoff.best <- c.0
  X.test <- dat.test %>% dplyr::select(all_of(variables.selected)) %>% as.matrix() 
  yhat0 <- predict(cv.fit, X.test, type="response", lambda=lambda.1se)
  yhat0 <- yhat0 %>% round(digits = 4) %>% as.vector
  ypred=sign(yhat0>= cutoff.best)+0
  table(ypred)
  
  final_dat<- data.frame(cbind("Label"=test[,1], "Response"=factor(ypred),"score"= yhat0, test[,-1]))
  
  positive<-final_dat %>% 
    filter(Response == 1) %>% 
    dplyr::select(Label, Response, score)%>%
    kbl(caption = "Labels that appeared to be positive after prediction") %>%
    kable_classic_2(full_width = F)
  
  return(list(ypred=ypred, data=final_dat, pos_class=positive))
}