require(caret)
require(tidyverse)
require(Information)
require(data.table)


dados <- readRDS('data/processed/treino_teste_oot.RDS')



# Retira variáveis irrelevantes -------------------------------------------
# Menos que .02 de IV

treino <- dados[sample=='treino']
treino[,sample:=NULL]
setnafill(treino, fill = -500)

iv <- Information::create_infotables(treino, y = 'y', parallel = F)

iv <- iv$Summary

setDT(iv)

pre_selected <- iv[IV>=0.02, Variable]

treino[,tgt:= as.factor(ifelse(y,'inad', 'ad'))]


# Treina RF ---------------------------------------------------------------
set.seed(123)
rf <- train(
  tgt~.,
  treino[,c(pre_selected, 'tgt'), with = F],
  method = 'ranger',
  trControl = trainControl(
    method= 'cv',number = 5, 
    verboseIter = T,
    summaryFunction = twoClassSummary,
    classProbs = T),
  metric = 'ROC',
  importance = "impurity",
  tuneGrid = expand.grid(
    splitrule = 'gini',
    mtry = c(4,6,8,10,12,20,40), # priorizar Valores em torno de raiz de m,
    min.node.size = 10
   ))

plot(rf)



varImp(rf,scale = F) %>% plot()

rf$results

teste <- dados[sample=='teste']
teste[,sample:=NULL]
setnafill(teste, fill = -500)

teste[,pred_rf := predict(rf,.SD,type = 'prob')$inad]
teste[, MLmetrics::AUC(y_pred = pred_rf,
                            y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_rf,
                       y_true = y)]



# Treina Xgb --------------------------------------------------------------
set.seed(123)
xgb <- train(
  tgt~.,
  treino[,c(pre_selected, 'tgt'), with = F],
  method = 'xgbTree',
  trControl = trainControl(
    method= 'cv',number = 5, 
    verboseIter = T,
    summaryFunction = twoClassSummary,
    classProbs = T),
  metric = 'ROC',
  tuneGrid = expand.grid(
    nrounds = 1:5 *100, 
    eta = c(0.01, 0.05, 0.1),
    max_depth = c(3, 6, 9), 
    gamma = c(0, 1, 5), 
    colsample_bytree = c(0.5, 0.7),
    min_child_weight = c( 3, 5),
    subsample = c( 0.75, 1) 
  ))

plot(xgb)

varImp(xgb,scale = F) %>% plot()



teste[,pred_xgb := predict(xgb,.SD,type = 'prob')$inad]
teste[, MLmetrics::AUC(y_pred = pred_xgb,
                       y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_xgb,
                           y_true = y)]

saveRDS(xgb, 'models/xgb.RDS')


# Retira variaveis pouco importantes --------------------------------------


imp <- varImp(xgb,scale = F)$importance

selected1 <- setdiff(row.names(imp)[imp>.01], c('VAR_27', 'VAR_30'))

set.seed(123)
xgb1 <- train(
  tgt~.,
  treino[,c(selected1, 'tgt'), with = F],
  method = 'xgbTree',
  trControl = trainControl(
    method= 'cv',number = 5, 
    verboseIter = T,
    summaryFunction = twoClassSummary,
    classProbs = T),
  metric = 'ROC',
  tuneGrid = expand.grid(
    nrounds = c(100,200,300), # restringindo parametros para evitat ober
    eta = c(0.01,0.02),
    max_depth = xgb$bestTune$max_depth, 
    gamma = c(xgb$bestTune$gamma,1,5), 
    colsample_bytree = xgb$bestTune$colsample_bytree,
    min_child_weight = xgb$bestTune$min_child_weight,
    subsample = xgb$bestTune$subsample
  ))

plot(xgb1)
varImp(xgb1,scale = F) %>% plot()

teste[,pred_xgb1 := predict(xgb1,.SD,type = 'prob')$inad]
teste[, MLmetrics::AUC(y_pred = pred_xgb1,
                       y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_xgb1,
                           y_true = y)]
teste[, MLmetrics::AUC(y_pred = pred_xgb1,
                       y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_xgb1,
                           y_true = y), keyby = safra]

# queda de ks nas ultimas safras

saveRDS(xgb1, 'models/xgb1.RDS')



# Retira variaveis pouco importantes --------------------------------------

# Retira tbm variável 1 para testar se ela está causando instabilidade

selected2 <- setdiff(row.names(imp)[imp>.01], c('VAR_1', 'VAR_27', 'VAR_30'))

set.seed(123)
xgb2 <- train(
  tgt~.,
  treino[,c(selected2, 'tgt'), with = F],
  method = 'xgbTree',
  trControl = trainControl(
    method= 'cv',number = 5, 
    verboseIter = T,
    summaryFunction = twoClassSummary,
    classProbs = T),
  metric = 'ROC',
  tuneGrid = expand.grid(
    nrounds = c(100,200,300), # restringindo parametros para evitar overfiting
    eta = c(0.01,0.02),
    max_depth = xgb$bestTune$max_depth, 
    gamma = xgb$bestTune$gamma, 
    colsample_bytree = xgb$bestTune$colsample_bytree,
    min_child_weight = xgb$bestTune$min_child_weight,
    subsample = xgb$bestTune$subsample
  ))

plot(xgb2)
varImp(xgb2,scale = F) %>% plot()

teste[,pred_xgb2 := predict(xgb2,.SD,type = 'prob')$inad]
teste[, MLmetrics::AUC(y_pred = pred_xgb2,
                       y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_xgb2,
                           y_true = y)]

teste[, MLmetrics::KS_Stat(y_pred = pred_xgb2,
                           y_true = y), keyby = safra]

# queda de ks na ultima safras

saveRDS(xgb2, 'models/xgb2.RDS')


# Retira variaveis pouco importantes --------------------------------------

# Retira tbm variável 1 para testar se ela está causando instabilidade

set.seed(123)
xgb3 <- train(
  tgt~.,
  treino[,c(selected1, 'tgt'), with = F],
  method = 'xgbTree',
  trControl = trainControl(
    method= 'cv',number = 5, 
    verboseIter = T,
    summaryFunction = twoClassSummary,
    classProbs = T),
  metric = 'ROC',
  tuneGrid = expand.grid(
    nrounds = c(100,200,300,400,500), # restringindo parametros para evitar overfiting
    eta = c(0.005,0.001),
    max_depth = xgb$bestTune$max_depth, 
    gamma = xgb$bestTune$gamma, 
    colsample_bytree = xgb$bestTune$colsample_bytree,
    min_child_weight = xgb$bestTune$min_child_weight,
    subsample = xgb$bestTune$subsample
  ))

plot(xgb3)
varImp(xgb3,scale = F) %>% plot()

teste[,pred_xgb3 := predict(xgb3,.SD,type = 'prob')$inad]
teste[, MLmetrics::AUC(y_pred = pred_xgb3,
                       y_true = y)]
teste[, MLmetrics::KS_Stat(y_pred = pred_xgb3,
                           y_true = y)]

teste[, MLmetrics::KS_Stat(y_pred = pred_xgb3,
                           y_true = y), keyby = safra]

# queda de ks na ultima safras

saveRDS(xgb3, 'models/xgb3.RDS')


