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

rf <- train(
  tgt~.,
  treino[,c(pre_selected, 'tgt'), with = F],
  method = 'ranger' )




