require(caret)
require(data.table)
require(tidyverse)


# Divide em treino, teste e oot -------------------------------------------
dados <- readRDS('data/processed/base_modelo.RDS')

oot <- dados[safra >= '2014-11-01']
dados <- dados[safra < '2014-11-01']

set.seed(123)
idx <- createDataPartition(dados$y,p=.7, list = F)

dados[idx, sample:= 'treino']

dados[-idx, sample:= 'teste']


oot[,sample:='oot']

dados <- rbind(dados,oot)


dados[,.(.N, BadRate = mean(y)),sample]


saveRDS(dados,'data/processed/treino_teste_oot.RDS')