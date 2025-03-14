require(data.table)
require(lubridate)


# Le dados ----------------------------------------------------------------

dados <- fread('https://drive.google.com/uc?export=download&id=1DBWmSbe4RZvS1TbNF-NrqB1FRRvOagDU')

# Verifica se id é unico --------------------------------------------------

dados[,.N,id][N>1,.N]


# Ver a classe dos dados -------------------------------------

str(dados)

# Transformar safra para data para facilitar as analises

dados[, safra:= ymd(paste0(safra, '01'))]


# Verifica se há na na resposta -------------------------------------------

dados[is.na(y),.N]

# Salva dados com alterações ----------------------------------------------

saveRDS(dados, 'data/processed/base_modelo.RDS')
