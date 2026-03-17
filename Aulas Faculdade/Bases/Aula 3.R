library(data.table)

dados <- as.data.table(execucao.pnae.2021) # Convertendo para data table

summary(dados) # Sumarizando os dados
nrow(dados) # Contando número de linhas
ncol(dados) # Contando número de colunas

View(dados[sigla.uf == "SP", list(sigla.uf, nome.mun, repasse.ano)]) # Pega o estado de SP, e exibe as colunas sigla da UF, nome do município e o repasse do ano

dados[, mean(dados$pc.executado.ano, na.rm = T), by = list(sigla.uf)] # Média do executado ano, agrupado por Mun

dados.sp = dados[sigla.uf == "SP"]
write.csv2(dados.sp, file = "Execucao PNAE 2021 SP.csv") # Salva o arquivo em csv

# -----------------------------------------------------------------------------

dim(dados) # Vê quantas linhas e colunas temos
names(dados) # Vê os nomes das variáveis

#grep(names(dados), patterns = "execucao", value = T)

str(dados) # Descrição bem completa dos dados

length(unique(dados$cod.mun)) # Os códigos dos municípios se repetem?


dados[, i.baixa.execucao := pc.executado.ano < .70] # Ve se o executado é abaixo de 70%

dados[, .N, by = list(sigla.uf)] # Conta a quantidade de municípios

dados[, frec.nome.num := .N, by = list(nome.mun)] # Ve a frequÊncia de cada nome do município e cria nova coluna

lapply(lapply(dados, is.na), sum) # Conta quantos NA tem por coluna
