library(data.table)

load("C:/Users/a14565900/Downloads/Execucao PNAE 2021.RData")

dados <- as.data.table(execucao.pnae.2021) # Convertendo para data table

# Vendo onde o valor executado é negativo
dados[valor.executado.ano < 0, summary(valor.executado.ano)]

# Vê as linhas onde os valores são negativos
i <- which(dados[, valor.executado.ano < 0])
View(dados[i])

table(dados[valor.executado.ano < 0, summary(pc.executado.ano)])

table(dados[valor.executado.ano < 0, summary(valor.executado.ano)])

dados[i, valor.executado.ano := NA]
dados[i, pc.executado.ano := NA]

# -------------------------------------------------------------------

dados[pc.executado.ano < .70, i.baixa.exec := TRUE]
dados[pc.executado.ano >= .70, i.baixa.exec := FALSE]

dados[, summary(i.baixa.exec)]
# -------------------------------------------------------------------

dados[, table(pc.executado.ano < .70)]

dados[, sum(i.baixa.exec, na.rm = T)]

# -------------------------------------------------------------------

dados[, list(nome.mun, pc.repasse.trim.1.ano,
             pc.repasse.trim.2.ano,
             pc.repasse.trim.3.ano,
             pc.repasse.trim.4.ano)]

dados[, pc.repasse.sem.1 := pc.repasse.trim.1.ano + pc.repasse.trim.2.ano]
dados[, summary(pc.repasse.sem.1)]
dados[, hist(pc.repasse.sem.1)]


# -------------------------------------------------------------------

table(dados[, pc.repasse.sem.1 < .50])

# -------------------------------------------------------------------

dados[, list(media.pc.executado = mean(pc.executado.ano, na.rm = T)), by = dados$sigla.uf]

dados[, mean(pc.executado.ano, na.rm = T), by = dados$regiao]

dados[, mean(pc.executado.ano, na.rm = T), by = list(sigla.uf, regiao)]

dados[, sum(repasse.ano, na.rm = T), by = regiao]

# ------------------------------------------------------------------

dados[i.baixa.exec == TRUE, sum(repasse.ano, na.rm = T)]/10^9

dados[, sum(repasse.ano, na.rm = T), by = i.baixa.exec]

dados[i.baixa.exec == FALSE, sum(repasse.ano, na.rm = T)]/10^9

# ------------------------------------------------------------------

covariaveis <- covariaveis.2021

dados_mesclados <- merge(dados, covariaveis, by = "cod.mun")

View(dados_mesclados)

nrow(dados_mesclados)
ncol(dados_mesclados)
lapply(lapply(dados_mesclados, is.na), sum)

