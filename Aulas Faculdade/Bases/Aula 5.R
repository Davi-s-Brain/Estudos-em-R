library(data.table)
library(scales)

load("C:/Users/a14565900/Downloads/Execucao Programas Universais FNDE.RData")

dados <- as.data.table(execucao)

View(dados)

length(unique(covariaveis$cod.mun))


# ---------------------------------------------------

covariaveis[, .N, by=list(cod.mun)] # Contando os quantos códigos de município aparecem

# ---------------------------------------------------

dados[, table(programa.fnde, ano)]

# ---------------------------------------------------
# Fazendo a soma dos repasses dos anos
soma_repasse <- dados[, list(soma_repasse = sum(repasse.ano)), by=list(programa.fnde, ano)]

soma_repasse[, list(programa.fnde, ano, soma_repasse/10^6)]

dados[, soma_repasse := sum(repasse.ano), by=list(programa.fnde, ano)]

View(dados[, list(cod.mun, soma_repasse, ano)])

# ---------------------------------------------------
# Filtrando os valores presentes usando o %in%
dados <- dados[programa.fnde %in% c("PDDE", "PNAE", "PNATE")]
dados <- dados[! programa.fnde %in% c("PDDE ESTRUTURA", "PDDE QUALIDADE")]

dados[, table(programa.fnde, ano)]

# ---------------------------------------------------

dados <- dados[! pc.executado.ano == -Inf]

dados[, 
      list(media.pc.executado.ano = mean(pc.executado.ano, na.rm = T)), 
      by = list(programa.fnde, ano)]

View(dados[pc.executado.ano == -Inf])

# ---------------------------------------------------

dados[, i.execucao.100.pc := pc.executado.ano == 1]
dados[, table(i.execucao.100.pc)]

dados[, sum(i.execucao.100.pc), by=list(programa.fnde, ano)]

# ---------------------------------------------------

dados[programa.fnde == "PNAE", table(i.execucao.100.pc)]

# ---------------------------------------------------

# Gráficos

dados.2 <- merge(execucao, covariaveis[, - "sigla.uf"], by = c("cod.mun", "ano"), all.x = T)
names(dados.2)
View(dados.2)


dados.2[pc.executado.ano < 0, pc.executado.ano := NA]
dados.2[pc.executado.ano > 1, pc.executado.ano := 1]

dados.2 <- execucao[!is.na(pc.executado.ano) & programa.fnde == "PNAE" & ano == 2021]

table(dados.2$regiao)

barplot(table(dados.2$regiao), 
        main = "Distribuição por Região",
        ylab = "Frequência",
        col = "#f2e57f",
        border = "white",
        cex.main = 2.9,
        cex.names = .9)

boxplot(dados.2$pc.executado.ano,
        main = "Proporção PDDE Executado em 2021",
        ylab = "Proporção Executada",
        col = "red"
        )

hist(dados.2$pc.executado.ano,
     main = "Proporção PDDE Executado em 2021",
     breaks = seq(0, 1, by = 0.05),
     col = "pink"
     )

media <- mean(dados.2$pc.executado.ano)
medias.uf <- dados.2[, list(media = mean(pc.executado.ano)), by = sigla.uf]

barplot(media ~ sigla.uf, data = medias.uf, 
        main = "Média de Execução por UF",
        cex.names = 0.5,  # fator de expansão do tamanho da fonte dos rótulos dos nomes
        las = 2,          # rótulos perpendiculares ao eixo
        col = "green")


names(dados.2)

