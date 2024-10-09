dados = read.csv2("datatran2021.csv")

dados[sapply(dados, is.character)] = lapply(dados[sapply(dados, is.character)], as.factor)
summary(dados)

ftabela = table(dados$causa_acidente, useNA = "ifany") 
ptabela = round(prop.table(ftabela)*100,1)
tabela1 <- data.frame(Causa_dos_acidentes = names(ftabela), Frequencia = as.vector(ftabela), Porcentagem = as.vector(ptabela))
tabela1 <- tabela1[order(-tabela1$Frequencia), ]
colnames(tabela1) <- c("Causa dos acidentes","Frequencia","Porcentagem") 
tabela1
write.table(tabela1,"Tabela.csv", sep=";", dec=",", row.names=FALSE)

#qual dia da semana ocorre mais acidentes
diatabela = table(dados$dia_semana, useNA = "ifany")
ptabela = round(prop.table(diatabela)*100,1)
tabela2 <- data.frame(Dias_semana = names(diatabela), Frequencia = as.vector(diatabela), Porcentagem = as.vector(ptabela))
tabela2 <- tabela2[order(-tabela2$Frequencia), ]
colnames(tabela2) <- c("Dia Da Semana","Frequencia Total","Porcentagem") 

#condicao metereologica
dtabela = table(dados$condicao_metereologica, useNA = "ifany")
ptabela = round(prop.table(dtabela)*100,1)
tabela3 <- data.frame(condicao_meteorologica = names(dtabela), Frequencia = as.vector(dtabela), Porcentagem = as.vector(ptabela))         ##BRUNO: cpa faz sentido colocar a fase do dia na msm tabela (n sei fazer isso)
tabela3 <- tabela3[order(-tabela3$Frequencia), ]
colnames(tabela3) <- c("Condicao metereologica","Frequencia Total","Porcentagem") 

#tipo_acidente
tipotabela = table(dados$tipo_acidente, useNA = "ifany")
ptabela = round(prop.table(tipotabela)*100,1)
tabela4 <- data.frame(Tipos_acidente = names(tipotabela), Frequencia = as.vector(tipotabela), Porcentagem = as.vector(ptabela))
tabela4 <- tabela4[order(-tabela4$Frequencia), ]
colnames(tabela4) <- c("Tipo de Acidente","Frequencia Total","Porcentagem") 

#cidade com mais acidentes
cidadetabela = table(dados$municipio, useNA = "ifany")
ptabela = round(prop.table(cidadetabela)*100,1)
tabela5 <- data.frame(Cidade_tabela = names(cidadetabela), Frequencia = as.vector(cidadetabela), Porcentagem = as.vector(ptabela))
tabela5 <- tabela5[order(-tabela5$Frequencia), ]
colnames(tabela5) <- c("Municipio","Frequencia Total","Porcentagem") 

#causa de acidente mais frequente
library(dplyr)
acidentes_fatais <- dados %>% filter(mortos >= 1)
causa_acidente_fatal <- acidentes_fatais %>%
  group_by(causa_acidente) %>%
  summarise(total_mortos = sum(mortos)) %>%
  arrange(desc(total_mortos))

dados <- dados %>% mutate(acidente_fatal = ifelse(mortos > 0, 1, 0))
acidentes_por_causa <- dados %>%
  group_by(causa_acidente) %>%
  summarise(
    total_acidentes = n(),
    acidentes_fatais = sum(acidente_fatal)
  ) %>%
  mutate(taxa_mortalidade = acidentes_fatais / total_acidentes * 100) %>%
  arrange(desc(taxa_mortalidade))

#quantidade de feridos por causa de acidente
acidentes_feridos <- dados %>% filter(feridos >= 1)
causa_acidente_ferido <- acidentes_feridos %>%
  group_by(causa_acidente) %>%
  summarise(total_feridos = sum(feridos)) %>%
  arrange(desc(total_feridos))

#Gravidade por tipo de acidente
gravidade_por_tipo_acidente <- dados %>%
  group_by(tipo_acidente) %>%
  summarise(
    total_mortes = sum(mortos),
    total_feridos = sum(feridos),
    total_acidentes = n()
  ) %>%
  mutate(
    media_mortes_por_acidente = total_mortes / total_acidentes,
    media_feridos_por_acidente = total_feridos / total_acidentes
  ) %>%
  arrange(desc(total_mortes))

