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
tabela2 = data.frame(diatabela,ptabela)
tabela2 = tabela2[,-3]
colnames(tabela2) <- c("Dia Da Semana","Frequencia Total","Porcentagem") 

#condicao metereologica
dtabela = table(dados$condicao_metereologica, useNA = "ifany")
ptabela = round(prop.table(dtabela)*100,1)
tabela3 = data.frame(dtabela,ptabela)         ##BRUNO: cpa faz sentido colocar a fase do dia na msm tabela (n sei fazer isso)
tabela3 = tabela3[,-3]
colnames(tabela3) <- c("Condicao metereologica","Frequencia Total","Porcentagem") 

#tipo_acidente
tipotabela = table(dados$tipo_acidente, useNA = "ifany")
ptabela = round(prop.table(tipotabela)*100,1)
tabela4 = data.frame(tipotabela,ptabela)
tabela4 = tabela4[,-3]
colnames(tabela4) <- c("Tipo de Acidente","Frequencia Total","Porcentagem") 

#cidade com mais acidentes
cidadetabela = table(dados$municipio, useNA = "ifany")
ptabela = round(prop.table(cidadetabela)*100,1)
tabela5 = data.frame(cidadetabela,ptabela)
tabela5 = tabela5[,-3]
colnames(tabela5) <- c("Municipio","Frequencia Total","Porcentagem") 

#causa de acidente mais frequente
library(dplyr)
acidentes_fatais <- dados %>% filter(mortos > 1)
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

