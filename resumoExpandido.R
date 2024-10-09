dados = read.csv2("datatran2021.csv")

dados[sapply(dados, is.character)] = lapply(dados[sapply(dados, is.character)], as.factor)
summary(dados)

#Frequência da causa de acidentes
ftabela = table(dados$causa_acidente, useNA = "ifany") 
ptabela = round(prop.table(ftabela)*100,1)
tabela1 <- data.frame(Causa_dos_acidentes = names(ftabela), Frequencia = as.vector(ftabela), Porcentagem = as.vector(ptabela))
tabela1 <- tabela1[order(-tabela1$Frequencia), ]
colnames(tabela1) <- c("Causa dos acidentes","Frequencia","Porcentagem") 
tabela1
write.table(tabela1,"Tabela.csv", sep=";", dec=",", row.names=FALSE)

#Gráfico da frequência
top_n <- 10
tabela_top <- tabela1[1:top_n, ]

library(ggplot2)
ggplot(tabela_top, aes(x = reorder(`Causa dos acidentes`, Frequencia), y = Frequencia)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  xlab("Causa dos acidentes") +
  ylab("Frequência") +
  ggtitle(paste("Top", top_n, "Causas de Acidentes")) +
  theme_minimal()

#qual dia da semana ocorre mais acidentes
diatabela = table(dados$dia_semana, useNA = "ifany")
ptabela = round(prop.table(diatabela)*100,1)
tabela2 <- data.frame(Dias_semana = names(diatabela), Frequencia = as.vector(diatabela), Porcentagem = as.vector(ptabela))
tabela2 <- tabela2[order(-tabela2$Frequencia), ]
write.table(tabela2,"Tabela2.csv", sep=";", dec=",", row.names=FALSE)

#Gráfico de frequencia de acidentes por cada dia de semana
library(ggplot2)
ggplot(tabela2, aes(x = reorder(Dias_semana, -Frequencia), y = Frequencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência de Acidentes por Dia da Semana", x = "Dia da Semana", y = "Frequência Total") +
  theme_minimal()


#condicao metereologica
dtabela = table(dados$condicao_metereologica, useNA = "ifany")
ptabela = round(prop.table(dtabela)*100,1)
tabela3 <- data.frame(condicao_meteorologica = names(dtabela), Frequencia = as.vector(dtabela), Porcentagem = as.vector(ptabela))         ##BRUNO: cpa faz sentido colocar a fase do dia na msm tabela (n sei fazer isso)
tabela3 <- tabela3[order(-tabela3$Frequencia), ]
write.table(tabela3,"Tabela3.csv", sep=";", dec=",", row.names=FALSE)

#Gráfico da relação  frequência de acidentes em cada condição metereológica
tabela3$condicao_meteorologica <- factor(tabela3$condicao_meteorologica, levels = tabela3$condicao_meteorologica)
ggplot(data = tabela3, aes(x = condicao_meteorologica, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência de Acidentes por Condição Meteorológica",
       x = "Condições Meteorológicas",
       y = "Frequência Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Tipo de Acidente
tipotabela = table(dados$tipo_acidente, useNA = "ifany")
ptabela = round(prop.table(tipotabela)*100,1)
tabela4 <- data.frame(Tipos_acidente = names(tipotabela), Frequencia = as.vector(tipotabela), Porcentagem = as.vector(ptabela))
tabela4 <- tabela4[order(-tabela4$Frequencia), ]
colnames(tabela4) <- c("Tipo de Acidente","Frequencia Total","Porcentagem")
write.table(tabela4,"Tabela4.csv", sep=";", dec=",", row.names=FALSE)


#Gráfico dos tipos de acidentes
ggplot(data = tabela4, aes(x = factor(`Tipo de Acidente`, levels = tabela4$`Tipo de Acidente`), 
                           y = `Frequencia Total`, fill = `Tipo de Acidente`)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_text(aes(label = paste0(Porcentagem, "%")), 
            position = position_dodge(0.9), 
            vjust = -0.5,  
            color = "black") +
  labs(title = "Frequência de Acidentes por Tipo",
       x = "Tipos de Acidente",
       y = "Frequência Total") +
  theme_minimal() +
  coord_flip()

#Cidade com mais acidentes
cidadetabela = table(dados$municipio, useNA = "ifany")
ptabela = round(prop.table(cidadetabela)*100,1)
tabela5 <- data.frame(Cidade_tabela = names(cidadetabela), Frequencia = as.vector(cidadetabela), Porcentagem = as.vector(ptabela))
tabela5 <- tabela5[order(-tabela5$Frequencia), ]
colnames(tabela5) <- c("Municipio","Frequencia Total","Porcentagem")
write.table(tabela5,"Tabela5.csv", sep=";", dec=",", row.names=FALSE)

#Gráfico das 5 cidades com mais acidentes
ggplot(tabela_top, aes(x = reorder(Municipio, -`Frequencia Total`), y = `Frequencia Total`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  xlab("Município") +
  ylab("Frequência Total") +
  ggtitle(paste("Top", top_n, "Cidades com Mais Acidentes")) +
  theme_minimal() +
  geom_text(aes(label = `Frequencia Total`), 
            position = position_stack(vjust = 0.5), 
            hjust = -0.2, 
            color = "black")



#Taxa de mortalidade dos acidentes por causa do acidente
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

#Gráfico taxa de mortalidade dos acidentes por causa do acidente
library(ggplot2)

# Criar um gráfico de barras para visualizar a taxa de mortalidade
ggplot(acidentes_por_causa, aes(x = reorder(causa_acidente, -taxa_mortalidade), y = taxa_mortalidade)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Causa do Acidente", y = "Taxa de Mortalidade (%)", 
       title = "Taxa de Mortalidade por Causa de Acidente") +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_text(aes(label = round(taxa_mortalidade, 1)), 
            position = position_stack(vjust = 0.5), 
            hjust = -0.2, 
            color = "black")



#Quantidade de feridos por causa de acidente
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