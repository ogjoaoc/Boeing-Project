---
title: "Alguns graficos de correlação"
author: "Tomás Mello"
date: "2023-12-12"
output:
  html_document:
    css: ../ava.css
    highlight: pygments
    theme: cerulean
    charset: "utf-8"
  pdf_document: default
---
-Pontos para analisar, correlacoes com batida em relacao ao voo, trajetoria, velocidade, altura. Se quanto mais rapido estiver mais dano tem o aviao
-Da para tirar conclusoes do que é cada parte do voo, em qual altura qual velocidade, etc
-Sugerir mudancas nos trajetos ou horarios. Estacoes dos anos que tem mais passaros migrando
-Ver quais aeroportos tem mais indices assim
-Tipos de avioes, e quais tem mais danos

Variaveis de Interesse com dados completos ou quse completos:

SPECIES_ID ou SPECIES especie que mais tem impacto

SOURCE

NR_FATALITIES OU NR_INJURIES, Poucos observacoes para o tamanho da amostra, mas é muito interessante

Size tamanho do passaro

Birds_Struck muito bom pare correlacionar com o dano, o tamanho também

REMAINS_COLLECTED para combinar com a especie e ver se ajudou a identificar

EFFECT poucos efeitos na maioria dos casos, mas interessante ver se o impacto teve que interditar voo ou etc

COST_REPAIRS & COST_OTHERS importante para ver os custos de concertar a aeronave, ou os custos com gasolina, cancelar o voo, rembolssar passagens, hotel, etc. Porem so tem 5000 observacoes para cada. É uma variavel quantitativa, boa para fazer correlação/boxplot. Tem os ajustados a inflação para 
comparar melhor

AOS Tempo que a aeronave teve que ficar sem vooar dps, 14k observações. Outra quantitativa, boa. Se for blank/vazio, é pq nao ficou nenhuma hora sem

Distance quantitativa, mas boa parte é zero

PRECIPITATION - Se tinha chuva, nevoa, etc
Latitude e longitude, mas como usar?

E variaveis de tempo/data: INCIDENT_MONTH, INCIDENT_YEAR, INCIDENT_MONTH, TIME_OF_DAY, TIME

E muitas quantitativas falando onde na aeronave ocorreu o impacto e onde teve dano, achar algum jeito de incorporar


Carregar bibliotecas e ler a base de dados
```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
def_strikes <- read.csv( "STRIKE_REPORTS.csv")
```

Traduzir ou transformar valores
```{r}
#Mudar as siglas da classe de aeronave para os nomes inteiros"
def_strikes <- def_strikes %>%
   mutate(Tipo_de_Aeronave = case_when(
    AC_CLASS == "A" ~ "Avião",
    AC_CLASS == "B" ~ "Helicóptero",
    AC_CLASS == "C" ~ "Planador",
    AC_CLASS == "D" ~ "Balão",
    AC_CLASS == "F" ~ "Dirigível",
    AC_CLASS == "I" ~ "Autogiro",
    AC_CLASS == "J" ~ "Aeronave Ultraleve",
    AC_CLASS == "Y" ~ "Outros",
    AC_CLASS == "Z" | AC_CLASS == "" | AC_CLASS == "NA" ~ "Desconhecido"))

#"Operador, sair da sigla para o nome mesmo"
#def_strikes <- def_strikes %>%
#   mutate(Operadores = case_when(
#     OPERATOR

#Nivel de dano"
def_strikes <- def_strikes %>%
   mutate(Nível_do_Dano = case_when(
    DAMAGE_LEVEL== "M" ~ "Mínimo",
    DAMAGE_LEVEL== "M?" ~"Indeterminado",
    DAMAGE_LEVEL== "D"~ "Destruído",
    DAMAGE_LEVEL== "S"~ "Considerável",
    DAMAGE_LEVEL== ""| DAMAGE_LEVEL== "NA" ~ "Desconhecido",
    DAMAGE_LEVEL== "N" ~ "Sem Dano"))

#Mudando as dummys de massa para a massa
def_strikes <- def_strikes %>%
   mutate(Massa_Aeronave = case_when(
    AC_MASS == 1 ~ "2.250=<",
    AC_MASS == 2 ~ "2.251-5700",
    AC_MASS == 3 ~ "5701-27.000",
    AC_MASS == 4 ~ "27.001-272.000",
    AC_MASS == 5 ~ ">272.000",
    AC_MASS == "NA"|AC_MASS == ""  ~ "Desconhecido"))
    
```

Tabelas (Lembrar de fazer com e sem os valores desconhecidos, pq eles sao os maiores), e também adicionar mais informacoes na tabela fora o numero de voos, o que botar?
```{r}
#Operadores por Sigla
table_opid <- def_strikes %>%
  group_by(OPID) %>%
  summarize(
    Voos = n())  %>%
  arrange(desc(Voos))
print(table_opid)

#Operador sem ser sigla
table_operadores <- def_strikes %>%
  group_by(OPERATOR) %>%
  summarize(
    Voos = n())  %>%
  arrange(desc(Voos))
print(table_operadores)

#OPERADORES SEM SIGLAS E SEM OS DESCONHECIDOS
table_operadoresfiltrado <- def_strikes %>%
  filter(OPERATOR != "UNKNOWN") %>%
  group_by(OPERATOR) %>%
  summarize(
    Voos = n())  %>%
  arrange(desc(Voos))
print(table_operadoresfiltrado)

#Aeroportos com mais voos que tiveram impact
table_aeroportos <- def_strikes %>%
  group_by(AIRPORT) %>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_aeroportos)

#Aeroportos com mais voos que tiveram impactO SEM UNKNOWN
table_aeroportosFILTRADO <- def_strikes %>%
  filter(AIRPORT != "UNKNOWN") %>%
  group_by(AIRPORT) %>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_aeroportosFILTRADO)


#Os avioes que mais tem impcato sao os comerciais grandes, fazer alguma analise de correlação com o dano, porque acho que eles nao tem muitos danos
table_aircrafts <- def_strikes %>%
  group_by(AIRCRAFT)%>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_aircrafts)

table_aircraftsFILTRADO <- def_strikes %>%
  filter(AIRCRAFT != "UNKNOWN") %>%
  group_by(AIRCRAFT)%>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_aircraftsFILTRADO)

#Estados com mais Impactos
table_ESTADO <- def_strikes %>%
  group_by(STATE) %>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_ESTADO)

table_ESTADOFILTRADO <- def_strikes %>%
  filter(STATE != "") %>%
  group_by(STATE) %>%
  summarize(
    Voos = n()) %>%
  arrange(desc(Voos))
print(table_ESTADOFILTRADO)
```

```{r, fig.width =10}
#Grafico de Correlação entre altura e velocidade, mas nao acho que esse seja bom, pois são duas variaveis quantitativas
def_strikes %>%
  ggplot(aes(x=HEIGHT , y=SPEED, fill=PHASE_OF_FLIGHT)) + geom_boxplot(outlier.color = "red", outlier.size = 2, outlier.fill="red") + labs(title = "Correlação Altura x Velocidade", x="Altura", y="Velocidade", fill="Fase do Voo")

#Boxplot da velocidade em cada etapa do Voo
def_strikes %>%
  ggplot(aes(y=SPEED, fill=PHASE_OF_FLIGHT)) + geom_boxplot(outlier.color = "red", outlier.size = 2, outlier.fill="red") + labs(title = "Velocidade em cada Etapa do Voo", y="Velocidade", fill="Fase do Voo")

#Altura em cada fase de voo"
def_strikes %>%
  ggplot(aes(x=PHASE_OF_FLIGHT, y=HEIGHT, fill=PHASE_OF_FLIGHT)) + geom_boxplot(outlier.color = "red", outlier.size = 2, outlier.fill="red") + labs(title = "Altura em cada Etapa do Voo", y="Altura", x = "Fase de Voo")


#Grafico de Barras do dano, o fill por AC_Mass nao esta funcionando
def_strikes %>%
  ggplot(aes(x= DAMAGE_LEVEL, fill =AC_MASS)) + geom_bar() + labs(title="Contagem Dano", x="Severidade", y="Ocorrencias")

#Contagem da severidade dos impactos
def_strikes %>%
  ggplot(aes(x= DAMAGE_LEVEL)) + geom_bar() + labs(title="Contagem Dano", x="Severidade", y="Ocorrencias")

#Anos e batidas de aeronave, fazer a sem os unknow/desconhecidos tambem mais para frente
def_strikes %>%
  ggplot(aes(x= INCIDENT_YEAR, fill = Tipo_de_Aeronave)) + geom_bar() + labs(title="Strikes por Ano", x="Severidade", y="Ocorrencias", fill="Tipos de Aeronave")

#teste para ver se a mudanca de nome esta entregando os mesmos resultados para a contagem de ocorrencias com cada tipo de aeronave
def_strikes %>%
  ggplot(aes(x= AC_CLASS)) + geom_bar() + labs(title="Contagem Dano", x="Tipo", y="Ocorrencias")
def_strikes %>%
  ggplot(aes(x= Tipo_de_Aeronave)) + geom_bar() + labs(title="Contagem Dano", x="Tipo", y="Ocorrencias")

#QUal etapa do Voo tem mais batidas, com isso percebemos que quanto mais baixo mais bate, o que é logico, porque as avez nao voam super alto
def_strikes %>%
  ggplot(aes(x= PHASE_OF_FLIGHT)) + geom_bar() + labs(title="Contagem por Etapa", x="Etapa", y="Ocorrencias")



#def_strikes %>%
#  gg(plot)

#def_strikes %>%
  
```
