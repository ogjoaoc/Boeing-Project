---
title: "Untitled"
author: "Tomás Mello"
date: "2023-12-15"
output: html_document
---
Carregar bibliotecas e ler a base de dados
```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
def_strikes <- read.csv( "STRIKE_REPORTS.csv")
```

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

#Transformar as variaveis de custo em numericas em vez de texto (numeric em vez de string)
def_strikes$NCOST_REPAIRS <- as.numeric(gsub(",", "", def_strikes$COST_REPAIRS))    
def_strikes$NCOST_OTHER <- as.numeric(gsub(",", "", def_strikes$COST_OTHER))  
```

```{r}
#Operadores por Sigla

table_opid <- def_strikes %>%
  group_by(OPID) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    )  %>%
  arrange(desc(Impactos)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), "-", .)))
print(table_opid)


#Operador sem ser sigla
table_operadores <- def_strikes %>%
  group_by(OPERATOR) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    )  %>%
  arrange(desc(Impactos))
print(table_operadores)

#OPERADORES SEM SIGLAS E SEM OS DESCONHECIDOS
table_operadoresfiltrado <- def_strikes %>%
  filter(OPERATOR != "UNKNOWN") %>%
  group_by(OPERATOR) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    )  %>%
  arrange(desc(Impactos))
print(table_operadoresfiltrado)

#top 6 e ultimos 6 resultados dos operadores
head(table_operadores)
tail(table_operadores)

#Aeroportos com mais voos que tiveram impact
table_aeroportos <- def_strikes %>%
  group_by(AIRPORT) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    ) %>%
  arrange(desc(Impactos))
print(table_aeroportos)

#Aeroportos com mais voos que tiveram impactO SEM UNKNOWN
table_aeroportosFILTRADO <- def_strikes %>%
  filter(AIRPORT != "UNKNOWN") %>%
  group_by(AIRPORT) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    ) %>%
  arrange(desc(Impactos))
print(table_aeroportosFILTRADO)

#top 6 e ultimos 6 resultados dos aeroportos
head(table_aeroportos)
tail(table_aeroportos)


#Os avioes que mais tem impcato sao os comerciais grandes, fazer alguma analise de correlação com o dano, porque acho que eles nao tem muitos danos
table_aircrafts <- def_strikes %>%
  group_by(AIRCRAFT)%>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    ) %>%
  arrange(desc(Impactos))
print(table_aircrafts)

#Tipos de avioes sem os desconhecidos que mais batem
table_aircraftsFILTRADO <- def_strikes %>%
  filter(AIRCRAFT != "UNKNOWN") %>%
  group_by(AIRCRAFT)%>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    ) %>%
  arrange(desc(Impactos))
print(table_aircraftsFILTRADO)

#Aviões mais fatais
mortestable <- table_aircrafts %>%
  arrange(desc(Fatalidades))

machucadostable <- table_aircrafts  %>%
  arrange(desc(`Pessoas Machucadas`))

print(mortestable)
print(machucadostable)

#top 6 e ultimos 6 resultados dos tipos de avião
head(table_aircrafts)
tail(table_aircrafts)

#Estados com mais Impactos
table_ESTADO <- def_strikes %>%
  group_by(STATE) %>%
  summarize(
     Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)
    ) %>%
  arrange(desc(Impactos))
print(table_ESTADO)

#Estados com mais impactos sem os UNknown
table_ESTADOFILTRADO <- def_strikes %>%
  filter(STATE != "") %>%
  group_by(STATE) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)) %>%
    arrange(desc(Impactos))
print(table_ESTADOFILTRADO)

#top 6 e ultimos 6 resultados dos tipos de avião
head(table_ESTADO)
tail(table_ESTADO)

#passaros e seus efeitos
table_passaros <- def_strikes %>%
  group_by(SPECIES) %>%
  summarize(
    Impactos = n(),
    "Custo Médio de Reparos" = mean(NCOST_REPAIRS, na.rm = TRUE),
    "Custo Médio Outros" = mean(NCOST_OTHER, na.rm = TRUE),
    "Fora de Serviço" = mean(AOS, na.rm = TRUE),
    "Pessoas Machucadas" = mean(NR_INJURIES, na.rm = TRUE),
    "Fatalidades" = mean(NR_FATALITIES, na.rm = TRUE)) %>%
    arrange(desc(Impactos))
print(table_passaros)

#Podemos ordenar qualquer tabela para deixar a variavel que queremos em ordem decrescente. Para passaros, pensei em botar na ordem dos custos de reparo, ou de fatalidades/machucados dependendo de qual seja, ou por horas que a aeronave ficou sem funcionar

avesfatais_table <- table_passaros %>%
  arrange(desc(Fatalidades))

avesforadeservico_table <- table_passaros %>%
  arrange(desc('Fora de Serviço'))

print(avesforadeservico_table)
print(avesfatais_table)
```