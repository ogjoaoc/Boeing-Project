---
title: "Untitled"
author: "Tomás Mello"
date: "2023-12-15"
output: html_document
---

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

#etapas de voo
def_strikes <- def_strikes %>%
   mutate(EtapadoVoo = case_when(
     PHASE_OF_FLIGHT == "" ~ "Desconhecido",
     PHASE_OF_FLIGHT == "Approach" ~ "Aproximação",
     PHASE_OF_FLIGHT == "Arrival" ~ "Chegada",
     PHASE_OF_FLIGHT == "Climb" ~ "Subida",
     PHASE_OF_FLIGHT == "Departure" ~ "Decolagem",
     PHASE_OF_FLIGHT == "Descent" ~ "Descida",
     PHASE_OF_FLIGHT == "En Route" ~ "Na rota",
     PHASE_OF_FLIGHT == "Local" ~ "Local",
     PHASE_OF_FLIGHT == "Parked" ~ "Parado",
     PHASE_OF_FLIGHT == "Landing Roll" ~ "Pousando na Pista",
     PHASE_OF_FLIGHT == "Take-off Run " ~ "Corrida Decolagem",
     PHASE_OF_FLIGHT == "Taxi" ~ "Taxi"))
     
def_strikes$NCOST_REPAIRS <- as.numeric(gsub(",", "", def_strikes$COST_REPAIRS))
    
```

```{r, fig.height=4, fig.width=17}
#Boxplot da velocidade em cada etapa do Voo
def_strikes %>%
  ggplot(aes(x=EtapadoVoo , y=SPEED, fill=EtapadoVoo)) + geom_boxplot(outlier.color = "red", outlier.size = 2, outlier.fill="red") + labs(title = "Velocidade em cada Etapa do Voo", y="Velocidade", fill="Fase do Voo")

#Altura em cada fase de voo"
def_strikes %>%
  ggplot(aes(x=EtapadoVoo, y=HEIGHT, fill=EtapadoVoo)) + geom_boxplot(outlier.color = "red", outlier.size = 2, outlier.fill="red") + labs(title = "Altura em cada Etapa do Voo", y="Altura", x = "Fase de Voo") 

#Boxplot: tempo sem voar por dano e tamanho do passaro
def_strikes %>%
  ggplot(aes(x=Nível_do_Dano, y=AOS)) +  geom_boxplot(outlier.color = "red", outlier.size = 1, outlier.fill="red") + facet_grid(. ~ SIZE ) + labs(title = "Tempo Fora de Serviço por Tamanha do Passáro Atingido", y="Horas Sem Voar", x = "Nível do Dano")   + coord_cartesian(ylim = c(0,500))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot custos de reparo por dano e tamanho do passaro
def_strikes %>%
  ggplot(aes(x=Nível_do_Dano, y=NCOST_REPAIRS)) +  geom_boxplot(outlier.color = "red", outlier.size = 1, outlier.fill="red") + facet_grid(. ~ SIZE ) + labs(title = "Tempo Fora de Serviço por Tamanha do Passáro Atingido", y="Custos de Reparos em Dólares", x = "Nível do Dano") +  coord_cartesian(ylim = c(0,1000000))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
````

```{r}

#Meses do ano com mais ocorrencias
def_strikes %>%
  ggplot(aes(x= INCIDENT_MONTH)) + geom_bar() + labs(title="Meses com mais acidentes", x="Meses", y="Ocorrencias")  +
  scale_x_continuous(breaks = 1:12) + theme_minimal()

#Contagem da severidade dos impactos
def_strikes %>%
  ggplot(aes(x= Nível_do_Dano)) + geom_bar() + labs(title="Contagem Dano", x="Severidade", y="Ocorrencias")

def_strikes %>%
  ggplot(aes(x = factor(Nível_do_Dano, levels = rev(levels(factor(Nível_do_Dano)))))) +
  geom_bar() +
  labs(title = "Contagem Dano", x = "Severidade", y = "Ocorrências") 

#Anos e batidas de aeronave, fazer a sem os unknow/desconhecidos tambem mais para frente
def_strikes %>%
  ggplot(aes(x= INCIDENT_YEAR, fill = Tipo_de_Aeronave)) + geom_bar() + labs(title="Strikes por Ano", x="Severidade", y="Ocorrencias", fill="Tipos de Aeronave")

def_strikes %>%
  ggplot(aes(x= Tipo_de_Aeronave)) + geom_bar() + labs(title="Contagem Dano", x="Tipo", y="Ocorrencias")


def_strikes %>%
  ggplot(aes(x = factor(EtapadoVoo, levels = unique(EtapadoVoo)))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Contagem por Etapa", x = "Etapa", y = "Ocorrências") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
