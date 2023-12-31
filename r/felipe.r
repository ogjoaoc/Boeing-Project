# saving 

```{r}
# estados e a qnt de acidentes
aero <- c(STRIKE_REPORTS$V12)
contagem <- table(aero)
print(contagem) 
```

```{r}
# ordem decrescente dos estados com mais acidentes
acidentes_ordem_decresc <- sort(contagem, decreasing = TRUE)
print(acidentes_ordem_decresc)
```

```{r}
# GRAFICO ACIDENTES NOS ESTADOS DO MAIOR NUMERO PRO MENOR NUMERO
estados_mais <- c(acidentes_ordem_decresc)
barplot(acidentes_ordem_decresc, names.arg = estados_mais, col = "skyblue", main = "Grafico acidentes nos estados dos EUA", xlab = "Total de Acidentes nos estados ", ylab = "Num. acidentes" ) 
```

```{r}
estados_mais <- c("TX", "FL", "CA", "NY", "CO")
barplot(acidentes_ordem_decresc[2:6], names.arg = estados_mais, col = "skyblue", main = "Estados com mais acidentes", xlab = "Estados com mais acidentes", ylab = "Num. acidentes" ) 
```

```{r}
# resumo aeroportos de origem dos acidentes
aeroporto <- c(STRIKE_REPORTS$V8)
contador <- table(aeroporto)
print(contador)
```

```{r}
# resumo anterior em ordem decrescente
a <- sort(contador, decreasing = TRUE)
print(a)
```

```{r}
# AEOROPORTOS COM MAIORES ORIGENS DE VOOS QUE COMETERAM ACIDENTES
aero_mais <- c("Denver INT", "Dallas INT", "Chicago INT", "JFK", "Memphis INT")
barplot(a [2:6], names.arg = aero_mais, col = "grey", main = "Estados de maior origem de voos que cometeram acidentes", xlab = "Aeroportos com maior origem de acidentes", ylab = "Num. acidentes vindos dos aeroportos")
```

```{r}
# ESTADOS DOS AEROPORTOS QUE SAO ORIGENS DE ACIDENTES
ideal <- c("CO", "TX", "IL", "NY", "TN", "UT", "CA", "MI", "FL", "GA", "CA")
barplot(a [2:12], names.arg = ideal, col = "grey", main = "Estados com maiores origens de acidentes", xlab = "Estados", ylab = "Num. acidentes vindos dos aeroportos" )
```

Desse modo, os cinco estados com maior número de acidentes, em ordem decrescente, são o Texas, Flórida, California, Nova York e Colorado. No entanto, percebe-se que os voos que causam mais acidentes não são predominantemente vindos desses estados,
como é o caso dos aviões que saem de Denver, que é a cidade que tem o maior número de voos de origem que causam acidentes. Ainda, dos onze aeroportos de maior origem de acidentes, seis deles são dos estados que registram os maiores números de 
acidentes, sendo apenas um do Texas, que é o estado com maior registro de ocorrências. Portanto, a origem desses voos não tem muita relação com o loval de acontecimento dos acidentes.


