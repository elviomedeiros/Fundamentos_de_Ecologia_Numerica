############################################################
### Section 1: Teste bicaudal (sem direção especificada)
############################################################

################################################
### Os brotos tem em média 10 cm           ? ###
################################################

# Vamos adotar um nível de erro de 5%.

# Antes de começar, vamos formular nossas hipóteses:

# Hipótese nula (H0): Os brotos têm, em média, v_esperado cm
# Hipótse alternativa (HA): Os brotos não têm, em média, v_esperado cm

v_esperado <- 28.8

# Visualizar a distribuição

library(ggplot2)
ggplot(data.frame(brotos_v), aes(x = brotos_v)) +
  geom_histogram(bins = 9) +
  xlab("Diâmetro de brotos") +
  ylab("Contagem") +
  geom_vline(
    aes(xintercept = mean(brotos_v), color = "Média observada"),
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = v_esperado, color = "Valor (média) esperado"),
    linewidth = 1
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "Valor (média) esperado" = "orangered",
      "Média observada" = "blue"
    )
  )

#Passo 1: Decidir o valor do erro.

#>> 5% = 0.05

erro_nivel = 0.05

# Step 2: Calculate the test statistic and lookup the resulting p-value.

mean <- mean(brotos_v)
sd <- sd(brotos_v)
n <- length(brotos_v)

erro_pad <- sd/sqrt(n)

# Valor padronizado (t)

t <- (mean - (v_esperado)) / erro_pad
t

p_valor <- pt(t, df = n - 1) * 2 #probabilidade t
p_valor <- 2 * (1 - pt(abs(t), df = n - 1))

# Observe como calculamos apenas p(t) aqui. Como temos uma estatística
# de teste negativa, isso significa que estamos interessados na
# extremidade inferior ("cauda inferior") da distribuição,
# ou seja, na área à esquerda da nossa estatística de teste.

# Além disso, precisamos multiplicar o p-valor por dois, para considerar
# ambas as extremidades da distribuição (teste bicaudal).

# Etapa 3: O p-valor é menor que o nível de significância?

# Se sim, rejeite a hipótese nula.

erro_nivel
p_valor
p_valor < erro_nivel

# O p-valor está abaixo do nível de erro aceito de 5%, portanto temos
# evidências suficientes para rejeitar a hipótese nula. Isso significa
# que podemos aceitar a hipótese alternativa de que, em média, os
# piratas não têm 10 tatuagens.

# Também podemos representar graficamente nosso nível de erro aceito
# e a posição da estatística de teste.
# Isso não é essencial, mas ajuda a mostrar o que calculamos.

# A primeira parte calcula os valores de x e y para o gráfico com base
# na distribuição t e também os valores críticos da distribuição t
# para o nosso gráfico.

# A segunda parte então passa essas informações para o ggplot().

# Fazendo o gráfico

x <- seq(-4,4,0.01)
y <- dt(x, df=n-1)

t_erro_sup <- qt(1-erro_nivel/2, df=n-1)
t_erro_inf <- qt(erro_nivel/2, df=n-1)

# Gráfico

ggplot(NULL, aes(c(-4, 4))) +
  xlab("x") +
  geom_area(stat = "function",
            fun = dt,
            args = list(df = n - 1),
            fill = "red",
            xlim = c(-4, t_erro_inf)) +
  
  geom_area(stat = "function",
            fun = dt,
            args = list(df = n - 1),
            fill = "white",
            xlim = c(t_erro_inf, t_erro_sup)) +
  
  geom_area(stat = "function",
            fun = dt,
            args = list(df = n - 1),
            fill = "red",
            xlim = c(t_erro_sup, 4)) +
  
  scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
  geom_vline(
    aes(xintercept = t, color = "Valor de t ajustado"),
    linewidth = 1
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "Valor de t ajustado" = "black")
  )

t.test(x = brotos_v,
       mu = v_esperado,
       alternative = "two.sided",
       conf.level = 0.95)

#Assumindo que a hipótese nula seja verdadeira (ou seja,
#que a média populacional seja 28,8), existe uma probabilidade de 54,65%
#de observar uma estatística de teste tão extrema quanto a obtida,
#ou mais extrema, apenas devido à variabilidade amostral aleatória.

#Ou, seja, o teste t de uma amostra não indicou evidências de que a média
#populacional difira de 28,8 (t(499) = 0,603, p = 0,547). Portanto, não se
#rejeita a hipótese nula ao nível de significância de 5%.
