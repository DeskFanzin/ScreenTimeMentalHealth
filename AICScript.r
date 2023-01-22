# Script usado para todos os testes em AIC.
# Autores: André Pardo Maurell, Gabriel do Pinho Martins
# Matrícula : 142365, 142356

# instalando o pacote rlang para poder usar a função read_excel
install.packages("rlang")
install.packages("readxl")
# instalando ggplot, necessário para que gráficos sejam criados
install.packages("ggplot2")

# lendo o arquivo Excel com os dados das idades do Canadá:
library(readxl)
Idades_Canada <- read_excel("Idades-Canada.xlsx",
    col_types = c("numeric", "numeric")
)
View(Idades_Canada)

# retirando a média das idades da variável Canadá, que precisamos para calcular as médias gerais das idades dos países selecionados.
# consideramos como peso da média o total da população de acordo com a idade, sendo o total da população igual a 4627.
peso_idades <- c(0.05, 0.13, 0.17, 0.16, 0.21, 0.26)
weighted.mean(Idades_Canada$Idade, peso_idades)
# 49.83673
# os outros países selecionados já possuiam suas respectivas médias.

# lendo o arquivo Excel com os dados globais:
library(readxl)
Populacao_Idades_geral <- read_excel("Populacao_Idades_geral.xlsx",
    col_types = c("text", "numeric", "numeric")
)
View(Populacao_Idades_geral)

# retirando a média das idades dos países selecionados, considerando que nao está disponivel o total na lista original:
mean(Populacao_Idades_geral$Idades)
# 18.3534

# retirando o total da população dos países selecionados, **:
sum(Populacao_Idades_geral$Pessoas)
# 10.658

# retirando o desvio padrão das idades dos países selecionados, **:
sd(Populacao_Idades_geral$Idades)
# 11.39959

# retirando a média das idades dos países selecionados, considerando o peso da população e guardando em uma variável e o total está disponível na lista:
pond_paises_prcnt <- c(Populacao_Idades_geral[1, 2] / Populacao_Idades_geral[5, 2] * 100, Populacao_Idades_geral[2, 2] / Populacao_Idades_geral[5,2] * 100, Populacao_Idades_geral[3, 2] / Populacao_Idades_geral[5,2] * 100, Populacao_Idades_geral[4, 2] / Populacao_Idades_geral[5,2] * 100)
#7.918934, 6.126853, 43.50722, 42.44699

# lendo o arquivo Excel com os dados da análise de dados da correlação de Ordem Zero de Camerini:
library(readxl)
Ordem_Zero_Camerini <- read_excel("Ordem-Zero-Camerini.xlsx", 
    range = "A1:B4", col_types = c("text", 
        "numeric"))
View(Ordem_Zero_Camerini)

# retirando a média da correlação de ordem zero de Camerini:
mean(Ordem_Zero_Camerini$`Mental Health Problems (M6)(p)`)
# 0.454
# este resultado condiz com uma correlação razoavelmente forte, de acordo com a tabela de correlação de Pearson.

# lendo o arquivo Excel com os dados da análise de dados da correlação de Pearson de Peng:
library(readxl)
Correlacao_Peng <- read_excel("Correlacao_Peng.xlsx", 
    col_types = c("text", "numeric"))
View(Correlacao_Peng)

# calculando a média dos problemas mentais em correlação de pearson com a dependência de celular:
 mean(Correlacao_Peng$`Mobile Phone Addiction`)
# 0.42, desconsiderando o número negativo, temos uma correlação razoavelmente forte, de acordo com a tabela de correlação de Pearson.

# correlacionando a média dos problemas mentais com a média da correlação de ordem zero de Camerini:
valores_camerini <- Ordem_Zero_Camerini$`Mental Health Problems (M6)(p)`
valores_peng <- Correlacao_Peng$`Mobile Phone Addiction`

## para a correlação acontecer, precisamos criar uma hipótese: considerarmos que a média dos valores de Peng, subtraidos e somados com o
#Desvio Padrão, o representaria em três meses distintos, como de Camerini.
mean(valores_peng)
#0.1533333
sd(valores_peng)
#0.4992327
media_sd_peng <- c(-0.346, 0.15, 0.652)
cor.test(valores_camerini, media_sd_peng, method = c("pearson"))
#0.9437693, correlação muito forte, de acordo com a tabela de correlação de Pearson.
# importante lembrar que, como são apenas três valores, a correlação pode ser influenciada grandemente por outliers.

#analisando o gráfico
library(ggplot2)
scatter_graph <- ggplot(Camerini_Peng, aes(x=`Mental Health Problems (Camerini)`, y=`Mobile Phone Addiction (Peng)`))
scatter_graph +
    geom_point(size=3) +
    geom_smooth(method=lm, se=FALSE, color="red") +
    labs(title="Correlação entre os problemas mentais e a dependência de celular", x="Problemas mentais (Camerini)", y="Dependência de celular (Peng)")

# lendo o arquivo Excel com os dados da análise de dados de Colley:
library(readxl)
Saude_mental_canada <- read_excel("Saude_mental_canada.xlsx", 
    col_types = c("text", "numeric", "numeric"))
View(Saude_mental_canada)

#removendo a linha de texto para que seja possível calcular a média das linhas:
library(readxl)
Saude_mental_canada <- read_excel("Saude_mental_canada.xlsx", 
    range = "b1:c9", col_types = c("numeric", 
        "numeric"))
View(Saude_mental_canada)

# retirando a média de cada linha da lista saude_mental_canada:
rowMeans(Saude_mental_canada)
# 63.600 49.900 61.850 51.450 57.650 38.350 61.030 46.465

# lendo a tabela da população onde foram encontradas evidências do impacto na saúde mental e sua ponderação, para que seja possível calcular o total de pessoas afetadas:
library(readxl)
ponderacao_paises <- read_excel("ponderacao_paises.xlsx", 
    col_types = c("text", "numeric", "numeric", 
        "numeric", "numeric"))
View(ponderacao_paises)

# retirando a porcentagem da população afetada por cada país:
total_pop_afetada <- c(((ponderacao_paises[1, 2] * ponderacao_paises[2, 2]) + (ponderacao_paises[1, 3] * ponderacao_paises[2, 3]) + (ponderacao_paises[1, 4] * ponderacao_paises[2, 4]) + (ponderacao_paises[1, 5] * ponderacao_paises[2, 5])) / 4)
total_pop_afetada
# 402.6225