install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")

library("tidyverse")

# como o objetivo é para estudo, foi utilizado o dataset do Titanic
# https://www.kaggle.com/competitions/titanic/data
dados <- read.csv2('../dados/test.csv',header = TRUE,sep = ',')


#--------------------Visualização-----------------------------------------------

# Algumas formas para visualizar informações do dataset

View(dados) # Mostra a base de dados completa em uma nova aba
head(dados, n=5) # Mostra as 5 primeiras observações da base de dados
str(dados) # Mostra a estrutura da base de dados
glimpse(dados) # Função parecida com a str
print(dados) # Apresenta a base de dados no console
dim(dados) # As dimensões do dataset: linhas e colunas, respectivamente
names(dados) # Para ver os nomes das variáveis

# Poderíamos fazer o print de apenas uma variável
# O símbolo "$" é utilizado para especificar uma variável do dataset

dados$Name

# Relembrando algumas definições sobre as variáveis:

# Variáveis <Chr> são caracteres ("characters"), isto é, contêm textos
# Variáveis <dbl> são "doubles", isto é, contêm números
# Variáveis <int> são integers, isto é, contêm números inteiros

#--------------------Rename-----------------------------------------------------

# Função "rename": utilizada para alterar o nome das variáveis

# No dataset de exemplo, os nomes das variáveis contêm:
# Espaços, maiúsculas, acentos e caracteres especiais...
# É melhor não utilizá-los, podem gerar conflito e dificultam a escrita

# Inicialmente, sem utilizar a função, poderíamos fazer da seguinte forma:
# 1º:Combinamos os novos nomes desejados em um vetor

novos_nomes <- c("ID_PASSAGEIRO",
                 "CLASSE_SOCIAL",
                 "NOME",
                 "SEXO",
                 "IDADE",
                 "IRMAOS_CONJUGES",
                 "PAIS_FILHOS",
                 "TICKET",
                 "TARIFA_EMBARQUE",
                 "CABINE",
                 "PORTO_EMBARQUE"
                 )

print(novos_nomes)

names(dados) <- novos_nomes

head(dados, n=5)
# obs: algumas crianças viajavam só com babás, por isso na coluna de PAIS_FILHOS 
# aparece como valor 0


# Com o uso do operador pipe - %>% - atalho: Ctrl+Shift+M
# podemos tirar o primeiro argumento do código

dados %>% rename(
  CABINE_EMBARQUE = "CABINE",
  EMBARQUE = "PORTO_EMBARQUE"
)

# É possível passar apenas a posição para a função rename para alterar o nome
# da coluna

names(dados)

nova_base <- dados %>% rename(
  TARIFA = 9
)

names(nova_base)

#--------------------Mutate-----------------------------------------------------

# Função "mutate": apresenta duas utilidades principais
# 1. Inclui variáveis no dataset, mantendo as existentes
# 2. Transforma o conteúdo das variáveis

# Numa primeira situação, são adicionados duas variáveis a um dataset existente 
# As observações no dataset e nas variáveis devem estar igualmente ordenadas


variavel_nova_1 <- c(1:418)
variavel_nova_2 <- c(419:836)
print(variavel_nova_1)
print(variavel_nova_2)


nova_base %>% mutate(variavel_nova_1, variavel_nova_2)

# com o mutate é possível criar novas variáveis 

nova_base %>%  mutate(variavel_nova_1 * 2)


# é possível modificar uma variável que já exista

head(nova_base)
nova_base %>%  mutate(
  SEXO = replace(SEXO, SEXO == "male", "masculino"),
  SEXO = replace(SEXO, SEXO == "female", "feminino"))
