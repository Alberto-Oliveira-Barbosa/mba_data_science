install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")

library("tidyverse")
library(readxl)
# define o diretório de trabalho para reconhcecer mais facilmente os caminhos dos arquivos
setwd("~/projetos/mba_data_science/data_wrangling")

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

dados$Sex


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
                 "CLASSE",
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
# podemos encadear um conjunto de sequencias 
# usando o pipe ele nos permite retirar o primeiro argumento de dentro da função

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
nova_base %>%  mutate(variavel_mutate = variavel_nova_1 * 2)


# é possível modificar uma variável que já exista

head(nova_base)
nova_base %>%  mutate(
  SEXO = replace(SEXO, SEXO == "male", "masculino"),
  SEXO = replace(SEXO, SEXO == "female", "feminino"))

# similar a função replace temos a recode que substitui valores 

nova_base %>%  mutate(
  SEXO = recode(SEXO,
                'male' = "masculino",
                'female' = 'feminino')
)

# e podemos encadear funções com o pipe

names(nova_base)
nova_base %>% rename(
  PORTO = "PORTO_EMBARQUE"
) %>% mutate(
  PORTO = recode(
    PORTO,
    C = "Cherbourg",
    Q = "Queenstown",
    S = "Southampton"
  )
)

# é possível transformar uma variável para um novo tipo
# o que pode ser nescessário em alguns modelos de ML
nova_base %>%  mutate(
  SEXO = recode(SEXO,
                'male' = 1,
                'female' = 2)
)

# Algumas vezes, é necessário utilizar o mutate para critérios mais detalhados
# Para critérios mais complexos, a função case_when pode ser mais adequada

nova_base <- nova_base %>% mutate(
  FAIXA_IDADE = case_when(
    IDADE <= 17 ~ "MENOR DE IDADE",
    IDADE > 17 & IDADE < 65 ~ "ADULTO",
    IDADE > 65 ~ "IDOSO")
)

View(nova_base)

# Para deletar colunas com o mutate podemos usar o operador NULL

nova_base <- mutate(nova_base,
                          FAIXA_IDADE = NULL)

#--------------------Transmute--------------------------------------------------

# Função "transmute": inclui variáveis no dataset, excluindo as existentes
# Depois de informar o dataset, informe as variáveis mantidas e adicionadas

transmute(nova_base,SEXO,IDADE,variavel_nova_1, variavel_nova_2)

# Similar ao que foi feito acima com a função case_when
# temos a função cut que cria intervalos de valores

help("cut")

base_sumarizada <- nova_base %>% 
  transmute(NOME,SEXO, IDADE) %>% 
  mutate(
    IDADE = as.double(IDADE),
    FAIXA_IDADE = cut(
      IDADE, 
      breaks = c(0,17,64,Inf),
      labels = c("MENOR DE IDADE","ADULTO" , "IDOSO"))
  )

# Ao aplicar a função "summary" à variável factor, o resultado é uma contagem
# summary: gera estatísticas descritivas para variáveis

summary(base_sumarizada$FAIXA_IDADE)

names(base_sumarizada)

#--------------------Select-----------------------------------------------------

# Função "select": tem a finalidade principal de extair variáveis selecionadas 
# Também pode ser utilizada para reposicionar as variáveis no dataset

# Relembrando, sem utilizar a função, poderia ser feito:
base_sumarizada[,c("NOME","FAIXA_IDADE")] # critérios após a vírgula
base_sumarizada[,1:3] # todas as linhas e colunas de 1 a 3
base_sumarizada[,c(2:3, 4)] # pulando posições

# É possível selecionar parte do dataset (incluindo a seleção de linhas):
# Linhas antes da vírgula, colunas após a vírgula
  
base_sumarizada[3:7, c("SEXO", "FAIXA_IDADE")]
base_sumarizada[3:7, 1:2]

# Função "select" utilizada para selecionar e manter variáveis no dataset
# Portanto, seleciona as variáveis que devem ficar no dataset

names(nova_base)
select(nova_base, CLASSE, IDADE, TARIFA) # especificando
select(nova_base, -CLASSE, -IDADE, -TARIFA) # todas menos algumas
select(nova_base, CLASSE:IDADE) # de uma a outra
select(nova_base, starts_with("I")) # para algum início comum
select(nova_base, ends_with("a")) # para algum final comum


# Reposicionar variáveis do dataset com "select"

nova_base %>% select(SEXO, IDADE,CLASSE, everything())

# O mesmo trabalho poderia ser feito com a função "relocate"

names(base_sumarizada)
base_sumarizada %>% relocate(SEXO, .after = IDADE)
base_sumarizada %>% relocate(SEXO, .before = IDADE)

# A seguir, com "select", informaremos a ordem (inclusive, excluindo variáveis)

base_sumarizada %>% select(IDADE, FAIXA_IDADE)

# A função "pull" executa trabalho semelhante ao "select", porém gera um vetor

names(base_sumarizada)
base_sumarizada %>% 
  pull(var = 3) 


#--------------------Summarise--------------------------------------------------

# Função "summarise": função que resume o dataset, podendo criar outros
# Abaixo, as observações da variável "idade" são resumidas em descritivas

help("quantile")

nova_base %>%  
  mutate(
    IDADE = as.double(IDADE),
  ) %>% 
  drop_na() %>%
  summarise(
    observações = n(),
    média = mean(IDADE),
    mediana = median(IDADE),
    desv_pad = sd(IDADE),
    mínimo = min(IDADE),
    máximo = max(IDADE),
    quartil_3 = quantile(IDADE, probs = 0.75))


# Então, acima, criamos um data frame com uma linha de descritivas da variável

# Poderia ser utilizada para criar informações mais específicas sobre o dataset
# Para isto, o "summarise" é utilizado em conjunto com a função "group by"
# A seguir, vamos agrupar as informações do dataset pelo critério de "período"

base_grupo <- group_by(base_sumarizada, FAIXA_IDADE)

# Aparentemente, nada mudou na "base_grupo" em relação à "nova_base"

View(base_grupo)
View(base_sumarizada)

glimpse(base_grupo)

# Porém, o "group by" fica registrado no objeto

base_grupo %>% 
  mutate(
    IDADE = as.double(IDADE),
  ) %>% 
  drop_na() %>%
  summarise(média = mean(IDADE),
            desvio_pad = sd(IDADE),
            n_obs = n())

# O resultado do "summarise" acima é para cada grupo especificado no objeto
# Também criamos um data frame com duas linhas, uma para cada grupo
# Caso queira retirar o agrupamento criado, basta aplicar o "ungroup"

base_sem_grupo <- base_grupo %>% ungroup()

glimpse(base_sem_grupo)

base_sem_grupo %>% 
  mutate(
    IDADE = as.double(IDADE),
  ) %>% 
  drop_na() %>%
  summarise(
    mean(IDADE)) # informações para a base completa

# Também poderia agrupar por mais de um critério e gerar o dataset

head(nova_base)

nova_base %>% 
  mutate(
    IDADE = as.double(IDADE),
    SEXO = recode(
      SEXO,
      'female' = 'feminino',
      'male' = 'masculino')
  ) %>% 
  drop_na() %>%
  group_by(SEXO, CLASSE) %>% 
  summarise(IDADE_MEDIA = mean(IDADE),
            IDADE_MINIMA = min(IDADE),
            IDADE_MAXIMA = max(IDADE),
            contagem = n()) %>% 
  arrange(desc(CLASSE))
 

# A função "arrange" apenas fez a organização de apresentação do dataset
# Foi pedido que fosse organizado de forma decrescente (desc)
# Se retirar o desc(), fica na ordem crescente

# No contexto de resumo do dataset, a função "table" é útil para as contagens
# Portanto, é utilizada para criar tabelas de frequências:

head(nova_base)
table(nova_base$PORTO_EMBARQUE)
table(nova_base$SEXO)
table(nova_base$PORTO_EMBARQUE,
      nova_base$SEXO)

# Caso necessário, seria possível armazenar a tabela em um objeto:

as.data.frame(table(nova_base$PORTO_EMBARQUE,
                                    nova_base$SEXO))

# Um trabalho semelhante poderia ser feito por meio da função count()
# A função count() será muito útil para usar em conjunto com o "pipe"

nova_base %>% 
  count(PORTO_EMBARQUE)

nova_base %>% 
  count(SEXO)

nova_base %>% 
  count(PORTO_EMBARQUE, SEXO, name = "CONTAGEM")


#--------------------Filter-----------------------------------------------------

# A função "filter" tem o objetivo de gerar subconjuntos do dataset
# São especificados os critérios e as linhas que os atenderem serão filtradas

# Os principais operadores lógicos são:

# ==: igual
# !=: diferente
# > e <: maior e menor (podem conter o igual >= e <=)
# &: indica "E"
# |: indica "OU"

# Inicialmente, sem utilizar a função, poderia ser feito:

nova_base[nova_base$IDADE > 20,] # critérios antes da vírgula
nova_base[nova_base$IDADE > 20 & nova_base$CLASSE < 2,]
nova_base[nova_base$IDADE <=15 | nova_base$CLASSE == 3,]

# Função "filter": filtra a base de dados de acordo com os critérios escolhidos

nova_base %>% filter(IDADE > 30)

nova_base %>% filter(IDADE> 20 & CLASSE == 1)

nova_base %>% filter(CLASSE == 3)

nova_base %>%
  mutate(TARIFA = as.double(TARIFA)
         ) %>% 
  filter(SEXO != 'male' & between(TARIFA, 10.1, 20.0))

nova_base %>% 
  mutate(TARIFA = as.double(TARIFA)
  ) %>% filter(IDADE <= 15 | TARIFA < 20)


nova_base %>% 
  mutate(
    IDADE = as.double(IDADE)
  ) %>% 
  drop_na() %>% 
  filter(IDADE > mean(IDADE))


# A função filter também pode ser aplicada em datasets com grupos (group by)
# Neste caso, a função é aplicada dentro de cada grupo


nova_base %>%
  mutate(
    IDADE = as.double(IDADE)
  ) %>% 
  drop_na() %>%
  group_by(CLASSE) %>% 
  summarise(mean(IDADE))
# com isso temos uma média de:
# 40.9 anos para a primeira classe
# 28.8 anos para a segunda classe
# 24 anos para a terceira classe



nova_base %>% 
  mutate(
    IDADE = as.double(IDADE)
  ) %>% 
  drop_na() %>%
  group_by(CLASSE) %>% 
  filter(IDADE > mean(IDADE)) %>% 
  ungroup()

# com isso as médias consideradas no filtro correspondem aos valores de cada
# classe, e como em seguida é desfeito o grupo os valores aparecem desagrupados  





# A seguir, vamos realizar algumas operações sequencialmente
# O objetivo é obter estatísticas condicionais para grupos separados
# Note que estamos adicionando o argumento na.rm = T nas funções
# Embora não seja o caso aqui, é necessário quando há valores faltantes

nova_base %>% 
  mutate(IDADE = as.double(IDADE)) %>% 
  filter(IDADE > 20) %>% 
  group_by(CLASSE) %>% 
  summarise(observações = sum(!is.na(IDADE)),
            média = mean(IDADE, na.rm = T),
            mediana = median(IDADE, na.rm = T),
            desv_pad = sd(IDADE, na.rm = T),
            mínimo = min(IDADE, na.rm = T),
            máximo = max(IDADE, na.rm = T),
            quartil_1 = quantile(IDADE, probs = 0.25, na.rm = T),
            quartil_3 = quantile(IDADE, probs = 0.75, na.rm = T)) %>% 
  arrange(média)



# Outro operador útil para realizar comparações e filtros é: %in%
# É utilizado para verificar se os elementos de um objeto constam em outro

selecao_pessoas <- c(
  "Ashby, Mr. John", 
  "Herman, Mr. Samuel", 
  "Cor, Mr. Ivan", 
  "Spencer, Mr. William Augustus")

nova_base %>% 
  filter(NOME %in% selecao_pessoas)

# Podemos calcular estatísticas na sequência

nova_base %>% 
  filter(NOME %in% selecao_pessoas) %>% 
  mutate(IDADE = as.double(IDADE)) %>% 
  summarise(IDADE_MEDIA_GRUPO = mean(IDADE, na.rm = T))

# Também é possível encontrar a negação  do %in%
# Utilizando o ponto de exclamação

nova_base %>%
  filter(!(NOME %in% selecao_pessoas))

nova_base %>% 
  filter(!(NOME %in% selecao_pessoas)) %>% 
  mutate(IDADE = as.double(IDADE)) %>% 
  summarise(IDADE_MEDIA_GRUPO = mean(IDADE, na.rm = T))


#--------------------Slice------------------------------------------------------

# A função "filter" seleciona linhas com base em critérios lógicos
# A função "slice" pode ser utilizada para a seleção de linhas usando posições

nova_base %>% slice(5:9) # com base na posição das linhas
nova_base %>% slice(1:2, 5:9) # com base na posição das linhas com intervalo
nova_base %>% slice_head(n=3) # as três primeiras linhas
nova_base %>% slice_tail(n=3) # as três últimas linhas
nova_base %>% slice_min(order_by = IDADE, prop = 0.40) # os prop % menores
nova_base %>% slice_max(order_by = IDADE, prop = 0.10) # os prop % maiores




#--------------------Join-------------------------------------------------------

# Funções "join": utilizadas para realizar a junção (merge) de datasets
# Para ser possível, é necessária pelo menos uma variável em comum nos datasets

# Left Join: traz as variáveis do dataset Y para o dataset X 
# Nas funções, X é o primeiro argumento a ser inserido na função

glimpse(nova_base)

dataset_inicial <- nova_base %>% 
  mutate(
    ID_PASSAGEIRO = as.integer(ID_PASSAGEIRO),
    SEXO = recode(
      SEXO,
      'male' = 'masculino',
      'female' = 'feminino'),
    IDADE = as.double(IDADE),
    TARIFA = as.double(TARIFA)
  ) %>% 
  rename(EMBARQUE = 'PORTO_EMBARQUE')


dataset_merge <- read.csv2("../dados/gender_submission.csv",
                           header = TRUE, sep = ",") %>% 
  rename(ID_PASSAGEIRO = "PassengerId")


glimpse(dataset_inicial)
glimpse(dataset_merge)



# Em seguida, podemos realizar o merge

left_join(dataset_inicial, dataset_merge,
                            by = "ID_PASSAGEIRO")

# O argumento "by" indica a variável que será a "chave" para a combinação

# Da mesma forma, mas usando o pipe

dataset_inicial %>% left_join(dataset_merge, by = "ID_PASSAGEIRO")

# Podemos verificar que a variável que está em Y foi trazida para X
# Como uma observação de X não está presente em Y, o dataset final aponta "NA"
# NA = é um missing value
# Então, no dataset final, todas as observações de X estão presentes
# Por outro lado, observações de Y que não estejam em X são excluídas
# As observações que estão na "chave" de X são aquelas no dataset final

# Right Join: leva as variáveis de X para Y (X é o primeiro argumento)

right_join(dataset_inicial, dataset_merge,
                              by = "ID_PASSAGEIRO")


dataset_inicial %>% right_join(dataset_merge, by = "ID_PASSAGEIRO")

# Neste caso, o dataset final contém somente as observações de Y
# Isto é, uma observação de X que não está presente em Y foi excluída
# As observações que estão na "chave" de Y são aquelas no dataset final
# São gerados NA na a observação de Y que não está em X

# Inner Join: cria um novo dataset com as observações que estão em X e Y
# Para fazer parte do dataset final, deve estar em ambos os datasets iniciais
# Colocando de outra forma, é a interseção de X e Y

inner_join(dataset_inicial, dataset_merge,
           by = "ID_PASSAGEIRO")


# Não há missing values, só as observações que estão em X e Y ficam após o merge



# Full Join: cria um novo dataset contendo todas as informações de X e Y
# Ou seja, pode estar em X e não estar em Y e vice-versa

full_join(dataset_inicial, dataset_merge,
          by = "ID_PASSAGEIRO")


# As próximas duas funções, Semi Join e Anti Join, são formas de comparação
# Isto significa que elas não realizam o merge, apenas comparam datasets

# Semi Join: mantém em X as observações que coincidem com Y, sem realizar merge

semi_join(dataset_inicial, dataset_merge,
          by = "ID_PASSAGEIRO")


# No dataset final, constam apenas as variáveis que já estavam em X
# Porém, as observações são somente aquelas que também estão em Y

# Anti Join: mantém em X suas observaçoes que não coincidem com Y
# Também não ocorre o merge

anti_join(dataset_inicial, dataset_merge,
          by = "ID_PASSAGEIRO")


#--------------------Bind-------------------------------------------------------

# Existem formas simples de combinar datasets, adequados em casos particulares
# As funções "bind" combinam datasets sem a especificação de uma "chave"
# Isto significa que as observações ou variáveis devem estar na mesma ordem

# Vamos criar alguns datasets para exemplificar:

dataset_bind_1 <- tibble(var1 = c("obs1", "obs2", "obs3", "obs4"),
                         var2 = sample(1:100, 4, replace=TRUE),
                         var3 = sample(1:100, 4, replace=TRUE))

dataset_bind_2 <- tibble(var4 = c("obs1", "obs2", "obs3", "obs4"),
                         var5 = sample(1:100, 4, replace=TRUE))

dataset_bind_3 <- tibble(var6 = c("obs50", "obs51", "obs52", "obs53"),
                         var7 = sample(1:100, 4, replace=TRUE))

dataset_bind_4 <- tibble(var1 = c("obs5", "obs6", "obs7", "obs8", "obs9"),
                         var2 = sample(1:100, 5, replace=TRUE),
                         var3 = sample(1:100, 5, replace=TRUE))

# Combinar colunas (variáveis): deve haver o mesmo número de observações
# junta as novas colunas a direita dos dados, a quantidade de linhas
# deve ser igual para os dois dataframes

bind_cols(dataset_bind_1, dataset_bind_2)

# Combinar linhas (observações): as variáveis devem estar na ordem
# junta os dois dataframes colocando o segundo abaixo do primeiro
# para esse caso a quantidade de colunas (variávies) devem ser iguais

bind_rows(dataset_bind_1, dataset_bind_4)

#--------------------Iterações com Purrr----------------------------------------

# O pacote purrr oferece funções que realizam iterações mais facilmente

# https://purrr.tidyverse.org/

# As iterações evitam a repetição de códigos
# São adequadas quando a intenção é realizar a mesma tarefa em vários inputs
# Por exemplo, evitam repetir o código que seria aplicado em diversas variáveis 

# No purrr, as funções map() realizam tais tarefas
# O map() parte de um vetor e aplica dada função para cada elemento dele
# Retorna um vetor de mesmo comprimento do vetor input. Vetores resultantes:

# map(): listas
# map_lgl(): lógicos
# map_int(): inteiros
# map_dbl(): doubles
# map_chr(): caracteres 

# A seguir, vamos criar o vetor que contém os inputs para a função map()
# Para o dataset_inicial, vamos selecionar as variáveis numéricas:
glimpse(dataset_inicial)

vetor_input <- c("IDADE", "TARIFA")

# O objetivo é criar um vetor (numérico) que contém estatísticas por variável

# A seguir, cada linha gera um tipo de estatística para cada variável do vetor
# A tarefa que realizamos em 3 linhas antes, é realizada em uma linha agora

map_dbl(dataset_inicial[vetor_input], mean, na.rm = T)
map_dbl(dataset_inicial[vetor_input], median, na.rm = T)
map_dbl(dataset_inicial[vetor_input], sd, na.rm = T)
map(dataset_inicial[vetor_input], quantile, probs = c(0.25, 0.50, 0.75), na.rm = T)

# ATENÇÃO: embora não seja necessário nesta base de dados, usamos na.rm = T
# O argumento solicita a remoção de NAs antes de fazer as contas (T é TRUE)
# É um argumento importante, pois evita erros quando há dados faltantes

# Nos caso os percentis, utilizamos apenas o map, pois é gerada uma lista
# A justificativa é que pedimos 2 informações no mesmo código
# Vamos analisar o objeto gerado em uma lista

lista_quartis <- map(dataset_inicial[vetor_input], quantile, probs = c(0.25, 0.50, 0.75), na.rm = T)

lista_quartis[["IDADE"]]
lista_quartis[["IDADE"]][["25%"]]

# A seguir, vamos utilizar o map() e gerar descritivas completas das variáveis

map(dataset_inicial[vetor_input], ~ summary(.))

# Portanto, o cógigo acima gerou mais informação em uma única linha
# O ~ indica que trata-se de uma função, ou seja, escreveremos uma função
# Os pontos substituem a indicação dos dados (usa nova_base[vetor_input])

# Acima, foram utilizadas funções já existentes (mean, median, sd, quantile)
# Porém, também poderiam conter funções (functions) criadas por nós
# A seguir, combinaremos o map() como a função do coeficiente de variação

coef_var <- function(x) {
  cv <- ((sd(x, na.rm=T))/(mean(x, na.rm=T)))*100
  return(cv)
}

# Após elaborar a nova função, basta utilizá-la no map()

map_dbl(dataset_inicial[vetor_input], coef_var)

# Também poderíamos adicionar diretamente ao map() com os atalhos ~ e .
# onde o ~ significa uma função 
# e o . são os argumentos passados para ela

map_dbl(dataset_inicial[vetor_input], ~ (sd(., na.rm=T) / mean(., na.rm=T))*100)

# A seguir, utilizamos o map pedindo os elementos da 5ª linha

map(dataset_inicial, 5)

# Também podemos identificar os tipos de elementos contidos no vetor

map_chr(dataset_inicial, typeof)

# E os elementos únicos deste objeto

map(dataset_inicial, unique)

# Em resumo, podemos utilizar a função map() de forma bastante flexível
# A ideia é sempre replicar uma função aos elementos do vetor input

#------------------------MAP2---------------------------------------------------

# O map() também pode ser aplicado quando há múltiplos inputs

# Por exemplo, vamos gerar variáveis com as seguintes médias e desvios padrão

médias_var <- list(5, 10, 15)
desv_pad_var <- list(1, 2, 3)

map2(médias_var, desv_pad_var, rnorm, n = 5)

# Os parâmetros interagiram em sequencia: 5 e 1, 10 e 2, 15 e 3
# No map2(), os inputs que variam estão antes da função e os dados fixos depois


#---------------------PMAP------------------------------------------------------

# Para vários inputs, utiliza-se pmap()
# Vamos variar o tamanho "n" das variáveis

tamanho_var <- list(7, 9, 11)

parametros <- list(tamanho_var, médias_var, desv_pad_var) # sequência da fórmula

pmap(parametros, rnorm)

# Na prática, para evitar erros, é melhor nomear os argumentos

parametros2 <- list(mean = médias_var, sd = desv_pad_var, n = tamanho_var)

pmap(parametros2, rnorm)


#-----------------------UNIQUE-------------------------------------------------

# Com a função unique podemos ver os valores únicos dos dados

unique(dados$Embarked)

glimpse(dados)
 
#---------------------na_if------------------------------------------------------
# com ela podemos substituir algo nos dados por NA

dados %>% mutate(
  Cabin = na_if(Cabin, "")
) %>% select(Cabin)

# caso seja usado umafunção de conversão numérica os valores que não podem ser convertidos
# para números são transformados em NA

dados %>% mutate(
  Cabin = as.double(Age)
) %>% 
select(Cabin) %>% 
slice_tail(n=10)


#-----------------------PIVOT-------------------------------------------------
# Para fazer o pivot de um dataframe é usado a função pivot_wider


head(fish_encounters)

fish_encounters %>%
  pivot_wider(
    names_from = station, 
    values_from = seen)
# Fill in missing values
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)

# Generate column names from multiple variables
head(us_rent_income)
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )

df_wdi <- read_excel("../dados/wdi_world_bank.xlsx")

df_wdi <- df_wdi[1:383572,]

df_wdi <- df_wdi %>% rename(
  pais = 1,
  cod_pais = 2,
  serie = 3,
  cod_serie = 4,
  ano_2021 = 5,
  topico = 6) %>% mutate(
    ano_2021 = as.double(ano_2021)
  ) %>% filter(
    str_detect(topico, "^Economic Policy & Debt")
  )


df_pivot <- df_wdi %>% pivot_wider(
  id_cols = c("pais", "cod_pais"), # linhas
  names_from = "serie", # colunas 
  values_from = "ano_2021") # valor das linhas

print("linhas e colunas")
dim(df_pivot)

# é possível remover as colunas que tiverem todos os seus valores NA
df_pivot <- df_pivot %>%
  purrr::discard(~ all(is.na(.)))

print("linhas e colunas")
dim(df_pivot)

glimpse(df_pivot)
 
#------------------------VALORES INFINITOS-------------------------------------
# O R pode criar valores considerados infinitos(inf) 
# para encontrar valores infinitos usar a função is.infinite(coluna)

# gerar um df com valores infinitos para exemplo
df_inf = fish_encounters %>% mutate(
  col_infinito = sample(0:1, dim(fish_encounters)[1], replace=TRUE),
  infinito = seen / col_infinito, # gera valores aleatórios infinitos por causa
  # divisão por 0
  infinito = is.infinite(infinito) # Atribui True para infinitos
) %>% select(-col_infinito)


head(df_inf)

# com isso podemos usar a condição para filtrar

df_inf %>% filter(
  infinito == TRUE
)
