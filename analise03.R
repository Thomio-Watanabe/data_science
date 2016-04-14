# -------------------------------------------------------------------------------------
# Thomio Watanabe
# Ciência de dados
# Abril 2016
# USP ICMC

# KDD - Knowledge Discovery in Database
# Etapas do KDD
# 1 - Seleção
# 2 - Pre-processamento e limpeza
# 3 - Transformação
# 4 - Mineração de dados
# 5 - Avaliação

# -------------------------------------------------------------------------------------
# Subfunctions
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Visualizalção
# -------------------------------------------------------------------------------------

# Carrega as informações
table_name <- 'train_complete_na.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
table = read.csv(table_name)

# Faces de Chernoff
# instale o pacote: 
#   install.packages("aplpack")
# chame a biblioteca:
#   library(aplpack)
# chame a função:
#  faces(table[,3:140])


# Pie chart para o atributo target:
# Pode-se observar que há uma tendencia na aprovação das requisições de reembolso (relação de 3x1)
# Isso significa que os dados são desbalanceados e pesos devem ser usados no algoritmo de AM
slices <- c(sum(table$target == 1),sum(table$target == 0))
rotulos <- c("1","0")
pie(slices, labels = rotulos)


# -------------------------------------------------------------------------------------

# Seleção de subconjunto de atributos para redução da dimensionalidade
# Método empregado: Filtragem -> independente do algoritmo de AM
# Correlation-based feature extraction 
# Instalar dependencias do systema:
#    sudo apt-get install r-cran-plyr r-cran-reshape2 r-cran-ggplot2 r-cran-car
# Instala pacotes:
#    install.packages("mlbench")
#    install.packages('caret', dependencies = TRUE)


set.seed(7)
# load the library
library(mlbench)
library(caret)

numeric_tab <- sapply(table[,1:ncol(table)], as.numeric)
correlationMatrix <- cor(table$target, numeric_tab[,5:ncol(numeric_tab)])
pos <- which( abs(correlationMatrix) > 0.05)

sink("matrix_correl.txt")
correlationMatrix
cat( paste("\n\n ", length(correlationMatrix[,pos])," atributos com correlação maior que +-5%: \n") )
correlationMatrix[,pos]
sink()


# -------------------------------------------------------------------------------------
# Salva resultados


# Deleta a variável que contém a tabela
rm(table)

# -------------------------------------------------------------------------------------
