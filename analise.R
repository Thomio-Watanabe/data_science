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

# Carrega as informações
table_name <- 'train.csv'
paste("Carregando tabela de trainamento: ",table_name)
table = read.csv(table_name)

# Número de linhas 
numero_linhas <- nrow(table)
paste("Número de linhas da tabela: ", numero_linhas)

# Número de colunas
numero_colunas <- ncol(table)
paste("Número de colunas: ", numero_colunas)

# Mostra nome dos atributos
paste("Nome dos atributos: ")
table[0, ]

# ----------------------- SELEÇÃO DE DADOS (INSTÂNCIAS) -------------------------------
# Número de campos NA na table
paste("Número de NAs na tabela: ", sum(is.na(table)) )

# Mostra a quantidade de linhas (instâncias) com NA
linhas_NA = c()
total = 0 
for(i in 1:numero_linhas)
{
    # Se existir pelo menos um NA na linha
    if( sum(is.na(table[i,])) > 0)
    {
        linhas_NA <- c(linhas_NA,i)
        total = total + 1
    }
}
paste("Linhas com pelo menos um NA: ",linhas_NA )
paste("Número de linhas (instâncias) com NA: ", total )
paste("Porcentagem de instâncias com pelo menos um NA: ", 100*(total/numero_linhas) )


total70 <- 0
for(j in numero_linhas:1)
{
    # Entra no if se a instância possuir mais de 70% de NAs
    if( sum(is.na(table[j,])) > (numero_colunas-2)*0.7 )
    {
        table <- table[-c(j),]
        total70 <- total70 + 1
    }
}
paste("Número de linhas (instâncias) com 70% de NAs: ", total70 )
paste("Porcentagem de instâncias com mais de 70% de NAs", 100*(total70/numero_linhas))

write.csv(table, file = "filtered_train.csv")



# # Percorre todas as colunas
# for(i in 1:numero_colunas)
# {
#     cat( paste( "Classe da coluna: ", class(table[[i]]), '\n' ) )
# }




# --------------------------- Pré-processamento e Limpeza ---------------------------




# Deleta a variável que contém a tabela
rm(table)


# --------------------------- Subfunções ---------------------------
# Função para retornar a moda
estimate_mode <- function(x){
  d <- density(x)
  d$x[which.max(d$y)]
}

# -------------------------------------------------------------------------------------
