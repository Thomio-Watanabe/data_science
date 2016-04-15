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
# --------------------------- Subfunções ---------------------------
# Função para calcular o número de NAs em cada instância
# X = tabela (data.frame)
# percentage (0 - 1) = porcentagem mínima de NAs (para pelo menos um NA inserir 0%)
calcNA <- function(x, percentage){
    linhas_NA <- c()
    total_linhas <- 0
    nb_NAs <- (ncol(x) - 2)* percentage
    for(i in 1:nrow(x))
    {
        # Entra no if se a instância possuir mais nb_NAs de NAs
        if( sum(is.na(x[i,])) > nb_NAs )
        {
            linhas_NA <- c(linhas_NA,i)
            total_linhas <- total_linhas + 1
        }
    }

    cat( paste("Número de linhas (instâncias) com mais de ", percentage*100,"% de NAs: ", total_linhas,'\n' ))
    cat( paste("Porcentagem de instâncias com mais de ", percentage*100,"% de NAs", 100*(total_linhas/nrow(x)), '\n') )

    return (linhas_NA)
}
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
linhas_NA <- calcNA(table,0.0)

linhas_NA85 <- calcNA(table,0.70)

filtered_table <- table[-linhas_NA85,]
write.csv(filtered_table, file = "train_filtered.csv")


# # Percorre todas as colunas
# for(i in 1:numero_colunas)
# {
#     cat( paste( "Classe da coluna: ", class(table[[i]]), '\n' ) )
# }



# --------------------------- Pré-processamento e Limpeza ---------------------------




# Deleta a variável que contém a tabela
rm(table)

# -------------------------------------------------------------------------------------
