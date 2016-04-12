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
table_name <- 'filtered_train.csv'
paste("Carregando tabela de trainamento: ",table_name)
table = read.csv(table_name)

# Mostra as classes de dados de cada atributo.
sapply(table,class)

# Mostra o tipo de dado de cada atributo
sapply(table,typeof)

# Recebe e salva sumario dos tipos de dados
sink("sumario_atributos.txt")
str(table, list.len = 145)
sink()

# Outras funções 
# levels(table#v22)
# summary(table$v22)

# --------------------------- Pré-processamento e Limpeza ---------------------------
nomes_atributos <- names(table)
for( i in 4:ncol(table) ){
    class_col <- class( table[[i]] )
    media <- 0 
    cat(paste("Classe da coluna",nomes_atributos[i],": ", class_col, '\n') )

#    # Calcula a trimmed mean - exclui uma percentagem dos extremos
#    mean_col <- mean( as.numeric(na.omit(table[[i]])), trim = 0.1 )
#     cat(paste("Substitui NA pela média: ", mean_col,' \n'))
#     table[which(is.na(table[[i]])), i] <- mean_col
# }
    if(class_col == "numeric" ){
        # Calcula a trimmed mean - exclui uma percentagem dos extremos
        mean_col <- mean( na.omit(table[[i]]), trim = 0.1 )
        cat(paste("Classe numérica. Substitui NA pela média: ", mean_col,' \n'))
        table[which(is.na(table[[i]])), i] <- mean_col
    }else{
        cat(paste("Classe integer ou Factor. Substitui NA pelo valor anterior. \n") )
        for( j in 1:nrow(table) ){
            if( table[j,i] == "" ){
               table[j,i] <- table[j-1,i]
            }
        }
    }
}

cat( paste("Substituição de NAs completa !\n") )
cat( paste("Saving table. \n") )

write.csv(table, file = "completed_train.csv")

# Deleta a variável que contém a tabela
rm(table)

# -------------------------------------------------------------------------------------
