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
getLastValue <- function(x,k){
    while(x[k] == ""){
        k <- k -1
    }
    # cat(paste("Retorno de getLastValue: ", x[k],'\n'))
    return (x[k])
}
# -------------------------------------------------------------------------------------
# Carrega as informações
table_name <- 'train_filtered.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
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


# Calcula a frequencia dos dados não numéricos e salva em arquivo
nomes_atributos <- names(table)
sink("frequencia.txt")
for( i in 4:ncol(table) ){
    class_col <- class( table[[i]] )
    if(class_col != "numeric" ){
        cat(paste("\n\n Frequencia do atributo ", nomes_atributos[i],'\n'))
        cat(paste( round(100*ftable(table[[i]])/nrow(table),digits = 1) ))
    }
}
sink()

# --------------------------- Pré-processamento e Limpeza ---------------------------

# Remove coluna v113 -> não tem dados suficientes, muitos NAs
table$v113 <- NULL
# Elimina atributos não numéricos com baixa distribuição. Ver a distribuição de frequencia: frequencia.txt
table$v3 <- NULL
table$v38 <- NULL
table$v74 <- NULL


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
        posi <- c()
        val <- c()
        cat(paste("Classe integer ou Factor. Substitui NA pelo valor anterior. \n") )
        for( j in 1:nrow(table) ){
            if( table[j,i] == ""  ){
                if( j != 1 ){
                    posi <- c( posi,j )
                    val <- c( val, as.character( getLastValue(table[[i]], j)) )
                    # table[j,i] <- table[j-1,i]
                }else{ # Se não existir nada na primeira posição... pega o valor da segunda
                    pose <- c(posi,j)
                    val <- c(val,table[j+1,i])
                }
            }
        }
        table[posi,i] <- val
    }
}

nome_arquivo_saida <- "train_complete_na.csv"
cat( paste("Substituição de NAs completa !\n") )
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )

write.csv( table, file = nome_arquivo_saida )

# Deleta a variável que contém a tabela
rm(table)

# -------------------------------------------------------------------------------------
