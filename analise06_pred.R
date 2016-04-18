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
# Antes de predizer os dados de teste é necessário condicionar a tabela
# -------------------------------------------------------------------------------------
# Carrega a tabela de teste
table_name <- 'test.csv'
paste("Carregando tabela de teste: ",table_name)
test <- read.csv(table_name)
# -------------------------------------------------------------------------------------

# Carrega o nome dos atributos usados no treinamento
attr_names <- readRDS("attr_names.rds")
cat(paste("Atributos usados: ",attr_names, '\n') )
test <- test[attr_names]



for( i in 2:ncol(test) ){
    class_col <- class( test[[i]] )
    media <- 0 
    cat(paste("Classe da coluna",attr_names[i],": ", class_col, '\n') )

#    # Calcula a trimmed mean - exclui uma percentagem dos extremos
#    mean_col <- mean( as.numeric(na.omit(test[[i]])), trim = 0.1 )
#     cat(paste("Substitui NA pela média: ", mean_col,' \n'))
#     test[which(is.na(test[[i]])), i] <- mean_col
# }
    if(class_col == "numeric" ){
        # Calcula a trimmed mean - exclui uma percentagem dos extremos
        mean_col <- mean( na.omit(test[[i]]), trim = 0.1 )
        cat(paste("Classe numérica. Substitui NA pela média: ", mean_col,' \n'))
        test[which(is.na(test[[i]])), i] <- mean_col
    }else{
        posi <- c()
        val <- c()
        cat(paste("Classe integer ou Factor. Substitui NA pelo valor anterior. \n") )
        for( j in 1:nrow(test) ){
            if( test[j,i] == ""  ){
                if( j != 1 ){
                    posi <- c( posi,j )
                    val <- c( val, as.character( getLastValue(test[[i]], j)) )
                    # test[j,i] <- test[j-1,i]
                }else{ # Se não existir nada na primeira posição... pega o valor da segunda
                    pose <- c(posi,j)
                    val <- c(val,test[j+1,i])
                }
            }
        }
        test[posi,i] <- val
    }
}

nome_arquivo_saida <- "test_complete_na.csv"
cat( paste("Substituição de NAs completa !\n") )
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )
write.csv( test, file = nome_arquivo_saida, row.names = FALSE )


# -------------------------------------------------------------------------------------
# Avaliação
# -------------------------------------------------------------------------------------
# Transforma valores não numéricos da tabela

# carrega o modelo treinado
# rf <- load("model_randomForest1000Trees.rda")
rf <- readRDS("model_randomForest1000Trees.rds")

# Faz a predição dos dados de teste
library(randomForest)
prediction <- predict(rf, test)

# PredictedProb
predic_submission <- as.data.frame( cbind(test$ID, prediction) )
colnames(predic_submission) <- c("ID","PredictedProb")
str(predic_submission)

# -------------------------------------------------------------------------------------
# Salva resultados
# -------------------------------------------------------------------------------------
nome_arquivo_saida <- "resultado_rf.csv"
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )
write.csv( predic_submission, file = nome_arquivo_saida , row.names = FALSE)

# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
rm(table)
# -------------------------------------------------------------------------------------
