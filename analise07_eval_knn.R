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
# Obtido de https://www.kaggle.com/wiki/LogarithmicLoss
# Modificação: nrow -> length
# MultiLogLoss <- function(act, pred)
#    {
#      eps = 1e-15;
#      nr <- nrow(pred)
#      pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
#      pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
#      ll = sum(act*log(pred) + (1-act)*log(1-pred))
#      ll = ll * -1/(nrow(act))      
#      return(ll);
#    }

MultiLogLoss <- function(act, pred)
{
    eps = 1e-15;
    nr <- length(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(length(act))
    return(ll);
}

min_function <- function(par,data){
    lower_threshold <- par[1]
    upper_threshold <- par[2]
    lower_values <- which(prediction <= lower_threshold)
    upper_values <- which(prediction >= upper_threshold)
    min_test <- prediction
    min_test[lower_values] <- 0.0
    min_test[upper_values] <- 1.0
    return (MultiLogLoss(data,min_test))
}
# -------------------------------------------------------------------------------------

# Carrega a tabela de treinamento
table_name <- 'train_sampled.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
table = read.csv(table_name)

# k-NN não resolve atributos categóricos
table$v31 <- NULL
table$v47 <- NULL
table$v56 <- NULL
table$v110 <- NULL

cat( paste( "n col table: ", ncol(table), '\n' ))
cat( paste( "n linhas table: ", nrow(table), '\n' ))
cat( paste( "n na table: ", sum(is.na(table)), '\n' ))


table_name <- 'test_sampled.csv'
cat( paste("\n Carregando tabela de teste: ",table_name, '\n') )
test_table <- read.csv(table_name)

test_table$v31 <- NULL
test_table$v47 <- NULL
test_table$v56 <- NULL
test_table$v110 <- NULL

cat( paste( "n col test_table: ", ncol(test_table), '\n' ))
cat( paste( "n linhas table: ", nrow(test_table), '\n' ))
cat( paste( "n na test_table: ", sum(is.na(test_table)), '\n' ))


# Divide a tabela de treinamento em duas
total_samples <- length(test_table$target)
samples <- 1:total_samples
num_samples <- round( 0.5 * total_samples )
test_samples <- sample( samples, size = num_samples )

test_table_01 <- test_table[ test_samples, ]
test_table_02 <- test_table[ -test_samples, ]



# Gera o modelo kNN
set.seed(100)
library(FNN)
# knn.reg(train, test = NULL, y, k = 3, algorithm=c("kd_tree", "cover_tree", "brute"))
# knn_model <- knn.reg(table[4:ncol(table)], test = test_table[4:ncol(test_table)], y = table$target, k = 19, algorithm=c("kd_tree"))
knn_model_01 <- knn.reg(table[4:ncol(table)], test = test_table_01[4:ncol(test_table)], y = table$target, k = 19, algorithm=c("kd_tree"))
knn_model_02 <- knn.reg(table[4:ncol(table)], test = test_table_02[4:ncol(test_table)], y = table$target, k = 19, algorithm=c("kd_tree"))


# Carrega modelo do randomForest
# model <- readRDS("model_knn19.rds")    

# library(randomForest)
# prediction_01 <- predict(model, test_table_01)
# prediction_02 <- predict(model, test_table_02)


# Calcula o log loss das duas tabelas e mostra o resultado
# log_loss <- MultiLogLoss(test_table$target, knn_model$pred)
# cat( paste("Log loss tabela de test: ", log_loss, '\n') )     # Erro -> 0.

log_loss_01 <- MultiLogLoss(test_table_01$target, knn_model_01$pred)
cat( paste("Log loss tabela de test 01: ", log_loss_01, '\n') )     # Erro -> 0.591478820637994

log_loss_02 <- MultiLogLoss(test_table_02$target, knn_model_02$pred)
cat( paste("Log loss tabela de test 02: ", log_loss_02, '\n') )     # Erro -> 0.618049013861708


## Otimização -> calcula os valores de threshold que minimizam o logLoss
## lower_threshold <- seq(from = 0.1, to = 0.4, by = 0.01)
## upper_threshold <- seq(from = 0.85, to = 0.99, by = 0.01)
# result_01 <- optim(par = c(0.3, 0.9),
#                 min_function,
#                 data = test_table_01$target)
# 
# result_02 <- optim(par = c(0.3, 0.9),
#                 min_function,
#                 data = test_table_02$target)
# result_01
# result_02
## cat( paste("Threshold minimo e máximo: ",result) )


# -------------------------------------------------------------------------------------
# Gera tabela com os resultados
# Carrega a tabela de teste
table_name <- 'test.csv'
paste("Carregando tabela de teste: ",table_name)
test <- read.csv(table_name)

# Carrega o nome dos atributos usados no treinamento
attr_names <- readRDS("attr_names_knn.rds")
cat(paste("Atributos usados: ",attr_names, '\n') )
test <- test[attr_names]

# k-NN não resolve atributos categóricos
test$v31 <- NULL
test$v47 <- NULL
test$v56 <- NULL
test$v110 <- NULL

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


knn_model <- knn.reg(table[4:ncol(table)], test = test[2:ncol(test)], y = table$target, k = 19, algorithm=c("kd_tree"))

# PredictedProb
predic_submission <- as.data.frame( cbind(test$ID, knn_model$pred) )
colnames(predic_submission) <- c("ID","PredictedProb")
str(predic_submission)

# -------------------------------------------------------------------------------------
# Salva resultados
# -------------------------------------------------------------------------------------
nome_arquivo_saida <- "resultado_knn.csv"
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )
write.csv( predic_submission, file = nome_arquivo_saida , row.names = FALSE) # resultado do kaggle = 0.60282 (log loss), posição = 2738



# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
rm(test_table)
rm(test_table_01)
rm(test_table_02)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
