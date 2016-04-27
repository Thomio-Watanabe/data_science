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
table_name <- 'test_sampled.csv'
cat( paste("Carregando tabela de treino: ",table_name) )
test_table <- read.csv(table_name)


# Divide a tabela de treinamento em duas
total_samples <- length(test_table$target)
samples <- 1:total_samples
num_samples <- round( 0.5 * total_samples )
test_samples <- sample( samples, size = num_samples )

test_table_01 <- test_table[ test_samples, ]
test_table_02 <- test_table[ -test_samples, ]


# Carrega modelo do randomForest
rf <- readRDS("model_randomForest1000Trees.rds")   # Erro -> 0.167809
# rf <- readRDS("model_randomForest3000Trees.rds")    # Erro -> 0.167793

library(randomForest)
prediction_01 <- predict(rf, test_table_01)
prediction_02 <- predict(rf, test_table_02)


# Calcula o log loss das duas tabelas e mostra o resultado
log_loss_01 <- MultiLogLoss(test_table_01$target,prediction_01)
cat( paste("Log loss tabela de test 01: ",log_loss_01, '\n') )     # Erro -> 0.1101543

log_loss_02 <- MultiLogLoss(test_table_02$target,prediction_02)
cat( paste("Log loss tabela de test 02: ",log_loss_02, '\n') )     # Erro -> 0.1101543


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
# Deleta a variável que contém a tabela
rm(test_table)
rm(test_table_01)
rm(test_table_02)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
