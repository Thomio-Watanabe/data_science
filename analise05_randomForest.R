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
# Treinamento e avaliação 
# -------------------------------------------------------------------------------------

# Carrega as informações
table_name <- 'train_sampled.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
table = read.csv(table_name)

# Deleta atributo v56 -> Random forest não resolve atributos categóricos com muitos níveis
table$v56 <- NULL

# Calcula o randomForest
set.seed(100)
library(randomForest)
rf <- randomForest(target~.,table[3:ncol(table)], ntree=1000 )
# rf <- randomForest(target~., table[3:ncol(table)], ntree=3000 )

# Plota os atributos mais importantes no cálculo
varImpPlot(rf)

# Nome dos atributos da tabela de treino
attr_names <- names(table)
# Remove atributos "X" e "target"
attr_names <- attr_names[c(-1,-3)]
# Salva em arquivo o nome dos atributos que foram usados
saveRDS(attr_names,file="attr_names.rds")
# save(attr_names,file="attr_names.rda")

# Salva modelo
# saveRDS(rf,file="model_randomForest3000Trees.rda")
saveRDS(rf,file="model_randomForest1000Trees.rds")


# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
# rm(table)
# -------------------------------------------------------------------------------------
