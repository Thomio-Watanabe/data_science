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


# Carrega as tabelas
full_table <- read.csv("train_complete_na.csv")
train_table <- read.csv("train_sampled.csv")

# retorna as posições dos valores amostrados que estão na tabela original
train_samples <- which( full_table$ID %in% train_table$ID )

# Cria uma table apenas com as instâncias que não foram usadas no teste
test_table <- full_table[-train_samples,]

# Recupera apenas os atributos que serão utilizados
attrib_names <- c("ID",   "target", "v4",   "v10",
                  "v12",  "v14",    "v21",  "v31",
                  "v34",  "v47",    "v48",  "v50",
                  "v56",  "v62",    "v64",  "v72",
                  "v93",  "v101",   "v106", "v110",
                  "v114", "v119",   "v123", "v129")

# Obs: o atributo v56 não roda no randomForest
# table$v56 <- NULL

reduced_table <- as.data.frame( cbind( test_table[attrib_names] ) )

# -------------------------------------------------------------------------------------
# Salva resultados
# -------------------------------------------------------------------------------------

nome_arquivo_saida <- "test_sampled.csv"
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )
write.csv( reduced_table, file = nome_arquivo_saida )


# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
rm(full_table)
rm(train_table)
rm(test_table)
rm(reduced_table)
# -------------------------------------------------------------------------------------
