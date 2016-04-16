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
# Amostragem da tabela 
# -------------------------------------------------------------------------------------

# Carrega as informações
table_name <- 'train_complete_na.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
table = read.csv(table_name)


# Recupera apenas os atributos que serão utilizados
attrib_names <- c("ID",   "target", "v4",   "v10",
                  "v12",  "v14",    "v21",  "v31",
                  "v34",  "v47",    "v48",  "v50",
                  "v56",  "v62",    "v64",  "v72",
                  "v93",  "v101",   "v106", "v110",
                  "v114", "v119",   "v123", "v129")
reduced_table <- as.data.frame( cbind( table[attrib_names] ))

# Realiza a amostragem mantendo a relação entre 0 e 1 do target:
target_mean <- mean(reduced_table$target)
sampled_mean <- 0.0
sampled_table <- data.frame()

# Número de amostras
number_samples <- round( 0.4*nrow(reduced_table) )
cat( paste("Número de amostras final: ", number_samples, '\n') )

while ((sampled_mean <= 0.9*target_mean)||( sampled_mean >= 1.1*target_mean)) {
    sampled_rows <- sample(nrow(reduced_table), number_samples )
    sampled_table <- reduced_table[sampled_rows,]
    sampled_mean <- mean(sampled_table$target)
}

cat( paste("Média do target nos dados amostrados: ", sampled_mean, '\n') )

# -------------------------------------------------------------------------------------
# Salva resultados
# -------------------------------------------------------------------------------------
nome_arquivo_saida <- "train_sampled.csv"
cat( paste("Salvando tabela no arquivo ", nome_arquivo_saida ,'\n') )
write.csv( sampled_table, file = nome_arquivo_saida )



# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
rm(table)
# -------------------------------------------------------------------------------------
