# Grupo: Letícia, Mario, Guilherme, Larissa, Maria Emilia

# carrega as bibliotecas
pacman::p_load(car, caret, corrplot, data.table, dplyr, forcats, funModeling, mltools, randomForest, tidyverse)

# AED 
status(swiss) # explorar a qualidade das variáveis
plot_num(swiss) # exploração das variáveis numéricas
profiling_num(swiss) # estatísticas das variáveis numéricas

corrplot(cor(treino[ , c(6:12)])) # correlação entre as variáveis

# Treino e Teste: Pré-processamento
particaoswiss = createDataPartition(1:nrow(swiss), p=.7) # cria a partição 70-30
treinoswiss = swiss[particaoswiss$Resample1, ] # treino
testeswiss = swiss[-particaoswiss$Resample1, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
# train.control <- trainControl(method = "boot", number = 100)

train.control <- trainControl(method = "cv", number = 10) # controle de treino
# Treinamento
swiss_LM <- train(Infant.Mortality ~ ., data = swiss, method = "glmboost", trControl = train.control)
# Sumários
print(swiss_LM)
summary(swiss_LM)

# Bagging para Regressão
swiss_RF = randomForest(treinoswiss[ , c(3, 8, 12)], treinoswiss[ , 6], ntree = 100, keep.forest=T, keep.inbag = TRUE, importance=T) # floresta aleatória

plot(swiss_RF)

varImp(swiss_RF, scale = T) # importância de cada variável
varImpPlot(swiss_RF, type=2) # importância de cada variável

# Boosting
# Treinamento
swiss_ADA <- train(Infant.Mortality ~ ., data = swiss, method = "glmboost", trControl = train.control)

# evolução do modelo
plot(swiss)

# Sumários
print(swiss_ADA)
summary(swiss_ADA)

# Bagging para Classificação
swiss_RF = randomForest(swiss[ , 1:4], swiss[ , 5], ntree = 100, keep.forest=T, keep.inbag = TRUE, importance=T) # floresta aleatória

plot(swiss_RF)

varImp(swiss_RF, scale = T) # importância de cada variável
varImpPlot(swiss_RF, type=2) # importância de cada variável
