###############################################
# Vamos separar a base em treinamento e teste #
set.seed(123)
bool_treino <- stats::runif(dim(titanic)[1])>.25

treino <- titanic[bool_treino,]
teste  <- titanic[!bool_treino,]

titanic %>% str
# Deixar a árvore ser feliz
# ATENÇÂO! NÃO PLOTAR ESTA ÁRVORE!

set.seed(123)
arvore <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=treino,
                method='class',
                xval=5,
                control = rpart.control(cp = 0,         # parametro de complexidade
                                        minsplit = 1,   # minimo de obs por split
                                        maxdepth = 30)  # 
)

# Verificando a complexidade da árvore
arvore$frame

############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))

p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_treino, treino$Survived)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

sink(file = '_out/script_002/output/acc_treino.txt')
print(acc)
sink()

tab <- table(c_teste, teste$Survived)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de teste: %s ', percent(acc))

sink(file = '_out/script_002/output/acc_teste.txt')
print(acc)
sink()


###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs=treino$Survived, 
                         pred=c_treino,
                         Y = p_treino[,2],
                         N = 1-p_treino[,2]
                         )

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))


sink(file = '_out/script_002/output/aval_treino.txt')
print(caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs)))
sink()

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")
  
CurvaROC


# salvando em .png
dev.print(file = '_out/script_002/figures/roc_treino.png',
          device = png, width = 1024, height = 768, res = 2*72)


############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$Survived, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

sink(file = '_out/script_002/output/aval_teste.txt')
print(twoClassSummary(aval_teste, lev=levels(aval_teste$obs)))
sink()

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

# salvando em .png
dev.print(file = '_out/script_002/figures/roc_teste.png',
          device = png, width = 1024, height = 768, res = 2*72)



##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore)
tab_cp

sink(file = '_out/script_002/output/tab_cp_pos_poda_Grid_Search.txt')
print(tab_cp)
sink()

plotcp(arvore)
# salvando em .png
dev.print(file = '_out/script_002/figures/arvore.png',
          device = png, width = 1024, height = 768, res = 2*72)


tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

sink(file = '_out/script_002/output/cp_min.txt')
print(cp_min)
sink()

set.seed(1)
arvore_poda <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
aval_treino <- data.frame(obs=treino$Survived, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

sink(file = '_out/script_002/output/aval_treino_pos_poda.txt')
print(caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs)))
sink()

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

# salvando em .png
dev.print(file = '_out/script_002/figures/CurvaROC_pos_poda_treino.png',
          device = png, width = 1024, height = 768, res = 2*72)

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$Survived, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

sink(file = '_out/script_002/output/aval_teste_pos_poda.txt')
print(twoClassSummary(aval_teste, lev=levels(aval_teste$obs)))
sink()

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

# salvando em .png
dev.print(file = '_out/script_002/figures/CurvaROC_pos_poda_teste.png',
          device = png, width = 1024, height = 768, res = 2*72)

