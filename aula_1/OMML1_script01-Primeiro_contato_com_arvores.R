######################################
# Vamos trabalhar com a base titanic #
# cuja fonte está na library(titanic)#
#data("Titanic")
#Titanic
setwd("C:\\Users\\Renato\\Dropbox\\pos_usp\\ML\\neural_network_algorithms\\aula_1")
titanic <- as.data.frame(read_excel("titanic.xlsx"))
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Survived <- as.factor(titanic$Survived)
head(titanic)
titanic %>% head
#################################
# Nosso objetivo:
#      Classificar passageiros sobreviventes de acordo 
#      somente com variáveis do registro deles
################################


#################################### 
# Vamos fazer uma breve descritiva #

# Vamos criar uma base temporária para manter a base original intacta
tmp <- titanic
tmp$survived <- as.integer(titanic$Survived=="Y")
titanic
##########################################
# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de sobreviventes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("Sex")
descritiva("Pclass")
descritiva("Embarked")
descritiva("SibSp")
descritiva("Parch")

# Vamos categorizar as variáveis contínuas para analisar
tmp$cat_age <- quantcut(tmp$Age, 20)
descritiva("cat_age")

# salvando em .png
dev.print(file = '_out/script_001/figures/cat_age.png',
          device = png, width = 1024, height = 768, res = 2*72)

tmp$cat_fare <- quantcut(tmp$Fare, 10)
descritiva("cat_fare")

# salvando em .png
dev.print(file = '_out/script_001/figures/cat_fare.png',
          device = png, width = 1024, height = 768, res = 2*72)

# Listagem das variáveis com algumas características
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Survived <- as.factor(titanic$Survived)
titanic %>% str

#############################################
# Vamos construir a árvore de classificação #
arvore <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=titanic,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class'                # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

# salvando em .png
dev.print(file = '_out/script_001/figures/plotando_arvore.png',
          device = png, width = 1024, height = 768, res = 2*72)


##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de sobreviver
prob = predict(arvore, titanic)

# Classificação dos sobreviventes
class = prob[,2]>.5 #2 - sobreviveu
# Matriz de confusão
tab <- table(class, titanic$Survived)
tab

sink(file = '_out/script_001/output/matriz_confusao.txt')
print(tab)
sink()

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

sink(file = '_out/script_001/output/acc.txt')
print(acc)
sink()

# importancia das variáveis -> variable.importance
arvore$variable.importance

sink(file = '_out/script_001/output/variable_importance.txt')
print(arvore$variable.importance)
sink()
