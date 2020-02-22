rm(list = ls())

# Instalando os pacotes
install.packages("labeling", dependencies = TRUE)
install.packages("rlang",dependencies = TRUE)
install.packages("tibble",dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("lattice", dependencies = TRUE)
# carregando os pacotes

library(tibble)
library(ggplot2)
library(lattice)
library(dplyr)
library(stringr)


# Onde é o diretorio que está:
getwd()
# Trocar o diretorio
setwd("G:/TCC/Codigos para o TCC")
# Lendo o banco de dados com o R
# Mais Educação
MaisEdu14 <-read.csv2("repasses_mais_educacao_adesao_2014.csv")
summary(MaisEdu14)
# olhando os primeiros dados
head(MaisEdu14)
dim(MaisEdu14)

# olhando os tipos das variaveis
str(MaisEdu14)

# transformando em tibble
MaisEdu <- as_tibble(MaisEdu14)
MaisEdu
head(MaisEdu$TOTAL_RECEBIDO_ADESAO)

# Arrumando a banco de dados, para futura analise

# Arrumando Total Recebido
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[A-z]", "", MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[$]", "", MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[.]", "", MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[[:space:]]", "", MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[,]", ".", MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- gsub("[-]","0" , MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu$TOTAL_RECEBIDO_ADESAO <- as.numeric(MaisEdu$TOTAL_RECEBIDO_ADESAO)
MaisEdu
MaisEdu$TOTAL_RECEBIDO_ADESAO

# Arrumando Alunado
MaisEdu
MaisEdu$ALUNADO <- gsub("[A-z$.[:space:]]", "", MaisEdu$ALUNADO)
MaisEdu$ALUNADO <- as.integer(MaisEdu$ALUNADO)
summary(MaisEdu)

# criando um banco de dados para mostrar de duas formas
MaisEduT <- MaisEdu 

# Renomeando as colunas
nomes <-colnames(MaisEduT)
nomes
nomes[8] <- "ALUNOS_2014"
nomes[9] <- "TOTAL_RECEBIDO_2014"
nomes
colnames(MaisEduT) <- nomes


# Renomeando as colunas usando dplyr
MaisEdu <- MaisEdu %>% 
  rename(
    TOTAL_RECEBIDO_2014 = TOTAL_RECEBIDO_ADESAO,
    ALUNOS_2014 = ALUNADO
    )
# Deletando o banco de dados que usamos para mostrar como fazer de outra forma
remove(MaisEduT)
MaisEdu
# Removendo a colunas
MaisEdu <- MaisEdu[,-1]

# Sabendo que os INEP_ESCOLA são unicos, Continuar o nosso processo adicionando os valores recebidos dos anos de 2016 e 2017

MaisEdu16 <-read.csv("G:/TCC/Banco de dados/mais educação/pda-repasses-mais-educacao-adesao-2016.csv", head=TRUE,sep=";")
MaisEdu17 <-read.table("G:/TCC/Banco de dados/mais educação/pda-repasses-mais-educacao-adesao-2017.csv", head=TRUE ,sep=";")

# Olhando os dois novos banco de dados
# Primeiro o tamanho
dim(MaisEdu16)
dim(MaisEdu17)
# os primeiro elementos
head(MaisEdu16)
head(MaisEdu17)
# e os tipos que estão as colunas
str(MaisEdu16)
str(MaisEdu17)

# transformando em tibble
ME16 <- as_tibble(MaisEdu16)
ME17 <- as_tibble(MaisEdu17)

# renomeando as colunas das novas duas tabelas
ME16 <- ME16 %>% 
  rename(
    TOTAL_RECEBIDO_2016 = TOTAL_RECEBIDO_ADESAO,
    ALUNOS_2016 = ALUNADO
    )
ME17 <- ME17 %>% 
  rename(
    TOTAL_RECEBIDO_2017 = TOTAL_RECEBIDO_ADESAO,
    ALUNOS_2017 = ALUNADO
    )


ME17$TOTAL_RECEBIDO_2017 <- gsub("[A-z$.[:space:]]", "", ME17$TOTAL_RECEBIDO_2017)
ME17$TOTAL_RECEBIDO_2017 <- gsub("[,]", ".", ME17$TOTAL_RECEBIDO_2017)
ME17$TOTAL_RECEBIDO_2017 <- gsub("[-]", "0", ME17$TOTAL_RECEBIDO_2017)
ME17$TOTAL_RECEBIDO_2017 <- as.numeric(ME17$TOTAL_RECEBIDO_2017)

ME16$TOTAL_RECEBIDO_2016 <- gsub("[A-z$.[:space:]]", "", ME16$TOTAL_RECEBIDO_2016)
ME16$TOTAL_RECEBIDO_2016 <- gsub("[,]", ".", ME16$TOTAL_RECEBIDO_2016)
ME16$TOTAL_RECEBIDO_2016 <- gsub("[-]", "0", ME16$TOTAL_RECEBIDO_2016)
ME16$TOTAL_RECEBIDO_2016 <- as.numeric(ME16$TOTAL_RECEBIDO_2016)

ME16
ME17

# Verificando se os INEP_ESCOLA são unicos ou os nomes
length(unique(MaisEdu$NOME_ESCOLA))
length(unique(MaisEdu$INEP_ESCOLA))
dim(MaisEdu)

length(unique(ME16$INEP_ESCOLA))
dim(ME16)

length(unique(ME17$INEP_ESCOLA))
dim(ME17)


# Arrumando as outras tabelas

# vamos Tirar todas as colunas que não vamos precisar quando formos juntar as colunas, que serão repetidas
# colocando um tabela valores alunos e 
MEV16 <- ME16[,c(6,8,9)]
MEV17 <- ME17[,c(6,8,9)]
MEV14 <- MaisEdu[,c(5,7,8)]
MEV14
ME16T <- ME16[,c(2:7)]
ME17T <- ME17[,c(2:7)]
ME14T <- MaisEdu[,c(1:6)]
ME14T


# Colocando todos os nomes da cidades no mesmo formato, primeiro colocando tudo em Minuscula
# Municipios
ME14T$MUNICIPIO_ESCOLA<- str_to_upper(ME14T$MUNICIPIO_ESCOLA)
ME16T$MUNICIPIO_ESCOLA<- str_to_upper(ME16T$MUNICIPIO_ESCOLA)
ME17T$MUNICIPIO_ESCOLA<- str_to_upper(ME17T$MUNICIPIO_ESCOLA)
# Escolas
ME14T$NOME_ESCOLA<- str_to_upper(ME14T$NOME_ESCOLA)
ME16T$NOME_ESCOLA<- str_to_upper(ME16T$NOME_ESCOLA)
ME17T$NOME_ESCOLA<- str_to_upper(ME17T$NOME_ESCOLA)
# Agora removendo os Acentos
#Municipios
source("rm_accent.R")
ME14T$MUNICIPIO_ESCOLA <- rm_accent(ME14T$MUNICIPIO_ESCOLA)
ME16T$MUNICIPIO_ESCOLA <- rm_accent(ME16T$MUNICIPIO_ESCOLA)
ME17T$MUNICIPIO_ESCOLA <- rm_accent(ME17T$MUNICIPIO_ESCOLA)
# Escola
ME14T$NOME_ESCOLA<- rm_accent(ME14T$NOME_ESCOLA)
ME16T$NOME_ESCOLA<- rm_accent(ME16T$NOME_ESCOLA)
ME17T$NOME_ESCOLA<- rm_accent(ME17T$NOME_ESCOLA)

# criando um tibble, com todos os INEP_ESCOLA
MaisEduF <- ME14T[,1] %>% full_join(ME16T[,1], by = "INEP_ESCOLA")
MaisEduF <- MaisEduF %>% full_join(ME17T[,1], by = "INEP_ESCOLA")


# Agora vamos juntar as tabelas relacionando a colunas INEP_ESCOLA das tabelas
MaisEduF <- MaisEduF %>% left_join(MEV14, by = "INEP_ESCOLA")
MaisEduF <- MaisEduF %>% left_join(MEV16, by = "INEP_ESCOLA")
MaisEduF <- MaisEduF %>% left_join(MEV17, by = "INEP_ESCOLA")

# unindo todos os dados de escolas 
ME <-union(ME14T,ME16T)
ME <-union(ME,ME17T)
# olhando se existe duplicação do INEP_ESCOLA
ME[duplicated(ME$INEP_ESCOLA),]
ME<-ME[!duplicated(ME$INEP_ESCOLA),]
ME
MaisEduF <- MaisEduF %>% left_join(ME, by = "INEP_ESCOLA")
MaisEduF <-  as_tibble(MaisEduF)
MaisEduF
MaisEduF[duplicated(MaisEduF$INEP_ESCOLA),]
# outra forma de tirar as repetições é usar

ME %>% distinct(INEP_ESCOLA, .keep_all = TRUE)


summary(MaisEduF)

MaisEduF$LOCALIZACAO_ESCOLA <-as.factor(MaisEduF$LOCALIZACAO_ESCOLA)
summary(MaisEduF)
# outro modo de fazer
as_tibble(merge(MaisEdu,MEV16, all.x = TRUE, by = "INEP_ESCOLA"))

#Para vermos quais escolas não foram colocas na tabela
MaisEdu17 %>% anti_join(MaisEduF, by = "INEP_ESCOLA")
MaisEdu16 %>% anti_join(MaisEduF, by = "INEP_ESCOLA")
MaisEdu %>% anti_join(MaisEduF, by = "INEP_ESCOLA")

#estatistica
summary(MaisEduF)

# Repasses 
# Recebido
TR14 <- sum(MaisEduF$TOTAL_RECEBIDO_2014, na.rm = TRUE)
TR16 = sum(MaisEduF$TOTAL_RECEBIDO_2016, na.rm = TRUE)
TR17 = sum(MaisEduF$TOTAL_RECEBIDO_2017, na.rm = TRUE)


# ALUNADO
A14 = sum(MaisEduF$ALUNOS_2014, na.rm = TRUE)
A16 = sum(MaisEduF$ALUNOS_2016, na.rm = TRUE)
A17 <- sum(MaisEduF$ALUNOS_2017, na.rm = TRUE)

# tamanhos são diferentes
length(MaisEdu$TOTAL_RECEBIDO_2014) == length(MaisEdu$TOTAL_RECEBIDO_2014)
length(MaisEdu$TOTAL_RECEBIDO_2014) == length(ME16$ALUNOS_2061)
length(MaisEdu$TOTAL_RECEBIDO_2014) == length(ME17$ALUNOS_2017)
length(ME16$ALUNOS_2016) == length(ME17$ALUNOS_2017)


# fazendo a medias a mão
# ADESÃO
TR14/length(MaisEdu$ALUNOS_2014)
TR16/length(ME16$ALUNOS_2016)
TR17/length(ME17$ALUNOS_2017)

# ALUNADO
A14/length(MaisEdu$ALUNOS_2014)
A16/length(ME16$ALUNOS_2016)
A17/length(ME17$ALUNOS_2017)


#Moda
sort.default(table(MaisEdu$ALUNOS_2014), decreasing = TRUE)
sort.default(table(ME16$ALUNOS_2016), decreasing = TRUE)
sort.default(table(ME17$ALUNOS_2017), decreasing = TRUE)
#Graficos

# temos que mudar o tipo do LOCALIZACAO_ESCOLA para factor
str(MaisEduF)
MaisEduF$LOCALIZACAO_ESCOLA <- as.factor(MaisEduF$LOCALIZACAO_ESCOLA)

# Criando o dataframe
UF_ESCOLA <- as.data.frame(table(MaisEduF$UF_ESCOLA))

# Divindo em faixas os alunos, mas sem utilizar o calculo 
# é melhor usar o calculo ou deixar o proprio R utilizar para dividir os intervalos
summary(MaisEduF)
FA14<- MaisEdu$ALUNOS_2014
FA14 <- cut(FA14, breaks=c(0, 180, 360, 540,720,900,1080,1260,1440,1620,Inf), 
            labels = c("0-180","181-360","361-540","541-720","721-900","901-1080","1081-1260","1261-1440","1441-1620","1620 <"))
TA14<- table(FA14)
TA14

FA16<- ME16$ALUNOS_2016
FA16 <- cut(FA16, breaks=c(0, 180, 360, 540,720,900,1080,1260,1440,1620,Inf),
            labels = c("0-180","181-360","361-540","541-720","721-900","901-1080","1081-1260","1261-1440","1441-1620","1620 <"))
TA16<- table(FA16)
table(FA16)
TA16

FA17<- ME17$ALUNOS_2017
FA17 <- cut(FA17, breaks=c(0, 180, 360, 540,720,900,1080,1260,1440,1620,Inf),
            labels = c("0-180","181-360","361-540","541-720","721-900","901-1080","1081-1260","1261-1440","1441-1620","1620 <"))
TA17<- table(FA17)
table(FA17)
TA17

FR14<- cut(MaisEdu$TOTAL_RECEBIDO_2014, breaks = 10)
FR16<- cut(ME16$TOTAL_RECEBIDO_2016, breaks = 10)
FR17<- cut(ME17$TOTAL_RECEBIDO_2017, breaks = 10)
table(FR17)

#Lattice
#Histogramas
# Quantidades
histogram(FA14, type = "count", col = "Gray", main = "ALunos",  ylab = "Frequencia" , xlab = "Fatores"  )
histogram(FA16, type = "count", col = "Gray", main = "ALunos",  ylab = "Frequencia" , xlab = "Fatores"  )
histogram(FA17, type = "count", col = "Gray", main = "ALunos",  ylab = "Frequencia" , xlab = "Fatores"  )

# Porcentagem
histogram(FA14, col = "Gray", main = "ALunos",  ylab = "Porcentagem" , xlab = "Fatores"  )
histogram(FA16, col = "Gray", main = "ALunos",  ylab = "Porcentagem" , xlab = "Fatores"  )
histogram(FA17, col = "Gray", main = "ALunos",  ylab = "Porcentagem" , xlab = "Fatores"  )
# para explicar pq fica com problema 
summary(MaisEduF)

#Graficos de barras
ggplot(MaisEduF, aes(x = UF_ESCOLA, fill = UF_ESCOLA)) + geom_bar() +theme(legend.position= "none") + labs(title="UF Escolas" , x = "Estados" , y = "Quantidade") + theme(panel.grid.major = element_line(colour = "Black"))

graficoEG <-ggplot(MaisEduF, aes(x = ESFERA_GOVERNO, fill = ESFERA_GOVERNO)) + geom_bar()+ labs(title="Esfera Governo" ,x= "Esfera" , y = "Quantidade") + theme(panel.grid.major = element_line(colour = "Black"))
graficoEG
# Tirando o titulo
graficoEG + guides(fill=guide_legend(title=NULL))
# Trocando o nome da legenda
graficoEG + scale_fill_discrete(name="Esferas")

qplot(ESFERA_GOVERNO,data = MaisEduF,fill= ESFERA_GOVERNO, geom = "bar", xlab = "Esfera" , ylab = "Quantidade" , main = "Esfera Governo", ) + scale_fill_discrete(name="Esferas")

# Boxplot
ggplot(UF_ESCOLA) + geom_boxplot(aes(y = Freq),)+ labs(title="Alunos 2014" ,x= "Alunos" , y = "Quantidade")
#padrão
boxplot(UF_ESCOLA$Freq)
#lattice
bwplot(UF_ESCOLA$Freq,horizontal=TRUE, main = "Alunos 2014", ylab = "Quantidade")

#pie
pie(UF_ESCOLA$Freq ,labels=UF_ESCOLA$Var1, main = "UF Escolas" )


# Fazendo correlações
cor(MEV14$TOTAL_RECEBIDO_2014,MEV14$ALUNOS_2014)
cor(MEV16$TOTAL_RECEBIDO_2016,MEV16$ALUNOS_2016)
cor(MEV17$TOTAL_RECEBIDO_2017,MEV17$ALUNOS_2017)

#Fazendo uma Regressão e graficos
#2014
R_2014<-lm(TOTAL_RECEBIDO_2014~ALUNOS_2014,data=MEV14)
R_2014
summary(R_2014)
# Fazendo o grafico
ggplot(MEV14 , aes(x= TOTAL_RECEBIDO_2014, y= ALUNOS_2014)) + geom_point() + geom_smooth()+ labs(title="Grafico Recebido x Alunos 2014" , x = "Valor recebido" , y = "Alunos")+ theme(panel.grid.major = element_line(colour = "Black"))


#2016
R_2016<-lm(TOTAL_RECEBIDO_2016~ALUNOS_2016, data=MEV16)
R_2016
summary(R_2016)
# Fazendo o grafico
ggplot(data = MEV16,mapping = aes(x= TOTAL_RECEBIDO_2016 , y= ALUNOS_2016)) + geom_point() + geom_smooth(mapping = aes(x=TOTAL_RECEBIDO_2016, y=ALUNOS_2016)) + labs(title="Grafico Recebido x Alunos 2016" , x = "Valor recebido" , y = "Alunos")+ theme(panel.grid.major = element_line(colour = "Black"))

R_2017<-lm(TOTAL_RECEBIDO_2017~ALUNOS_2017,data=MEV17)
R_2017
summary(R_2017)
# Fazendo o grafico
ggplot(data=MEV17, aes(x=TOTAL_RECEBIDO_2017, y=ALUNOS_2017)) + geom_point() + geom_smooth(aes(x=TOTAL_RECEBIDO_2017, y=ALUNOS_2017)) + labs(title="Grafico Recebido x Alunos 2017" , x = "Valor recebido" , y = "Alunos")+ theme(panel.grid.major = element_line(colour = "Black"))


# Agora afunilando para somente o estado do Sul
MESUL <- MaisEduF %>% filter(UF_ESCOLA == "RS")
MERG <- MESUL %>% filter(MUNICIPIO_ESCOLA == "rio grande")
# mesmo dando o mesmo resultado, existem cidade com o mesmo nome no pais, mas existe uma lei que proibe mesmo nome no estado
MaisEduF[duplicated(MaisEduF$MUNICIPIO_ESCOLA),]
MaisEduF %>% filter(MUNICIPIO_ESCOLA == "rio grande")

MERG
MERG <- MERG[,c(12,2:11,1)]
view(MERG)
write.csv(MERG,"MaisEducação RG.csv")

# Machine Learning	

install.packages('rpart',dependencies=TRUE)
library(rpart)

dim(ME16)
summary(ME16)
amostra <- sample(2,length(ME16$ALUNOS_2016),replace=TRUE, prob=c(0.7,0.3))
MEtreino = ME16[amostra==1,]
MEteste = ME16[amostra==2,]
# modelo1

modelo = rpart(TOTAL_GOVERNO ~ ALUNOS_2016 + ESFERA_RECEBIDO_2016 +  UF_ESCOLA + LOCALIZACAO_ESCOLA ,data=MEtreino)

modelo

plot(modelo)
text(modelo, use.n=TRUE, all=TRUE, cex=.6)

predicao = predict(modelo, MEteste)
head(predicao)

comparacao = cbind(predicao,MEteste$TOTAL_RECEBIDO_2016,predicao - MEteste$TOTAL_RECEBIDO_2016 )

head(comparacao)

tabela = cbind(MEteste,predicao)

fix(tabela)
tabela['Resultado'] = ifelse(tabela$TOTAL_RECEBIDO_2016>=23343,"bad","good")
view(tabela)


table = table(tabela$Resultado)
taxaacerto = table[2] / sum(confusao)
taxaacerto
taxaerro = table[1] / sum(confusao)
taxaerro

# modelo 2

modelo2 = rpart(TOTAL_GOVERNO ~ ALUNOS_2016 + LOCALIZACAO_ESCOLA ,data=MEtreino)

modelo2

plot(modelo2)
text(modelo2, use.n=TRUE, all=TRUE, cex=.6)

predicao2 = predict(modelo2, MEteste)
head(predicao2)

comparacao2 = cbind(predicao2,MEteste$TOTAL_RECEBIDO_2016,predicao2 - MEteste$TOTAL_RECEBIDO_2016 )

head(comparacao2)

tabela2 = cbind(MEteste,predicao2)

fix(tabela2)
tabela['Resultado'] = ifelse(tabela$TOTAL_RECEBIDO_2016>=23343,"bad","good")
view(tabela2)

cor(MESUL[,c("ALUNOS_2014","TOTAL_RECEBIDO_2014")], use="complete")
table2 = table(tabela2$Resultado, tabela)
taxaacerto2 = table2[2] / sum(table2)
taxaacerto2
taxaerro2 = table2[1] / sum(table2)
taxaerro2



