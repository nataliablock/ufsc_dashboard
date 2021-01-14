###########################################################################################
#########  Dashboard de demonstração para o workshop do grupo RI e CTI/UFSC  ############# 
##########################################################################################

#Upload de pacotes
pacman::p_load(tidyverse, readxl, flexdashboard, plotly)

# Fixando diretório
setwd("~/Dropbox/repositories/ufsc_dashboard")


#####################   UPLOAD DE BANCOS   ##########################

###########  DADOS DE IMPORTAÇAO E EXPORTACAO  ###############

importacao<-read_excel("import_sc.xlsx")
exportacao<-read_excel("export_sc.xlsx")


###########  DADOS DE TURISMO  ###########

### Turismo - O banco está sujo, misturando valores totais de continente e específicos de país
#Preciso tirar a vírgula dos milhares

#banco de totais por local
tur_local<-read.csv("turistas_sc_local.csv")

##retirando espaços da coluna de total
tur_local$total<-gsub(" ","",tur_local$total)

##retirando as vírgulas
tur_local$total<-gsub(",","",tur_local$total)

##transformando - em NA
tur_local$total<-gsub("-",NA,tur_local$total)


#Estão faltando dados da França, Itália e Alemanha de 2016 em diante. Deve ter sido problema
#ao estruturar os dados copiando e colando. Vou completar esses dados utilizando os relatório
#já feitos. Vou criar um banco com dados faltando e vou integrá-lo ao banco maior.

local<-c("Alemanha", "Alemanha", "Alemanha", "Alemanha", "França", "França", "França", "França",
         "Itália", "Itália", "Itália", "Itália")

ano<-c(2016,2017,2018,2019,2016,2017,2018,2019,2016,2017,2018,2019)

total<-c(248,1281,362,413,112,176,259,180,411,476,512,530)

turis_falt<-data.frame(local,ano,total)

#incluindo no banco "turismo"
turismo<-rbind(tur_local,turis_falt)


##salvando esses novos bancos
write_csv(turismo, "turistas_sc_local_final.csv")

############   DADOS DE MIGRACAO   #################

##Baixei os dados referentes a data de registro

#Esses dados estão bastante confusos, variando de tipo de documento e conteúdo a cada ano.
#Para criar um banco mais uniforme e fácil de trabalhar farei os seguintes passos:
#1) farei o upload dos txt e depois dos csv salvando em listas separadas. 
#2) Depois juntarei as duas listas, selecionarei as variáveis desejadas e criarei uma variável de ano
#3) concatenarei todos os bancos da lista em um único banco

##### PASSO 1: Uploads

#alterando temporariamente o diretório para trabalhar com os dados salvos na pasta "migracao"
setwd("~/Dropbox/repositories/imp_exp_dashboard/migracao")

#upload dos documents txt
#salvando os nomes dos documentos em lista
files_txt<-list.files(pattern ='sincre_')

#fazendo upload txt de bancos e salvando em lista
mig_txt<-lapply(files_txt, read.table, sep=";", header=T)

#dando nome para esses bancos:
#criando vetor com os nomes
nomes_text<-c("mig2010", "mig2011", "mig2012", "mig2013", "mig2014", "mig2015", "mig2016")

#aplicando vetor para nomear os bancos dentro da lista
names(mig_txt)<-nomes_text

#upload dos documentos csv
#Para conseguir salvar os nomes separados dos txt (que também tinham csv no nome)
#eu alterei os nomes deles na pasta para inicir com "mig" para que eu possa usar 
#essa string como pattern para a coleta
#salvando os nomes dos documentos em lista
files_csv<-list.files(pattern='mig_')

#upload dos bancos csv em uma segunda lista
mig_csv<-lapply(files_csv, read.csv)

#alterando os nomes:
nomes_csv<-c("mig2017", "mig2018","mig2019")
names(mig_csv)<-nomes_csv


##### PASSO 2: concatenando listas, selecionando variáveis e criando variável de ano

#concatenando as duas listas
mig<-c(mig_txt,mig_csv)

#removendo objetos que não serão mais utilizados para limpar memória
rm(mig_csv, mig_txt, files_csv, files_txt, nomes_csv, nomes_text)

#Vou criar uma função selecionando as variáveis de interesse que estão presentes
#em todos os bancos e vou aplicá-la à lista de bancos

coleta_var<-function(dados){
  novo_banco<-dados%>%
    select(DATA_REG, CLASSIFICACAO, UFRES, PNASC_DESC, SEXO_DESCRICAO)
}

#Aplicando a função coleta_var à lista mig utilizando a função map do purrr
mig<-map(mig, coleta_var)

## criando variável de ano nos bancos de dados dentro das listas com loop for

#criando objeto com valor do primeiro ano das listas
ano<-2010

#fazendo o loop percorrendo a lista e criando a variável de ano nova
for (i in seq_along(mig)){
  mig[[i]][, dim(mig[[i]])[2] + 1] <- as.character(rep(ano,dim(mig[[i]])[1]))  
  ano = ano + 1
}

#alterando o nome da variável de ano
mig = lapply(mig, function(x) {colnames(x)[6] = "ano"; x})

##### PASSO 3: concatenando todos os bancos:

migracao<-bind_rows(mig)

#removendo objetos que não serão mais necessários
rm(mig, ano, i)

###Salvando o banco final como csv
write.csv(migracao, "migracao_completo.csv")

##############################################################################

############# TRATANDO DADOS E PLOTANDO GRAFICOS

#retornando ao diretório base
setwd("~/Dropbox/repositories/imp_exp_dashboard")

###### EXPORTAÇAO E IMPORTACAO

#fazendo o reshape dos bancos de wide para long
exp_long<-exportacao%>%
  gather(exportacao,valor, `2019 - Valor FOB (US$)`:`2010 - Valor FOB (US$)`, convert=T)

#criando variável de ano
exp_long$ano<-substr(exp_long$exportacao, start=1, stop=4)


#realizando as mesmas alterações no banco de importação

#fazendo o reshape dos bancos de wide para long
imp_long<-importacao%>%
  gather(importacao,valor, `2019 - Valor FOB (US$)`:`2010 - Valor FOB (US$)`, convert=T)

#criando variável de ano
imp_long$ano<-substr(imp_long$importacao, start=1, stop=4)


##deletando variáveis exportacao e importacao e criando uma chamada categoria
#em que dirá se a transação é importação ou exportação

#deletando variáveis
exp_long$exportacao<-NULL
imp_long$importacao<-NULL

#criando variáveis
exp_long$categoria<-"exportação"
imp_long$categoria<-"importação"

#convertendo valores para milhões
exp_long<-exp_long%>%
  mutate(valor=valor/1000000)

imp_long<-imp_long%>%
  mutate(valor=valor/1000000)
  
#arredondando para dois dígitos
exp_long$valor<-round(exp_long$valor, 2)

imp_long$valor<-round(imp_long$valor, 2)


## selecionando apenas os países de interesse para a análise

#vetor com países de interesse:
paises_exp_imp<-c("Alemanha", "China", "Estados Unidos", "França", "Itália", "Japão",
                  "Reino Unido")

exp_long<-exp_long%>%
  filter(País %in% paises_exp_imp)

imp_long<-imp_long%>%
  filter(País %in% paises_exp_imp)


#fazendo o spread do banco para conseguir plotar no plotly
#como esses são os bancos finais vou subscrever os bancos
#exportacao e importacao já que não serão mais usados
exportacao<-exp_long%>%
  spread(País,valor)

importacao<-imp_long%>%
  spread(País,valor)

#Estou tendo problema na hora da plotagem com o espaçamento entre "Reino Unido" e "Estados Unidos"
#vou precisar alterar o nome da variável para conseguir manipulá-la no plotly
names(exportacao)[6]<-"Estados_Unidos"
names(importacao)[6]<-"Estados_Unidos"
names(exportacao)[10]<-"Reino_Unido"
names(importacao)[10]<-"Reino_Unido"


#Criando gráfico com plotly  EXPORTACAO

exportacao_graf<-plot_ly(exportacao, x=~ano, y=~Alemanha, name="Alemanha",type='scatter', mode='lines',
                      linetype = ~categoria)
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~China,name='China', mode='lines')
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~Estados_Unidos,name='Estados Unidos', mode='lines')
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~França,name='França', mode='lines')
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~Itália,name='Itália', mode='lines')
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~Japão,name='Japão', mode='lines')
exportacao_graf<-exportacao_graf%>%
  add_trace(y=~Reino_Unido,name='Reino Unido', mode='lines')
exportacao_graf<-exportacao_graf%>% layout(xaxis=list(title=""),
                                           yaxis=list(title="milhões de US$"))


### Gráfico para IMPORTACOES

importacao_graf<-plot_ly(importacao, x=~ano, y=~Alemanha, name="Alemanha",type='scatter', mode='lines',
                         linetype = ~categoria)
importacao_graf<-importacao_graf%>%
  add_trace(y=~China,name='China', mode='lines')
importacao_graf<-importacao_graf%>%
  add_trace(y=~Estados_Unidos,name='Estados Unidos', mode='lines')
importacao_graf<-importacao_graf%>%
  add_trace(y=~França,name='França', mode='lines')
importacao_graf<-importacao_graf%>%
  add_trace(y=~Itália,name='Itália', mode='lines')
importacao_graf<-importacao_graf%>%
  add_trace(y=~Japão,name='Japão', mode='lines')
importacao_graf<-importacao_graf%>%
  add_trace(y=~Reino_Unido,name='Reino Unido', mode='lines')
importacao_graf<-importacao_graf%>% layout(xaxis=list(title=""),
                                           yaxis=list(title="milhões de US$"))


######### TURISMO

## FICOU FALTANDO O REINO UNIDO!!! Pode ter sido problema na montagem do banco. Vamos sem ele

#selecionando os casos de interesse

turismo<-turismo%>%
  filter(local %in% paises_exp_imp)


#transformando ano em fator
turismo$ano<-as.factor(turismo$ano)

#transformando total em numérico
turismo$total<-as.numeric(turismo$total)

### Farei um gráfico com total de entrada de turistas nos últimos 9 anos

turismo<-turismo%>%
  group_by(local)%>%
  summarise(total=sum(total))

#criando o gráfico com plotly

turismo_graf<-plot_ly(turismo, x= ~local, y=~ total, type='bar')
turismo_graf<-turismo_graf %>%layout(xaxis=list(title=""), yaxis=list(title="total de turistas entre 2010 e 2019"))



######## MIGRACAO

#selecionando apenas os países de interesse

#criando vetor de países
pais_mig<-c("ALEMANHA", "REPUBLICA POPULAR DA CHINA", "ESTADOS UNIDOS DA AMERICA",
            "FRANCA", "ITALIA", "JAPAO", "REINO UNIDO")

migracao<-migracao%>%
  filter(PNASC_DESC %in% pais_mig)

#transformando variável de sexo em fator
migracao$SEXO_DESCRICAO<-as.factor(migracao$SEXO_DESCRICAO)

##agora preciso:
# 1) selecionar apenas SC
# 2) agrupar por país, sexo
# 3) calcular a média

migracao<-migracao%>%
  select(UFRES, PNASC_DESC, SEXO_DESCRICAO,ano)%>%
  filter(UFRES=="SC")%>%
  group_by(PNASC_DESC, SEXO_DESCRICAO)%>%
  count()

#alterando a escrita dos nomes dos países
migracao<-migracao%>%
  mutate(paises=case_when(PNASC_DESC=="REPUBLICA POPULAR DA CHINA" ~ "China",
            PNASC_DESC=="REINO UNIDO" ~ "Reino Unido",
            PNASC_DESC=="JAPAO" ~ "Japão",
            PNASC_DESC=="ITALIA" ~ "Itália",
            PNASC_DESC=="FRANCA" ~ "França",
            PNASC_DESC=="ESTADOS UNIDOS DA AMERICA" ~ "Estados Unidos",
            PNASC_DESC=="ALEMANHA" ~ "Alemanha"))

#deletando variável com nome de países anterior
migracao$PNASC_DESC<-NULL

##criando banco spread
migracao<-migracao%>%
  spread(SEXO_DESCRICAO, n)


### Gráfico

migracao_graf<-plot_ly(migracao, color= I("gray80"))
migracao_graf<-migracao_graf %>% add_segments(x= ~Feminino, xend= ~Masculino, 
                                              y= ~paises, yend= ~paises, showlegend=FALSE)
migracao_graf<-migracao_graf%>% add_markers(x= ~Feminino, y= ~paises, name="Mulheres",
                                            color=I("red"))
migracao_graf<-migracao_graf%>% add_markers(x= ~Masculino, y= ~paises, name="Homens",
                                            color= I("blue"))
migracao_graf<-migracao_graf%>% layout(xaxis = list(title = "total de migrantes entre 2010 e 2019"),
                                                    yaxis= list(title =""),
  margin = list(l = 65))
  



### para renderizar o dashboard:
rmarkdown::render("index.Rmd", output_format = "flexdashboard::flex_dashboard")  

