#Configuracoes
library(geobr)
library(sf)
library(units)
library(crul)
library(escale)
library(tidyverse)
library(readxl)

# Carregar tabelas "IAP e IAPM  de 2013- 2019 - CEPERJ
#IAP 2009
IAP2009 <- read.csv2("dados/csv/IAP-2009.csv", sep = ";", 
                     stringsAsFactors = FALSE)
IAP2009$IAP<-replace_na(IAP2009$IAP,0)
IAP2009$IAPM<-replace_na(IAP2009$IAPM,0)
Ano<-rep(2009,92)
IAP2009<-cbind(Ano,IAP2009)
IAP2009$Municípios<-stringi::stri_trans_general(IAP2009$Municípios , "Latin-ASCII")
IAP2009$Municípios<-str_to_lower(IAP2009$Municípios)

#IAP 2010
IAP2010 <- read.csv2("dados/csv/IAP-2010.csv", sep = ";", 
                     stringsAsFactors = FALSE)
IAP2010<-IAP2010[-c(93:232),]
Ano<-rep(2010,92)
IAP2010<-cbind(Ano,IAP2010)
IAP2010$Municípios<-stringi::stri_trans_general(IAP2010$Municípios , "Latin-ASCII")
IAP2010$Municípios<-str_to_lower(IAP2010$Municípios)

#IAP 2011
IAP2011 <- read.csv2("dados/csv/IAP-2011.csv", sep = ";", 
                     stringsAsFactors = FALSE)
IAP2011<-IAP2011[-c(93:95),]
Ano<-rep(2011,92)
IAP2011<-cbind(Ano,IAP2011)
IAP2011$Municípios<-stringi::stri_trans_general(IAP2011$Municípios , "Latin-ASCII")
IAP2011$Municípios<-str_to_lower(IAP2011$Municípios)

#IAP 2012
IAP2012 <- read.csv2("dados/csv/IAP-2012.csv", sep = ";", 
                     stringsAsFactors = FALSE)
IAP2012<-IAP2012[-c(93:95),]
Ano<-rep(2012,92)
IAP2012<-cbind(Ano,IAP2012)
IAP2012$Municípios<-stringi::stri_trans_general(IAP2012$Municípios , "Latin-ASCII")
IAP2012$Municípios<-str_to_lower(IAP2012$Municípios)

#IAP 2013
IAP2013 <- read.csv2("dados/csv/IAP-2013.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2013<-rename(IAP2013,Municípios=municipio)
IAP2013$Municípios<-stringi::stri_trans_general(IAP2013$Municípios , "Latin-ASCII")
IAP2013$Municípios<-str_to_lower(IAP2013$Municípios)

#IAP 2014
IAP2014 <- read.csv2("dados/csv/IAP-2014.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2014$Municípios<-stringi::stri_trans_general(IAP2014$Municípios , "Latin-ASCII")
IAP2014$Municípios<-str_to_lower(IAP2014$Municípios)

#IAP 2015
IAP2015 <- read.csv2("dados/csv/IAP-2015.csv", sep = ";", 
                     stringsAsFactors = FALSE)
IAP2015<-IAP2015[-c(93:95),]

IAP2015$Municípios<-stringi::stri_trans_general(IAP2015$Municípios , "Latin-ASCII")
IAP2015$Municípios<-str_to_lower(IAP2015$Municípios)

#IAP 2016
IAP2016 <- read.csv2("dados/csv/IAP-2016.csv", sep = ";", 
                     stringsAsFactors = FALSE)
colnames(IAP2016)[1]<-c("Municípios")

IAP2016$Municípios<-stringi::stri_trans_general(IAP2016$Municípios , "Latin-ASCII")
IAP2016$Municípios<-str_to_lower(IAP2016$Municípios)

BaseIAP<-bind_rows(IAP2009,IAP2010,IAP2011,IAP2012,IAP2013,IAP2014,IAP2015,IAP2016,id=NULL)
#variavel IAPM possui muitos zeros, talvez deva transformar em log, adcionando 1 aos valores zero

# Serie Historica IFDM 2009 a 2016
IFDM <- read.csv2("dados/csv/IFDM.csv", sep = ";", 
                  stringsAsFactors = FALSE)%>%
  select(1:4,13,15,17,19,21,23,25,27)%>%
  filter(UF=="RJ")%>%
  select(1,4:12)%>%
  rename(Cod.IBGE=Código,Municípios=Município)%>%
  gather(ano,ifdm,-Cod.IBGE,-Municípios)

IFDM[,4] <- gsub("[,]", ".",IFDM[,4])
IFDM$ano<-str_sub(string = IFDM$ano, start = 2)
IFDM$ano<-as.numeric(IFDM$ano) 
IFDM$ifdm<-as.numeric(IFDM$ifdm) 

# IFDM por componente :saude, educacao, renda e emprego
SAUDE <- read_excel("dados/csv/IFDMSaude.xlsx")
SAUDE<-SAUDE[c(3177:3268),c(1,4,13,15,17,19,21,23,25,27)]
colnames(SAUDE)<-c("CODMUNIC","MUNIC","2009","2010","2011","2012","2013","2014","2015","2016")
saudepainel<-gather(SAUDE,ano,saude,-CODMUNIC,-MUNIC)
saudepainel$ano<-as.numeric(saudepainel$ano)
saudepainel$saude<-as.numeric(saudepainel$saude)
saudepainel$MUNIC<-stringi::stri_trans_general(saudepainel$MUNIC , "Latin-ASCII")
saudepainel$MUNIC<-str_to_lower(saudepainel$MUNIC)
saudepainel<-saudepainel[,-1]

educacao <- read_excel("dados/csv/IFDMEducacao.xlsx")
educacao<-educacao[c(3177:3268),c(1,4,13,15,17,19,21,23,25,27)]
colnames(educacao)<-c("CODMUNIC","MUNIC","2009","2010","2011","2012","2013","2014","2015","2016")
educacaopainel<-gather(educacao,ano,Educacao,-CODMUNIC,-MUNIC)
educacaopainel$ano<-as.numeric(educacaopainel$ano)
educacaopainel$Educacao<-as.numeric(educacaopainel$Educacao)
educacaopainel$MUNIC<-stringi::stri_trans_general(educacaopainel$MUNIC , "Latin-ASCII")
educacaopainel$MUNIC<-str_to_lower(educacaopainel$MUNIC)
educacaopainel<-educacaopainel[,-1]

RENDA<- read_excel("dados/csv/IFDMEmpregoRenda.xlsx")
RENDA<-RENDA[c(3177:3268),c(1,4,13,15,17,19,21,23,25,27)]
colnames(RENDA)<-c("CODMUNIC","MUNIC","2009","2010","2011","2012","2013","2014","2015","2016")
rendapainel<-gather(RENDA,ano,Renda.Emp,-CODMUNIC,-MUNIC)
rendapainel$ano<-as.numeric(rendapainel$ano)
rendapainel$Renda.Emp<-as.numeric(rendapainel$Renda.Emp)
rendapainel$MUNIC<-stringi::stri_trans_general(rendapainel$MUNIC , "Latin-ASCII")
rendapainel$MUNIC<-str_to_lower(rendapainel$MUNIC)
rendapainel<-rendapainel[,-1]

#Criando variaveis categoricas
saudepainel$factor_saude <- cut(saudepainel$saude, breaks = c(0,0.4,0.6,0.8,1), labels=c("Baixo","Regular",
                                                                                      "Moderado","Alto"))

rendapainel$factor_renda <- cut(rendapainel$Renda.Emp, breaks = c(0,0.4,0.6,0.8,1), labels=c("Baixo","Regular",
                                                                                         "Moderado","Alto"))

educacaopainel$factor_educacao <- cut(educacaopainel$Educacao, breaks = c(0,0.4,0.6,0.8,1), labels=c("Baixo","Regular",
                                                                                         "Moderado","Alto"))


#Fonte: Dados SICONFI-FINBRA
#https://www.tesourotransparente.gov.br/publicacoes/finbra-dados-contabeis-dos-municipios-1989-a-2012/2012/26
#bases despesas  pagas totais
#2009
desp2009<- read.csv2("dados/csv/Despesa Paga 2009.csv", sep = ";", 
                      stringsAsFactors = FALSE)
desp2009<-desp2009[,-c(1,2,3,5)]
colnames(desp2009)<-c("Municípios","Despesa.Total")
ano<-rep(2009,91)
desp2009<-cbind(desp2009,ano)

#2010
desp2010<- read.csv2("dados/csv/Despesa Paga 2010.csv", sep = ";", 
                     stringsAsFactors = FALSE)
desp2010<-desp2010[,-c(1,2,3,5)]
colnames(desp2010)<-c("Municípios","Despesa.Total")
ano<-rep(2010,91)
desp2010<-cbind(desp2010,ano)

#2011
desp2011<- read.csv2("dados/csv/Despesa Paga 2011.csv", sep = ";", 
                     stringsAsFactors = FALSE)
desp2011<-desp2011[,-c(1,2,3,5)]
colnames(desp2011)<-c("Municípios","Despesa.Total")
ano<-rep(2010,89)
desp2011<-cbind(desp2011,ano)

#2012
desp2012<- read.csv2("dados/csv/Despesa Paga 2012.csv", sep = ";", 
                     stringsAsFactors = FALSE)
desp2012<-desp2012[,-c(1,2,3,5)]
colnames(desp2012)<-c("Municípios","Despesa.Total")
ano<-rep(2012,85)
desp2012<-cbind(desp2012,ano)

#2013
desp2013 <- read.csv2("dados/csv/despesa2013.csv", sep = ";", 
                      stringsAsFactors = FALSE)%>%
            filter(Coluna=="Despesas Pagas")%>%
            filter(Conta=="Despesas (Exceto Intra-Orçamentárias)"|Conta=="Despesas (Intra-Orçamentárias)")

desp2013<-aggregate(desp2013$Valor, by=list(desp2013$Instituição), FUN=sum, na.rm=TRUE) 

desp2013$Group.1 <-str_sub(string = desp2013$Group.1, start =25, end= -6)
colnames(desp2013)<-c("Municípios","Despesa.Total")
ano<-rep(2013,90)
desp2013<-cbind(desp2013,ano)

#2014
desp2014 <- read.csv2("dados/csv/despesa2014.csv", sep = ";", 
                      stringsAsFactors = FALSE)%>%
  filter(Coluna=="Despesas Pagas")%>%
  filter(Conta=="Despesas (Exceto Intraorçamentárias)"|Conta=="Despesas (Intraorçamentárias)")

desp2014<-aggregate(desp2014$Valor, by=list(desp2014$Instituição), FUN=sum, na.rm=TRUE) 

desp2014$Group.1 <-str_sub(string = desp2014$Group.1, start =25, end= -6)
colnames(desp2014)<-c("Municípios","Despesa.Total")
ano<-rep(2014,85)
desp2014<-cbind(desp2014,ano)

#2015
desp2015 <- read.csv2("dados/csv/despesa2015.csv", sep = ";", 
                      stringsAsFactors = FALSE)%>%
  filter(Coluna=="Despesas Pagas")%>%
  filter(Conta=="Despesas (Exceto Intraorçamentárias)"|Conta=="Despesas (Intraorçamentárias)")

desp2015<-aggregate(desp2015$Valor, by=list(desp2015$Instituição), FUN=sum, na.rm=TRUE) 

desp2015$Group.1 <-str_sub(string = desp2015$Group.1, start =25, end= -6)
colnames(desp2015)<-c("Municípios","Despesa.Total")
ano<-rep(2015,90)
desp2015<-cbind(desp2015,ano)

#2016
desp2016 <- read.csv2("dados/csv/despesa2016.csv", sep = ";", 
                      stringsAsFactors = FALSE)%>%
  filter(Coluna=="Despesas Pagas")%>%
  filter(Conta=="Despesas (Exceto Intraorçamentárias)"|Conta=="Despesas (Intraorçamentárias)")

desp2016<-aggregate(desp2016$Valor, by=list(desp2016$Instituição), FUN=sum, na.rm=TRUE) 

desp2016$Group.1 <-str_sub(string = desp2016$Group.1, start =25, end= -6)
colnames(desp2016)<-c("Municípios","Despesa.Total")
ano<-rep(2016,89)
desp2016<-cbind(desp2016,ano)

Despesas<-bind_rows(desp2009,desp2010,desp2011,desp2012,desp2013,desp2014,desp2015,desp2016,id=NULL)

# Despesas empenhadas de gestao ambiental
Gest.amb<-read.csv2("dados/csv/Gest.Ambiental.csv", sep = ";", 
               stringsAsFactors = FALSE)
colnames(Gest.amb)<-c("Municípios","2009","2010","2011","2012","2013","2014","2015","2016")
Gest.amb$Municípios<-stringi::stri_trans_general(Gest.amb$Municípios, "Latin-ASCII")
Gest.amb$Municípios<-str_to_lower(Gest.amb$Municípios)
Gambientalpainel<-gather(Gest.amb,ano,Gest.ambiental,-Municípios)

#Serie Populacao dos municipios
pop<-read.csv2("dados/csv/populacao.csv", sep = ";", 
               stringsAsFactors = FALSE)
pop<-pop[-c(94:99),]
colnames(pop)<-c("Cod.IBGE","Municípios","2009","2010","2011","2012","2013","2014","2015","2016")

pop$`2009`<-as.numeric(pop$`2009`)
pop$`2010`<-as.numeric(pop$`2010`)
pop$`2011`<-as.numeric(pop$`2011`)
pop$`2012`<-as.numeric(pop$`2012`)
pop$`2013`<-as.numeric(pop$`2013`)
pop$`2014`<-as.numeric(pop$`2014`)
pop$`2015`<-as.numeric(pop$`2015`)
pop$`2016`<-as.numeric(pop$`2016`)
is.na(pop)

pop<-gather(pop,ano,pop,-Cod.IBGE,-Municípios)

#Area municipio
area<-read.csv2("dados/csv/Area.csv", sep = ";", 
                stringsAsFactors = FALSE)
ano<-rep(2009,92)
area2009<-cbind(area,ano)

ano<-rep(2010,92)
area2010<-cbind(area,ano)

ano<-rep(2011,92)
area2011<-cbind(area,ano)

ano<-rep(2012,92)
area2012<-cbind(area,ano)

ano<-rep(2013,92)
area2013<-cbind(area,ano)

ano<-rep(2014,92)
area2014<-cbind(area,ano)

ano<-rep(2015,92)
area2015<-cbind(area,ano)

ano<-rep(2016,92)
area2016<-cbind(area,ano)

area<-bind_rows(area2009,area2010,area2011,area2012,area2013,area2014,area2015,area2016,id=NULL)

#Removendo acentos da coluna municipios
library(stringi)
Despesas$Municípios<-stringi::stri_trans_general(Despesas$Municípios, "Latin-ASCII")
pop$Municípios<-stringi::stri_trans_general(pop$Municípios, "Latin-ASCII")
area$Município<-stringi::stri_trans_general(area$Município, "Latin-ASCII")
IFDM$Municípios<-stringi::stri_trans_general(IFDM$Municípios, "Latin-ASCII")

area$Município<-str_to_lower(area$Município)
Despesas$Municípios<-str_to_lower(Despesas$Municípios)
IFDM$Municípios<-str_to_lower(IFDM$Municípios)
pop$Municípios<-str_to_lower(pop$Municípios)

pop<-filter(pop,Cod.IBGE!="999999")
colnames(area)[2]<-c("Municípios")
area$ano<-as.numeric(area$ano)
pop$ano<-as.numeric(pop$ano)
colnames(BaseIAP)[1]<-c("ano")
IFDM<-IFDM[,-1]

basefinal<-full_join(area,pop,by=c("Municípios","ano"))
basefinal<-basefinal[,-c(1,5)]
basefinal<-full_join(basefinal,IFDM,by=c("Municípios","ano"))
basefinal<-full_join(basefinal,BaseIAP,by=c("Municípios","ano"))

#Corrigindo valores NA e repetidos na base
basefinal<-basefinal[-c(737,738,739,740),]
basefinal[53,6]<-10.74
basefinal[53,7]<-0
basefinal[697,6]<-38.2
basefinal[697,7]<-0.001
basefinal[731,6]<-0.12
basefinal[731,7]<-0
basefinal[734,6]<-0.12
basefinal[734,7]<-0

colnames(basefinal)[1]<-c("MUNIC")
colnames(Despesas)[1]<-c("MUNIC")
Despesas[c(52,143,232,317),1]<-"paraty"
Despesas[c(86,177,266,351),1]<-"trajano de moraes"

basefinal<-left_join(Despesas,basefinal,by=c("MUNIC","ano"))
is.na(basefinal)
class(basefinal)
summary(basefinal)

basefinal<-left_join(basefinal,educacaopainel,by=c("MUNIC","ano"))
basefinal<-left_join(basefinal,rendapainel,by=c("MUNIC","ano"))
basefinal<-left_join(basefinal,saudepainel,by=c("MUNIC","ano"))

#criando variaveis
# Densidade Populacional
basefinal$den.pop<-basefinal$pop/basefinal$Area

basefinal$desp.pc<-basefinal$Despesa.Total/basefinal$Area
