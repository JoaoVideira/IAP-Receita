# CONFIGURAÇÔES -----------------------------------------------------------
library(tidyverse)
library(AER)
library(stargazer)
library(dlookr)
library(plm)

# DADOS -------------------------------------------------------------------

dir.create("dados/csv")

# Carregar tabelas "Receitas"  de 2013- 2019 - SICONFI
#Fonte: https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf

#Receita 2013
receita2013 <- read.csv2("dados/csv/Receita-2013.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2013$Instituição<-str_sub(string = receita2013$Instituição, start =25, end= -6)
receita2013<-filter(receita2013,receita2013$Coluna!="Deduções da Receita")
receita2013$Cod.IBGE<-as.character(receita2013$Cod.IBGE)
receita2013<-select(receita2013,-c(3,5))
receita2013<-spread(receita2013,Conta,Valor)
receita2013<-rename(receita2013,municipio=Instituição)
receita2013<-select(receita2013,c(1:4,9,10,15,69,70,73:78,88:90))
receita2013<-select(receita2013,-c(5,18))
receita2013<-mutate_all(receita2013,replace_na,0)

receita2013<-mutate(receita2013,royaltiepetroleo=receita2013$`1.7.2.1.22.30.00 - Cota-parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89`+receita2013$`1.7.2.1.22.40.00 - Cota-Parte Royalties pelo Excedente da Produção do Petróleo - Lei nº 9.478/97 artigo 49 I e II`+receita2013$`1.7.2.1.22.50.00 - Cota-Parte Royalties pela Participação Especial - Lei nº 9.478/97 artigo 50`+receita2013$`1.7.2.1.22.70.00 - Cota-Parte do Fundo Especial do Petróleo ¿ FEP` )
receita2013<-select(receita2013,-c(11:14))
receita2013<-mutate(receita2013,IPTUpercapita=receita2013$`1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU`/receita2013$População,
                    ISSpercapita=receita2013$`1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN`/receita2013$População,
                    CPFPMpercapita=receita2013$`1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM`/receita2013$População,
                    CPITRpercapita=receita2013$`1.7.2.1.01.05.00 - Cota-Parte do Imposto Sobre a Propriedade Territorial Rural ¿ ITR`/receita2013$População,
                    CPRHpercapita=receita2013$`1.7.2.1.22.11.00 - Cota-parte da Compensação Financeira de Recursos Hídricos`/receita2013$População,
                    CPRMpercapita=receita2013$`1.7.2.1.22.20.00 - Cota-parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2013$População,
                    CPICMSpercapita=receita2013$`1.7.2.2.01.01.00 - Cota-Parte do ICMS`/receita2013$População,
                    CPIPVApercapita=receita2013$`1.7.2.2.01.02.00 - Cota-Parte do IPVA`/receita2013$População,
                    Rpetroleopercapita=receita2013$royaltiepetroleo/receita2013$População)%>%
  select(-c(5:13))


#Receita 2014
receita2014 <- read.csv2("dados/csv/Receita-2014.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2014$Instituição<-str_sub(string = receita2014$Instituição, start =25, end= -6)
receita2014<-filter(receita2014,receita2014$Coluna=="Receitas Brutas Realizadas")
receita2014$Cod.IBGE<-as.character(receita2014$Cod.IBGE)
receita2014<-select(receita2014,-c(3,5))
receita2014<-spread(receita2014,Conta,Valor)
receita2014<-rename(receita2014,municipio=Instituição)
receita2014<-select(receita2014,c(1:4,10,21,79:80,84:89,99:100))

receita2014<-mutate(receita2014,royaltiepetroleo=receita2014$`1.7.2.1.22.30.00 - Cota-Parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89` +receita2014$`1.7.2.1.22.40.00 - Cota-Parte Royalties pelo Excedente da Produção do Petróleo - Lei nº 9.478/97 artigo 49 I e II` +receita2014$`1.7.2.1.22.50.00 - Cota-Parte Royalties pela Participação Especial - Lei nº 9.478/97 artigo 50` +receita2014$`1.7.2.1.22.70.00 - Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2014<-select(receita2014,-c(11:14))
receita2014<-mutate_all(receita2014,replace_na,0)

receita2014<-mutate(receita2014,IPTUpercapita=receita2014$`1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU`/receita2014$População,
                    ISSpercapita=receita2014$`1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN`/receita2014$População,
                    CPFPMpercapita=receita2014$`1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM`/receita2014$População,
                    CPITRpercapita=receita2014$`1.7.2.1.01.05.00 - Cota-Parte do Imposto Sobre a Propriedade Territorial Rural ¿ ITR`/receita2014$População,
                    CPRHpercapita=receita2014$`1.7.2.1.22.11.00 - Cota-Parte da Compensação Financeira de Recursos Hídricos`/receita2014$População,
                    CPRMpercapita=receita2014$`1.7.2.1.22.20.00 - Cota-Parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2014$População,
                    CPICMSpercapita=receita2014$`1.7.2.2.01.01.00 - Cota-Parte do ICMS`/receita2014$População,
                    CPIPVApercapita=receita2014$`1.7.2.2.01.02.00 - Cota-Parte do IPVA`/receita2014$População,
                    Rpetroleopercapita=receita2014$royaltiepetroleo/receita2014$População)%>%
  select(-c(5:13))

#Receita 2015
receita2015 <- read.csv2("dados/csv/Receita-2015.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2015$Instituição<-str_sub(string = receita2015$Instituição, start =25, end= -6)
receita2015<-filter(receita2015,receita2015$Coluna=="Receitas Brutas Realizadas")
receita2015$Cod.IBGE<-as.character(receita2015$Cod.IBGE)
receita2015<-select(receita2015,-c(3,5))
receita2015<-spread(receita2015,Conta,Valor)
receita2015<-rename(receita2015,municipio=Instituição)
receita2015<-select(receita2015,c(1:4,10,25,99,100,101,102,108:113,123,124))

receita2015<-mutate_all(receita2015,replace_na,0)
receita2015<-mutate(receita2015,royaltiepetroleo=receita2015$`1.7.2.1.22.30.00 - Cota-Parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89` +receita2015$`1.7.2.1.22.40.00 - Cota-Parte Royalties pelo Excedente da Produção do Petróleo - Lei nº 9.478/97 artigo 49 I e II` +receita2015$`1.7.2.1.22.50.00 - Cota-Parte Royalties pela Participação Especial - Lei nº 9.478/97 artigo 50` +receita2015$`1.7.2.1.22.70.00 - Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2015<-select(receita2015,-c(13:16))
receita2015<-mutate(receita2015,FPM=receita2015$`1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM`+receita2015$`1.7.2.1.01.03.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota Anual`+receita2015$`1.7.2.1.01.04.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho (67)(I)`)
receita2015<-select(receita2015,-c(7:9))

receita2015<-mutate(receita2015,IPTUpercapita=receita2015$`1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU`/receita2015$População,
                    ISSpercapita=receita2015$`1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN`/receita2015$População,
                    CPFPMpercapita=receita2015$FPM/receita2015$População,
                    CPITRpercapita=receita2015$`1.7.2.1.01.05.00 - Cota-Parte do Imposto Sobre a Propriedade Territorial Rural ¿ ITR`/receita2015$População,
                    CPRHpercapita=receita2015$`1.7.2.1.22.11.00 - Cota-Parte da Compensação Financeira de Recursos Hídricos`/receita2015$População,
                    CPRMpercapita=receita2015$`1.7.2.1.22.20.00 - Cota-Parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2015$População,
                    CPICMSpercapita=receita2015$`1.7.2.2.01.01.00 - Cota-Parte do ICMS`/receita2015$População,
                    CPIPVApercapita=receita2015$`1.7.2.2.01.02.00 - Cota-Parte do IPVA`/receita2015$População,
                    Rpetroleopercapita=receita2015$royaltiepetroleo/receita2015$População)%>%
  select(-c(5:13))
                    
#Receita 2016
receita2016 <- read.csv2("dados/csv/Receita-2016.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2016$Instituição<-str_sub(string = receita2016$Instituição, start =25, end= -6)
receita2016<-filter(receita2016,receita2016$Coluna=="Receitas Brutas Realizadas")
receita2016$Cod.IBGE<-as.character(receita2016$Cod.IBGE)
receita2016<-select(receita2016,-c(3,5))
receita2016<-spread(receita2016,Conta,Valor)
receita2016<-rename(receita2016,municipio=Instituição)
receita2016<-select(receita2016,c(1:4,10,21,107:110,113:118,128,129))

receita2016<-mutate_all(receita2016,replace_na,0)
receita2016<-mutate(receita2016,royaltiepetroleo=receita2016$`1.7.2.1.22.30.00 - Cota-Parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89` +receita2016$`1.7.2.1.22.40.00 - Cota-Parte Royalties pelo Excedente da Produção do Petróleo - Lei nº 9.478/97 artigo 49 I e II` +receita2016$`1.7.2.1.22.50.00 - Cota-Parte Royalties pela Participação Especial - Lei nº 9.478/97 artigo 50` +receita2016$`1.7.2.1.22.70.00 - Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2016<-select(receita2016,-c(13:16))
receita2016<-mutate(receita2016,FPM=receita2016$`1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM`+receita2016$`1.7.2.1.01.03.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota Anual`+receita2016$`1.7.2.1.01.04.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho (67)(I)`)
receita2015<-select(receita2015,-c(7:9))

receita2016<-mutate(receita2016,IPTUpercapita=receita2016$`1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU`/receita2016$População,
                    ISSpercapita=receita2016$`1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN`/receita2016$População,
                    CPFPMpercapita=receita2016$FPM/receita2016$População,
                    CPITRpercapita=receita2016$`1.7.2.1.01.05.00 - Cota-Parte do Imposto Sobre a Propriedade Territorial Rural ¿ ITR`/receita2016$População,
                    CPRHpercapita=receita2016$`1.7.2.1.22.11.00 - Cota-Parte da Compensação Financeira de Recursos Hídricos`/receita2016$População,
                    CPRMpercapita=receita2016$`1.7.2.1.22.20.00 - Cota-Parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2016$População,
                    CPICMSpercapita=receita2016$`1.7.2.2.01.01.00 - Cota-Parte do ICMS`/receita2016$População,
                    CPIPVApercapita=receita2016$`1.7.2.2.01.02.00 - Cota-Parte do IPVA`/receita2016$População,
                    Rpetroleopercapita=receita2016$royaltiepetroleo/receita2016$População)%>%
 select(-c(5:13))
receita2016<-select(receita2016,-c(5:7))

#Receita 2017
receita2017 <- read.csv2("dados/csv/Receita-2017.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2017$Instituição<-str_sub(string = receita2017$Instituição, start =25, end= -6)
receita2017<-filter(receita2017,receita2017$Coluna=="Receitas Brutas Realizadas")
receita2017$Cod.IBGE<-as.character(receita2017$Cod.IBGE)
receita2017<-select(receita2017,-c(3,5))
receita2017<-spread(receita2017,Conta,Valor)
receita2017<-rename(receita2017,municipio=Instituição)
receita2017<-select(receita2017,c(1:4,10,21,105:108,111:116,126,127))

receita2017<-mutate_all(receita2017,replace_na,0)
receita2017<-mutate(receita2017,royaltiepetroleo=receita2017$`1.7.2.1.22.30.00 - Cota-Parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89` +receita2017$`1.7.2.1.22.40.00 - Cota-Parte Royalties pelo Excedente da Produção do Petróleo - Lei nº 9.478/97 artigo 49 I e II` +receita2017$`1.7.2.1.22.50.00 - Cota-Parte Royalties pela Participação Especial - Lei nº 9.478/97 artigo 50` +receita2017$`1.7.2.1.22.70.00 - Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2017<-select(receita2017,-c(13:16))
receita2017<-mutate(receita2017,FPM=receita2017$`1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM`+receita2017$`1.7.2.1.01.03.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota Anual`+receita2017$`1.7.2.1.01.04.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho (67)(I)`)
receita2017<-select(receita2017,-c(7:9))

receita2017<-mutate(receita2017,IPTUpercapita=receita2017$`1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU`/receita2017$População,
                    ISSpercapita=receita2017$`1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN`/receita2017$População,
                    CPFPMpercapita=receita2017$FPM/receita2017$População,
                    CPITRpercapita=receita2017$`1.7.2.1.01.05.00 - Cota-Parte do Imposto Sobre a Propriedade Territorial Rural ¿ ITR`/receita2017$População,
                    CPRHpercapita=receita2017$`1.7.2.1.22.11.00 - Cota-Parte da Compensação Financeira de Recursos Hídricos`/receita2017$População,
                    CPRMpercapita=receita2017$`1.7.2.1.22.20.00 - Cota-Parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2017$População,
                    CPICMSpercapita=receita2017$`1.7.2.2.01.01.00 - Cota-Parte do ICMS`/receita2017$População,
                    CPIPVApercapita=receita2017$`1.7.2.2.01.02.00 - Cota-Parte do IPVA`/receita2017$População,
                    Rpetroleopercapita=receita2017$royaltiepetroleo/receita2017$População)%>%
  select(-c(5:13))

#Receita 2018
receita2018 <- read.csv2("dados/csv/Receita-2018.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2018$Instituição<-str_sub(string = receita2018$Instituição, start =25, end= -6)
receita2018<-filter(receita2018,receita2018$Coluna=="Receitas Brutas Realizadas")
receita2018$Cod.IBGE<-as.character(receita2018$Cod.IBGE)
receita2018<-select(receita2018,-c(3,5))
receita2018<-spread(receita2018,Conta,Valor)
receita2018<-rename(receita2018,municipio=Instituição)
receita2018<-select(receita2018,c(1:4,17,21,101:104,107:112,138,139))

receita2018<-mutate_all(receita2018,replace_na,0)
receita2018<-mutate(receita2018,royaltiepetroleo=receita2018$`1.7.1.8.02.3.0 Cota-parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89`+receita2018$`1.7.1.8.02.4.0 Cota-parte Royalties pelo Excedente da Produção do Petróleo ¿ Lei nº 9.478/97, artigo 49, I e II`+receita2018$`1.7.1.8.02.5.0 Cota-parte Royalties pela Participação Especial ¿ Lei nº 9.478/97, artigo 50`  +receita2018$`1.7.1.8.02.6.0 Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2018<-select(receita2018,-c(13:16))
receita2018<-mutate(receita2018,FPM=receita2018$`1.7.1.8.01.2.0 Cota-Parte do Fundo de Participação dos Municípios - Cota Mensal` +receita2018$`1.7.1.8.01.3.0 Cota-Parte do Fundo de Participação do Municípios ¿ 1% Cota entregue no mês de dezembro` +receita2018$`1.7.1.8.01.4.0 Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho`)
receita2018<-select(receita2018,-c(7:9))

receita2018<-mutate(receita2018,IPTUpercapita=receita2018$`1.1.1.8.01.1.0 Imposto sobre a Propriedade Predial e Territorial Urbana`/receita2018$População,
                    ISSpercapita=receita2018$`1.1.1.8.02.3.0 Imposto sobre Serviços de Qualquer Natureza`/receita2018$População,
                    CPFPMpercapita=receita2018$FPM/receita2018$População,
                    CPITRpercapita=receita2018$`1.7.1.8.01.5.0 Cota-Parte do Imposto Sobre a Propriedade Territorial Rural`/receita2018$População,
                    CPRHpercapita=receita2018$`1.7.1.8.02.1.0 Cota-parte da Compensação Financeira de Recursos Hídricos`/receita2018$População,
                    CPRMpercapita=receita2018$`1.7.1.8.02.2.0 Cota-parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2018$População,
                    CPICMSpercapita=receita2018$`1.7.2.8.01.1.0 Cota-Parte do ICMS`/receita2018$População,
                    CPIPVApercapita=receita2018$`1.7.2.8.01.2.0 Cota-Parte do IPVA`/receita2018$População,
                    Rpetroleopercapita=receita2018$royaltiepetroleo/receita2018$População)%>%
  select(-c(5:13))

#Receita 2019
receita2019 <- read.csv2("dados/csv/Receita-2019.csv", sep = ";", 
                         stringsAsFactors = FALSE)

receita2019$Instituição<-str_sub(string = receita2019$Instituição, start =25, end= -6)
receita2019<-filter(receita2019,receita2019$Coluna=="Receitas Brutas Realizadas")
receita2019$Cod.IBGE<-as.character(receita2019$Cod.IBGE)
receita2019<-select(receita2019,-c(3,5))
receita2019<-spread(receita2019,Conta,Valor)
receita2019<-rename(receita2019,municipio=Instituição)
receita2019<-select(receita2019,c(1:4,17,24,142:145,149:154,190,191))


receita2019<-mutate_all(receita2019,replace_na,0)
receita2019<-mutate(receita2019,royaltiepetroleo=receita2019$`1.7.1.8.02.3.0 Cota-parte Royalties ¿ Compensação Financeira pela Produção de Petróleo ¿ Lei nº 7.990/89`+receita2019$`1.7.1.8.02.4.0 Cota-parte Royalties pelo Excedente da Produção do Petróleo ¿ Lei nº 9.478/97, artigo 49, I e II`+receita2019$`1.7.1.8.02.5.0 Cota-parte Royalties pela Participação Especial ¿ Lei nº 9.478/97, artigo 50`  +receita2019$`1.7.1.8.02.6.0 Cota-Parte do Fundo Especial do Petróleo ¿ FEP`)
receita2019<-select(receita2019,-c(13:16))
receita2019<-mutate(receita2019,FPM=receita2019$`1.7.1.8.01.2.0 Cota-Parte do Fundo de Participação dos Municípios - Cota Mensal` +receita2019$`1.7.1.8.01.3.0 Cota-Parte do Fundo de Participação do Municípios ¿ 1% Cota entregue no mês de dezembro` +receita2019$`1.7.1.8.01.4.0 Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho`)
receita2019<-select(receita2019,-c(7:9))

receita2019<-mutate(receita2019,IPTUpercapita=receita2019$`1.1.1.8.01.1.0 Imposto sobre a Propriedade Predial e Territorial Urbana`/receita2019$População,
                    ISSpercapita=receita2019$`1.1.1.8.02.3.0 Imposto sobre Serviços de Qualquer Natureza`/receita2019$População,
                    CPFPMpercapita=receita2019$FPM/receita2019$População,
                    CPITRpercapita=receita2019$`1.7.1.8.01.5.0 Cota-Parte do Imposto Sobre a Propriedade Territorial Rural`/receita2019$População,
                    CPRHpercapita=receita2019$`1.7.1.8.02.1.0 Cota-parte da Compensação Financeira de Recursos Hídricos`/receita2019$População,
                    CPRMpercapita=receita2019$`1.7.1.8.02.2.0 Cota-parte da Compensação Financeira de Recursos Minerais - CFEM`/receita2019$População,
                    CPICMSpercapita=receita2019$`1.7.2.8.01.1.0 Cota-Parte do ICMS`/receita2019$População,
                    CPIPVApercapita=receita2019$`1.7.2.8.01.2.0 Cota-Parte do IPVA`/receita2019$População,
                    Rpetroleopercapita=receita2019$royaltiepetroleo/receita2019$População)%>%
  select(-c(5:13))

# Carregar tabelas "IAP e IAPM  de 2013- 2019 - CEPERJ
#Fonte: https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf

#IAP 2013
IAP2013 <- read.csv2("dados/csv/IAP-2013.csv", sep = ";", 
                         stringsAsFactors = FALSE)

IAP2013<-rename(IAP2013,municipio=Municípios)

#IAP 2014
IAP2014 <- read.csv2("dados/csv/IAP-2014.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2014<-rename(IAP2014,municipio=Municípios)

#IAP 2015
IAP2015 <- read.csv2("dados/csv/IAP-2015.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2015<-rename(IAP2015,municipio=Municípios)

#IAP 2016
IAP2016 <- read.csv2("dados/csv/IAP-2016.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2016<-rename(IAP2016,municipio=MUNICIPIO)

#IAP 2017
IAP2017 <- read.csv2("dados/csv/IAP-2017.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2017<-rename(IAP2017,municipio=MUNICIPIO)

#IAP 2018
IAP2018 <- read.csv2("dados/csv/IAP-2018.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2018<-rename(IAP2018,municipio=MUNICIPIO)

#IAP 2019
IAP2019 <- read.csv2("dados/csv/IAP-2019.csv", sep = ";", 
                     stringsAsFactors = FALSE)

IAP2019<-IAP2019[-c(93:127),-c(5:16)]

IAP2019<-rename(IAP2019,municipio=MUNICIPIO)


#Juntando Bases Receita
Receitas<-bind_rows(receita2013,receita2014,id=NULL)
Receitas<-bind_rows(Receitas,receita2015,id=NULL)
Receitas<-bind_rows(Receitas,receita2016,id=NULL)
Receitas<-bind_rows(Receitas,receita2017,id=NULL)
Receitas<-bind_rows(Receitas,receita2018,id=NULL)
Receitas<-bind_rows(Receitas,receita2019,id=NULL)
Receitas<-rename(Receitas,ano=Ano)

#Juntando bases IAP
IAP<-bind_rows(IAP2013,IAP2014,id=NULL)
IAP<-bind_rows(IAP,IAP2015,IAP2016,IAP2017,IAP2018,IAP2019,id=NULL)
IAP<-rename(IAP,ano=Ano)

# Juntando as bases
basepainel <- Receitas%>%
  inner_join(IAP, by=(c('ano'='ano', 'municipio'='municipio')))
  
b<-any(table(basepainel$ano, basepainel$municipio )>1)
basepainel2<-filter(basepainel,basepainel$ano!=b|basepainel$municipio!=b)
