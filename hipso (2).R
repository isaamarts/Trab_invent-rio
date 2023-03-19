#### LISTA 3 ####

dir()
arv = read.csv2('fustes.csv')
View (arv)
names(arv)


##----------------NÃO RODAR SCRIPT ABAIXO SE JÁ TIVER A COLUNA HDOM--------------------------------

## ANALISAR QUANTAS DOMINANTES POR PARCELA (PROCURAR ERROS)
nhdom<-with(subset(arv,cat=='Dominante'),
           aggregate(list(nhdom=ht),
                     list(parcela=parcela),
                     length))
View(nhdom);
unique(nhdom$nhdom);

##ESCOLHER A MÉDIA DAS 5 DOMINANTES (ALTURA DAS 5 MAIS GROSSAS) EM CADA PARCELA (EVITA ERROS DA BASE)
#fhdom<-function(x){
  #x<-x[order(x$dap, decreasing = T),]
  #x<-x[!is.na(x$ht) & x$ht>0,]
  #x<-x[1:round(x$areaparc[1]/100),];
  #return(data.frame(parcela=x$parcela[1],hdom=mean(x$ht)))
#}

## CRIAR COLUNA ALTURA DOMINANTE (CASO AS 5 MAIS GROSSAS SEJAM AS DOMINANTES - BASE CORRETA)
hdom<-with(subset(arv,cat=='Dominante'),
           aggregate(list(hdom=ht),
                     list(parcela=parcela),
                     mean, na.rm=T))

arv<-merge(arv,hdom,by='parcela'); ##CRIA A COLUNA DA MÉDIA DAS DOMINANTES POR PARCELA

head(arv);
View(arv);
#----------------SE JÁ TIVER COLUNA HDOM, RODAR A PARTIR DAQUI---------------------------------------------------------

#MODELO HIPSOMETRICO GENÉRICO
modelo<-'log(ht)~I(log(hdom))+I(1/dap)';

#Excluir árvores que a altura não foi medida (NA)
selarv<-subset(arv,!is.na(ht) & ht> 0 & dap>0); 
nrow(arv)
nrow(selarv) 

#CRIANDO O MODELO LINEAR PARA A HIPSOMETRIA GENÉRICA
ajhipso<-lm(modelo, selarv)
coef(ajhipso)
sumario<-summary(ajhipso)

#RETORNANDO OS VALORES DOS BETAS (Questão 1-a,b,c)
(bs<-as.vector(coef(ajhipso)));
paste(bs[1],bs[2],bs[3]);
                
#Coeficiente de determinacao (R-Squared)
R2adj=sumario$adj.r.squared
R2adj

#Coeficiente de determinacao em porcentagem (%) - Round define a qtde de casas decimais 
R2adjp=round(R2adj*100,2) 
R2adjp #Questão 1-f

#INDICE DE FURNIVAL (corrigir o erro residual para m)
y<-selarv$ht;
D(expression(log(y)),'y'); #Derivada
dy<-1/y;
(medgeo<-exp(mean(log(dy)))); #média geometrica

#sumario$sigma = Erro padrão residual em m³  ♥ 
(IF<-1/medgeo*sumario$sigma); #Questão 1-d
#Indice de furnival em porcentagem
(IFperc<-round(IF/mean(y)*100,2));#Questão 1-e

#CALCULANDO A ALTURA ESTIMADA A PARTIR DO MODELO 
arv$htest<-exp(bs[1]+bs[2]*log(arv$hdom)+bs[3]/arv$dap); 
View(arv)

x11();
par(mfrow=c(1,2))
with(arv,plot(htest~dap,xlab='dap(cm)',
              ylab='htest(m)',pch='*',col='red'))
with(arv,plot(ht~dap,xlab='dap(cm)',
              ylab='ht(m)',pch='*',col='green'))

x11();
with(arv,plot(ht~dap,xlab='dap(cm)',
              ylab='ht(m)',pch='*',col='green'))
with(arv,points(htest~dap,pch='*',col='red'));

#Htre = altura real
arv$htre<-arv$ht;
ii<-is.na(arv$ht)
sum (ii) #total de arvores com ht nula

#todo mundo que NÃO TIVER ALTURA REAL, vai receber a HTEST
arv$htre[ii]<-arv$htest[ii];
View(arv)

#agora tenho a altura de todas as árvores (real ou estimada)

#---------------------------------------------------------------------------
dir()
cubagem=read.csv2('cubagem_smalian.csv')
names(cubagem)
View(cubagem)

#cubagem$vprod_7<-cubagem$vprod_7;
cubagem<-cubagem[,c('matgen','arv','dap','ht','vprod_7')];
View(cubagem);

#Modelo Volumétrico de Schumacher e Hall: v=b0*dap^b1*ht^b2 NO MEU CASO ? O QUE SER? USADO

modelo_schu='vprod_7~b0*dap^b1*ht^b2';
ajnlin=nls(modelo_schu,cubagem,start=list(b0=pi/40000*0.45,b1=2,b2=1));
coef(ajnlin);
(bs<-as.vector(coef(ajnlin)));
paste(bs[1],bs[2],bs[3]);
summary(ajnlin);
sumario=summary(ajnlin);
syx=sumario$sigma; # erro padrão residual (m³)
syx
syx_perc=syx/mean(cubagem$vprod_7)*100; # erro padrão residual (%)
syx_perc


#Estimando o volume
cubagem$vest=predict(ajnlin);
View (cubagem)

#Juntando o vprod_7 na base de dados "arv"   b0*dap^b1*ht^b2
arv$vprod_7 = with(arv,bs[1]*(dap) ^ bs[2] * (htre ^ bs[3]))
View(arv)


#arv[is.na(arv)]<-0;

#Salvar o arquivo em Excel no formato csv
write.csv2(arv,'fustes_final.csv',row.names=F); #abrir a estratificação

#AGREGANDO OS DADOS (somando o talhão, area talhão, volume....)
parc=aggregate(list(vtsc=arv$vprod_7), list(talhao=arv$codplantio, areatal=arv$areaplantio, parcela=arv$parcela, areaparc=arv$areaparc),sum,na.rm=TRUE)
View(parc) #consigo visualizar o volume de cada parcela


#-----------------------------------------------------------------------------------------#

#AGORA RODAR O INVENTÁRIO - ERRO DE 1% ######### ACS #########

#Area
areaparc=mean(arv$areaparc)
areaparc

# Média, variância e CV  (Volume comercial sem casca) [m³/parcela]
y<-parc$vtsc #Escolher VTCC ou VTSC
xmed = mean(y);
xmed #Questão 3-a
xvar = var(y);
xvar
xcv=(sd(y)/xmed)*100;
xcv

#Número de amostras
n=length(y);
n

talhao=subset(arv,duplicated(codplantio)==F,c('codplantio','areaplantio'));
talhao

#area da fazenda em ha
(area_fazenda<-sum(talhao$areaplantio))

#Numero de parcelas que cabem na fazenda 
(N=area_fazenda*10000/areaparc) 

# --------- Resultado por parcela ------------

#Variância da media
(xvarmed<-(xvar/n)*(1-n/N));

#Erro padrão da média (m³/parc)
(xdesvmed<-sqrt(xvarmed));

#Erro do inventário (1%) #Questão 3-b
(erro_inv=qt(0.995,n-1)*xdesvmed)

#Erro padrão (%) - #Questão 3-c
(erro_inv_perc=erro_inv/xmed*100)

#Intervalo de Confiança #Questão 3-d,e
cat(paste(round(xmed-erro_inv,2),
          '<= média => ',
          round(xmed+erro_inv,2),
          'm³/parcela com 99% de confiança',sep=''));

#FUNÇÃO - CONFERIR SE ATEU USANDO O PACOTE DO CLÁUDIO
?cmrinvflor::estats_acs
library(cmrinvflor);
as.data.frame(cmrinvflor :: estats_acs(vy=y, nt= N, sig=1))

# -------- RESULTADO POR HECTARE ------ USANDO ACS########

#Média por ha (m³/ha)
(medha<-xmed*(10000/areaparc));
medha #Questão 4-a

#Erro inventário (m³/ha)
(erroinvha<-erro_inv*(10000/areaparc));

#Intervalo de confiança em m?/ha - #Questão 4-b,c
cat(paste(round(medha-erroinvha,2),' <= média => ',round(medha+erroinvha,2),'m³/ha com 99% de confiança',sep=''))

# --------- RESULTADO DO POVOAMENTO --------

#Total populacional (m³) - #Questão 4-a
(tot<-medha*area_fazenda);

#Erro inventário para a população (m³)
(erroinvtot<-erroinvha*area_fazenda);

#Intervalo de confiança para a população - #Questão 4-b,c

cat(paste(round(tot-erroinvtot,2),'<= total <=', round(tot+erroinvtot,2),'m³\n'))






