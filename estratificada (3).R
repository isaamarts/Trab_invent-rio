#Estratificada
dados=read.csv2('fustes_final.csv');
View(dados);
names(dados);

#names(dados)=ifelse(names(dados))=='vcom','vary',names(dados))   #renomeando a coluna
dados$vary=dados$vprod_7; #vari?vel de interesse chamada de vary

#conhecendo poss?veis estratifica??es
#parcela=subset(dados,!duplicated(parcela),c('parcela')); #v?rias
#fazenda=subset(dados,!duplicated(fazenda),c('fazenda')); #uma s?
#talhao=subset(dados,!duplicated(talhao),c('talhao')); #v?rios, mas tem talh?o com s? uma parcela. n?o d? pra fazer por talh?o
matgen=subset(dados,!duplicated(matgen),c('matgen')); #quatro
View(matgen)

#espacamento=subset(dados,!duplicated(espacamento),c('espacamento')); #um s?
#idade=subset(dados,!duplicated(idade),c('idade')); #v?rias

calc=subset(dados,!duplicated(codplantio),c('codplantio','areaplantio','matgen'));
View(calc)


mat1=subset(calc,matgen==26);mat1;
areamat1=sum(mat1$areaplantio);areamat1;

mat2=subset(calc,matgen==35);mat2;
areamat2=sum(mat2$areaplantio);areamat2;

mat3=subset(calc,matgen==54);mat3;
areamat3=sum(mat3$areaplantio);areamat3;


areamattot=c(areamat1,areamat2,areamat3)
areatot=sum(areamattot)

dados$areaest=NA
dados$areaest[dados$matgen==26]<-areamat1
dados$areaest[dados$matgen==35]<-areamat2
dados$areaest[dados$matgen==54]<-areamat3
View(dados)

#escolhendo estratifica??o

dados$estrato=NULL
dados$estrato=dados$matgen

sig=0.01 #alfa=1%

dados$vary<-dados$vprod_7; 

names(dados)
View(dados)
#VOLUME DE INDIV?DUO SOMADO P/ OBTER VOLUME POR PARCELA
dados2=with(dados,aggregate(list(vary=vary),
                            list(areaest=areaest,areaparc=areaparc,estrato=estrato,parcela=parcela),sum,na.rm=TRUE));
View(dados2);

# PROCESSAMENTO
estrato=with(dados2,aggregate(
  list(areaest=areaest,areaparc=areaparc,ym=vary),
  list(estrato=estrato),
  mean
)); estrato

#N?mero de parcelas lan?adas por estrato
calc=with(dados2,aggregate(
  list(anj=parcela),
  list(estrato=estrato),
  length
)); calc

estrato=merge(estrato,calc); #atentar-se pela poss?vel bagun?a com os nomes id?nticos

#Vari?ncia e desvio padr?o por estrato. atentar-se para a substitui??o de vetores de mesmo nome
calc=with(dados2,aggregate(
  list(s2y=vary),
  list(estrato=estrato),
  var
)); calc
calc$sy=sqrt(calc$s2y);

estrato=merge(estrato,calc);

estrato$areaparc=estrato$areaparc/10000; #passando a ?rea das parcelas para ha
estrato$pnj=estrato$areaest/estrato$areaparc; #n?mero de parcelas cab?veis por estrato

#Nomeando a popula??o
estrato$populacao='Fazenda da isa'

#?rea, n. de amostras e n. de amostras cab?veis na popula??o
populacao=with(estrato,aggregate(
  list(area=areaest,an=anj,pn=pnj),
  list(populacao=populacao),
  sum
)); populacao

estrato=merge(estrato,populacao);

#Peso de cada estrato na popula??o, ligado ao n?mero de parcelas lan?adas em cada estrato
estrato$pwj=estrato$pnj/estrato$pn;

#C?lculo da m?dia estratificada/ponderada
calc=with(estrato,aggregate(
  list(ymstr=pwj*ym),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);

#Vari?ncia da m?dia estratificada
calc=with(estrato,aggregate(
  list(calc1=pwj^2*(s2y/anj)),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);

calc=with(estrato,aggregate(
  list(calc2=(pwj*s2y)/pn),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);
populacao$s2ystr=with(populacao,calc1-calc2);
populacao$calc1=NULL;
populacao$calc2=NULL;

#Grau de liberdade efetivo
estrato$calcgl=with(estrato,pnj*(pnj-anj)/anj); #Representa??o de um peso

calc=with(estrato,aggregate(
  list(calc1=calcgl*s2y,
       calc2=(calcgl*s2y)^2/(anj-1)),
  list(populacao=populacao),
  sum
));

calc$gle=calc$calc1^2/calc$calc2;
calc$calc1=NULL;
calc$calc2=NULL;

populacao=merge(populacao,calc);populacao;

#Erro na unidade
populacao$errounid=with(populacao,qt(1-sig/2,gle)*sqrt(s2ystr));

#Erro percentual
populacao$erroperc=with(populacao,errounid/ymstr*100);

#Total e erro populacional 
populacao$ytstr=with(populacao,ymstr*pn);
populacao$errototal=with(populacao,errounid*pn);
total<-with(populacao,ymstr*pn);
etotal<-with(populacao,errounid*pn);

##Intervalo de confiança da população
litot<-total-etotal;
lstot<-total+etotal;

#3b e 3c
print(paste('IC:',round(litot,2),'<=T<=',round(lstot,2),'m³',sep=''));

#Total e erro por hectare
populacao$ymha=with(populacao,ytstr/area);
populacao$erroha=with(populacao,errototal/area);

View(populacao);
write.csv2(populacao,'ACE_calculada.csv',row.names=F);

library(cmrinvflor);

View(dados)
estrato<-subset(dados2,
                !duplicated(estrato),
                select=c('estrato','areaest'))
amostra<-dados2[,c('estrato','parcela','areaparc','vary')]

ace<-estats_ace(estrato,amostra,sig=sig*100,fc_dim=1/10000);
View(ace);


##################################################################################################################3
