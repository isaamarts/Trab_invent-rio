#cubagem

dir();
dados<-read.csv2('cubagem.csv');
View(dados)
names(dados)
#-------

#volume comercial com casca
#diametro mínimo de 5cm com casca

#Deixei apenas uma coluna de dicc (diametro da iésima seção com casca)
dados$dicc=((dados$dicc1+dados$dicc2)/2)
dados$dicc1=NULL
dados$dicc2=NULL
dados$espcasca=NULL
View(dados)

cub<-dados[,c('arv','dap','ht','hi','dicc')];
View(cub)
selarv=subset(cub,!ht<hi);
View(selarv);
#--------

#Cubagem rigorosa
#dcoms= diametro minimo comercial, 5 cm no caso)
library(cmrinvflor);
vsmal<-smalian(selarv,dcoms=7,htoco=10,comcasca=F,di_ou_ci='di', dbase_ponta = 7)
names(vsmal);
View(vsmal);
cubagem <- vsmal[,c('arv','dap','ht','vprod_7')]
View(cubagem);

matgen<-aggregate(list(matgen=dados$matgen),
                  list(arv=dados$arv 
                  ),mean)
cubagem <- merge(matgen,cubagem)
View(cubagem)

#Fator de Forma
#vsmal$ffcom<-with(vsmal,vprod_5/vcilcc)

#Salvar o arquivo em Excel no formato csv
write.csv2(cubagem,'cubagem_smalian.csv',row.names=F)


