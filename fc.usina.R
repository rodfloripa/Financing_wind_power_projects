# 1-O maximo de amortização de financiamento do BNDES é 16 anos
# 2-O prazo de carencia do BNDES é de 2 anos
# 3-O desembolso do BNDES é feito em uma parcela.
# 3-O emprestimo ponte é metade do inv. total,ele é pago no ano 2
# 4-Receita bruta total no ano-calendário inferior a R$ 72.000.000,00
#  para optar pelo lucro presumido,como feito aqui,se for maior é
# preciso colocar impostos no beta e wacc
# 5-DEFINIR VALORES ABAIXO SEMPRE ANTES DE RODAR!!!!!################
# capp: % capital total em capital proprio
# captb: % capital total vindo do bndes
# captd: % capital total de debentures
# 6-Foi usada a formula para tjlp > 6%, para tlp < 6% alterar tn 

#source('fc.usina-sem os loops.R')
#fc.us(110.51,20,10,70)
#fc.us(110.51,89.719,82681.8,20,10,70) 

# Caso base com lucro presumido VPL=  9774842

# tjlp atual 7% + spread de 2.6% = 6%(tjlp>6) + 2.6% (sp),infl de 5% -> VPL= 37.131.183
# fc.us(110.51,20,10,70) -> 2017 -> vpl 34.887.779
# debentures: o spread ntnb medio é 1.5%
#capp <- 30
#captb <- 70
#captd <- 0
fc.us<-function(pld,en,inv,capp,captb,captd){
options(digits=5)
library(FinancialMath)


  
#anos <- readline(prompt="Entre com o número de anos de operação da usina: ")
anos <- 35
#anosi <- readline(prompt="Entre com o número de anos para construção da usina: ")
anosi <- 2
#invb <- readline(prompt="Entre com o investimento percentual do BNDES:")
invb <- 70
invb <- captb
#e.ponte <- readline(prompt="Entre com a taxa de juros em % a.a nominal da CCB(ponte):")
e.ponte <- 10


#inv.ac <- readline(prompt="Entre com o investimento percentual do acionista:")
inv.ac <- 10
inv.ac <- capp


#imp <- readline(prompt="Entre com o percentual de impostos PIS+COFINS:")
#corrigir para valor de 2023 a 2033
# 9.25% real e 3.65% presumido | PIS+COFINS+IR+CSLL= 6.73%
imp <- 6.73


#cust <- readline(prompt="Entre com os custos de operação em % do investimento:")
#corrigir para o preço em 2023
#cust <- 19.85
# cust anterior que eu usava era 2.5
cust <- 28.4 
# o valor acima é sem efeito, 12% da renda bruta 

#tj <- readline(prompt="Entre com a TJLP anual do BNDES em %:")  
#tj <- 7 em 2017, 9.5 em 2023
tj <- 7
#sp <- readline(prompt="Entre com o spread em %(2.6 até 5):")
sp <- 2.6

#infl <- readline(prompt="Entre com a inflação anual em %:")
#corrigir para valor de 2023 a 2033(3.5%)

infl <- 4
#t.amort <- readline(prompt="Entre com os anos de amortização:")
t.amort <- 16


#ir <- readline(prompt="Entre com o valor % de IR+CSLL(34%):")
# 34% real e (8%*25% +12%*9% ) presumido(renda bruta < 72 milhoes por ano)
ir <- 0
#td <- readline(prompt="Entre com o valor % da taxa de desconto ao ano:")
td <- 10
#debv <- readline(prompt="Entre com o valor da emissão de debentures:")
debv <- inv*(captd/100)

#debp <- readline(prompt="Entre com o período das debentures:")
debp <- 17
#debi <- readline(prompt="Entre com o spread das debentures(%):")
# 1.8% acima da ntnb
debi <- 5.5
#deba <- readline(prompt="Entre com a amortização anual das debentures(%):")
deba <- 0.5
# estava em 4, 20/02
deba <- seq(from=deba,to=7,length.out=debp) #nao atende o icsd
#deba <- 2*(1-(0.95^(0:(debp-1))))
#rfg <- readline(prompt="Entre com a taxa livre de risco:")
rfg <- 5.29
#rmg <- readline(prompt="Entre com o prêmio de mercado:")
rmg <- 5.54
#rmg <- readline(prompt="Entre com o risco país:")
rp <- 4.07
anos <- as.integer(anos)
anos <- anos-1
anosi <- as.integer(anosi)
fc.usina <- data.frame("Ano"=0:anos, "Energia Produzida"='',
                       "Investimento no Financiamento"='',
                       "Saldo.Eponte.Deb.BNDES"='',
                       "Receita Bruta"='',"Impostos Diretos"='',
                       "Custos"='',"Ebitda"='',
                       "Depreciação20%"='',"Despesa Financeira(max 16 anos)"='',
                       "LAIR"='',"IR+CSLL"='',
                       "Lucro Liquido"='',"Soma Depreciação"='',
                       "Amort.Deb.mais.BNDES"='',
                       "Fluxo Caixa Acionista"='',"FC Acionista Acumulado"='',
                       "Fluxo Caixa Acionista Descontado"='',
                       "FC Acionista Desc. Acumulado"='',
                       "VPL(R$)"='',"VF do Acionista no banco(R$)"='',
                       "VF do Acionista no projeto(R$)"='',
                       "Juros debentures (R$)"='',"ICSD"='',
                       "Perc. Capital de Terceiros(%)"='',"Beta Desalavancado"='',
                       "D/E.mais.D"='',"Beta Realavancado"='',"Ret.CP.N"='',
                       "Ret.C.Terc.N"='',"Wacc Nom."='',"Wacc R."='',
                       "VPL final"='')


fc.usina <- t(fc.usina)
en <- as.double(en)
# Energia Produzida
fc.usina[2,(anosi+1):(anos+1)] <- en 
# Investimento Total
fc.usina[3,0:anosi] <- inv/anosi 
invb <- (invb/100)*inv
# Receita com a venda de energia
pld <- as.double(pld)
fc.usina[5,(anosi+1):(anos+1)] <- as.double(pld*en) 
# Impostos Diretos
imp <- as.double(imp)/100
fc.usina[6,(anosi+1):(anos+1)] <- as.double(-imp*as.double(fc.usina[5,
                                  (anosi+1):(anos+1)]))
cust <- as.double(cust)*0.01
# Cálculo de Custos- 12% da receita bruta media do leilao
# Alterei para o caso base
fc.usina[7,(anosi+1):(anos+1)] <- -as.double(fc.usina[5,(anosi+1):(anos+1)])*0.12
#fc.usina[7,(anosi+1):(anos+1)] <- -as.double(fc.usina[2,(anosi+1):(anos+1)])*0.1985*100
# Ebitda
fc.usina[8,(anosi+1):(anos+1)] <- as.double(fc.usina[5,
                                  (anosi+1):(anos+1)])+
                                   as.double(fc.usina[6,(anosi+1):(anos+1)])+
                                  as.double(fc.usina[7,(anosi+1):(anos+1)])
# Depreciação em 20 anos
fc.usina[9,(anosi+1):(anos+1)] <- 0
fc.usina[10,(anosi+1):(anos+1)] <- 0
fc.usina[9,(anosi+1):(anosi+20)] <- -(1)*inv*0.05
tj <- as.double(tj)
sp <- as.double(sp)
infl <- as.double(infl)
pmto <- invb
# vdev é o valor devido ao BNDES após um ano total corrigido pela taxa treal 
# Saldo Devedor após 1 ano com tjlp > 6%

tn <- tj                            
treal <- 100*((1+(tn/100))/(1+(infl/100))-1)
treal <- treal/100
fc <- ((1+treal)/1.06)^(730/360)
sj <- ((1+((sp+6)/100))^(730/360))-1
sd <- invb*0.5
j  <- sd*fc*sj                       
vdev <- -2*((sd*fc)+j)
#vdev <-  -2*0.9818*TVM(pv=invb*0.5,n=2,i=treal,ic=1)[2] caso base
              #tjlp < 6

# Cálculo do Financiamento pelo BNDES
t.amort <- as.double(t.amort)
pm.prest <- ((1+treal)^(t.amort))*treal
pm.prest.aux <- ((1+treal)^(t.amort))-1
pm.prest <- vdev*pm.prest/pm.prest.aux
amort <- data.frame("Periodo"=1:t.amort,
                    "Saldo Devedor In."=0,"Juros"=0,"Amortização"=0,
                    "Prestação"=0,"Saldo Devedor F."=0)
amort[1,2] <- as.double(vdev) 

for (i in 1:t.amort){
  if(i > 1){
    
    amort[i,2] <-amort[i-1,2]- amort[i-1,4] 
  }
  amort[i,5] <- pm.prest
  amort[i,3] <-as.double(amort[i,2]*treal)
  amort[i,4] <- as.double(amort[i,5])-as.double(amort[i,3])
  amort[i,6] <-as.double(amort[i,2]-amort[i,4])
  }

for (i in (anosi+1):(t.amort+anosi)){
# Despesa Financeira BNDES
  fc.usina[10,i] <- amort[i-anosi,3]
# Amortização BNDES+Debentures
    fc.usina[15,i] <- amort[i-anosi,4]
}
ir <- as.double(ir)
# Cálculo de Debentures
# debt(valor,anos,juros pagos%,amortização anual%)
assign("dev1",0, envir = .GlobalEnv)
source('deb.r')
# Juros de Debentures - o '+3' siginifica mais 3 emissões de deb.
# Tirar o '3' abaixo para fazer emissão unica de debentures
fc.usina[23,3:(debp+2)] <- -debt(debv,debp,debi,deba)
#print(-debt(debv,debp,debi,deba))
# Emissão de mais 3 series de debentures
# Tirar o '3' para fazer emissão unica de debentures
debp <- debp
# Despesa Financeira(JUROS) de Debentures+BNDES

fc.usina[10,3:(debp+2)] <- as.numeric(fc.usina[10,3:(debp+2)])+as.numeric(fc.usina[23,3:(debp+2)])

for (i in (anosi+1):(anos+1)){
# Amortização BNDES+Debentures
  fc.usina[15,i] <- 0
# LAIR
  fc.usina[11,i] <- as.double(fc.usina[8,i])+
    as.double(fc.usina[9,i])+as.double(fc.usina[10,i])
  a <- as.double(fc.usina[11,i])
# Calculo de tributos IR+CSLL , 34% de tributos se tiver lucro
# No lucro presumido é 0!
  if(a > 0){
# IR+CSLL
    fc.usina[12,i] <- 0
  } else {
    fc.usina[12,i] <- 0
  }
# Lucro Liquido
  fc.usina[13,i] <- as.double(fc.usina[11,i])+as.double(fc.usina[12,i])
# Soma Lucro com Depreciação
  fc.usina[14,i] <- -as.double(fc.usina[9,i])
}
# Amortização BNDES
fc.usina[15,(anosi+1):(t.amort+anosi)] <- as.double(amort[1:t.amort,4])

fc.usina[15:17,1:anosi] <- 0
fc.usina[13:14,1:anosi] <- 0

# Soma Amortização BNDES+Debentures

fc.usina[15,3:(debp+2)] <- as.numeric(fc.usina[15,3:(debp+2)])-dev1[1:debp]
# Fluxo de Caixa do Acionista
fc.usina[16,1] <- 0
fc.usina[16,1] <- -(capp/100)*inv*0.5
fc.usina[16,2] <- -(capp/100)*inv*0.5
# Emprestimo Ponte
saldo <-  as.numeric(fc.usina[3,2])-(invb+debv-as.numeric(fc.usina[16,2]))
# Saldo Emprestimo Ponte+Debentures+BNDES
fc.usina[4,1] <- as.numeric(fc.usina[3,1])+as.numeric(fc.usina[16,1])
treal1 <- 100*((1+(e.ponte/100))/(1+(infl/100))-1)
treal1 <- treal1/100
vdev1 <- as.numeric(fc.usina[4,1])*(1+treal1)
fc.usina[4,2] <- invb+debv-vdev1-as.numeric(fc.usina[16,2])
# Fluxo de Caixa do Acionista Acumulado
fc.usina[17,0] <- 0
fc.usina[17,1] <- as.double(fc.usina[16,1])
fc.usina[17,(2:anosi)] <- as.double(fc.usina[16,(2:anosi)])+
  as.double(fc.usina[17,(1:(anosi-1))])
td <- as.double(td)/100
for (i in (anosi):anos+1){
# Fluxo de Caixa do Acionista=Lucro+Soma Depreciação+Amortização  
  fc.usina[16,i] <- as.double(fc.usina[15,i])+as.double(fc.usina[14,i])+
    as.double(fc.usina[13,i])
  fc.usina[17,i] <- as.double(fc.usina[16,i])+as.double(fc.usina[17,i-1])
# Fluxo de Caixa Descontado 
  fc.usina[18,i] <- as.double(fc.usina[16,i])/((1+td)^(i-1))
  }
#print(fc.usina[15,])
fc.usina[18,1] <- as.double(fc.usina[16,1])  
# Fluxo de Caixa Descontado Acumulado
fc.usina[19,1:(anos+1)] <- cumsum(as.double(fc.usina[18,1:(anos+1)])) 
z <- as.vector(fc.usina[16,])
y <- as.double(z)
y <- y[-1]
# VPL

fc.usina[20,1] <- 1000*NPV(as.double(fc.usina[16,1]),y,td,
                           times=as.vector(1:anos),plot=FALSE)
inv <- -inv
inv.acionista <- as.double(fc.usina[17,anosi])
# Compara Investimento na Renda Fixa
fc.usina[21,1] <- 1000*TVM(pv=-inv.acionista,n=anos+1-anosi,i=td)[2]
# Investimento no Projeto
fc.usina[22,1] <- 1000*as.double(fc.usina[17,anos+1])
anos <- anos+1
# Percentual de Capital de Terceiros
# Troquei t.amort por debp
fc.usina[25,] <- 0
fc.usina[25,(anosi+1):(t.amort+anosi)] <-  as.numeric(amort[1:t.amort,4])

for (i in 3:(debp+2)){
  
  fc.usina[25,i] <- as.numeric(-dev1[i-2])+as.numeric(fc.usina[25,i])
}
fc.usina[25,3:(debp+anosi)] <- cumsum(fc.usina[25,3:(debp+anosi)])
cter <- ((inv*captd/100)+vdev)
fc.usina[25,3:(debp+anosi)] <- cter-(as.numeric(fc.usina[25,3:(debp+anosi)]))
fc.usina[25,3:(debp+anosi)] <- 100*as.numeric(fc.usina[25,3:(debp+anosi)])/(cter+(inv*capp/100))

fc.usina[25,anosi-1] <- 100
fc.usina[25,anosi] <- captb+captd
# D/D+E
fc.usina[27,1:anos] <- as.numeric(fc.usina[25,1:(anos)])/(100-as.numeric(fc.usina[25,1:(anos)]))
fc.usina[27,1:(anosi-1)] <- 1
# Beta Desalavancado
fc.usina[26,1:anos] <- 0.515
# Receita bruta anual menor que 48 milhoes,regime tribut. de lucro presumido
# sem aliquota marginal de IR
# Beta Realavancado
fc.usina[28,1:(anos)] <- as.numeric(fc.usina[26,1:(anos)])*(1+(as.numeric(fc.usina[27,1:(anos)])))
# Retorno Capital Proprio Nominal(CAPM)
fc.usina[29,1:(anos)] <- rfg+(as.numeric(fc.usina[28,1:(anos)])*rmg)+rp
# ICSD
#fc.usina[24,3:anos] <- as.numeric(fc.usina[14,3:anos])-as.numeric(fc.usina[10,3:anos])
#fc.usina[24,3:anos] <- as.numeric(fc.usina[24,3:anos])/(-as.numeric(fc.usina[10,3:anos])-as.numeric(fc.usina[15,3:anos]))
fc.usina[24,3:anos] <- as.numeric(fc.usina[8,3:anos])/(as.numeric(fc.usina[15,3:anos])+as.numeric(fc.usina[10,3:anos]))

# (1+taxa final) = (1+IPCA)(1+spread) 
# calcular taxa nominal de juros da debenture
debi <- 100*((1+(infl/100))*(1+(debi/100))-1)
# Cálculo do Custo Ponderado do Capital de Terceiros
cter1[1:(anos-2)] <- (cter+(inv*capp/100))*0.01*as.numeric(fc.usina[25,3:anos])
percdeb[1:anos] <- 0
percdeb[1:(debp)] <- -(-debv+cumsum(dev1[1:debp]))/as.numeric(cter1[1:(debp)])
percdeb[debp] <- 0
percbndes[1:debp] <- (vdev-as.numeric(cumsum(amort[1:debp,4])))/as.numeric(cter1[1:(debp)])
percbndes[debp] <- 0
custoct[3:anos] <- ((tn)*as.numeric(percbndes[1:(anos-2)])+(debi)*as.numeric(-percdeb[1:(anos-2)]))
custoct[1] <- e.ponte
custoct[2] <- (captb*tn+(captd*(debi)))/(captb+captd)
#custoct[(anosi+t.amort):anos] <- 0
# Cálculo do Wacc
waccn1 <- as.numeric(fc.usina[25,1:(anos)])*0.01*custoct[1:anos]
waccn <- waccn1+(1-(0.01*as.numeric(fc.usina[25,1:(anos)])))*as.numeric(fc.usina[29,1:(anos)])
# CPI de 2.5%
waccr <- 100*((1+(waccn/100))/(1+(2.5/100))-1)
# Custo Ponderado do Capital de Terceiros
fc.usina[30,2:(anos)] <- custoct[2:anos] 
fc.usina[30,1] <- custoct[1] 
# Wacc Nominal
fc.usina[31,1:(anos)] <- waccn
# Wacc Real
fc.usina[32,1:(anos)] <- waccr
vpl2 <- 0
fc.usina[5:12,1] <- 0
fc.usina[5:12,2] <- 0

# Calculo do VPL do acionista(taxa de desconto é o retorno sobre cap. proprio)
# Valor real do custo de capital proprio com CPI de 2.5%
a[1:anos] <-  100*((1+(as.double(fc.usina[29,1:anos])/100))/(1+(2.5/100))-1) 
for (i in 2:35){
vpl2[i] <- (as.double(fc.usina[16,i]))/(1+(0.01*a[i]))^(i-1)
#vpl2[i] <- (as.double(fc.usina[16,i]))/(1+0.1)^(i-1)
  }
vpl2[1] <- as.double(fc.usina[16,1])
fc.usina[33,1] <- 1000*(cumsum(vpl2)[35])

# Calculo do VPL total da empresa,usando o fluxo caixa livre para empresa(FCFF)
#for (i in 2:35){
#vpl2[i] <- (as.double(fc.usina[8,i])+as.double(fc.usina[12,i])-as.double(fc.usina[15,i])-as.double(fc.usina[10,i]))/(1+(0.01*as.double(fc.usina[32,i])))^(i-1)

#  }
#vpl2[2] <- inv*0.5+(vpl2[2])
#vpl2[1] <- inv*0.5
#fc.usina[33,1] <- 1000*(cumsum(vpl2)[35])

# Limpar NAs e Formatar com 2 casas após a virgula
for (i in 1:33){
  for (j in 1:35){
    fc.usina[i,j] <- format(round(as.numeric(fc.usina[i,j]), 2), nsmall = 2)
    fc.usina[i,j] <- gsub( "NA" , "",fc.usina[i,j])
  }}
# Grafico do FC
aux <- (as.double(fc.usina[17,]))/1000
cols <- c("red", "blue")[(aux > 0) + 1]  
#barplot(aux,col=cols,main="Fluxo de Caixa Acumulado",
#       xlab='Anos',ylab='R$(milhões)',ylim=c(-50,100),names.arg=c(1:anos))
assign("vp",rbind(vp,as.double(fc.usina[20,1])),envir = .GlobalEnv)
assign("VPLu",vp, envir = .GlobalEnv)
assign("VPLu",VPLu[-1], envir = .GlobalEnv)
assign("fc.usina",fc.usina, envir = .GlobalEnv)
assign("amort",amort, envir = .GlobalEnv)

return(as.numeric(fc.usina[33,1]))
}

