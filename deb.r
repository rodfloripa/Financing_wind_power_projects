# entrada: valor da debenture,periodo,taxa de juros paga pela debenture,
# amortização.Entrar com juros i como spread em % e tabela de amortização em %
# retorna: tabela com os pagamentos ao detentor
# IPCA 2022-2032 :6.5%
# media premio das emitidas(conservador) de IPCA+8,5 %

debt <- function(va,periodo,i,am){
  library(FinancialMath)
  
  valordevido <- 0
  # (1+taxa final) = (1+IPCA)(1+spread) , i(entrada de dados)= spread
  # não considero inflação no fluxo de caixa, ipca=0
  i <- i/100
  tab <- 0
  am[periodo] <- 0
  valordevido[1:periodo] <- va
  jj <- 2
  valordevido[1] <- va*(1-(am[1]/100))
  tab[1] <- va*(i/1)
 
  j <- 2
  for (j in 2:periodo){
    tab[jj] <- valordevido[j-1]*(i/1) 
    valordevido[j] <- valordevido[j-1]*(1-(am[j]/100))
    j <- j+1
    jj <- jj+1
  }
  
  dev[1] <- va*(am[1]/100)
  a <- va-as.numeric(dev[1])
  b <- as.numeric(dev[1])
  for (j in 2:(periodo-1)){
    dev[j] <- a*(am[j]/100)
    a <- a-as.numeric(dev[j])
    b <- b+as.numeric(dev[j])
  }
  dev[periodo] <- va-b
  dev[(periodo+1):(periodo+3)] <- 0
  dev1 <- dev
    
  assign("dev1",dev1, envir = .GlobalEnv)
  return(tab)
}


