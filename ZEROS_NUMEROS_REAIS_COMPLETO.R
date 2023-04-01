#============================================================================
# CALCULO NUMERICO
# RAIZES DE NúMEROS REAIS
# - METODO DA BISSECAO
#
# Versão 1.00 (2019) - 16.08.2019
# Autor: Prof. Giovanni Leopoldo Rozza (2019)
# Os algorítmos aqui implementados estão descritos no capítulo 2 do livro:
# 
#   RUGGIERO, Márcia A. Gomes; LOPES, Vera Lúcia da Rocha. 
#   Cálculo numérico: aspectos teóricos e computacionais. Makron Books do Brasil, 1997.
#
#============================================================================

# chama a bibliteca ggplot2 (gráfico) [tem que instalar o package antes]
library(ggplot2)

# diretorio de trabalho
setwd("G:/AULAS_ATUAL/Bagozzi/R_SOURCE/CALCULO_NUMERICO/CAP2_ZEROS_NR_REAIS")

# loga console em arquivo
sink("logconsole.txt", type = "output", append = FALSE, split = TRUE)

# define a funcao que se deseja obter o valor de f(x)
funcao_de_X <- function(dValorX) {
  dValor_fx <-   dValorX^3 - 9*dValorX + 3
  return(dValor_fx)
}

# define a funcao que se deseja-se obter o valor da derivada de f(x)
funcao_Derivada_de_X <- function(dValorX) {
  dValor_fx <-  3*dValorX^2 - 9 
  return(dValor_fx)
}


#============================================================================
# FUNCAO funcao_ZeroBissecao
#
# Parametros de Entradas
# dValorAk   --> valor inicial de a
# dValorAk   --> valor inicial de b
# dValorErro --> valor maximo de erro (para de iterar se sero < dValorErro & iIter=NULL)
# iIter=NULL  --> se não for null, ignora dValorErro e itera at? iIter
#
# # Parametros de Saída
# dataframe com o tipo de algo, erro, e ?ltimo valor de xk
#
#============================================================================
funcao_ZeroBissecao <- function(dValorAk,dValorBk,dValorErro,iIter=NULL)
{
  iTotalIteracoes <- 999
  
  if(!is.null(iIter))
  {
    iTotalIteracoes <- iIter
  }
  
  iConta<-1
  
  # dataframe com ak, bk, xk e f(xk)
  dfBissecao<- data.frame(    iCount<- integer(),
                              dAk  <-double(),
                              dBk  <-double(),
                              dXk  <-double(),
                              dfXk <-double()
  )
  
  dfAk<-  funcao_de_X(dValorAk) 
  dfBk<-  funcao_de_X(dValorBk) 
  dXk <-  0.5*(dValorAk + dValorBk)    
  dfXk<-   funcao_de_X(dXk) 
  
  # linha do dataframe a ser inserida
  dfB_temp<-data.frame( iConta,dValorAk,dValorBk,dXk,dfXk)
  # renomeia colunas do dataframe para o bind com dfBissecao
  names(dfB_temp)  <- c("iCount","dAk","dBk", "dXk","dfXk") 
  dfBissecao <- rbind( dfBissecao, dfB_temp)  # salva 1a iteracao no dataframe
  
  #imprime tabela
  print("|------------------------------------------------------------------------------------------------------------------------------------------|")
  print("|                                          ZERO DE FUNCAO REAL - MÉTODO DA BISSEÇAO                                                       |")
  print("|------------------------------------------------------------------------------------------------------------------------------------------|")
  
  linha<- sprintf("|             Intervalo [a,b] = [ % 12.8e, % 12.8e ]     Erro Máximo = [ % 12.8e ]                                 |",dValorAk,dValorBk,dValorErro)                                            
  print(linha)
  print("|------------------------------------------------------------------------------------------------------------------------------------------|")
  print("|         ak        |         bk        |     << xk >>      |      f(ak)        |      f(bk)       |       f(xk)       |        Erro       |")                         
  print("|------------------------------------------------------------------------------------------------------------------------------------------|")
  linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e  | % 12.8e   | % 12.8e   |",dValorAk,dValorBk,dXk,dfAk,dfBk,dfXk,abs(dValorBk-dValorAk))  
  print(linha)
  print("|-------------------|-------------------|-------------------|-------------------|------------------|-------------------|-------------------|")
  
  while ( is.null(iIter) & ((abs(dValorBk-dValorAk) > dValorErro )) | ( !is.null(iIter) & (iConta < iTotalIteracoes) ) ) 
  {
    iConta <- iConta + 1
    if((dfAk > 0 & dfXk > 0) | (dfAk < 0 & dfXk < 0) )
    {
      dValorAk <- dXk
      dfAk<-  funcao_de_X(dValorAk) 
      
    }
    if((dfBk > 0 & dfXk > 0) | (dfBk < 0 & dfXk < 0) )
    {
      dValorBk <- dXk
      dfBk<-  funcao_de_X(dValorBk)
    }
    
    dXk <-  0.5*(dValorAk + dValorBk)  
    dfXk<-  funcao_de_X(dXk)
    
    # linha do dataframe a ser inserida
    dfB_temp<-data.frame( iConta,dValorAk,dValorBk,dXk,dfXk)
    # renomeia colunas do dataframe para o bind com dfBissecao
    names(dfB_temp)  <- c("iCount","dAk","dBk", "dXk","dfXk") 
    dfBissecao <- rbind( dfBissecao, dfB_temp)  # salva 1a iteracao no dataframe    
    
    linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e  | % 12.8e   | % 12.8e   |",dValorAk,dValorBk,dXk,dfAk,dfBk,dfXk,abs(dValorAk-dValorBk))  
    
    print(linha)
    print("|-------------------|-------------------|-------------------|-------------------|------------------|-------------------|-------------------|")
  }
  
  # dataframe com ak, bk, xk e f(xk)
  dfComparaAlgos<- data.frame(cType <- character(),
                              Itera <- integer(),
                              dXk   <-double(),
                              dErro <-double()
  )
  
  for(ik in seq_len(nrow(dfBissecao)))
  {
    df_temp<-data.frame( "Bisseccao",dfBissecao$iCount[ik] ,dfBissecao$dXk[ik], abs(dfBissecao$dAk[ik] - dfBissecao$dBk[ik]) )
    # renomeia colunas do dataframe para o bind com dfPlot (tem que ter os mesmos nomes)
    names(df_temp)  <- c("cType", "Itera","dXk", "dErro") 
    dfComparaAlgos <- rbind( dfComparaAlgos, df_temp )   
    
  }
  
  return(dfComparaAlgos)
  
}


#============================================================================
# FUNCAO funcao_ZeroNewtonRaphson
#
# Parametros de Entradas
# dValorX    --> valor inicial de x
# dValorErro --> valor maximo de erro (para de iterar se sero < dValorErro & iIter=NULL)
# iIter=NULL  --> se n?o for null, ignora dValorErro e itera at? iIter
#
# # Parametros de Sa?da
# dataframe com o tipo de algo, erro, e ?ltimo valor de xk
#
#============================================================================
funcao_ZeroNewtonRaphson <- function(dValorX,dValorErro,iIter=NULL)
{
  
  
  iTotalIteracoes <- 999
  
  if(!is.null(iIter))
  {
    iTotalIteracoes <- iIter
  }
  
  iConta<-1
  
  # dataframe com xk e xk_1
  dfNR<- data.frame(          iCount<- integer(),
                              dXk  <-double(),
                              dXk_1  <-double() 
  )
  
  dfXk<-  funcao_de_X(dValorX) 
  dDer_Xk<-  funcao_Derivada_de_X(dValorX) 
  dValorXk_1 <- dValorX - (dfXk/dDer_Xk)
  
  # linha do dataframe a ser inserida
  dfNR_temp<-data.frame( iConta,dValorX,dValorXk_1 )
  # renomeia colunas do dataframe para o bind com dfBissecao
  names(dfNR_temp)  <- c("iCount","dXk","dXk_1") 
  dfNR <- rbind( dfNR, dfNR_temp)  # salva 1a iteracao no dataframe
  
  #imprime tabela
  print("|-----------------------------------------------------------|")
  print("|  ZERO DE FUNCAO REAL - MÉTODO DE NEWTON RAPHSON           |")
  print("|-----------------------------------------------------------|")
  
  linha<- sprintf("| Xk = [ % 12.8e ]  Erro Máx. = [ % 12.8e ] |",dValorX, dValorErro)                                            
  print(linha)
  print("|-----------------------------------------------------------|")
  print("|         xk        |      <<xk_1>>     |       Erro        |                                                                              |")
  print("|-----------------------------------------------------------|")
  linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   |",dValorX,dValorXk_1,abs(dValorX-dValorXk_1))  
  print(linha)
  print("|-------------------|-------------------|-------------------|")
  
  while ( is.null(iIter) & ((abs(dValorX-dValorXk_1) > dValorErro )) | ( !is.null(iIter) & (iConta < iTotalIteracoes) ) ) 
  {
    iConta <- iConta + 1
    dValorX <- dValorXk_1
    dfXk<-  funcao_de_X(dValorX) 
    dDer_Xk<-  funcao_Derivada_de_X(dValorX) 
    dValorXk_1 <- dValorX - (dfXk/dDer_Xk) 
    
    # linha do dataframe a ser inserida
    dfNR_temp<-data.frame( iConta,dValorX,dValorXk_1 )
    # renomeia colunas do dataframe para o bind com dfBissecao
    names(dfNR_temp)  <- c("iCount","dXk","dXk_1") 
    dfNR <- rbind( dfNR, dfNR_temp)  # salva 1a iteracao no dataframe
    
    linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   |",dValorX,dValorXk_1,abs(dValorX-dValorXk_1))  
    print(linha)
    print("|-------------------|-------------------|-------------------|")
  }
  
  # dataframe com dados do algoritmo
  dfComparaAlgos<- data.frame(cType <- character(),
                              Itera <- integer(),
                              dXk   <-double(),
                              dErro <-double()
  )
  
  for(ik in seq_len(nrow(dfNR)))
  {
    df_temp<-data.frame( "Newton Raphson",dfNR$iCount[ik],dfNR$dXk[ik], abs(dfNR$dXk[ik] - dfNR$dXk_1[ik]) )
    # renomeia colunas do dataframe para o bind com dfPlot (tem que ter os mesmos nomes)
    names(df_temp)  <- c("cType", "Itera","dXk", "dErro") 
    dfComparaAlgos <- rbind( dfComparaAlgos, df_temp )   
    
  }
  
  return(dfComparaAlgos)
  
}





#============================================================================
# FUNCAO funcao_ZeroSecante
#
# Parametros de Entradas
# dValorXk_m1   --> valor inicial de x0
# dValorXk_0    --> valor inicial de x1
# dValorErro --> valor maximo de erro (para de iterar se sero < dValorErro & iIter=NULL)
# iIter=NULL  --> se não for null, ignora dValorErro e itera até iIter
#
# # Parametros de Saída
# dataframe com o tipo de algo, erro, e último valor de xk
#
#============================================================================
funcao_ZeroSecante <- function(dValorXk_m1,dValorXk_0,dValorErro,iIter=NULL)
{
  iTotalIteracoes <- 999
  
  if(!is.null(iIter))
  {
    iTotalIteracoes <- iIter
  }
  
  iConta<-1
  
  # dataframe com xk e xk_1
  dfSecante<- data.frame(           iCount<- integer(),
                                    dXk_m1  <-double(),
                                    dXk_0   <-double(),
                                    dXk_1   <-double() 
  )
  
  # calcula 1a iteracao
  dfXk_m1<-  funcao_de_X(dValorXk_m1) 
  dfXfk_0<-  funcao_de_X(dValorXk_0) 
  dValorXk_1 <- ( (dValorXk_m1)*dfXfk_0 - ( dValorXk_0*dfXk_m1 ) )/(dfXfk_0 - dfXk_m1 )
  
  # linha do dataframe a ser inserida
  dfSec_temp<-data.frame( iConta,dValorXk_m1,dValorXk_0,dValorXk_1 )
  # renomeia colunas do dataframe para o bind com dfBissecao
  names(dfSec_temp)  <- c("iCount","dXk_m1","dXk_0","dXk_1") 
  dfSecante <- rbind( dfSecante, dfSec_temp)  # salva 1a iteracao no dataframe
  
  #imprime tabela
  print("|-------------------------------------------------------------------------------|")
  print("|        ZERO DE FUNCAO REAL - MÉTODO DA SECANTE                                |")
  print("|-------------------------------------------------------------------------------|")
  
  linha<- sprintf("| [xo,x1] = [ % 12.8e, % 12.8e ] Erro Máx. = [ % 12.8e ]|",dValorXk_m1,dValorXk_0,dValorErro)                                            
  print(linha)
  print("|-------------------------------------------------------------------------------|")
  print("|         xk-1      |         xk        |      <<xk+1>>     |        Erro       |")                         
  print("|-------------------------------------------------------------------------------|")
  linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e   |",dValorXk_m1,dValorXk_0,dValorXk_1,abs(dValorXk_0 - dValorXk_1))  
  print(linha)
  print("|-------------------|-------------------|-------------------|-------------------|")
  
  while ( is.null(iIter) & ((abs(dValorXk_0 - dValorXk_1) > dValorErro )) | ( !is.null(iIter) & (iConta < iTotalIteracoes) ) ) 
  {
    iConta <- iConta + 1
    
    # atualiza valores
    dValorXk_m1 <- dValorXk_0
    dValorXk_0  <- dValorXk_1
    
    
    dfXk_m1<-  funcao_de_X(dValorXk_m1) 
    dfXfk_0<-  funcao_de_X(dValorXk_0) 
    
    dValorXk_1 =  ( (dValorXk_m1)*dfXfk_0 - ( dValorXk_0*dfXk_m1 ) )/(dfXfk_0 - dfXk_m1 )
    
    # se diferen?a muito pequena, f(xk_0) - f(Xk_m1) -->0 e valor tende a Inf
    if(is.nan(dValorXk_1))
    {
      dValorXk_1<-dValorXk_0
    }
    
    # linha do dataframe a ser inserida
    dfSec_temp<-data.frame( iConta,dValorXk_m1,dValorXk_0,dValorXk_1 )
    # renomeia colunas do dataframe para o bind com dfBissecao
    names(dfSec_temp)  <- c("iCount","dXk_m1","dXk_0","dXk_1") 
    dfSecante <- rbind( dfSecante, dfSec_temp)  # salva 1a iteracao no dataframe  
    
    linha<- sprintf("| % 12.8e   | % 12.8e   | % 12.8e   | % 12.8e   |",dValorXk_m1,dValorXk_0,dValorXk_1,abs(dValorXk_0 - dValorXk_1))  
    
    print(linha)
    print("|-------------------|-------------------|-------------------|-------------------|")
  }
  
  # dataframe com dados do algoritmo
  dfComparaAlgos<- data.frame(cType <- character(),
                              Itera <- integer(),
                              dXk   <-double(),
                              dErro <-double()
  )
  
  for(ik in seq_len(nrow(dfSecante)))
  {
    df_temp<-data.frame( "Secante",dfSecante$iCount[ik],dfSecante$dXk_1[ik], abs(dfSecante$dXk_0[ik] - dfSecante$dXk_1[ik]) )
    # renomeia colunas do dataframe para o bind com dfPlot (tem que ter os mesmos nomes)
    names(df_temp)  <- c("cType", "Itera","dXk", "dErro") 
    dfComparaAlgos <- rbind( dfComparaAlgos, df_temp )   
  }
  
  return(dfComparaAlgos)
  
}




###########
#TODOS
###########
ITERACOES<-18   # define o nr de iteracoes desejadas
ERRO<-0.01       # define erro max (SE ITERACOES DIFERENTE DE NULL, IGNORA ESSE PARAMETRO)
###########
#BISSECCAO
###########
VALOR_A <- 1
VALOR_B <- 2
################
#NEWTON RAPHSON
################
NR_Xo<-1.5
################
#SECANTE
################
Sec_Xo <- 3
Sec_X1 <- 4


# monta dataframe com resultados das execucoes
dfResultados <- funcao_ZeroNewtonRaphson(NR_Xo,ERRO,ITERACOES)
dfResultados <- rbind( dfResultados, funcao_ZeroSecante(Sec_Xo,Sec_X1,ERRO,ITERACOES) )   
dfResultados <- rbind( dfResultados, funcao_ZeroBissecao(VALOR_A,VALOR_B,ERRO,ITERACOES))   


#converte cType em factor (gr?fico multiplas linhas)
dfResultados$cType<-factor(dfResultados$cType)

#require(ggplot2) ja chamei a lib no comeco do codigo

# imprime resultados no mesmo gr?fico
ggplot(dfResultados,aes(y=dErro, x=Itera, colour=cType,group=cType)) +ggtitle("Comparação Algos - Zero Função Real") + 
  geom_line() + geom_point()+ ylab("Erro") + xlab("Iteração") + labs(color = "Algoritmo") +
  scale_x_continuous(breaks = seq_len(ITERACOES))


# retorna console
sink(NULL, type = "output")

