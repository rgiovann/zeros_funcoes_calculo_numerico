


<h2 align="center">CALCULO DE ZEROS DE FUNÇÕES</h2>
<br>
<h3>Este programa implementa em R os algoritmos descritos no livro da Ruggiero de Cálculo Numérico cap 2: </h3> 
<br>
<h3>RUGGIERO, Márcia A. Gomes; LOPES, Vera Lúcia da Rocha. 
<b>Cálculo numérico: aspectos teóricos e computacionais.</b> Makron Books do Brasil, 1997.</h3> 
<h3>Os métodos implementados são:</h3>
<h3>
<ol>
    <li> Bisseção </li>
    <li> Secante </li>
    <li> Newton-Raphson </li> 
</ol>
</h3>

<h3>Para executar o código defina o diretório de trabalho, a função e a derivada desta função que se deseja achar os zeros:</h3>  
<br>

```sh
# diretorio de trabalho
setwd("G:/AULAS_ATUAL/Bagozzi/R_SOURCE/CALCULO_NUMERICO/CAP2_ZEROS_NR_REAIS")
 
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

```

<h3>Adicionalmente, defina o número de iterações, o erro máximo permitido e os valores iniciais para cada algoritmo:</h3>  
<br>

```sh
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

```  
<br>

<h3> Um gráfico (exemplo abaixo do código) é apresentado no final mostrando a evolução do erro a cada iteração.</h3> 

![gráfico erros metodos zero de função](https://github.com/rgiovann/image-repo/blob/main/zeros_erro.png)

