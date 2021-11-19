#Rosalind
#En un alfabeto ponderado , a cada s?mbolo se le asigna un n?mero real positivo llamado peso . Una cuerda formada a partir de un alfabeto ponderado se llama cuerda ponderada y su peso es igual a la suma de los pesos de sus s?mbolos.
#El peso est?ndar asignado a cada miembro del alfabeto de amino?cidos de 20 s?mbolos es la masa monoisot?pica del amino?cido correspondiente.
#Dado: una cadena de prote?nas PAG de longitud como m?ximo 1000 aa .
#Devoluci?n: el peso total de PAG. Consulte la tabla de masas monoisot?picas.

#Cada letra del alfabeto corresponde a un aminoacido que forman cadenas de proteinas
#Primero se le asigna un valor real de acuerdo a la tabla de masas monoisot?picas que viene en Rosalind
#El valor asignado corresponde al peso estandar de cada aminoacido
A <- 71.03711
R <- 156.10111
N <- 114.04293
D <- 115.02694
C <- 103.00919
Q <- 128.05858
E <- 129.04259
G <- 57.02146
H <- 137.05891
I <- 113.08406
L <- 113.08406
K <- 128.09496
M <- 131.04049
eFe <- 147.06841 #si pones solo F lo reconoce como False
P <- 97.05276
S <- 87.03203
Te <- 101.04768 #si pones solo T lo reconoce como True
W <- 186.07931
Y <- 163.06333
V <- 99.06841

#Ahora que ya tienes una variable para cada uno vamoa a juntarlo en un vector de trabajo
alfabeto <- c(1:20) #juntas en un vector todos los pesos
alfabeto_aa <- c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V") #creas un vector con el nombre de los aa
names(alfabeto) <- alfabeto_aa #asignas el nombre de los aminoacidos al vector que contiene los pesos
alfabeto #este vector es de donde puedes seleccionar la letra del aa y su peso correspondiente

#creamos condicionales para que el usuario seleccione el conjunto de aa que sumara a la cadena total
construccion_cadena <- function(){
  print("De la siguiente tabla, elige los numeros que corresponden a las letras de los aminoacidos con los cuales quieres formar tu proteina")
  print(alfabeto)
  aa <- readline(prompt = "Anota el numero primer aminoacido: ")
  aa <- as.numeric(aa)
if (aa == 1){uno <- c(A)
  }else if (aa == 2){
    uno <- c(R)
  }else if (aa == 3){
    uno <- c(N)
  }else if (aa == 4){
    uno <- c(D)
  }else if (aa == 5){
    uno <- c(C)
  }else if (aa == 6){
    uno <- c(Q)
  }else if (aa == 7){
    uno <- c(E)
  }else if (aa == 8){
    uno <- c(G)
  }else if (aa == 9){
    uno <- c(H)
  }else if (aa == 10){
    uno <- c(I)
  }else if (aa == 11){
    uno <- c(L)
  }else if (aa == 12){
    uno <- c(K)
  }else if (aa == 13){
    uno <- c(M)
  }else if (aa == 14){
    uno <- c(eFe)
  }else if (aa == 15){
    uno <- c(P)
  }else if (aa == 16){
    uno <- c(S)
  }else if (aa == 17){
    uno <- c(Te)
  }else if (aa == 18){
    uno <- c(W)
  }else if (aa == 19){
    uno <- c(Y)
  }else if (aa == 20){
    uno <- c(V)
  }
  aa2 <- readline(prompt = "Anota el numero segundo aminoacido: ")
  aa2 <- as.numeric(aa2)
if (aa2 == 1){
    dos <- c(A)
  }else if (aa2 == 2){
    dos <- c(R)
  }else if (aa2 == 3){
    dos <- c(N)
  }else if (aa2 == 4){
    dos <- c(D)
  }else if (aa2 == 5){
    dos <- c(C)
  }else if (aa2 == 6){
    dos <- c(Q)
  }else if (aa2 == 7){
    dos <- c(E)
  }else if (aa2 == 8){
    dos <- c(G)
  }else if (aa2 == 9){
    dos <- c(H)
  }else if (aa2 == 10){
    dos <- c(I)
  }else if (aa2 == 11){
    dos <- c(L)
  }else if (aa2 == 12){
    dos <- c(K)
  }else if (aa2 == 13){
    dos <- c(M)
  }else if (aa2 == 14){
    dos <- c(eFe)
  }else if (aa2 == 15){
    dos <- c(P)
  }else if (aa2 == 16){
    dos <- c(S)
  }else if (aa2 == 17){
    dos <- c(Te)
  }else if (aa2 == 18){
    dos <- c(W)
  }else if (aa2 == 19){
    dos <- c(Y)
  }else if (aa2 == 20){
    dos <- c(V)
  }
  aa3 <- readline(prompt = "Anota el numero tercer aminoacido: ")
  aa3 <- as.numeric(aa3)
  if (aa3 == 1){
    tres <- c(A)
  }else if (aa3 == 2){
    tres <- c(R)
  }else if (aa3 == 3){
    tres<- c(N)
  }else if (aa3 == 4){
    tres <- c(D)
  }else if (aa3 == 5){
    tres <- c(C)
  }else if (aa3 == 6){
    tres <- c(Q)
  }else if (aa3 == 7){
    tres <- c(E)
  }else if (aa3 == 8){
    tres <- c(G)
  }else if (aa3 == 9){
    tres <- c(H)
  }else if (aa3 == 10){
    tres <- c(I)
  }else if (aa3 == 11){
    tres <- c(L)
  }else if (aa3 == 12){
    tres <- c(K)
  }else if (aa3 == 13){
    tres <- c(M)
  }else if (aa3 == 14){
    tres <- c(eFe)
  }else if (aa3 == 15){
    tres <- c(P)
  }else if (aa3 == 16){
    tres <- c(S)
  }else if (aa3 == 17){
    tres <- c(Te)
  }else if (aa3 == 18){
    tres <- c(W)
  }else if (aa3 == 19){
    tres <- c(Y)
  }else if (aa3 == 20){
    tres <- c(V)
  }
  aa4 <- readline(prompt = "Anota el numero cuarto aminoacido: ")
  aa4 <- as.numeric(aa4)
  if (aa4 == 1){
    cua <- c(A)
  }else if (aa4 == 2){
    cua <- c(R)
  }else if (aa4 == 3){
    cua <- c(N)
  }else if (aa4 == 4){
    cua <- c(D)
  }else if (aa4 == 5){
    cua <- c(C)
  }else if (aa4 == 6){
    cua <- c(Q)
  }else if (aa4 == 7){
    cua <- c(E)
  }else if (aa4 == 8){
    cua <- c(G)
  }else if (aa4 == 9){
    cua <- c(H)
  }else if (aa4 == 10){
    cua <- c(I)
  }else if (aa4 == 11){
    cua <- c(L)
  }else if (aa4 == 12){
    cua <- c(K)
  }else if (aa4 == 13){
    cua <- c(M)
  }else if (aa4 == 14){
    cua <- c(eFe)
  }else if (aa4 == 15){
    cua <- c(P)
  }else if (aa4 == 16){
    cua <- c(S)
  }else if (aa4 == 17){
    cua <- c(Te)
  }else if (aa4 == 18){
    cua <- c(W)
  }else if (aa4 == 19){
    cua <- c(Y)
  }else if (aa4 == 20){
    cua <- c(V)
  }
  aa5 <- readline(prompt = "Anota el numero quinto aminoacido: ")
  aa5 <- as.numeric(aa5)
  if (aa5 == 1){
    cin <- c(A)
  }else if (aa5 == 2){
    cin <- c(R)
  }else if (aa5 == 3){
    cin <- c(N)
  }else if (aa5 == 4){
    cin <- c(D)
  }else if (aa5 == 5){
    cin <- c(C)
  }else if (aa5 == 6){
    cin <- c(Q)
  }else if (aa5 == 7){
    cin <- c(E)
  }else if (aa5 == 8){
    cin <- c(G)
  }else if (aa5 == 9){
    cin <- c(H)
  }else if (aa5 == 10){
    cin <- c(I)
  }else if (aa5 == 11){
    cin <- c(L)
  }else if (aa5 == 12){
    cin <- c(K)
  }else if (aa5 == 13){
    cin <- c(M)
  }else if (aa5 == 14){
    cin <- c(eFe)
  }else if (aa5 == 15){
    cin <- c(P)
  }else if (aa5 == 16){
    cin <- c(S)
  }else if (aa5 == 17){
    cin <- c(Te)
  }else if (aa5 == 18){
    cin <- c(W)
  }else if (aa5 == 19){
    cin <- c(Y)
  }else if (aa5 == 20){
    cin <- c(V)
  }
  aa6 <- readline(prompt = "Anota el numero sexto aminoacido: ")
  aa6 <- as.numeric(aa6)
if (aa6 == 1){
    six <- c(A)
  }else if (aa6 == 2){
    six <- c(R)
  }else if (aa6 == 3){
    six <- c(N)
  }else if (aa6 == 4){
    six <- c(D)
  }else if (aa6 == 5){
    six <- c(C)
  }else if (aa6 == 6){
    six <- c(Q)
  }else if (aa6 == 7){
    six <- c(E)
  }else if (aa6 == 8){
    six <- c(G)
  }else if (aa6 == 9){
    six <- c(H)
  }else if (aa6 == 10){
    six <- c(I)
  }else if (aa6 == 11){
    six <- c(L)
  }else if (aa6 == 12){
    six <- c(K)
  }else if (aa6 == 13){
    six <- c(M)
  }else if (aa6 == 14){
    six <- c(eFe)
  }else if (aa6 == 15){
    six <- c(P)
  }else if (aa6 == 16){
    six <- c(S)
  }else if (aa6 == 17){
    six <- c(Te)
  }else if (aa6 == 18){
    six <- c(W)
  }else if (aa6 == 19){
    six <- c(Y)
  }else if (aa6 == 20){
    six <- c(V)
  }
  aa7 <- readline(prompt = "Anota el numero septimo aminoacido: ")
  aa7 <- as.numeric(aa7)
if (aa7 == 1){
    sev <- c(A)
  }else if (aa7 == 2){
    sev <- c(R)
  }else if (aa7 == 3){
    sev <- c(N)
  }else if (aa7 == 4){
    sev <- c(D)
  }else if (aa7 == 5){
    sev <- c(C)
  }else if (aa7 == 6){
    sev <- c(Q)
  }else if (aa7 == 7){
    sev <- c(E)
  }else if (aa7 == 8){
    sev <- c(G)
  }else if (aa7 == 9){
    sev <- c(H)
  }else if (aa7 == 10){
    sev <- c(I)
  }else if (aa7 == 11){
    sev <- c(L)
  }else if (aa7 == 12){
    sev <- c(K)
  }else if (aa7 == 13){
    sev <- c(M)
  }else if (aa7 == 14){
    sev <- c(eFe)
  }else if (aa7 == 15){
    sev <- c(P)
  }else if (aa7 == 16){
    sev <- c(S)
  }else if (aa7 == 17){
    sev <- c(Te)
  }else if (aa7 == 18){
    sev <- c(W)
  }else if (aa7 == 19){
    sev <- c(Y)
  }else if (aa7 == 20){
    sev <- c(V)
  }
return(print(suma <- uno + dos + tres + cua + cin + six + sev))
}

# SKADYEK se comprueba con este y si da 821.3919