#A partir de los datos de insulina o glorias en formato Fasta elabora un program en R que genere lo siguiente:
  
  
#1. Alineamientos múltiples con al menos dos algoritmos
#2.Infiera árboles filogenéticos con dos métodos.
#3. Dibuje un árbol con sus respectivos alineamientos al lado (ver la documentación y viñetas de ggtree)
#4. Elabore un árbol con phylopic para indicar la especie correspondiente.
#En los dos casos el árbol debe estar con los nombre recortados para indicar sólo la especie. Debes realizar este proceso con R


#primero habrá que llamar a todas las librerías necesarias; 
#Es necesario para convertir los alineamientos 
library(seqinr)
library(ape) #Paraalgunos métodos de inferencia de filogenias, tiene funciones de análisis de filogenias
library(Biostrings)#para poder leer el archivo con las secuencias
library(msa)#para los alineamientos múltiples
library(phangorn)#para el método UPGMA por enfoque de distancia
library(ggtree)

#primero habrá que cargar todas las secuencias necesarias para poder trabajar, 
#en este caso reutilicé un concatenado de secuencias de insulinas del examen pasado, esto lo hice mediante
#bash

insulinas <-readAAStringSet("secuencias_concat.faa")
#adjunté el concatenado de secuencias que hice en bash
insulinas#imprimí el concatenado de secuencias

#ahora puedo hacer alineamientos múltiples con msa
alig1<-msa(insulinas, method = "Muscle") #le digo que con msa
#me haga un alineamiento mútiple, de insulinas, siguiendo el método MUSCLE
alig2<-msa(insulinas, method="ClustalOmega")
#Alineamiento múltiple, de insulinas, siguiendo el método ClustalOmega


#inferir árboles filogenéticos por 2 métodos
#1. A partir de una matriz de difererncias haré un UPGMA
#primero hay que convertir los alineamientos múltiples a clusters, o sea, para poder trabajar

clust.alig1<-msaConvert(alig1, type = "seqinr::alignment")
clust.alig2<-msaConvert(alig2, type = "seqinr::alignment")

clust.dist1<-dist.alignment(clust.alig1,"identity")
clust.dist2<-dist.alignment(clust.alig2,"identity")


#Ahora con las funciones de Ape y phagorn puedo hacer varios métodos de inferencia de árboles

arbol.met1<-nj(clust.dist1)
arbol.met1
#para interpretar puedo hacerlo con plot
arbolxmetodo1<-plot(arbol.met1, main="árbol filogenético x distancias, método neighbour Joining")

arbol.met2<-upgma(clust.dist2)
arbolxmetodo2<-plot(arbol.met2, main="árbol filogenético x distancias,  método UPGMA")


#3. Con ayuda de las funciones de ggtree iré personalizando los árboles que se generaron
arbol1mejorado<-ggtree(arbol.met1, layout = "dendrogram", branch.length="none", color="red", size=2, linetype=1)
arbol1mejorado
arbol1mejorado<-arbol1mejorado+geom_cladelab(data = insulinas, geom = "phylophic", label=node )
arbol1mejorado
