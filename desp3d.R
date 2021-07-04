# habilitando argumentos en R
args = commandArgs(trailingOnly=TRUE)

fileInput <- paste(args[1], ".dat", sep="")

#open .dat
con <- file(fileInput, "r")
linn <- readLines(con)

#variables estado
contador <- 1
estado <- 0

#variables del proceso
EI <- 0
Fx <- 0
Fy <- 0
Fz <- 0

TotalCoordinates <- 0
TotalElements <- 0
TotalDirichletCondx <- 0
TotalDirichletCondy <- 0
TotalDirichletCondz <- 0
TotalNeumannCond <- 0

coordinatesM <- NULL
elementsM <- NULL
dirichletxM <- NULL
dirichletyM <- NULL
dirichletzM <- NULL
neumannM <- NULL

for (i in 1:length(linn)){
   if(i == 1){
       
       auxlin <- strsplit(linn[i],' ')[[1]]
       
      
       EI <- as.numeric(auxlin[1])
       Fx <- as.numeric(auxlin[2])
       Fy <- as.numeric(auxlin[3])
       Fz <- as.numeric(auxlin[4])
       
       
   }

   if(i == 2){
       auxlin <- strsplit(linn[i],' ')[[1]]
       
       TotalCoordinates <- as.numeric(auxlin[1])
       TotalElements <- as.numeric(auxlin[2])
       TotalDirichletCondx <- as.numeric(auxlin[3])
       TotalDirichletCondy <- as.numeric(auxlin[4])
       TotalDirichletCondz <- as.numeric(auxlin[5])
       TotalNeumannCond <- as.numeric(auxlin[6])

       coordinatesM <- matrix(data = 0, nrow = TotalCoordinates, ncol = 4)
       elementsM <- matrix(data = 0, nrow = TotalElements, ncol = 11)
       dirichletxM <- matrix(data = 0, nrow = TotalDirichletCondx, ncol = 2)
       dirichletyM <- matrix(data = 0, nrow = TotalDirichletCondy, ncol = 2)
       dirichletzM <- matrix(data = 0, nrow = TotalDirichletCondz, ncol = 2)
       neumannM <- matrix(data = 0, nrow = TotalNeumannCond, ncol = 2)
   }

   if(linn[i] == "EndCoordinates"){
       estado <- 0
       contador <- 1
   }
   if(linn[i] == "EndElements"){
       estado <- 0
       contador <- 1
   }
   if(linn[i] == "EndDirichlet_x"){
       estado <- 0
       contador <- 1
   }
    if(linn[i] == "EndDirichlet_y"){
       estado <- 0
       contador <- 1
   }
    if(linn[i] == "EndDirichlet_z"){
       estado <- 0
       contador <- 1
   }

   if(linn[i] == "EndNeumann"){
       estado <- 0
       contador <- 1

   }


   if(estado == 1){
       auxlin <- strsplit(linn[i]," +")[[1]]
       coordinatesM[contador,1] = as.numeric(auxlin[1])
       coordinatesM[contador,2] = as.numeric(auxlin[2])
       coordinatesM[contador,3] = as.numeric(auxlin[3])
       coordinatesM[contador,4] = as.numeric(auxlin[4])
       contador <- contador + 1
   }

   if(estado == 2){
       auxlin <- strsplit(linn[i]," +")[[1]]
       elementsM[contador,1] = as.numeric(auxlin[1])
       elementsM[contador,2] = as.numeric(auxlin[2])
       elementsM[contador,3] = as.numeric(auxlin[3])
       elementsM[contador,4] = as.numeric(auxlin[4])
       elementsM[contador,5] = as.numeric(auxlin[5])
       elementsM[contador,6] = as.numeric(auxlin[6])
       elementsM[contador,7] = as.numeric(auxlin[7])
       elementsM[contador,8] = as.numeric(auxlin[8])
       elementsM[contador,9] = as.numeric(auxlin[9])
       elementsM[contador,10] = as.numeric(auxlin[10])
       elementsM[contador,11] = as.numeric(auxlin[11])
       contador <- contador + 1
   }

   if(estado == 3){
       auxlin <- strsplit(linn[i]," +")[[1]]
       dirichletxM[contador,1] = as.numeric(auxlin[1])
       dirichletxM[contador,2] = as.numeric(auxlin[2])
       
       contador <- contador + 1
   }

   if(estado == 4){
       auxlin <- strsplit(linn[i]," +")[[1]]
       dirichletyM[contador,1] = as.numeric(auxlin[1])
       dirichletyM[contador,2] = as.numeric(auxlin[2])
       
       contador <- contador + 1
   }
   
   if(estado == 5){
       auxlin <- strsplit(linn[i]," +")[[1]]
       dirichletzM[contador,1] = as.numeric(auxlin[1])
       dirichletzM[contador,2] = as.numeric(auxlin[2])
       
       contador <- contador + 1
   }


   if(estado == 6){
       auxlin <- strsplit(linn[i]," +")[[1]]
       neumannM[contador,1] = as.numeric(auxlin[1])
       neumannM[contador,2] = as.numeric(auxlin[2])
       
       contador <- contador + 1
   }


   if(linn[i] == "Coordinates"){
       estado <- 1
   }
   

   if(linn[i] == "Elements"){
       estado <- 2
   }

   if(linn[i] == "Dirichlet_x"){
       estado <- 3
   }

   if(linn[i] == "Dirichlet_y"){
       estado <- 4
   }

   if(linn[i] == "Dirichlet_z"){
       estado <- 5
   }

   if(linn[i] == "Neumann"){
       estado <- 6

   }
   
}

close(con)
#matriz de coordenadas
# print(coordinatesM) 
#matriz elementos
# print(elementsM)

# print(dirichletxM)
# print(dirichletyM) 
# print(dirichletzM) 
# print(neumannM) 
listaNodos <- vector("list", length = TotalCoordinates)
listaElements <- vector("list", length = TotalElements)

for (i in 1:TotalCoordinates) {
   listaNodos[[i]] <- c(coordinatesM[i,2:4])
   
}


for (i in 1:TotalElements) {
    listaElements[[i]] <- vector("list", length = 11)
    listaElements[[i]][[1]] = elementsM[i,1]
   for (j in 2:11) {
      listaElements[[i]][[j]] = list(elementsM[i,j],listaNodos[[elementsM[i,j]]])
   }
}



kfinal <- matrix(data = 0, nrow = TotalCoordinates * 3, ncol = TotalCoordinates * 3)
bfinal <- matrix(data=0, nrow= TotalCoordinates * 3, ncol=1)

for (i in 1:TotalElements) {
    jm <- matrix(data = 0, nrow = 3, ncol = 3)
    
    jm[1,1] = listaElements[[i]][3][[1]][[2]][1] - listaElements[[i]][2][[1]][[2]][1]
    jm[1,2] = listaElements[[i]][4][[1]][[2]][1] - listaElements[[i]][2][[1]][[2]][1]
    jm[1,3] = listaElements[[i]][5][[1]][[2]][1] - listaElements[[i]][2][[1]][[2]][1]

    jm[2,1] = listaElements[[i]][3][[1]][[2]][2] - listaElements[[i]][2][[1]][[2]][2]
    jm[2,2] = listaElements[[i]][4][[1]][[2]][2] - listaElements[[i]][2][[1]][[2]][2]
    jm[2,3] = listaElements[[i]][5][[1]][[2]][2] - listaElements[[i]][2][[1]][[2]][2]

    jm[3,1] = listaElements[[i]][3][[1]][[2]][3] - listaElements[[i]][2][[1]][[2]][3]
    jm[3,2] = listaElements[[i]][4][[1]][[2]][3] - listaElements[[i]][2][[1]][[2]][3]
    jm[3,3] = listaElements[[i]][5][[1]][[2]][3] - listaElements[[i]][2][[1]][[2]][3]
    

    ja <- det(jm)
    
    c1 <- 1/((listaElements[[i]][3][[1]][[2]][1] - listaElements[[i]][2][[1]][[2]][1])^2)
    c2 <- ((4 * listaElements[[i]][2][[1]][[2]][1]) + (4*listaElements[[i]][3][[1]][[2]][1]) - (8*listaElements[[i]][9][[1]][[2]][1]))/(listaElements[[i]][3][[1]][[2]][1] - listaElements[[i]][2][[1]][[2]][1])
    
    if(c2 == Inf){
        print("Hay Error cosita")
    }
    

    k <- (-4 * (c1 * c2))/3
    j <- (2*(c2^2))/15
    ii <-  -((16 * (c1^2))/3) - ((2 * (c2^2))/3)
    h <- ((2/3)*(c1 * c2)) + ((1/30) * (c2^2))
    g <- -((16 * (c1^2))/3) - ((4/3) * (c1 * c2)) - ((2/15) * (c2^2))
    f <- ((2/3) * (c1 * c2)) - ((1/30) * (c2^2))
    e <- ((8/3) * (c1^2)) + ((1/30) * (c2^2))
    d <- ((1/(192 * (c2^2))) * (((4 * c2) - (c1))^4) ) - ((1/(3840 * (c2^3))) * (((4 * c2) - (c1))^5) ) + ((1/(7680 * (c2^3))) * (((4 * c2) + (8*c1))^5) ) - ((7/(7680 * (c2^3))) * (((4 * c2) - (8*c1))^5) ) + ((1/(768 * (c2^3))) * ((-8 * c1)^5) ) - ((c1/(96 * (c2^3))) * (((4 * c2) - (-8*c1))^4) ) + ((((2*c1) - 1)/(192 * (c2^3))) * (((-8*c1))^4)) 
    c <- (4/15) * (c2^2)
    b <- -((1/(192 * (c2^2))) * (((4 * c1) + (c2))^4) ) + ((1/(24 * (c2))) * (((4 * c1) + (c2))^3) ) + ((1/(3840 * (c2^3))) * (((4 * c1) + (c2))^5) ) - ((1/(3840 * (c2^3))) * (((4 * c1) - (3* c2))^5) )
    a <- -((1/(192 * (c2^2))) * (((4 * c1) - (c2))^4) ) - ((1/(24 * (c2))) * (((4 * c1) - (c2))^3) ) - ((1/(3840 * (c2^3))) * (((4 * c1) - (c2))^5) ) + ((1/(3840 * (c2^3))) * (((4 * c1) + (3* c2))^5) )

    # fila 

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * a
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * a
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * a

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * e
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * e
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * e

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * -f
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * -f
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * -f

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * -f
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * -f
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * -f

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * g
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * g
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * g

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * f
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * f
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * f

    kfinal[listaElements[[i]][2][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * f
    kfinal[listaElements[[i]][2][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * f
    kfinal[listaElements[[i]][2][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * f

    # fila 

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * e
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * e
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * e

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * b
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * b
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * b

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * -h
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * -h
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * -h

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * -h
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * -h
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * -h

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * ii
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * ii
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * ii

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * h
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * h
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * h

    kfinal[listaElements[[i]][3][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * h
    kfinal[listaElements[[i]][3][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * h
    kfinal[listaElements[[i]][3][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * h

    # fila x

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * -f
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * -f
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * -f

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * -h
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * -h
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * -h

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * c
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * c
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * c

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * j
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * j
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * j

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * -k
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * -k
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * -k

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * -c
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * -c
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * -c

    kfinal[listaElements[[i]][6][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * -j
    kfinal[listaElements[[i]][6][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * -j
    kfinal[listaElements[[i]][6][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * -j

    # fila 

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * -f
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * -f
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * -f

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * -h
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * -h
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * -h

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * j
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * j
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * j

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * c
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * c
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * c

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * -k
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * -k
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * -k

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * -j
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * -j
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * -j

    kfinal[listaElements[[i]][8][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * -c
    kfinal[listaElements[[i]][8][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * -c
    kfinal[listaElements[[i]][8][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * -c

# 3

    # fila 

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * g
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * g
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * g

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * ii
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * ii
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * ii

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * -k
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * -k
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * -k

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * -k
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * -k
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * -k

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * d
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * d
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * d

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * k
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * k
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * k

    kfinal[listaElements[[i]][9][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * k
    kfinal[listaElements[[i]][9][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * k
    kfinal[listaElements[[i]][9][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * k

    # fila x

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * f
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * f
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * f

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * h
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * h
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * h

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * -c
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * -c
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * -c

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * -j
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * -j
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * -j

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * k
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * k
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * k

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * c
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * c
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * c

    kfinal[listaElements[[i]][10][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * j
    kfinal[listaElements[[i]][10][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]*2] = EI * ja * j
    kfinal[listaElements[[i]][10][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * j

    # fila 

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][2][[1]][[1]]] = EI * ja * f
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][2][[1]][[1]]*2] = EI * ja * f
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][2][[1]][[1]]*3] = EI * ja * f

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][3][[1]][[1]]] = EI * ja * h
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][3][[1]][[1]]*2] = EI * ja * h
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][3][[1]][[1]]*3] = EI * ja * h

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][6][[1]][[1]]] = EI * ja * -j
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][6][[1]][[1]]*2] = EI * ja * -j
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][6][[1]][[1]]*3] = EI * ja * -j

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][8][[1]][[1]]] = EI * ja * -c
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][8][[1]][[1]]*2] = EI * ja * -c
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][8][[1]][[1]]*3] = EI * ja * -c

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][9][[1]][[1]]] = EI * ja * k
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][9][[1]][[1]]*2] = EI * ja * k
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][9][[1]][[1]]*3] = EI * ja * k

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][10][[1]][[1]]] = EI * ja * j
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][10][[1]][[1]]*2] = EI * ja * j
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][10][[1]][[1]]*3] = EI * ja * j

    kfinal[listaElements[[i]][11][[1]][[1]],listaElements[[i]][11][[1]][[1]]] = EI * ja * c
    kfinal[listaElements[[i]][11][[1]][[1]]*2,listaElements[[i]][11][[1]][[1]]+10] = EI * ja * c
    kfinal[listaElements[[i]][11][[1]][[1]]*3,listaElements[[i]][11][[1]][[1]]*3] = EI * ja * c

    # b final

    

}
