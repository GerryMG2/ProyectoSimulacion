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
TotalDirichletCond <- 0
TotalNeumannCond <- 0

coordinatesM <- NULL
elementsM <- NULL
dirichletM <- NULL
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
       TotalDirichletCond <- as.numeric(auxlin[3])
       TotalNeumannCond <- as.numeric(auxlin[4])

       coordinatesM <- matrix(data = 0, nrow = TotalCoordinates, ncol = 4)
       elementsM <- matrix(data = 0, nrow = TotalElements, ncol = 5)
       dirichletM <- matrix(data = 0, nrow = TotalDirichletCond, ncol = 2)
       neumannM <- matrix(data = 0, nrow = TotalNeumannCond, ncol = 2)
   }

   if(linn[i] == "EndCoordinates"){
       estado <- 0
       contador <- 0
   }
   if(linn[i] == "EndElements"){
       estado <- 0
       contador <- 0
   }
   if(linn[i] == "EndDirichlet"){
       estado <- 0
       contador <- 0

   }
   if(linn[i] == "EndNeumann"){
       estado <- 0
       contador <- 0

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
       contador <- contador + 1
   }

   if(estado == 3){
       auxlin <- strsplit(linn[i]," +")[[1]]
       dirichletM[contador,1] = as.numeric(auxlin[1])
       dirichletM[contador,2] = as.numeric(auxlin[2])
       
       contador <- contador + 1
   }

   if(estado == 4){
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

   if(linn[i] == "Dirichlet"){
       estado <- 3
   }

   if(linn[i] == "Neumann"){
       estado <- 4

   }
   
}

close(con)


print(coordinatesM) 
print(elementsM)
print(dirichletM) 
print(neumannM) 