# habilitando argumentos en R
args = commandArgs(trailingOnly=TRUE)

fileInput <- paste(args[1], ".dat", sep="")

#open .dat
con <- file(fileInput, "r")
linn <- readLines(con)

#variables estado
contador <- 0
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
   }

   if(linn[i] == "EndCoordinates"){
       estado <- 0

   }
   if(linn[i] == "EndElements"){
       estado <- 0

   }
   if(linn[i] == "EndDirichlet"){
       estado <- 0

   }
   if(linn[i] == "EndNeumann"){
       estado <- 0

   }


   if(estado == 1){
       print(linn[i])
   }

   if(estado == 2){
       print(linn[i])
   }

   if(estado == 3){
       print(linn[i])
   }

   if(estado == 4){
       print(linn[i])
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

