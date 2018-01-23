library(genalg)

separateIndexFromValue <- function(data_frame_to_clear){
  number_of_rows = length(data_frame_to_clear[,1])
  DT <- data.frame(
                   V1_index = data_frame_to_clear[,1],
                   V1_value = matrix(1,number_of_rows),
                   V2_index = data_frame_to_clear[,2],
                   v2_value = matrix(1,number_of_rows),
                   V3_index = data_frame_to_clear[,3],
                   V3_value = matrix(1,number_of_rows)
                   )
  return(DT)
}

fixMinusAndValue <- function(DT){
  for (i in 1:length(DT[,1])){
    for (j in seq(1, 6, 2)){
      if (DT[i,j] < 0){
        DT[i,j] = abs(DT[i,j])
        DT[i,j+1] = 0
      }
    }
  }
  return(DT)
}

cleanData <- function(data_frame_to_clear){
  DT = separateIndexFromValue(data_frame_to_clear)
  DT = fixMinusAndValue(DT)
}

fitnessFunc <- function(chromosome = c(), DT = cleanDataFrame){
  value = 0
  number_of_rows = length(DT[,1])

  for (i in 1:number_of_rows){
      if (chromosome[DT[i,1]] == DT[i,2] || chromosome[DT[i,3]] == DT[i,4] || chromosome[DT[i,5]] == DT[i,6]){
        value = value -1
      }
  }

  return(value)
}

executeAlogithm <- function(file_string){
  print(file_string)
  data10 = read.table(file_string)
  cleanDataFrame <<- cleanData(data10)
  start.time <- Sys.time()
  genAlg = rbga.bin(size = 10, popSize = 20 , iters = 20, mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(tail(genAlg$best, 1))
  summary(genAlg, echo = TRUE)
  print(time.taken)
}

executeAlogithm("clause10.dimacs")
executeAlogithm("clause20.dimacs")
executeAlogithm("clause50.dimacs")
