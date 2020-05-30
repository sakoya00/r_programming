corr <- function(directory, threshold = 0){
  files2 <- list.files(directory, pattern = ".csv", full.names = TRUE)
  cor_vector <- numeric()
  
  for(i in 1:332){
    d <- read.csv(files2[i])
    if(sum(complete.cases(d))>threshold){
      cor_vector <- c(cor_vector, cor(d$sulfate, d$nitrate, use = "complete.obs"))
    }
  }
  return (cor_vector)
}