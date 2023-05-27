complete <- function(directory, id=1:332){
    id_3digit <-function(id_numeric){
        x <- if(0 < id_numeric & id_numeric < 10){
            paste('00', id_numeric, sep='')
        }else if(10<= id_numeric & id_numeric < 100){
            paste('0', id_numeric, sep='')
        }else{
            as.character(id_numeric)
        }
        return(x)
    }
    nobs <- c()
    for(id_each in id){
        dir <- paste(directory, paste(id_3digit(id_each), '.csv', sep=''), sep='/')
        data_csv <- read.csv(dir)
        nobs <- c(nobs, sum(complete.cases(data_csv)))
    }
    
    return(data.frame(id, nobs)) 
}