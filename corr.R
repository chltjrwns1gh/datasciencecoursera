corr <- function(directory, threshold = 0){
    id <- 1:length(dir(directory))
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
    correlation_data <- c()
    df <- complete(directory) # making complete() data.frame to compare with threshold
    for(id_each in df$id[df$nobs>threshold]){ # only above threshold id -> will be read
        dir <- paste(directory, paste(id_3digit(id_each), '.csv', sep=''), sep='/')
        data_csv <- na.omit(read.csv(dir)) # NA가 포함된 행 제거
        correlation_data <- c(correlation_data, cor(data_csv$sulfate, data_csv$nitrate))
        
    }
    return(correlation_data)
    
}