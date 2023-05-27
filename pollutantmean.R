pollutantmean <- function(directory, pollutant, id = 1:332){
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
    data_extracted <- c()
    for(id_each in id){
        dir <- paste(directory, paste(id_3digit(id_each), '.csv', sep=''), sep='/')
        data_csv <- read.csv(dir)
        #print(head(data_csv$pollutant)) -> "nitrate"가 아닌 nitrate로 $뒤에 들어가야 해서 불가능
        #print(head(data_csv[, pollutant])) #-> 이렇게 해야 함
        data_extracted <- c(data_extracted, data_csv[, pollutant])
    }
    mean(data_extracted, na.rm = TRUE)
}