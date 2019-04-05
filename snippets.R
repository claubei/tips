
#Predecir una variable no definida a partir del resto
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova") #anova continuas
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

#remover outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
dt$i <- remove_outliers(dt$i)

#comparación de histogramas
library(ggplot2)
library(scales)
histogram_comparison <- function(dt1, dt2, column, title = ' ', n = 15, tipo = mean, print = F) {
  p1 <- ggplot() + 
    geom_histogram( data = dt1[id %in% dt2[,id]],  aes(x=eval(column) , fill = "r", colour = "r"), bins = 40 , alpha = 0.2)  + geom_vline(xintercept=tipo(dt1[id %in% dt2[,id]][,eval(column) ],na.rm = T), color = "red") +
    geom_histogram( data = dt2[id %in% dt1[,id]],  aes(x=eval(column) , fill = "b", colour = "b"), bins = 40, alpha = 0.2)  + geom_vline(xintercept=tipo(dt2[id %in% dt1[,id]][,eval(column) ],na.rm = T), color = "blue") +
    scale_colour_manual(name=" ", values=c("r" = "red", "b"="blue"), labels=c( "r"="grupo mes 1", "b"="grupo mes 3" )) +
    scale_fill_manual(name=" ", values=c("r" = "red", "b"="blue"), labels=c( "r"="grupo mes 1", "b"="grupo mes 3" )) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = n)) + labs(title=title,x =column) + theme_bw()
  if(print) {
    print(paste0(title,', media 1er: ',round(mean(dt1[id %in% dt2[,id]][,eval(column) ],na.rm = T))))
    print(paste0(title,', media 2do: ',round(mean(dt2[id %in% dt1[,id]][,eval(column) ],na.rm = T))))
    print(paste0(title,', mediana 1er: ',round(median(dt1[id %in% dt2[,id]][,eval(column) ],na.rm = T))))
    print(paste0(title,', mediana 2do: ',round(median(dt2[id %in% dt1[,id]][,eval(column) ],na.rm = T))))
    print(paste0(title,', diff %: ',round((1-mean(dt2[id %in% dt1[,id]][,eval(column) ],na.rm = T)/mean(dt1[id %in% dt2[,id]][,eval(column) ],na.rm = T))*100,2)))
  }
  return(p1)
}
histogram_comparison(dt1, dt2, quote(farma), title = '1 v 3' , n = 5, print = F)

#datarobot consumer de modelos deployados
library(httr)
library(rjson)
library(plyr)
library(data.table)

base <- 'https://company.orm.zone.datarobot.com/predApi/v1.0/deployments/'
endpoint <- '/predictions'

DEPLOYMENT_ID = 'asdfasdfasdfasdf'
API_TOKEN = 'ASfsadfASdfASdfSAdfdsfASDf'
USERNAME = 'SADfdsafasddsf@company.com'

url <- paste0(base,DEPLOYMENT_ID,endpoint)

datarobot_consumer <- function(df) {
  data <- toJSON(unname(split(df, 1:nrow(df))))
  r <- POST(url,
            body = data,
            query = list(passthroughColumns = "uid"),
            add_headers(`Content-Type`= "application/json; charset=UTF-8", `datarobot-key`= "aasdfdfsa-asdfsadfdsa-asdfsafd-asdfasdfd"),
            authenticate(USERNAME, API_TOKEN), encode = "json")
  
  ct <- do.call("rbind.fill", lapply(content(r)$data, as.data.frame)) 
  colnames(ct) <- c('prob_y', 'prob_n','pred','id')
  dt <- data.table(ct)[,.(uid, proby, probn, pred)]
  return(dt)
}

