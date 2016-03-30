for (i in 1:length(unique_years)){
      year <- unique_years[i]
      my_data <- df[df$Year == year,]
      rownames(my_data) <- c(1:nrow(my_data))
      
      write.csv(my_data, paste(as.character(year),".csv",sep=""))
         
}#closing for loop

f <- function(years = 1960:2010, c_code){
      if(length(c_code)!=1 | length(years)<2){
            return("Incorrent inputs")
      }#closing if statement
      final_data <- data.frame()
      for(i in 1:length(years)){
            data_frame <- read.csv(paste(years[i],".csv",sep=""), header = TRUE)
            final_data <- rbind(final_data, data_frame[data_frame$Code == c_code,])
           
      }#closing for loop
      GDP <- final_data$GDP[!is.na(final_data)]
      GDP <- GDP[!is.na(GDP)]
      
      lm <- lm(final_data$GDP ~ final_data$Year)
      
      plot(final_data$Year,final_data$GDP, main = 
                 paste("GDP in", c_code, "between", 
                       years[1], "and", years[length(years)]
                       )
           )
      xx<- seq(years[1], years[length(years)], by = 0.1)
      yy <- lm$coefficients[1] + lm$coefficients[2] * xx
      
      lines(xx,yy)
      
      c(lm$coefficients[1], lm$coefficients[2])
}#closing fun
