rm(list = ls())
#library(haven)
library(AER)
library(sandwich)
library(lmtest)

setwd("/Users/dorothyyang/Desktop/26Fall/Econometric/rep")


#df <- read_dta("/Users/dorothyyang/Desktop/26Fall/Econometric/rep/final5.dta",encoding = "latin1")
#names(df)
#df <- df[, c("schlcode", "classize", "c_size", "tipuach", "avgmath", "avgverb")]
#write.csv(df,"repdf.csv",row.names = FALSE, fileEncoding = "UTF-8")
#---------------------------------------------------------------------
d<- read.csv("repdf.csv")

df <- subset(d, classize > 0 & classize < 45 & c_size > 5)

#Maimonides' rule
df$fsc  <- df$c_size / (floor((df$c_size - 1) / 40) + 1)
#discontinuity sample
df$disc <- df$c_size %in% c(36:45, 76:85, 116:125)           

#piecewise linear trend
df$trend <- with(df, ifelse(c_size <= 40,  c_size,
                            ifelse(c_size <= 80,  20 + c_size/2,
                                   ifelse(c_size <= 120, 100/3 + c_size/3,
                                          ifelse(c_size <= 160, 130/3 + c_size/4, NA)))))
df$c_size2 <- df$c_size^2/100

sum(!is.na(df$avgverb))
sum(!is.na(df$avgmath))
sum(df$disc & !is.na(df$avgmath))
#===================================================================

vars <- c("classize", "c_size", "tipuach", "avgverb", "avgmath")
summary(df[, vars])
summary(df[df$disc, vars])                 
                   

# actual vs predicted class size by enrollment 
pl <- aggregate(cbind(classize, fsc) ~ c_size, 
                data = df[df$c_size <= 160, ], mean)

plot(pl$c_size, pl$fsc, type = "l", xlab = "Enrollment", ylab = "Class size")
lines(pl$c_size, pl$classize, lty = 2)
legend("bottomright", c("Maimonides' rule", "Actual"), lty = 1:2)

# ============== 2SLS ===================================

run <- function(y, controls, data) {
  data <- data[!is.na(data[[y]]) & complete.cases(data[, c("classize", "fsc", controls)]), ]
  f <- as.formula(paste(y, "~ classize +", paste(controls, collapse = "+"),
                        "|  fsc +", paste(controls, collapse = "+")))
  m <- ivreg(f, data = data)
  cat("N =", nrow(data), "\n")
  coeftest(m, vcov = vcovCL(m, cluster = data$schlcode))
}

dd  <- df[df$disc, ]
d160 <- df[!is.na(df$trend), ]

#--------------------------------------------- math ----
#Math (8): full sample, tipuach + c_size
run("avgmath", c("tipuach", "c_size"), df)

#Math (9): +enrollment squared/100
run("avgmath", c("tipuach", "c_size", "c_size2"), df)

#Math (10): piecewise trend  
run("avgmath", c("trend"), d160)

#Math (12): discontinuity sample  
run("avgmath", c("tipuach", "c_size"), dd)

#------------------------------------------------------ Verbal ----
#Reading (2): full sample
run("avgverb", c("tipuach", "c_size"), df)

#Reading (4): piecewise trend             
run("avgverb", c("trend"), d160)

#Reading (6): discontinuity sample        
run("avgverb", c("tipuach", "c_size"), dd)

