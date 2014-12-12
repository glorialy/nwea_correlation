### Set working directory

### Load libraries
library(ggplot2)
library(scales)
#library(ggthemes)
library(plyr)
library(reshape2)


#setwd("~/nwea_correlation/correlation")
### Load data
# List files in the "data" folder
file <- list.files('../data')

df <- read.csv(paste0('../data/', file[1]), header = TRUE, stringsAsFactors = FALSE)
df_eto<-read.csv(paste0('../data/', file[2]), header = TRUE, stringsAsFactors = FALSE)
df_long<- read.csv(paste0('../data/', file[3]), header = TRUE, stringsAsFactors = FALSE)

df$change_last <- (df$score_t1/df$score_t0) - 1
df$change_max <- (df$score_max/df$score_t0) - 1
df$score_t0[df$score_t0 == 0] <- NA
df$score_t1[df$score_t1 == 0] <- NA
df$score_max[df$score_max == 0] <- NA

df <- na.omit(df)

### Compute correlation statistics
# attendance vs change_max
correlation_cmax <- ddply(df, .(topic), function(x){cor(x$attendance_rate, x$change_max)})
# attendance vs change_last
correlation_clast <- ddply(df, .(topic), function(x){cor(x$attendance_rate, x$change_last)})
# attendance vs score_max
correlation_max <- ddply(df, .(topic), function(x){cor(x$attendance_rate, x$score_max)})

### Merge data sets
df <- merge(df, correlation_cmax, by = 'topic', all.x = TRUE)
df <- merge(df, correlation_clast, by = 'topic', all.x = TRUE)
df <- merge(df, correlation_max, by = 'topic', all.x = TRUE)
# Update column names
my_columns <- colnames(df)
my_columns[13] <- "corr_max"
my_columns[12] <- "corr_clast"
my_columns[11] <- "corr_cmax"
colnames(df) <- my_columns

#connect ETO info
list = c("Age","Age Range","CA: ELL status", "City","Country of Birth",
        "Gender","Grade","Is client in special education (have IEP/504)?",
        "Is This Youth A WIA Grantee?",
        "LAYC Ethnicity",
        "LAYC Race" ,
        "MaritalStatus",
        "Primary Language Spoken at Home",
        "State","Zipcode" ,"Ward","Speak English?","Number of children",
        "Registered to Vote","IEP need level",
        "Do you work outside of school?",
        "IEP: Primary disability" ,
        "IEP hours")
df_l=df_long[df_long$demographic_name %in% list,]
df_w <- dcast(df_l,id+fname+lname ~ demographic_name, 
              value.var="domographic_value")
df_f <- merge(df,df_w,by.x="id",by.y="id",all.x=TRUE)
write.csv(df_f,"df.csv")
df_f <- read.csv("df.csv")
df_f <- df_f[,-1]
