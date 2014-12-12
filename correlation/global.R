#VERSION 1.0 
#VERSION 2.0 could do the fasit plots by gender, race, etc.

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
# ### Plot attendance rate vs score_max
# p_score_max <- ggplot(aes(x = attendance_rate, y = score_max ), data = df_f)
# p_score_max <- p_score_max + geom_point(aes(color = topic))
# p_score_max <- p_score_max + geom_smooth(aes(color = topic), method = 'lm')
# p_score_max <- p_score_max + scale_x_continuous(labels = percent)
# #p_score_max <- p_score_max + theme_tufte()
# p_score_max <- p_score_max + scale_colour_brewer(type = 'div', palette = 'Set1')
# p_score_max <- p_score_max + theme(legend.position = 'none',
#                                    axis.title = element_text(face = 'bold'),
#                                    strip.text = element_text(face = 'bold'))
# #p_score_max <- p_score_max + facet_wrap(Gender~ .)
# #p_score_max <- p_score_max +ggtitle('Max NWEA scores against attendance rate\n')
# p_score_max <- p_score_max + geom_text(aes(x = median(attendance_rate), 
#                                            y = max(score_max)*0.95,
#                                            label = paste0('corr: ', round(corr_max*100, digits = 1), '%')))
# p_score_max <- p_score_max + facet_wrap(Gender~topic, ncol = 3) #let's put it in server!!
# p_score_max
# ggsave(filename = '../output/plot/p_score_max.png', units = c("cm"), width = 25, height = 15)
# 
# 
# 
# ### Plot attendance rate vs change_max
# p_change_max <- ggplot(aes(x = attendance_rate, y = change_max ), data = df)
# p_change_max <- p_change_max + geom_point(aes(color = topic))
# p_change_max <- p_change_max + geom_smooth(aes(color = topic), method = 'lm')
# p_change_max <- p_change_max + geom_text(aes(x = median(attendance_rate), 
#                                              y = max(change_max)*0.95,
#                                              label = paste0('corr: ', round(corr_cmax*100, digits = 1), '%')))
# p_change_max <- p_change_max + facet_wrap(~topic, ncol = 3)
# p_change_max <- p_change_max + scale_y_continuous(labels = percent)
# p_change_max <- p_change_max + scale_x_continuous(labels = percent)
# #p_change_max <- p_change_max + theme_tufte()
# p_change_max <- p_change_max + scale_colour_brewer(type = 'div', palette = 'Set1')
# p_change_max <- p_change_max + theme(legend.position = 'none',
#                                      axis.title = element_text(face = 'bold'),
#                                      strip.text = element_text(face = 'bold'))
# #p_change_max <- p_change_max +ggtitle('Change in NWEA scores against attendance rate\n(Max score minus First score)\n')
# 
# ggsave(filename = '../output/plot/p_change_max.png', units = c("cm"), width = 25, height = 15)
# 
# ### Plot attendance rate vs change_last
# p_change_last <- ggplot(aes(x = attendance_rate, y = change_last ), data = df)
# p_change_last <- p_change_last + geom_point(aes(color = topic))
# p_change_last <- p_change_last + geom_smooth(aes(color = topic), method = 'lm')
# p_change_last <- p_change_last + geom_text(aes(x = median(attendance_rate), 
#                                                y = max(change_last)*0.95,
#                                                label = paste0('corr: ', round(corr_clast*100, digits = 1), '%')))
# p_change_last <- p_change_last + facet_wrap(~topic, ncol = 3)
# p_change_last <- p_change_last + scale_y_continuous(labels = percent)
# p_change_last <- p_change_last + scale_x_continuous(labels = percent)
# #p_change_last <- p_change_last + theme_tufte()
# p_change_last <- p_change_last + scale_colour_brewer(type = 'div', palette = 'Set1')
# p_change_last <- p_change_last + theme(legend.position = 'none',
#                                        axis.title = element_text(face = 'bold'),
#                                        strip.text = element_text(face = 'bold'))
# #p_change_last <- p_change_last +ggtitle('Change in NWEA scores against attendance rate\n(Last score minus First score)\n')
# 
# ggsave(filename = '../output/plot/p_change_last.png', units = c("cm"), width = 25, height = 15)

