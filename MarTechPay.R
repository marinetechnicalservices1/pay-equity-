######################
#
# At-Sea Marine Technican Pay Equity Data
# November 2023
# 
#######################


# clear environment 

rm(list = ls())


# load packages

library(ggplot2)



##########################
## Create function to summarizes data:
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
##########################

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

###############################


# set working directory
setwd()
getwd()
list.files()


# load data
paydata <- read.csv("MarineTechPay.csv")
pay.df <- as.data.frame(paydata)
pay.df <-pay.df[1:12,]



#################
# Summarize and visualize the data 
###################


# annual salary with overtime by gender

pay.gender <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("gender"))

plot.pay.gender <- ggplot(pay.gender, aes(x = factor(gender), y = ave_salary_w_overtime)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Gender", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.gender               



# annual salary with overtime by position

pay.position <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("position"))

plot.pay.position <- ggplot(pay.position, aes(x = factor(position), y = ave_salary_w_overtime)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Position", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.position  



# annual salary with overtime by position and gender

pay.gender.position <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("gender", "position"))

plot.pay.gender.position <- ggplot(pay.gender.position, aes(x = factor(position), y = ave_salary_w_overtime, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Position", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.gender.position  


      
# annual salary with overtime by education level 

pay.edu <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("education"))

plot.pay.edu <- ggplot(pay.edu, aes(x = factor(education), y = ave_salary_w_overtime)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Education", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("GED or equivalent","Highschool","Associate's Degree", "Bachelor's Degree",
                            "Master's Degree")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.edu



# annual salary with overtime by education level and gender

pay.gender.edu <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("gender", "education"))

plot.pay.gender.edu <- ggplot(pay.gender.edu, aes(x = factor(education), y = ave_salary_w_overtime, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Education", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("GED or equivalent","Highschool","Associate's Degree", "Bachelor's Degree",
                            "Master's Degree")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.gender.edu



# annual salary with overtime by total experience 

pay.exp<- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("total_years_exp"))

plot.pay.exp <- ggplot(pay.exp, aes(x = factor(total_years_exp), y = ave_salary_w_overtime)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.exp



# annual salary with overtime by total experience and gender

pay.exp.gender<- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("gender", "total_years_exp"))

plot.pay.exp.gender <- ggplot(pay.exp.gender, aes(x = factor(total_years_exp), y = ave_salary_w_overtime, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.exp.gender



# annual salary with overtime by total experience and education

pay.exp.edu <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("education", "total_years_exp"))

plot.pay.exp.edu <- ggplot(pay.exp.edu, aes(x = factor(total_years_exp), y = ave_salary_w_overtime, fill = education)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.exp.edu



# annual salary with overtime by annual days at sea

pay.atsea <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("annual_days_at_sea"))

plot.pay.atsea <- ggplot(pay.atsea, aes(x = factor(annual_days_at_sea), y = ave_salary_w_overtime)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Days at sea", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.atsea



# annual salary with overtime by annual days at sea and position

pay.atsea.pos <- summarySE(pay.df, measurevar = "ave_salary_w_overtime", groupvars = c("annual_days_at_sea", "position"))

plot.pay.atsea.pos <- ggplot(pay.atsea.pos, aes(x = factor(annual_days_at_sea), y = ave_salary_w_overtime, fill = position)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = ave_salary_w_overtime - se, ymax = ave_salary_w_overtime + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Days at sea", y = "Annual Salary with Overtime", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.pay.atsea.pos



#################
# same as above but with stated salary (does not include sea pay)
#################

# stated salary  by gender

sal.gender <- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("gender"))

plot.sal.gender <- ggplot(sal.gender, aes(x = factor(gender), y = stated_salary)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Gender", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.gender               



# stated salary by position

sal.position <- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("position"))

plot.sal.position <- ggplot(sal.position, aes(x = factor(position), y = stated_salary)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Position", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.position  



# stated salary by position and gender

sal.gender.position <- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("gender", "position"))

plot.sal.gender.position <- ggplot(sal.gender.position, aes(x = factor(position), y = stated_salary, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Position", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.gender.position  



# stated salary by education level 

sal.edu <- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("education"))

plot.sal.edu <- ggplot(sal.edu, aes(x = factor(education), y = stated_salary)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Education", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("GED or equivalent","Highschool","Associate's Degree", "Bachelor's Degree",
                            "Master's Degree")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.edu



# stated salary by education level and gender

sal.gender.edu <- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("gender", "education"))

plot.sal.gender.edu <- ggplot(sal.gender.edu, aes(x = factor(education), y = stated_salary, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Education", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("GED or equivalent","Highschool","Associate's Degree", "Bachelor's Degree",
                            "Master's Degree")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.gender.edu



# stated salary by total experience 

sal.exp<- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("total_years_exp"))

plot.sal.exp <- ggplot(sal.exp, aes(x = factor(total_years_exp), y = stated_salary)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.exp



# stated salary by total experience and gender

sal.exp.gen<- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("gender","total_years_exp"))

plot.sal.exp.gen <- ggplot(sal.exp.gen, aes(x = factor(total_years_exp), y = stated_salary, fill = gender)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.exp.gen



# stated salary by total experience and education

sal.exp.edu<- summarySE(pay.df, measurevar = "stated_salary", groupvars = c("education","total_years_exp"))

plot.sal.exp.edu <- ggplot(sal.exp.edu, aes(x = factor(total_years_exp), y = stated_salary, fill = education)) + 
  geom_bar(position = position_dodge(), color = "black", stat= "identity") +
  geom_errorbar(aes(ymin = stated_salary - se, ymax = stated_salary + se), 
                width = 0.2, position = position_dodge(0.9))+
  theme(axis.title = element_text(face = "bold")) +
  labs(x = "Total Experience", y = "Stated Salary", title = "") +
  scale_y_continuous(breaks = seq(0, 130000, by = 30000)) +
  scale_x_discrete(limits=c("3-5","6-10","11+")) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) 
plot.sal.exp.edu

