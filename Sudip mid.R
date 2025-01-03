setwd("F:/R for Agricultural Research/Humayun sir/")
getwd()
remdata <- read.csv(file.choose(), header = TRUE)
str(remdata)

library(multcompView)
library(datasets)
library(ggplot2)
library(dplyr)
library(data.table)
library(agricolae)
library(multcomp)
remdata$amendment<-as.factor(remdata$amendment)
remdata$fertilizer <- as.factor(remdata$fertilizer)

############### Answer 1 ###################
# Mean and standard error for lead concentration
lead_summary <- remdata %>%
  group_by(amendment) %>%
  summarise(mean_lead = mean(lead),
            se_lead = sd(lead) / sqrt(n()))
# ANOVA
model <- aov(lead ~ amendment, data = remdata)
summary(model)

# Perform Tukey's HSD post hoc test
tukey <- glht(model, linfct = mcp(amendment = "Tukey"))
summary(tukey)

# Get significance letters
tukey_letters <- cld(tukey, level = 0.05, decreasing = TRUE)


# Create the bar plot
ggplot(lead_summary, aes(x = amendment, y = mean_lead, fill=amendment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_lead - se_lead, ymax = mean_lead + se_lead), width = 0.2) +
  geom_text(aes(label = tukey_letters$mcletters$Letters), 
            position = position_dodge(width = 0.9),
            vjust = -1.8) +
  labs(title = "Effect of Different Amendments on Lead Concentration",
       x = "Different Amendments",
       y = "Lead Concentration (ppm)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("lead_concentration_plot.jpeg", width = 6, height = 4)
################### End ######################



################ Answer 2 ####################
library(graphics)
model3 = aov(cadmium ~ amendment+fertilizer+amendment*fertilizer, data=remdata)

#table with factors, means and standard deviation 

data_summ = group_by(remdata, fertilizer, amendment) %>% 
  summarise(mean=mean(cadmium), sd=sd(cadmium)) %>% 
  arrange(desc(mean))

#tukey test

Tukey= TukeyHSD(model3)
print(Tukey)

Cld=multcompLetters4(model3, Tukey)
print(Cld)

#add letter

Cld= as.data.frame.list(Cld$`amendment:fertilizer`)
data_summ$Cld=Cld$Letters
print(data_summ)

#ggplot

ggplot(remdata, aes(x=amendment:fertilizer, y=cadmium, fill = amendment))+geom_boxplot()+
  geom_text(data=data_summ,aes(label = Cld,x=amendment:fertilizer,y=mean+sd),vjust=-1,hjust=0.50,size=4,color="darkblue")+
  labs(title = "Effect of Different Amendments on Cadmium Concentration",
       x = "Different Amendments",
       y = "Cadmium Concentration (ppm)")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Answer 2.png", width = 10, height = 8)

################## End #######################