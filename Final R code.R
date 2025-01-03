setwd("F:/R for Agricultural Research/Humayun sir/")
getwd()
book1 <- read.csv(file.choose(), header = TRUE)
str(book1)

library(multcompView)
library(datasets)
library(ggplot2)
library(dplyr)
library(data.table)
library(agricolae)
library(multcomp)
book1$Spirulina<-as.factor(book1$Spirulina)
book1$Zinc <- as.factor(book1$Zinc)

############### Answer 3 ###################
# Mean and standard error for lead concentration
spirulina_summary <- book1 %>%
  group_by(Spirulina) %>%
  summarise(mean_weight = mean(Tomato.fresh.weight),
            se_weight = sd(Tomato.fresh.weight) / sqrt(n()))
# ANOVA
model <- aov(Tomato.fresh.weight ~ Spirulina, data = book1)
summary(model)

# Perform Tukey's HSD post hoc test
tukey <- glht(model, linfct = mcp(Spirulina = "Tukey"))
summary(tukey)

# Get significance letters
tukey_letters <- cld(tukey, level = 0.05, decreasing = TRUE)
tukey_letters

# Create the bar plot
ggplot(spirulina_summary, aes(x = Spirulina, y = mean_weight, fill=Spirulina)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2) +
  geom_text(aes(label = tukey_letters$mcletters$Letters), 
            position = position_dodge(width = 0.9),
            vjust = -3) +
  labs(title = "Spirulina Vs Tomato Fresh Weight",
       x = "Spirulina Doses (g/m2)",
       y = "Tomato Fresh weight (g/plot)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Spirulina Vs Tomato Fresh Weight.jpeg", width = 6, height = 5)
################### End ######################



################ Answer 4 ####################
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(multcompView)


data <- read.csv(file.choose(), header = TRUE)


data$Spirulina <- as.factor(data$Spirulina)
data$Zinc <- as.factor(data$Zinc)

# ANOVA and Tukey HSD for two factors
anova_box <- aov(Tomato.fresh.weight ~ Spirulina * Zinc, data = data)
tukey_box <- TukeyHSD(anova_box)

# Significant letters for boxplot
tukey_cld_box <- multcompLetters4(anova_box, tukey_box)

# Significant letters for each group
letters_df <- data.frame(
  Spirulina = rep(levels(data$Spirulina), each = nlevels(data$Zinc)),
  Zinc = rep(levels(data$Zinc), times = nlevels(data$Spirulina)),
  Letters = tukey_cld_box$`Spirulina:Zinc`$Letters
)

# Summarize data for boxplot annotations
data_summary <- data %>%
  group_by(Spirulina, Zinc) %>%
  summarize(
    mean_weight = mean(Tomato.fresh.weight),
    se_weight = sd(Tomato.fresh.weight) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  left_join(letters_df, by = c("Spirulina", "Zinc"))

# Boxplot with significant letters
box_plot <- ggplot(data, aes(x = Spirulina, y = Tomato.fresh.weight, fill = Zinc)) +
  geom_boxplot() +
  geom_text(data = data_summary, aes(x = Spirulina, y = mean_weight + se_weight + 0.5, label = Letters),
            position = position_dodge(width = 0.9), size = 4, color = "black") +
  labs(title = "Effect of Spirulina and Zinc on Tomato Fresh Weight",
       x = "Spirulina Treatment",
       y = "Tomato Fresh Weight (g)") +
  theme_minimal() +
  scale_fill_discrete(name = "Zinc Levels")

box_plot
################## End #######################












