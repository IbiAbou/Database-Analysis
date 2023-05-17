# Trust in police = trstplc, Trust in politicians = trstplt, Gender = gndr
# Highest lvl of education = edulvlb, Household's total net income = hinctnta
install.packages("tidyverse")

library(tidyverse)

ESSCSV <- read.table(file.choose(), header = TRUE, sep = ',')
ESSCSV_filtered <- ESSCSV %>%
  select(trstplc, trstplt, gndr, edulvlb, hinctnta) %>%
  rename(TPolice = trstplc, TPolitician = trstplt, Gender = gndr, Education = edulvlb, HIncome = hinctnta) %>%
  filter(TPolice < 11, TPolitician < 11, Gender < 3, Education < 801, HIncome < 11) %>%
  mutate(Gender = recode(Gender, "1" = "Male", "2" = "Female")) %>%
  drop_na(TPolice, TPolitician, Education, HIncome, Gender)
  
#Check of summary stats
summary(ESSCSV_filtered)

#Check of Filter
if (nrow(ESSCSV_filtered) > 0) {
  
  plot <- ggplot(ESSCSV_filtered, aes(x = TPolitician, y = TPolice)) +
    geom_point() +
    labs(x = "Trust in Politicians", y = "Trust in Police") +
    theme_minimal()
  
  ggsave("scatter_plot.png", plot, width = 8, height = 6)
  
  #Contingency table
  table <- table(ESSCSV_filtered$TPolice, ESSCSV_filtered$TPolitician)
  print(table)
} else {
  print("Filtered dataset is empty.")
}

#Chi-Squared Test
chisq.test(table, correct = TRUE)

# Calculate relative frequency of trust in police
table_TPolice <- table(ESSCSV_filtered$TPolice) / 31294*100
final_table_TPolice <- data.frame(TPolice_Values = as.numeric(rownames(table_TPolice)),
                                  RF_TPolice = as.vector(table_TPolice))

bar_chart_police <- ggplot(final_table_TPolice, aes(x = factor(TPolice_Values), y = RF_TPolice)) +
  ggtitle("Figure 1: A Bar Chart of Trust in Police and Relative Frequency") +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Trust in Police", y = "Relative Frequency(%)") +
  scale_x_discrete(
    labels = c(
      "0" = "No trust",
      "1" = "1",
      "2" = "2",
      "3" = "3", 
      "4" = "4",
      "5" = "5",
      "6" = "6",
      "7" = "7",
      "8" = "8",
      "9" = "9",
      "10" = "Complete Trust"
    ),
    limits = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  ) +
  theme_bw() + 
  theme_linedraw() +
  theme(
    plot.title = element_text(size=20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text( vjust = 0.5, hjust=0.5)
  )

# Save the police bar chart as a PNG file
ggsave("bar_chart_police.png", bar_chart_police, width = 12, height = 9)

# Calculate relative frequency of trust in politicians
table_TPolitician <- table(ESSCSV_filtered$TPolitician) / 31294*100
final_table_TPolice <- data.frame(TPolitician_Values = as.numeric(rownames(table_TPolitician)),
                                  RF_TPolitician = as.vector(table_TPolitician))

bar_chart_politicians <- ggplot(final_table_TPolice, aes(x = factor(TPolitician_Values), y = RF_TPolitician)) +
  ggtitle("Figure 2: A Bar Chart of Trust in Politicians and Relative Frequency") +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(x = "Trust in Politicians", y = "Relative Frequency(%)") +
  scale_x_discrete(
    labels = c(
      "0" = "No trust",
      "1" = "1",
      "2" = "2",
      "3" = "3", 
      "4" = "4",
      "5" = "5",
      "6" = "6",
      "7" = "7",
      "8" = "8",
      "9" = "9",
      "10" = "Complete Trust"
    ),
    limits = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  ) +
  theme_bw() + 
  theme_linedraw() +
  theme(
    plot.title = element_text(size=20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text( vjust = 0.5, hjust=0.5)
  )

# Save the politicians bar chart as a PNG file
ggsave("bar_chart_politicians.png", bar_chart_politicians, width = 12, height = 9)

# Calculate the count and mean trust in the police in each income bracket (excluding NAs)
income_sections <- ESSCSV_filtered %>%
  group_by(HIncome) %>%
  summarise(n = n(),
            mean_TPolice = mean(TPolice, na.rm = TRUE))

# Calculate the relative mean trust in police by income bracket
income_sections <- income_sections %>%
  mutate(rel_mean_TPolice = mean_TPolice / sum(mean_TPolice))

# Create the pie chart with relative mean trust in police by income bracket
pie_chart <- ggplot(income_sections, aes(x = "", y = rel_mean_TPolice, fill = as.factor(HIncome))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(fill = "Income Bracket", 
       x = NULL, 
       y = NULL,
       title = "Figure 3: Income Brackets and Relative Mean Trust in Police") +
  scale_fill_discrete(labels = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")) +
  theme_linedraw() +
  theme(
    plot.title = element_text(size=20, face="bold.italic", hjust = 0.5),
    legend.text=element_text(size=13), legend.title=element_text(size=16))

# Save the pie chart as a PNG file
ggsave("pie_chart_income.png", pie_chart, width = 12, height = 9)

# Convert Education variable to factor to prevent the discreet/continuous R error
ESSCSV_filtered$Education <- factor(ESSCSV_filtered$Education)

# Calculate the mean trust in police across highest level of education
mean_trust_police_education <- ESSCSV_filtered %>%
  group_by(Education) %>%
  summarise(mean_trust = mean(TPolice, na.rm = TRUE))
print(mean_trust_police_education)

# Create a bar chart for mean trust in police by education
bar_chart_education_trust_police <- ggplot(mean_trust_police_education, aes(x= Education, y = mean_trust, fill = Education)) +
  geom_bar(stat = "identity") +
  ggtitle("Figure 4: Mean Trust in Police by Highest Level of Education") +
  xlab("Highest Level of Education") +
  ylab("Mean Trust in Police") +
  scale_fill_viridis_d()+
  theme_bw() + 
  theme_linedraw() +
  theme(
    plot.title = element_text(size=20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text( vjust = 0.5, hjust=0.5),
    legend.text=element_text(size=13), legend.title=element_text(size=16)
  )

# Save the bar chart as a png file
ggsave("bar_chart_education.png", bar_chart_education_trust_police, width = 12, height = 9)

# Education into numeric variable to avoid errors
ESSCSV_filtered$Education_num <- as.numeric(ESSCSV_filtered$Education)

# Calculate the correlation between Education_num and TPolice
correlation_new <- cor(ESSCSV_filtered$Education_num, ESSCSV_filtered$TPolice, use = "pairwise.complete.obs")
print(correlation_new)

# Calculate the mean trust in police for males and females
mean_trust_police <- ESSCSV_filtered %>%
  group_by(Gender) %>%
  summarise(mean_trust = mean(TPolice, na.rm = TRUE))
print(mean_trust_police)

# Create a bar chart for mean trust in police by gender
bar_chart_gender_trust_police <- ggplot(mean_trust_police, aes(x = Gender, y = mean_trust, fill = Gender)) +
  geom_bar(stat = "identity") +
  ggtitle("Figure 5: Mean Trust in Police by Gender") +
  xlab("Gender") +
  ylab("Mean Trust in Police") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  theme_bw() + 
  theme_linedraw() +
  theme(
    plot.title = element_text(size=20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text( vjust = 0.5, hjust=0.5),
    legend.text=element_text(size=13), legend.title=element_text(size=16)
  )

# Save the bar chart as a png file
ggsave("bar_chart_gender.png", bar_chart_gender_trust_police, width = 12, height = 9)


