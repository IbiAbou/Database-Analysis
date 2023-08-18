Python 3.11.4 (v3.11.4:d2340ef257, Jun  6 2023, 19:15:51) [Clang 13.0.0 (clang-1300.0.29.30)] on darwin
Type "help", "copyright", "credits" or "license()" for more information.
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import chi2_contingency

# Read CSV file
ESSCSV = pd.read_csv('/Users/ibrahimabousaada/Downloads/ESS7e02_2.csv')

# Filter and rename columns
ESSCSV_filtered = ESSCSV[['trstplc', 'trstplt', 'gndr', 'edulvlb', 'hinctnta']].copy()
ESSCSV_filtered.rename(columns={'trstplc': 'TPolice', 'trstplt': 'TPolitician', 'gndr': 'Gender',
                                'edulvlb': 'Education', 'hinctnta': 'HIncome'}, inplace=True)

# Apply filters
ESSCSV_filtered = ESSCSV_filtered[(ESSCSV_filtered['TPolice'] < 11) & 
                                  (ESSCSV_filtered['TPolitician'] < 11) & 
                                  (ESSCSV_filtered['Gender'] < 3) & 
                                  (ESSCSV_filtered['Education'] < 801) & 
                                  (ESSCSV_filtered['HIncome'] < 11)]

# Map gender values
ESSCSV_filtered['Gender'] = ESSCSV_filtered['Gender'].map({1: 'Male', 2: 'Female'})

# Drop rows with NA values
ESSCSV_filtered.dropna(subset=['TPolice', 'TPolitician', 'Education', 'HIncome', 'Gender'], inplace=True)

# Summary stats
print(ESSCSV_filtered.describe())

# Create scatter plot
plt.figure()
sns.scatterplot(x='TPolitician', y='TPolice', data=ESSCSV_filtered)
plt.xlabel("Trust in Politicians")
plt.ylabel("Trust in Police")
plt.title("Scatter Plot of Trust in Politicians vs. Trust in Police")
plt.savefig("scatter_plot.png")
plt.close()

# Contingency table
contingency_table = pd.crosstab(ESSCSV_filtered['TPolice'], ESSCSV_filtered['TPolitician'])
print(contingency_table)

# Chi-Squared Test
chi2_result = chi2_contingency(contingency_table)
print("Chi-Squared Test Result:")
print("Chi-Square:", chi2_result[0])
print("p-value:", chi2_result[1])

# Calculate relative frequency of trust in police
table_TPolice = ESSCSV_filtered['TPolice'].value_counts(normalize=True) * 100
final_table_TPolice = pd.DataFrame({'TPolice_Values': table_TPolice.index, 'RF_TPolice': table_TPolice.values})

# Create bar chart for trust in police
plt.figure()
sns.barplot(x='TPolice_Values', y='RF_TPolice', data=final_table_TPolice)
plt.title("A Bar Chart of Trust in Police and Relative Frequency")
plt.xlabel("Trust in Police")
plt.ylabel("Relative Frequency (%)")
plt.xticks(rotation=45)
plt.savefig("bar_chart_police.png")
plt.close()

# Calculate relative frequency of trust in politicians
table_TPolitician = ESSCSV_filtered['TPolitician'].value_counts(normalize=True) * 100
final_table_TPolitician = pd.DataFrame({'TPolitician_Values': table_TPolitician.index, 'RF_TPolitician': table_TPolitician.values})

# Create bar chart for trust in politicians
plt.figure()
sns.barplot(x='TPolitician_Values', y='RF_TPolitician', data=final_table_TPolitician)
plt.title("A Bar Chart of Trust in Politicians and Relative Frequency")
plt.xlabel("Trust in Politicians")
plt.ylabel("Relative Frequency (%)")
plt.xticks(rotation=45)
plt.savefig("bar_chart_politicians.png")
plt.close()

# Group data by income and calculate mean trust in police
income_sections = ESSCSV_filtered.groupby('HIncome')['TPolice'].agg(['count', 'mean'])

# Calculate relative mean trust in police by income bracket
income_sections['rel_mean_TPolice'] = income_sections['mean'] / income_sections['mean'].sum()

# Create pie chart for relative mean trust in police by income bracket
... plt.figure()
... plt.pie(income_sections['rel_mean_TPolice'], labels=income_sections.index, autopct='%1.1f%%', startangle=90)
... plt.title("Income Brackets and Relative Mean Trust in Police")
... plt.savefig("pie_chart_income.png")
... plt.close()
... 
... # Calculate mean trust in police across highest level of education
... mean_trust_police_education = ESSCSV_filtered.groupby('Education')['TPolice'].mean().reset_index()
... 
... # Create bar chart for mean trust in police by education
... plt.figure()
... sns.barplot(x='Education', y='TPolice', data=mean_trust_police_education)
... plt.title("Mean Trust in Police by Highest Level of Education")
... plt.xlabel("Highest Level of Education")
... plt.ylabel("Mean Trust in Police")
... plt.xticks(rotation=45)
... plt.savefig("bar_chart_education.png")
... plt.close()
... 
... # Calculate the correlation between Education_num and TPolice
... correlation_new = ESSCSV_filtered['Education_num'].corr(ESSCSV_filtered['TPolice'])
... print("Correlation between Education_num and TPolice:", correlation_new)
... 
... # Calculate the mean trust in police for males and females
... mean_trust_police_gender = ESSCSV_filtered.groupby('Gender')['TPolice'].mean().reset_index()
... 
... # Create bar chart for mean trust in police by gender
... plt.figure()
... sns.barplot(x='Gender', y='TPolice', data=mean_trust_police_gender, palette=['darkred', 'darkgreen'])
... plt.title("Mean Trust in Police by Gender")
... plt.xlabel("Gender")
... plt.ylabel("Mean Trust in Police")
... plt.savefig("bar_chart_gender.png")
... plt.close()
