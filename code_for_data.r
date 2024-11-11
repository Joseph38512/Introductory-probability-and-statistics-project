
library(readxl)
library(data.table)

#Read in the data.
data1 <- data.table(read_excel('biomarkers.xlsx'))
data2 <- read_excel('covariates.xlsx')



#Split the PatientID and biomarker into seperate columns.
data1[, c("PatientID","Biomarker") := tstrsplit(Biomarker, "-",
                fixed = TRUE)]


#Split the data into males and females.
males <- data2['Sex (1=male, 2=female)']==1
females <- data2['Sex (1=male, 2=female)']==2

males <- data2[males,]
females <- data2[females,]
males_ID <- males[['PatientID']]
females_ID <- females[['PatientID']]




#Merge the two datasets
covariates_merged <- merge(data2, data1, all = TRUE,
       by = "PatientID")


#Remove all but at inclusion for linear regression.
covariates_merged <- covariates_merged[covariates_merged["Biomarker"] == "0weeks",]




print(covariates_merged["Smoker (1=yes, 2=no)"])

#Create a regression model.

# model = lm(`Vas-12months` ~ `Biomarker` + `Sex (1=male, 2=female)`+`Age`+`Smoker (1=yes, 2=no)`,covariates_merged)
# print(model)







male_indices <- data1[,PatientID %in% males_ID]
female_indices <- data1[,PatientID %in% females_ID]


male_data <- data1[male_indices]
female_data <- data1[female_indices]


#remove all values not at inclusion
male_data_inclusion <- male_data[Biomarker == '0weeks',]
female_data_inclusion <- female_data[Biomarker == '0weeks',]



IL_8 <- t.test(male_data_inclusion[['IL-8']],female_data_inclusion[['IL-8']])
VegaF_A <- t.test(male_data_inclusion[['VEGF-A']],female_data_inclusion[['VEGF-A']])
OPG <- t.test(male_data_inclusion[['OPG']],female_data_inclusion[['OPG']])
TGF <- t.test(male_data_inclusion[['TGF-beta-1']],female_data_inclusion[['TGF-beta-1']])
IL_6 <- t.test(male_data_inclusion[['IL-6']],female_data_inclusion[['IL-6']])
CXCL9 <- t.test(male_data[['CXCL9']],female_data[['CXCL9']])
CXCL1 <- t.test(male_data[['CXCL1']],female_data[['CXCL1']])
IL_18 <- t.test(male_data[['IL-18']],female_data[['IL-18']])
CSF_1 <- t.test(male_data[['CSF-1']],female_data[['CSF-1']])
CXCL1 <- t.test(male_data[['CXCL1']],female_data[['CXCL1']],p.adjust.method="bonferroni")


par(mfrow = c(3, 6))
dev.new(width=300, height=150, unit="px")
hist(male_data[['IL-8']])
hist(female_data[['IL-8']])
hist(male_data[['VEGF-A']])
hist(female_data[['VEGF-A']])
hist(male_data[['OPG']])
hist(female_data[['OPG']])
hist(male_data[['TGF-beta-1']])
hist(female_data[['TGF-beta-1']])
hist(male_data[['IL-6']])
hist(female_data[['IL-6']])
hist(male_data[['OPG']])
hist(female_data[['OPG']])







#Print p values to be put into table.
p_values <- c(IL_8$p.value,VegaF_A$p.value,OPG$p.value,TGF$p.value,IL_6$p.value
,CXCL9$p.value,CXCL1$p.value,IL_18$p.value,CSF_1$p.value)

print(p_values)





male_averages <- c(IL_8$estimate[1],VegaF_A$estimate[1],OPG$estimate[1],TGF$estimate[1],IL_6$estimate[1]
,CXCL9$estimate[1],CXCL1$estimate[1],IL_18$estimate[1],CSF_1$estimate[1])

female_averages <- c(IL_8$estimate[2],VegaF_A$estimate[2],OPG$estimate[2],TGF$estimate[2],IL_6$estimate[2]
,CXCL9$estimate[2],CXCL1$estimate[2],IL_18$estimate[2],CSF_1$estimate[2])
print(male_averages)
print(female_averages)

# hist(male_data_inclusion[['VEGF-A']])
# hist(female_data_inclusion[['VEGF-A']])