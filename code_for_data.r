
library(readxl)
library(data.table)

#Read in the data.
data1 <- data.table(read_excel('biomarkers.xlsx'))
data2 <- read_excel('covariates.xlsx')



#Split the PatientID and biomarker into seperate columns.
data1[, c("PatientID","Biomarker") := tstrsplit(Biomarker, "-",
                fixed = TRUE)]



males <- data2['Sex (1=male, 2=female)']==1
females <- data2['Sex (1=male, 2=female)']==2

males <- data2[males,]
females <- data2[females,]
males_ID <- males[['PatientID']]
females_ID <- females[['PatientID']]



male_indices <- data1[,PatientID %in% males_ID]
female_indices <- data1[,PatientID %in% females_ID]


male_data <- data1[male_indices]
female_data <- data1[female_indices]

#remove all values not at inclusion
male_data_inclusion <- male_data[Biomarker == '0weeks',]
female_data_inclusion <- female_data[Biomarker == '0weeks',]


IL_8 <- t.test(male_data_inclusion[['IL-8']],female_data_inclusion[['IL-8']])
Vega_A <- t.test(male_data_inclusion[['VEGF-A']],female_data_inclusion[['VEGF-A']])
OPG <- t.test(male_data_inclusion[['OPG']],female_data_inclusion[['OPG']])
TGF <- t.test(male_data_inclusion[['TGF-beta-1']],female_data_inclusion[['TGF-beta-1']])
IL_6 <- t.test(male_data_inclusion[['IL-6']],female_data_inclusion[['IL-6']])
CXCL9 <- t.test(male_data[['CXCL9']],female_data[['CXCL9']])
CXCL1 <- t.test(male_data[['CXCL1']],female_data[['CxCL1']])
IL_18 <- t.test(male_data[['IL-18']],female_data[['IL-18']])
CSF_1 <- t.test(male_data[['CSF-1']],female_data[['CSF-1']])


#create a table of the p values as well as mu values for each compound
table_of_data <- matrix(rep(0,36),nrow = 4,ncol = 9)
colnames(table_of_data) <- c('IL-8','VEGF-A','OPG','TGF-beta-1','IL-6','CXCL9','CXCL1','IL-18','CSF-1')
row.names(table_of_data) <- c('p value','Mu of males','Mu of females','interval')
png('table.png')
as.table(table_of_data)
dev.off()

