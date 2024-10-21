
library(readxl)
library(data.table)

data1 <- data.table(read_excel('biomarkers.xlsx'))
data2 <- read_excel('covariates.xlsx')




data1[, c("PatientID","Biomarker") := tstrsplit(Biomarker, "-",
                fixed = TRUE)]



males <- data2['Sex (1=male, 2=female)']==1
females <- data2['Sex (1=male, 2=female)']==2

males <- data2[males,]
females <- data2[females,]
males_ID <- males['PatientID']
females_ID <- females['PatientID']
print(males_ID)




