library(tidyverse)

raw_data <- read_csv(file="exam_data_f16.csv")

# Make gender categorical for later
categorical_variables <- select(raw_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

# Select columns
agreeableness <- select (raw_data, A1,A2,A3,A4,A5)
conscientiousness <- select (raw_data, C1,C2,C3,C4,C5)
performance <- select (raw_data, JP1,JP2,JP3,JP4,JP5)
age <- select (raw_data, age)

# Remove bad values
is_bad_value <- agreeableness<1 | agreeableness>6
agreeableness[is_bad_value] <- NA
is_bad_value <- conscientiousness<1 | conscientiousness>6
conscientiousness[is_bad_value] <- NA
is_bad_value <- performance<1 | performance>6
performance[is_bad_value] <- NA

# Reverse key items
agreeableness <- mutate(agreeableness,A1=7-A1)
conscientiousness <- mutate(conscientiousness,C4=7-C4)
conscientiousness <- mutate(conscientiousness,C5=7-C5)
performance <- mutate(performance,JP1=7-JP1)
performance <- mutate(performance,JP2=7-JP2)

# Bind data
agreeableness <- psych::alpha(as.data.frame(agreeableness),check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness),check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance),check.keys=FALSE)$scores
analytic_data <- cbind(age,agreeableness,conscientiousness,performance)

# Analytic data CSV
write_csv(analytic_data,path="analytic_data.csv")

# APA correlation table
library(apaTables)
apa.cor.table(analytic_data, filename="Table1.doc", table.number=1,
              show.conf.interval = TRUE, landscape = TRUE)

psych::alpha(analytic_data, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)

# Graphical representation for linear/non-linear relations
psych::pairs.panels(analytic_data)

# Multiple regression
library(tidyverse)
my.data <- read_csv("analytic_data.csv")
block1 <- lm(performance~conscientiousness, data=my.data)
block2 <- lm(performance~conscientiousness + agreeableness, data=my.data)
apa.reg.table(block1,block2, filename = "Table2.doc", table.number=2)

# Data file with gender
analytic_data_gender <- cbind(categorical_variables,agreeableness,conscientiousness,performance)

write_csv(analytic_data_gender,path="analytic_data_gender.csv")

# Filter data for men only
analytic_data_male <- filter (analytic_data_gender, gender=='Male')
analytic_data_male <- select(analytic_data_male, -gender)

write_csv(analytic_data_male,path="analytic_data_male.csv")

# Multiple regression for men only
my.data.male <- read_csv("analytic_data_male.csv")
block1 <- lm(performance~conscientiousness, data=my.data.male)
block2 <- lm(performance~conscientiousness + agreeableness, data=my.data.male)
apa.reg.table(block1,block2, filename = "Table3.doc", table.number=3)

# Filter data for women only
analytic_data_female <- filter (analytic_data_gender, gender=='Female')
analytic_data_female <- select(analytic_data_female, -gender)

write_csv(analytic_data_female,path="analytic_data_female.csv")

# Multiple regression for women only
my.data.female <- read_csv("analytic_data_female.csv")
block1 <- lm(performance~conscientiousness, data=my.data.female)
block2 <- lm(performance~conscientiousness + agreeableness, data=my.data.female)
apa.reg.table(block1,block2, filename = "Table4.doc", table.number=4)


