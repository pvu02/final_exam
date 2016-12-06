library(tidyverse)

raw_data <- read_csv(file="exam_data_f16.csv")

agreeableness <- select (raw_data, A1,A2,A3,A4,A5)
conscientiousness <- select (raw_data, C1,C2,C3,C4,C5)
performance <- select (raw_data, JP1,JP2,JP3,JP4,JP5)
age <- select (raw_data, age)


is_bad_value <- agreeableness<1 | agreeableness>6
agreeableness[is_bad_value] <- NA
is_bad_value <- conscientiousness<1 | conscientiousness>6
conscientiousness[is_bad_value] <- NA
is_bad_value <- performance<1 | performance>6
performance[is_bad_value] <- NA

agreeableness <- mutate(agreeableness,A1=7-A1)
conscientiousness <- mutate(conscientiousness,C4=7-C4)
conscientiousness <- mutate(conscientiousness,C5=7-C5)
performance <- mutate(performance,JP1=7-JP1)
performance <- mutate(performance,JP2=7-JP2)


agreeableness <- psych::alpha(as.data.frame(agreeableness),check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness),check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance),check.keys=FALSE)$scores
analytic_data <- cbind(age,agreeableness,conscientiousness,performance)

write_csv(analytic_data,path="analytic_data.csv")

library(apaTables)
apa.cor.table(analytic_data, filename="Table1.doc", table.number=1,
              show.conf.interval = TRUE, landscape = TRUE)

psych::pairs.panels(analytic_data)
