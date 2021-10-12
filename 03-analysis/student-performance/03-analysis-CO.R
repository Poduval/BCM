# Initialization ====
library(dplyr)
library(ggplot2)
data_path <- "03-analysis/student-performance/data"

# Import data ====
df.CO <- read.csv(file.path(data_path, "CO.csv"), header = TRUE)
df.CO %>% head
df.CO %>% nrow # 19966
df.CO <- df.CO %>% mutate(PRN = as.character(PRN))

df.COWeights <- read.csv(file.path(data_path, "COWeights.csv"), header = TRUE)
df.COWeights %>% head
df.COWeights %>% nrow # 398

df.COPO <- read.csv(file.path(data_path, "COPO.csv"), header = TRUE)
df.COPO <- df.COPO %>% select(-starts_with("X"))
df.COPO %>% head
df.COPO %>% nrow # 6368

# Preparations ====
df <- df.CO %>% 
  select(Course_Code, PRN, starts_with("CO", ignore.case = FALSE)) %>% 
  tidyr::gather(CO, score, -Course_Code, -PRN) %>% 
  inner_join(
    df.COWeights %>% 
      select(Course_Code, starts_with("CO", ignore.case = FALSE)) %>% 
      tidyr:::gather(CO, credits, -Course_Code),
    by = c("Course_Code", "CO")) %>% head
  mutate(COAdj = gsub("\\..*", "", CO)) %>% 
  group_by(Course_Code, COAdj, PRN) %>% 
  mutate(scoreAdj = weighted.mean(score, credits))

# Analysis ====
df %>% 
  arrange(Course_Code, PRN, COAdj, CO) %>% 
  filter(Course_Code == 'BO1CMT01', PRN == '170021037511') %>% 
  print(n = Inf)

