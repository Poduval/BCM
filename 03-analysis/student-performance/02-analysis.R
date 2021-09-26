# Initialization ====
library(dplyr)
library(ggplot2)

# Import data ====
load("03-analysis/student-performance/01-prep.RData", verbose = T)

# Analysis ====
df %>% dim
df %>% print(width = Inf)

# _1. Objective ====
# Merge data from two or more excel sheets
df %>% count(Year, Semester)

# _2. Objective ====
# Classify the students into various program Viz. B Sc Mathematics, 
# B Sc Physics, B Sc Botany, B Com Etc based on the range of register 
# numbers --> Used paper code instead
df %>% 
  group_by(Year, Semester, Program) %>% 
  summarize(
    noOfPapers = n_distinct(PaperCode),
    noOfStudents = n_distinct(StudentId)) %>% 
  mutate(n = sprintf("%i (%i) ", noOfPapers, noOfStudents)) %>% 
  select(-noOfPapers, -noOfStudents) %>% 
  tidyr::spread(Semester, n) %>% 
  print(n = Inf)

# _3. Objective ====
# Frequency table of Grades received by students for various papers (Class wise) 
# as a single table
df %>% pull(Grade) %>% levels

df %>% 
  group_by(Year, Semester, Program, Paper, Grade) %>% 
  summarise(n = n_distinct(StudentId)) %>% # Avoiding duplicates 
  tidyr::spread(Grade, n, fill = 0) %>% 
  ungroup() %>% 
  mutate(NoOfStudents = rowSums(select(.,-c(1:4)))) %>% 
  arrange(Semester, Program) %>% 
  # filter(Semester == "S1") %>% 
  View
  
# Note: One student could be enrolled for multiple papers
#       
# color hash codes: https://colorbrewer2.org
year <- 2017
semester <- "S1"

gg <- df %>% 
  filter(Year == year, Semester == semester) %>% 
  # assuming same student could not enroll twice for the same paper
  count(Program, Paper, Grade) %>% 
  group_by(Program, Grade) %>% 
  # assuming one student could be enrolled for multiple papers, same program
  summarise(NoOfStudents = sum(n)) %>% 
  group_by(Program) %>%
  # attaching number of students enrolled for the program
  # mutate(across(NoOfStudents, sum, .names = "TotalStudents")) %>% 
  mutate(TotalStudents = sum(NoOfStudents)) %>% 
  ungroup() %>% 
  mutate(`% Students` = round(100*NoOfStudents/TotalStudents, 2)) %>% 
  ggplot(aes(Program, `% Students`)) +
  geom_point(aes(color = Grade, size = NoOfStudents)) + 
  scale_color_manual(
    values = c("S" = "#990000", 
               "A+" = "#d7301f", "A" = "#ef6548", 
               "B+" = "#fc8d59", "B" = "#fdbb84",
               "C" = "#fdd49e", "D" = "#fef0d9")) +
  ylab("Proportion of students") + xlab("Major") +
  ggtitle("Grade per program - 2017 Semester 1") +
  coord_flip() +
  theme_classic() 
gg
pp <- gg %>% plotly:::ggplotly()  
pp

# save to share
htmlwidgets::saveWidget(pp, "03-analysis/student-performance/Objective-1.html")

# theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# _4. Objective ====
# Frequency table of Grades received for same paper various years (Class wise) 
# as a single table
df %>% filter(Program == "Mathematics") %>% pull(Paper) %>% unique

paper <- "Real Analysis"
# "Statistics - Descriptive Statistics"
# "Real Analysis"
df %>% 
  filter(Paper == paper) %>% 
  group_by(Year, Semester, Grade) %>% 
  summarise(n = n_distinct(StudentId)) %>% # Avoiding duplicates 
  tidyr::spread(Grade, n, fill = 0) %>% 
  ungroup() %>% 
  mutate(NoOfStudents = rowSums(select(.,-c(1:2)))) %>% 
  arrange(Semester) 

# _5. Objective ====
# Descriptive statistics (Mean, Median, SD and QD) for percentage of marks 
# received for different paper by students of same class

year <- 2017
semester <- "S1"

df %>% 
  filter(Year == year, Semester == semester) %>% 
  mutate(MarkInPercentage = round(100*TotalMark/TotalMarkMax, 2)) %>% 
  group_by(Program, Paper) %>% 
  summarise(NoOfStudents = n_distinct(StudentId),
            Avg = mean(MarkInPercentage, na.rm = T),
            Med = median(MarkInPercentage, na.rm = T),
            Std = sd(MarkInPercentage, na.rm = T),
            QD = 0.5 * (quantile(MarkInPercentage, 0.75) - 
                          quantile(MarkInPercentage, 0.25))) %>% 
  print(n = Inf)

df %>% 
  filter(Year == year, Semester == semester) %>% 
  mutate(MarkInPercentage = round(100*TotalMark/TotalMarkMax, 2)) %>% 
  group_by(Program) %>% 
  summarise(MarkInPercentage = mean(MarkInPercentage, na.rm = T)) %>% 
  ggplot(aes(reorder(Program, MarkInPercentage), MarkInPercentage)) +
  geom_bar(stat = "identity", fill = "#a6bddb") +
  ylab("Average marks in percentage") + xlab("Major") +
  coord_flip() +
  theme_light()

# _6. Objective ====
# Spearman's Correlation between percentage of marks for different papers

# Assignment !!!

# _7. Objective ====
# Box plot of percentage of marks received for different paper by students of 
# same class
year <- 2017
semester <- "S1"

df %>% 
  filter(Year == year, Semester == semester) %>% 
  mutate(MarkInPercentage = round(100*TotalMark/TotalMarkMax, 2)) %>% 
  ggplot() +
  geom_boxplot(aes(MarkInPercentage, Paper)) +
  xlab("Marks in percentage") + ylab("Paper") +
  ggtitle(sprintf("Year = %i, Semester = %s", year, semester)) +
  theme_light()
  
# _8. Objective ====
# Box plot of marks received for same paper in different years
paper <- "Statistics - Descriptive Statistics"

df %>% 
  filter(Paper == paper) %>% 
  mutate(MarkInPercentage = round(100*TotalMark/TotalMarkMax, 2)) %>% 
  ggplot() +
  geom_boxplot(aes(Year, MarkInPercentage)) +
  xlab("Year") + ylab("Marks in percentage") +
  ggtitle(paper) +
  coord_flip() +
  theme_light()

# _9. Objective ====
# Line chart of the average marks (in percentage) obtained by students in 
# different semesters (each class)

df %>% 
  mutate(MarkInPercentage = round(100*TotalMark/TotalMarkMax, 2)) %>% 
  group_by(Year, Semester) %>% 
  summarise(MarkInPercentage = mean(MarkInPercentage, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(Semester, MarkInPercentage, group = Year)) +
  geom_point(size = 3) +
  geom_line() +
  ylab("Marks in percentage") +
  theme_light()

