# Initialization ====
library(dplyr)
library(ggplot2)
data_path <- "03-analysis/student-performance/data"

# Import ====
data_file_names <- list.files(data_path)
data_file_names

df <- lapply(data_file_names, function(x) {
  read.csv(file.path(data_path, x), header = F)
})

str(df, 1)
names(df) <- paste0("2017S", 1:length(data_file_names))
# stringr::str_extract(data_file_names, "\\d{4}[S][0-9]")

if (FALSE) { # Quick checks
  str(df, 1)
  lapply(df, head)
  bind_rows(lapply(df, function(x) apply(x, 2, class)))
  bind_rows(df)
  rlang::last_error()
  
  df[[1]] %>% str
  df[[2]] %>% str
}

# _Merging data sets ====
df <- lapply(df, function(x) apply(x, 2, as.character))
df <- lapply(df, as.data.frame)
df <- bind_rows(df, .id = "Semester") %>% as_tibble()
df %>% print(width = Inf)

# Data preparation ====

# _Variable description ====
# V1: PaperCode: code number of the paper
# V2: Paper: name of the paper
# V3: PaperCredit: credit score for the paper
# V4: ExternalMark: mark obtained from external assessment
# V5: ExternalMarkMax: max mark from external assessment 
# V6: InternalMark: mark obtained from internal assessment 
# V7: InternalMarkMax: max mark from internal assessment 
# V8: TotalMark: total mark from internal and external assessment combined
# V9: TotalMarkMax: max total mark --> internal and external combined
# V10: Grade: final grade (symbol)
# V11: GradePoint: final grade point
# V12: CreditScore: credit score (paper credit score * grade point --> V3 * V11)
# V13: FinalResult: final result --> passed/failed/..
# V14: StudentId: student's registered number

# _Renaming column names ====
df <- df %>% 
  rename(
    PaperCode = V1,
    Paper = V2,
    PaperCredit = V3,
    ExternalMark = V4,
    ExternalMarkMax = V5,
    InternalMark = V6,
    InternalMarkMax = V7, 
    TotalMark = V8,
    TotalMarkMax = V9,
    Grade = V10,
    GradePoint = V11,
    CreditScore = V12,
    FinalResult = V13,
    StudentId = V14) %>% 
  tidyr:::separate(col = Semester, into = c("Year", "Semester"), sep = "S") %>% 
  mutate(Semester = paste0("S", Semester))

# _Transformations ====

if(FALSE) { # Quick checks
  df %>% 
    mutate(
      PaperCredit = as.numeric(PaperCredit),
      ExternalMark = as.numeric(ExternalMark),
      ExternalMarkMax = as.numeric(ExternalMarkMax),
      InternalMark = as.numeric(InternalMark),
      InternalMarkMax = as.numeric(InternalMarkMax),
      TotalMark = as.numeric(TotalMark),
      TotalMarkMax = as.numeric(TotalMarkMax),
      GradePoint = as.numeric(GradePoint),
      CreditScore = as.numeric(CreditScore))
  
  df %>% count(ExternalMark, sort = T) %>% print(n = Inf)
  df %>% count(InternalMark, sort = T) %>% print(n = Inf)
  df %>% count(TotalMark, sort = T) %>% print(n = Inf)
  df %>% count(GradePoint, sort = T) %>% print(n = Inf)
  df %>% count(CreditScore, sort = T) %>% print(n = Inf)
}

#' convertToNumeric
#' @description Function to convert characters to numeric or integers
#' @param x input character vector
#' @param makeItInteger TRUE/FALSE, if FALSE it will return numeric
#' @examples 
#' convertToNumeric(2)
#' convertToNumeric("---")
#' convertToNumeric(c(1, 2, "Absent"))
#' 
convertToNumeric <- function(x, makeItInteger = FALSE) {
  x <- as.character(x)
  missing_list <- c("---", "Absent")
  out <- ifelse(x %in% missing_list, "", x)
  out <- if(makeItInteger) as.integer(out) else as.numeric(out)
  out
}

# df %>% print(width = Inf)
# dput(names(df))
numeric_cols <- c("Year", "PaperCredit", 
                  "ExternalMark", "ExternalMarkMax", 
                  "InternalMark", "InternalMarkMax", 
                  "TotalMark", "TotalMarkMax", 
                  "GradePoint", "CreditScore")

df <- df %>% mutate_at(numeric_cols, convertToNumeric) 
df %>% print(width = Inf)

if (FALSE) { # Quick analysis
  # Total number of students ?
  df %>% pull(StudentId) %>% unique %>% length
  df %>% 
    group_by(Year, Semester) %>% 
    summarise(noOfStudents = n_distinct(StudentId))
  
  # Range of credit score: 1:5
  df %>% count(PaperCredit) 
  
  df %>% 
    mutate(Score = TotalMark/TotalMarkMax) %>% 
    ggplot(aes(Score)) + 
    geom_histogram(position = "dodge") + 
    geom_vline(xintercept = 0.6, color = "red") + 
    facet_wrap(~Semester) +
    theme_light()
  
  df %>% count(ExternalMarkMax) # what does NA means ?
  df %>% count(InternalMarkMax) # what does NA means ?
  df %>% count(TotalMarkMax)
  df %>% count(Grade) # what does '---' means ?
  df %>% count(FinalResult) 
  df %>% summary()
  df %>% filter(is.na(Grade)) %>% print(width = Inf)
  df %>% filter(FinalResult == "Failed") %>% summary 
  df %>% filter(FinalResult == "Withheld") %>% summary 
}

# CR : Core
# CM : Complementary 
# CC : Common course
# OP : Open course
# CB : Choice based
df <- df %>% 
  mutate(Grade = ifelse(Grade == "---", NA, Grade),
         ProgramCode = substring(PaperCode, 1, 2)) 

if (FALSE) { # Quick analysis
  df %>% count(ProgramCode) %>% print(n = Inf)
  df %>% 
    filter(ProgramCode == 'ZY') %>% 
    count(Semester, PaperCode, Paper) %>% 
    print(n = Inf)
}

#' GetProgramFromCode
#' @description Function to get program from code
#' @param x program code
#' @examples 
#' GetProgramFromCode("BO")
#' 
GetProgramFromCode <- function(x) {
  y <- rep(NA, length(x))
  y[x == "BO"] <- "Botany"
  y[x == "CC"] <- "Computer Courses"
  y[x == "CH"] <- "Chemistry"
  y[x == "CO"] <- "Commerce"
  y[x == "CS"] <- "Computer Science"
  y[x == "EC"] <- "Economics"
  y[x == "EL"] <- "Electronics"
  y[x == "EN"] <- "English"
  y[x == "FR"] <- "French"
  y[x == "FS"] <- "Food Science"
  y[x == "HN"] <- "Hindi"
  y[x == "HS"] <- "Home Science"
  y[x == "HY"] <- "History"
  y[x == "ML"] <- "Malayalam"
  y[x == "MM"] <- "Mathematics"
  y[x == "MR"] <- "Project"
  y[x == "PE"] <- "Physical Education"
  y[x == "PH"] <- "Physics"
  y[x == "PY"] <- "Psychology"
  y[x == "SO"] <- "Sociology"
  y[x == "ST"] <- "Statistics"
  y[x == "SY"] <- "Syriac"
  y[x == "TA"] <- "Tamil"
  y[x == "ZY"] <- "Zoology"
  y
}

df <- df %>% mutate(Program = GetProgramFromCode(ProgramCode)) 

if (FALSE) { # Quick check
  df %>% 
    group_by(Year, Semester, Program) %>% 
    summarize(
      noOfPapers = n_distinct(PaperCode),
      noOfStudents = n_distinct(StudentId)) %>% 
    mutate(n = sprintf("%i (%i) ", noOfPapers, noOfStudents)) %>% 
    select(-noOfPapers, -noOfStudents) %>% 
    tidyr::spread(Semester, n) %>% 
    print(n = Inf)
  
  df %>% 
    filter(Program == 'Statistics') %>% 
    group_by(Semester, PaperCode, Paper) %>% 
    summarize(NoOfStudents = n_distinct(StudentId))
  
  df %>% print(width = Inf)
}

# _Filters ====
df <- df %>% filter(!FinalResult %in% c("Failed", "Withheld"))

# _Additional transformations ====
df$Grade <- factor(df$Grade, levels = c("S", "A+", "A", "B+", "B", "C", "D"))

# Save data ====
save(df, file = file.path(dirname(data_path), "01-prep.RData"))
