# ==== Alternative way ====

# Quick check: if we are missing any course codes in the weights table
setdiff(df.CO %>% pull(Course_Code) %>% unique,
        df.COWeights %>% pull(Course_Code))

# _Get unique Course Codes ====
course_code <- df.CO %>% pull(Course_Code) %>% unique
length(course_code) # 398

# _looping over each course code and CO types ====
dummy <- list()
output <- list()

for (i in 1:8) {
  
  dummy[[i]] <- lapply(course_code, function(x) {
    m1 <- df.CO %>% 
      filter(Course_Code == x) %>%
      # select(PRN, starts_with(paste0("CO", i))) %>% 
      select(PRN, starts_with("CO", ignore.case = FALSE)) %>% 
      as.data.frame
    
    m2 <- df.COWeights %>%
      filter(Course_Code == x) %>%
      # select(starts_with(paste0("CO", i))) %>% 
      select(starts_with("CO", ignore.case = FALSE))
    
    setNames(cbind.data.frame(
      m1$PRN, 
      (as.matrix(m1[match(names(m2), names(m1))]) %*% as.numeric(t(m2)))/sum(m2)),
      c("PRN", paste0("CO", i, "F")))
  })
  
  names(dummy[[i]]) <- course_code
  output[[i]] <- bind_rows(dummy[[i]], .id="course_code")
}

names(output) <- paste0("CO", 1:8)
final <- bind_rows(output, .id = "CO")
final %>% head
