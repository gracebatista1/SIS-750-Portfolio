# SIS-750-Portfolio

Final portfolio for Data Analysis

## Code Chunk

A small function I wrote called letter_grade(). It takes a single numeric test score and returns the corresponding letter grade ("A" through "F") using a standard 90/80/70/60 cutoff scale. I picked this snippet because it pulls together most of the building blocks we've covered in the course so far: defining a function with function(), using an if / else if / else chain to handle different cases, and then applying that function across a vector of values with sapply() so it works on a whole class of scores at once instead of just one student. The last line wraps the original scores and the new letter grades into a data.frame, which gives a clean side-by-side table you can print or pass into other functions later. I tried to keep the function short, the variable names obvious (score, scores, grades), and the cutoffs in the same order a person would read them.

```{r}
# Convert a numeric test score to a letter grade.
letter_grade <- function(score) {
  if (score >= 90) {
    "A"
  } else if (score >= 80) {
    "B"
  } else if (score >= 70) {
    "C"
  } else if (score >= 60) {
    "D"
  } else {
    "F"
  }
}

# Apply the function to a vector of scores and show the result as a table.
scores <- c(95, 82, 67, 73, 88, 54, 91, 78)
grades <- sapply(scores, letter_grade)

data.frame(score = scores, grade = grades)
```

![](images/clipboard-288400702.png)
