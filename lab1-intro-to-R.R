rm(list = ls())

head(mtcars)

# Goal: 
# - Model mpg ~ hp (horsepower) for each subset of cylinder type.
# - Extract the p-value for each

# Naive approach
unique(mtcars$cyl)
lm(mpg ~ hp, data = mtcars[mtcars$cyl == 4, ])
lm(mpg ~ hp, data = mtcars[mtcars$cyl == 6, ])
lm(mpg ~ hp, data = mtcars[mtcars$cyl == 8, ])

# Bad: Lots of repetition, cannot be changed

# ---- Vector, List ----

# Vector

## Numeric vector
c(1, 3, 10)
1:10
seq(1, 10, by = 2)

## Character vector
c("a", "b", "f")

## Vector cannot be of mixed types
c(1, "a")

# List

## List can store ANYTHING
list(1, "a")

list(1, "a", mtcars)

list(1, "a", lm(mpg ~ cyl, data = mtcars))

## Why learn about list?

m1 <- lm(mpg ~ cyl, data = mtcars)
summary(m1)
str(m1)
m1$coefficients # This is how we get access to p-value

# ---- Matrix, Data Frame ----

mat1 <- matrix(1:10, nrow = 2) # used in linear algebra, coding up estimator
mat1
mat1[1, 1]
mat1[1, c(1, 2)]

mat1[, 1]
mat1[1, ]

# used in most analysis
df_1 <- data.frame(a = c(1, 2, 3, 1),
                   b = c("a", "b", "c", "e"))
df_1$a
df_1$b

df_1[c(TRUE, FALSE, FALSE, TRUE), ]

df_1$a == 1

df_1[df_1$a == 1, ]

# ---- Loop ----

for (i in c(1, 2, 3)) {
  # do stuff with i
  print(i)
  print(i + 10)
}

# To construct a loop, think about what stays the same, and what changes


for (i in c(4, 6, 8)) {
  print(paste("Now building model for cyl =", i))
  print(lm(mpg ~ hp, data = mtcars[mtcars$cyl == i, ]))
}

for (i in c(4, 6, 8)) {
  m_cyl <- lm(mpg ~ hp, data = mtcars[mtcars$cyl == i, ])
  print(summary(m_cyl)$coefficients["hp", "Pr(>|t|)"])
}

# ---- Function ----

f_1 <- function(x) {
  # do stuff with x, then return the result
  y <- x ** 2
  return(y)
}

f_1(2)
f_1(10)

f_modeling <- function(variable_name) {
  unique_values <- unique(mtcars[, variable_name])
  result <- rep(NA, length(unique_values))
  for (i in 1:length(unique_values)) {
    value <- unique_values[i]
    my_data <- mtcars[mtcars[, variable_name] == value, ]
    my_model <- lm(mpg ~ hp, data = my_data)
    my_pvalue <- summary(my_model)$coefficients["hp", "Pr(>|t|)"]
    result[i] <- my_pvalue
  }
  return(result)
}

f_modeling("cyl")
f_modeling("gear")
