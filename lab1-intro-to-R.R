rm(list = ls())

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
df_1 <- data.frame(a = c(1, 2, 3, 4),
                   b = c("a", "b", "c", "d"))
df_1$a
df_1$b

# ---- Subsetting ----



# ---- Loop ----

for (i in c(1, 2, 3)) {
  # do stuff with i
  print(i)
  print(i + 10)
}

for (i in c(4, 6, 8)) {
  print(paste("Now building model for cyl =", i))
  print(lm(mpg ~ hp, data = mtcars[mtcars$cyl == i, ]))
}


# ---- Function ----



m1 <- lm(mpg ~ disp, data = mtcars)

m_cyl1 <- lm(mpg ~ disp, data = mtcars[mtcars$cyl == 6, ])


summary(m_cyl1)$coefficients["disp", "Pr(>|t|)"]


values_of_cylinders <- unique(mtcars$cyl)
for (cyl in values_of_cylinders) {
  lm(mpg ~ disp, data = mtcars[mtcars$cyl == 6, ])
}
