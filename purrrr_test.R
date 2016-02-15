library(purrr)

mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

mtcars %>%
  split(.$cyl) %>%
  # map(~lm(mpg ~ wt, data = .)) %>%
  map(.$wt, mean, na.rm=T)

mtcars %>%
  split(.$cyl) %>%
  map(~summary(.$wt, na.rm=T)) %>% 
  map_dbl("Max.")

x <- summary(mtcars)
xx <- summary(mtcars$wt)

str(x)
str(xx)
names(x)
names(xx)

x
