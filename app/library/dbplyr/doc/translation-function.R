## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- message = FALSE---------------------------------------------------------
library(dbplyr)
library(dplyr)

## -----------------------------------------------------------------------------
translate_sql((x + y) / 2)

## -----------------------------------------------------------------------------
translate_sql(x ^ 2L)
translate_sql(x ^ 2L, con = simulate_sqlite())
translate_sql(x ^ 2L, con = simulate_access())

## -----------------------------------------------------------------------------
# In SQLite variable names are escaped by double quotes:
translate_sql(x)
# And strings are escaped by single quotes
translate_sql("x")

## -----------------------------------------------------------------------------
translate_sql(substr(x, 5, 10))
translate_sql(log(x, 10))

## -----------------------------------------------------------------------------
translate_sql(1)
translate_sql(1L)

## -----------------------------------------------------------------------------
df <- tibble(
  x = c(10L, 10L, -10L, -10L), 
  y = c(3L, -3L, 3L, -3L)
)
mf <- tbl_memdb(df)

df %>% mutate(x %% y)
mf %>% mutate(x %% y)

## -----------------------------------------------------------------------------
translate_sql(mean(x))
translate_sql(mean(x, na.rm = TRUE))

## -----------------------------------------------------------------------------
translate_sql(mean(x, na.rm = TRUE), window = FALSE)

## -----------------------------------------------------------------------------
translate_sql(if (x > 5) "big" else "small")
translate_sql(switch(x, a = 1L, b = 2L, 3L))

## -----------------------------------------------------------------------------
translate_sql(foofify(x, y))

## -----------------------------------------------------------------------------
translate_sql(FOOFIFY(x, y))

## -----------------------------------------------------------------------------
translate_sql(x %LIKE% "%foo%")

## -----------------------------------------------------------------------------
translate_sql(x %||% y)

## -----------------------------------------------------------------------------
translate_sql(sql("x!"))
translate_sql(x == sql("ANY VALUES(1, 2, 3)"))

## -----------------------------------------------------------------------------
mf <- memdb_frame(x = 1, y = 2)

mf %>% 
  transmute(factorial = sql("x!")) %>% 
  show_query()

mf %>% 
  transmute(factorial = sql("CAST(x AS FLOAT)")) %>% 
  show_query()

## ---- error = TRUE------------------------------------------------------------
options(dplyr.strict_sql = TRUE)
translate_sql(glob(x, y))

## ----echo = FALSE, out.width = "100%"-----------------------------------------
knitr::include_graphics("windows.png", dpi = 300)

## -----------------------------------------------------------------------------
translate_sql(mean(G))
translate_sql(rank(G))
translate_sql(ntile(G, 2))
translate_sql(lag(G))

## -----------------------------------------------------------------------------
translate_sql(cummean(G), vars_order = "year")
translate_sql(rank(), vars_group = "ID")

## ---- eval = FALSE------------------------------------------------------------
#  mutate(players,
#    min_rank(yearID),
#    order_by(yearID, cumsum(G)),
#    lead(G, order_by = yearID)
#  )

