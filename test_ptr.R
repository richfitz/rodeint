library(rodeint)

obj <- rodeint:::make_foo(1, 2)
rodeint:::foo_run(obj) == 3
rodeint:::foo_run(1)

