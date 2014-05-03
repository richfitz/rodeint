controlled_stepper <- setRefClass("controlled_stepper",
                                  fields=list(
                                    name="character",
                                    atol="numeric",
                                    rtol="numeric",
                                    ptr="externalptr"))
controlled_stepper$lock(c("name", "atol", "rtol", "ptr"))

controlled_stepper$methods(initialize=function(name, atol=1e-6, rtol=1e-6) {
  name <<- name
  atol <<- atol
  rtol <<- rtol
  ptr  <<- rodeint:::controlled_stepper__ctor(name, atol, rtol)
})
