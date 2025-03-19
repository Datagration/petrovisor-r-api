library(testthat)
library(Myrconn.PetroVisor.Client)
test_check("Myrconn.PetroVisor.Client", reporter = JunitReporter$new(file = "test-out.xml"))
