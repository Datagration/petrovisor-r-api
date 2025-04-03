library(testthat)
library(Myrconn.PetroVisor.Client)

# Load connection data
load("connection_settings.RData")

# Create service provider instance using the loaded settings
sp <- ServiceProvider$new(
  url = discovery_url,
  workspace = workspace,
  user = user,
  password = password
)

test_check(
  "Myrconn.PetroVisor.Client",
  reporter = JunitReporter$new(file = "test-out.xml")
)
