# Create service provider instance using the loaded settings
sp <- ServiceProvider$new(
  url = Sys.getenv("R_UNIT_TEST_URL"),
  workspace = Sys.getenv("R_UNIT_TEST_WORKSPACE"),
  user = Sys.getenv("R_UNIT_TEST_USER"),
  password = Sys.getenv("R_UNIT_TEST_PWRD")
)
