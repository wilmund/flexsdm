test_that("part_senv", {
  require(terra)

  f <- system.file("external/somevar.tif", package = "flexsdm")
  somevar <- terra::rast(f)

  # Select a species
  spp1 <- spp %>% dplyr::filter(species == "sp1")

  # Test with presences and absences
  part1 <- part_senv(
    env_layer = somevar,
    data = spp1,
    x = "x",
    y = "y",
    pr_ab = "pr_ab",
    min_n_groups = 2,
    max_n_groups = 10,
    prop = 0.2
  )

  expect_equal(class(part1), "list")

  # env_sim must be a real environmental Euclidean distance between groups,
  # not a degenerate constant. It was previously computed on a logical vector
  # (split() had been overwritten by complete.cases()), which forced env_sim
  # to 0 for every partition and silently disabled this selection criterion.
  expect_true(all(is.finite(part1$best_part_info$env_sim)))
  expect_gt(max(part1$best_part_info$env_sim), 0)

  # Only with presences
  spp1 <- spp1 %>% dplyr::filter(pr_ab == 1)
  part2 <- part_senv(
    env_layer = somevar,
    data = spp1,
    x = "x",
    y = "y",
    pr_ab = "pr_ab",
    min_n_groups = 2,
    max_n_groups = 20,
    prop = 0.2
  )

  expect_equal(class(part2), "list")
})


test_that("misuse of arguments", {
  require(terra)

  f <- system.file("external/somevar.tif", package = "flexsdm")
  somevar <- terra::rast(f)

  # Select a species
  spp1 <- spp %>% dplyr::filter(species == "sp1")
  spp1$pr_ab[1:2] <- 3
  # Test with presences and absences
  expect_error(part_senv(
    env_layer = somevar,
    data = spp1,
    x = "x",
    y = "y",
    pr_ab = "pr_ab",
    min_n_groups = 2,
    max_n_groups = 10,
    prop = 0.2
  ))
})
