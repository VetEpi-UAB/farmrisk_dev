cattle_data_keys <- list(
  bsg = list(
    cols = names(bsg),
    keys = c("farm_id")
  ),
  mov = if(exists("mov")) {
    list(
      cols = names(mov),
      keys = c("mov_id", "farm_id")
    )
  } else NULL,
  animal = list(
    cols = names(animal),
    keys = c("animal_category")
  ),
  animal_point = list(
    cols = names(animal_point),
    keys = c("animal_category", "contact_point_type")
  ),
  animal_region = list(
    cols = names(animal_region),
    keys = c("animal_category", "region_code")
  ),
  pathogen = list(
    cols = names(pathogen),
    keys = c("pathogen")
  ),
  pathogen_animal = list(
    cols = names(pathogen_animal),
    keys = c("pathogen", "animal_category")
  ),
  pathogen_region = list(
    cols = names(pathogen_region),
    keys = c("pathogen", "region_code")
  ),
  pathogen_status = list(
    cols = names(pathogen_status),
    keys = c("pathogen", "health_status")
  ),
  pathogen_surface = list(
    cols = names(pathogen_surface),
    keys = c("pathogen", "surface")
  ),
  pathogen_test = list(
    cols = names(pathogen_test),
    keys = c("pathogen", "test")
  ),
  visit_region = list(
    cols = names(visit_region),
    keys = c("visit_type", "region_code")
  ),
  wildlife_pathogen_region = list(
    cols = names(wildlife_pathogen_region),
    keys = c("wildlife", "pathogen", "region_code")
  ),
  wildlife_point = list(
    cols = names(wildlife_point),
    keys = c("wildlife", "contact_point_type")
  ),
  wildlife_point_density = list(
    cols = names(wildlife_point_density),
    keys = c("wildlife", "contact_point", "density_level")
  ),
  wildlife_region = list(
    cols = names(wildlife_region),
    keys = c("wildlife", "region_code")
  )
)