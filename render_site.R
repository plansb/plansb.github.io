# load functions ----
source("functions.R")

# digest data ----
import_teams()
import_projects(use_cache = F)
import_files()
update_teams_menu()

# render projects ----
render_project <- function(key, name, htm){
  message(glue("rendering {htm}"))
  render(
    "_project_template.Rmd",
    params = list(
      project_name = name,
      project_key  = key),
    output_file = htm)}

read_csv(projects_csv) %>%
  mutate(
    project_title = `Project Title` %>% 
      str_replace_all('"', "'")) %>% 
  select(
    key = project_key, name = project_title, htm = project_htm) %>%
  pwalk(render_project)

# render teams ----
render_team <- function(key, name, htm){
  message(glue("rendering {htm}"))
  render(
    "_team_template.Rmd",
    params = list(
      team_name = name,
      team_key  = key),
    output_file = htm)}

read_csv(teams_csv) %>% 
  select(key = team_key, name = team_name, htm = team_htm) %>% 
  pwalk(render_team)

# render other Rmd pages ----
render_site()