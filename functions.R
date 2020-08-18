# load libraries ----
if (!require(librarian))
  install.packages(librarian)
library(librarian)
shelf(
  # utility
  glue,here,fs,glue,lubridate,stringr,yaml,googledrive,
  # data
  readr,dplyr,tidyr,purrr,
  # map
  sf,leaflet,
  # report
  knitr,rmarkdown,rstudio/DT,htmltools)

# paths ----

gsheet_keys <- list(
  projects = "1D0Zi7T85hTB-56G_A6VpHIAxDDOs-PgVi6SwbAbkem8",
  teams    = "12_bBf09mjxTgRoggEIAI81isTHoHWzwJaNLpEhBXbeM")

teams_matrix_csv <- here("data/teams_matrix.csv")
teams_lookup_csv <- here("data/teams_lookup.csv")
teams_csv        <- here("data/teams.csv")
people_csv       <- here("data/people.csv")
projects_csv     <- here("data/projects.csv")
areas_csv        <- here("data/areas.csv")
areas_kml        <- here("data/areas.kml")
files_csv        <- here("data/files.csv")

# functions ----

gsheet2tbl <- function(name = NULL, key = NULL, sheet = 0){
  
  # check function arguments
  if(sum(is.null(name), is.null(key)) != 1)
    stop("provide name or key, not both")
  if (!name %in% names(gsheet_keys))
    stop("name not in gsheet_keys list")
  
  if (!is.null(name)){
    key <- gsheet_keys[[name]]  
  }
  url <- glue("https://docs.google.com/spreadsheets/d/{key}/gviz/tq?tqx=out:csv&sheet={sheet}")

  read_csv(url)
}

import_teams <- function(){
  # weirdly, the Team Matrix google sheet downloads with cells as numbers,
  # so manually copied cells into Excel, saved as CSV, 
  # shifted non-numeric rows to right, shifted first column cells down, deleted numeric rows,
  # saved as data/teams_matrix.csv
  
  # create "long" form of teams_csv from "wide" teams_matrix
  d <- read_csv(teams_matrix_csv)
  
  names(d)[1] <- "role"
  
  people <- d %>% 
    # wide to long
    pivot_longer(-role, names_to = "team", values_to = "people") %>% 
    mutate(
      # replace newlines with space and trim
      role   = str_replace_all(role, "\\n", " ") %>% str_trim(),
      team   = str_replace_all(team, "\\n", " ") %>% str_trim(),
      people = str_replace_all(people, "\\n", " ") %>% str_trim(),
      # swap spaces after ) with people separator as comma
      people = str_replace_all(people, "(.*?\\))(\\s+)([A-Z]+.*?)", "\\1,\\3")) %>% 
    # separate people across rows to individual person
    separate_rows(people, sep = ",") %>% 
    rename(person = people) %>% 
    mutate(
      email = str_replace(person, "(.*)\\((.*)\\)", "\\2") %>% str_trim(),
      name  = str_replace(person, "(.*)\\((.*)\\)", "\\1") %>% str_trim()) %>% 
    filter(!is.na(person)) %>% 
    select(team, role, name, email) %>% 
    arrange(team, role, name, email)
  
  teams <- people %>% 
    group_by(team, role) %>% 
    summarize(n = n())  %>% 
    bind_rows(
      people %>% 
        group_by(team) %>% 
        summarize(n = n()) %>% 
        mutate(role = "ALL")) %>%
    pivot_wider(
      names_from = "role", names_prefix = "n_", 
      values_from = n, values_fill = 0)
  
  # teams_old -> teams_new -- manually created teams_new field
  # write_csv(teams, teams_lookup_csv)
  teams_lookup <- read_csv(teams_lookup_csv) %>% 
    select(-team_project)
  
  teams <- teams_lookup %>% 
    left_join(
      teams %>% 
        rename(team_matrix = team),
      by = "team_matrix") %>% 
    select(-team_matrix) %>% 
    mutate(
      team_htm = glue("./team_{team_key}.html"))
  
  people <- teams_lookup %>% 
    left_join(
      people %>% 
        rename(team_matrix = team),
      by = "team_matrix") %>% 
    select(-team_matrix)
    
  write_csv(teams , teams_csv)
  write_csv(people, people_csv)
}

url_friendly <- function(x){
  str_replace_all(x, "[^A-z0-9]", "-") %>% 
    str_replace_all("-{2,}","-") %>% 
    str_to_lower()
}

import_projects <- function(){
  
  projects <- gsheet2tbl("projects") %>% 
    select(-starts_with("X")) %>% 
    rename(team_project = Team) %>% 
    rename(area_project = `Focus Area`) %>% 
    mutate(
      project_key = map_chr(`Title`, url_friendly),
      project_htm = glue("./project_{project_key}.html")) %>% 
    select(project_key, everything())

  teams_lookup <- read_csv(teams_lookup_csv) %>% 
    select(team_key, team_name, team_project)
  
  # TODO: update project form with other areas
  areas <- read_csv(areas_csv) %>% 
    filter(!is.na(area_project)) %>% 
    select(
      area_key, area_name, area_project)
  
  projects <- projects %>% 
    left_join(
      teams_lookup, by = "team_project") %>% 
    select(-team_project) %>% 
    left_join(
      areas, by = "area_project") %>% 
    select(-area_project)
  
  write_csv(projects, projects_csv)
}

update_teams_menu <- function(){
  site <- read_yaml(here("_site.yml"))
  
  idx_teams <- which(map_chr(site$navbar$left, "text") == "Teams")
  
  # update teams nav menu ----
  teams_area <- read_csv(teams_csv) %>% 
    left_join(
      read_csv(areas_csv) %>% 
        select(area_key, area_name),
      by = "area_key")
     
  teams_menu <- teams_area %>% 
    filter(!is.na(area_key)) %>% 
    mutate(
      text_href = map2(
        team_name, team_htm, 
        function(x,y) 
          list(
            text = x, 
            href = y))) %>%
    group_by(area_name) %>% 
    summarize(
      list_text_href = list(text_href)) %>% 
    mutate(
      area_menu = map2(
        area_name, list_text_href,
        function(x, y)
          list(
            text = x,
            menu = y))) %>%
    bind_rows(
      teams_area %>% 
        filter(is.na(area_key)) %>% 
        mutate(
          area_menu = map2(
            team_name, team_htm, 
            function(x,y) 
              list(
                text = x, 
                href = y)))) %>% 
    pull(area_menu)
  site$navbar$left[[idx_teams]]$menu = teams_menu
  write_yaml(site, here("_site.yml"))
}

get_area_plys <- function(){
  # "Expertise Support"  NA
  areas    <- read_csv(areas_csv)
  projects <- read_csv(projects_csv)
  teams    <- read_csv(teams_csv)
  
  area_lyrs <- map(st_layers(areas_kml)$name, function(x)
    read_sf(areas_kml, layer = x) %>% 
      mutate(layer = x))
  
  area_plys <- do.call(rbind, area_lyrs) %>% 
    st_make_valid()  %>% 
    select(-layer) %>% 
    left_join(
      areas, 
      by = c("Name" = "area_kml")) %>% 
    left_join(
      projects %>% 
        filter(!is.na(area_key)) %>% 
        mutate(
          project_li = glue("<li><a href='./{project_htm}'>{Title}</a></li>")) %>% 
        group_by(area_key) %>% 
        summarise(
          projects_n    = n(),
          projects_ul = paste(
            "<ul>\n", 
            paste(project_li, collapse='\n'), 
            "\n</ul>")), 
      by = "area_key") %>% 
    left_join(
      teams %>% 
        filter(!is.na(area_key)) %>% 
        mutate(
          team_li = glue("<li><a href='./{team_htm}'>{team_name}</a></li>")) %>% 
        group_by(area_key) %>% 
        summarise(
          teams_n    = n(),
          teams_ul = paste(
            "<ul>\n", 
            paste(team_li, collapse='\n'), 
            "\n</ul>")), 
      by = "area_key") %>% 
    replace_na(list(projects_n = 0, teams_n = 0)) %>% 
    mutate(
      popup_html = paste(
        "<strong>", area_name, "</strong><br>",
        glue("{projects_n} projects"),
        ifelse(!is.na(projects_ul), projects_ul, "<br>"),
        glue("{teams_n} teams"),
        ifelse(!is.na(teams_ul), teams_ul, "")),
      label_html = paste(
        "<strong>", area_name, "</strong><br>",
        glue("{projects_n} projects<br>"),
        glue("{teams_n} teams")))

#     
#   
#   area_plys <- area_plys %>% 
#     mutate(
#       popup_html = if_else(
#         is.na(popup_html),
#         paste("<strong>", area_name, "</strong><br>no projects yet"),
#         popup_html))
}

import_files <- function(){
  projects <- read_csv(projects_csv)
  project_files <- projects %>% 
    rename(file = `Image for overview`) %>% 
    mutate(
      file_category = "image_overview") %>% 
    select(project_key, file_category, file) %>% 
    filter(!is.na(file)) %>% 
    bind_rows(
      projects %>% 
        select(project_key, file = `Other images for gallery`) %>% 
        filter(!is.na(file)) %>% 
        separate_rows(file, sep = ",") %>% 
        mutate(
          file_category = "images")) %>% 
    bind_rows(
      projects %>% 
        select(project_key, file = `Drawings and related documents`) %>% 
        filter(!is.na(file)) %>% 
        separate_rows(file, sep = ",") %>% 
        mutate(
          file_category = "drawings")) %>% 
    mutate(
      gid   = str_replace(file, "https://drive.google.com/open\\?id=(.*)", "\\1") %>% 
        str_trim(),
      fname = map_chr(gid, function(x) {
        fname <- try(drive_get(as_id(x))$name)
        if (class(fname) == "try-error") return(NA)
        fname }),
      path  = ifelse(!is.na(fname), glue("images/{fname}"), NA))
  write_csv(project_files, files_csv)
  
  pwalk(project_files, function(...) {
    d <- tibble(...)
    if (!file.exists(d$path))
      drive_download(as_id(d$gid), d$path)
  })
  
  # TODO: team images?
}

fld2str <- function(fld, lbl = fld){
  # fld = "Project Proposal Description"; lbl = "Description"
  str <- ""
  
  if(!fld %in% names(project)) stop(glue("fld not in project: {fld}"))
  
  if(!is.na(project[[fld]]) && nchar(project[[fld]]) > 0){
    str <- glue("**{lbl}:** {project[[fld]]}")
  } else {
    str <- glue("**{lbl}:** <span style='color:gray;'>[empty]</span>")
  }

  str
}

paths2carousel <- function(paths){
  
  if (length(paths) == 0) 
    return("")
  
  carousel <<- bs_carousel(id = "images_carousel", use_indicators = T)
    
  img2carousel <- function(path){
    caption = basename(path) %>% path_ext_remove()
    
    carousel <<- carousel %>% bs_append(
      content = bs_carousel_image(src = path),
      caption = bs_carousel_caption(caption))
  }
  
  walk(paths, img2carousel)
  carousel
}

