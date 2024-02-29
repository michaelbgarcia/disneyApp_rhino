# app/logic/data_fetch.R

box::use(
  dplyr[left_join, filter, mutate, case_match, select, across, all_of, arrange,
        desc, pull, recode],
  purrr[map, list_rbind, map_int, map_chr, pluck],
  lubridate[ymd_hms],
  themeparkr[tpr_entity_live, tpr_destinations, tpr_entity_children, tpr_entity]
)

#' @export
get_data_live <- function(static_data, column = "parkId") {
  static_data |>
    left_join(
      static_data[[column]] |>
        unique() |>
        map(.f = tpr_entity_live) |>
        list_rbind(),
      by = "id"
    ) |>
    filter(status %in% c("OPERATING", "DOWN")) |>
    mutate(
      status_cols = case_match(status,
                                      "OPERATING" ~ "blue",
                                      "DOWN" ~ "orange"
      )
    ) |>
    mutate(
      wait_time = queue |>
        map_int(pluck, "STANDBY", "waitTime", .default = NA_integer_),
      ll_time = queue |>
        map_chr(pluck, "RETURN_TIME", "returnStart", .default = NA_character_)
    ) |>
    # left_join(parks_ref, by = c("parkId" = "parks_id")) |>
    select(parks_name, id, name, status, status_cols, ll_time,wait_time,lastUpdated, latitude, longitude) |>
    mutate(
      across(
        .cols = all_of(c("ll_time", "lastUpdated")),
        .fns = ymd_hms
      )
    ) |>
    arrange(desc(wait_time))
}

#' @export
get_data_parks <- function() {
  print("Fetching parks data")
  tpr_destinations() |>
    select(parks_id, parks_name) |>
    mutate(
      parks_name = recode(
        parks_name,
        "Disney's Animal Kingdom Theme Park" = "Animal Kingdom",
        "Disney's Hollywood Studios" = "Hollywood Studios",
        "EPCOT" = "Epcot",
        "Magic Kingdom Park" = "Magic Kingdom"
      )
    )
}

#' @export
get_data_static <- function(parks_ref) {
  print("Fetching static data")
  tpr_destinations() |>
    filter(parks_name %in% c("Magic Kingdom Park", "EPCOT",
                                    "Disney's Hollywood Studios",
                                    "Disney's Animal Kingdom Theme Park")) |>
    pull(parks_id) |>
    map(.f = tpr_entity_children) |>
    list_rbind() |>
    filter(entityType == "ATTRACTION") |>
    pull(id) |>
    map(.f = tpr_entity) |>
    list_rbind() |>
    filter(attractionType == "RIDE") |>
    left_join(parks_ref, by = c("parkId" = "parks_id"))
}

# saveRDS(aa, "app/static/parks_data.rds")
# saveRDS(a, "app/static/parks_ref.rds")

