#' Pull ACS data from the census api
#'
#' This function pulls relevant variables at the census tract and MSA level from
#' the census API.
#'
#' @importFrom magrittr "%>%"
#'
#' @param state a character string containing a 2-digit state fips code
#' @param county a character string containing a 3-digit county fips code
#' @param msa a character string containing a 5-digit CBSA code. See \href{https://public.opendatasoft.com/explore/dataset/core-based-statistical-areas-cbsas-and-combined-statistical-areas-csas/table/}{here}
#'        for CBSA codes
#' @param neighborhood1 a character vector containing 6-digit TRACTCE character strings
#' @param neighborhood2 a character vector containing 6-digit TRACTCE character strings
#' @param neighborhood3 a character vector containing 6-digit TRACTCE character strings
#' @param vintage numeric year of data
#' @param census_key a character string for census key. Default is Jana's
#' @return A data frame containing neighborhood- and MSA-level data for Gen2 investment memo
#' @export
#'
TN_tracts <- function(state, county, msa, neighborhood1=NULL, neighborhood2=NULL, neighborhood3=NULL, vintage = 2019, census_key = "7e25a5365676472f0dd97248a68e5288a0e65338") {
        ##check inputs of the function
        state <- if (is.character(state) & length(state)==2){
                state
        } else {
                if (is.character(state) & length(state)==1) {
                        paste0("0", state)
                } else {
                        if (is.numeric(state) & floor(log10(state)) + 1==1) {
                                paste0("0", state)
                        } else {
                                state
                        }
                }
        }

        ##variables--list of the variables that we will call from the census api
        vars <- c('B19013_001E', #median HH income, total
                  'B25064_001E', #median gross rent
                  'B25071_001E', #median gross rent as % of income
                  'B25033_001E', #total pop in occupied units
                  'B25033_002E', #total pop in owner-occupied
                  'B25033_008E', #total pop in renter-occupied units
                  'B25002_001E', 'B25002_002E', 'B25002_003E', #total units, total occupied, total vacant
                  'B02001_001E', #race pop denom
                  'B02001_002E', 'B02001_003E', #white (alone), black (alone),
                  'B03001_001E', 'B03001_002E', 'B03001_003E', #hispanic denominator, not hisp, hisp
                  'B05002_001E', 'B05002_009E', #native denom, born outside US
                  'B25018_001E' #median number of rooms
        )

        acs <- censusapi::getCensus(name = "acs/acs5", vintage = vintage, key = census_key, region = "tract:*", regionin=paste0("state:", state, "+county:", county), vars = vars) %>%
                subset(tract %in% c(neighborhood1, neighborhood2, neighborhood3)) %>%
                dplyr::mutate(neighborhood = dplyr::if_else(tract %in% neighborhood1, "Neighborhood1",
                                                            dplyr::if_else(tract %in% neighborhood2, "Neighborhood2",
                                                                           dplyr::if_else(tract %in% neighborhood3, "Neighborhood3", ""))))
        acs_clean <- acs %>%
                replace(((.)==-666666666), NA) %>%
                clean_acs() %>%
                dplyr::select(!vars)

        acs_nbh <- acs_clean %>%
                dplyr::group_by(as.factor(neighborhood))  %>%
                dplyr::summarise(med_inc = mean(med_inc),
                          med_gross_rent = mean(med_gross_rent, na.rm = TRUE),
                          med_gross_rent_pct_inc = mean(med_gross_rent_pct_inc, na.rm = TRUE),
                          renter_pop = sum(renter_pop),
                          pop = sum(pop),
                          total_units = sum(total_units), occupied_units = sum(occupied_units), vacant_units = sum(vacant_units),
                          race_pop = sum(race_pop), white = sum(white), black = sum(black),
                          hispan_pop = sum(hispan_pop), nonhispan = sum(nonhispan), hispan = sum(hispan),
                          nativity_pop = sum(nativity_pop), foreign_born = sum(foreign_born),
                          med_rooms = mean(med_rooms,  na.rm = TRUE)) %>%
                mutate_acs() %>%
                as.data.frame()
        rownames(acs_nbh) <- acs_nbh$`as.factor(neighborhood)`

        acs_nbh <- acs_nbh %>% dplyr::select(pop, renter_pop, renter_share, black_share, hispan_share, nonwhite_share,vac_rate,  med_inc, med_gross_rent) %>%
                as.matrix() %>% t() %>% as.data.frame()

        acs_msa <- censusapi::getCensus(name = "acs/acs5", vintage = vintage, key = census_key, region=paste0("metropolitan statistical area/micropolitan statistical area:", msa), vars = vars) %>%
                clean_acs() %>%
                dplyr::select( !vars) %>%
                mutate_acs() %>%
                as.data.frame()
        acs_msa <- acs_msa %>% dplyr::select(pop, renter_pop, renter_share, black_share, hispan_share, nonwhite_share,vac_rate,  med_inc, med_gross_rent) %>%
                as.matrix() %>% t() %>% as.data.frame() %>% dplyr::rename(MSA = V1)

        memo_table <- dplyr::full_join(tibble::rownames_to_column( acs_nbh),tibble::rownames_to_column(acs_msa)) %>%
                dplyr::mutate_if(is.numeric, round, digits = 1)
        memo_table

}


clean_acs <- function(data) {
        data%>%dplyr::mutate(med_inc = B19013_001E,
                      med_gross_rent = B25064_001E,
                      med_gross_rent_pct_inc = B25071_001E,
                      renter_pop = B25033_008E,
                      renter_share = B25033_008E/B25033_001E,
                      pop = B25033_001E,
                      total_units = B25002_001E, occupied_units = B25002_002E, vacant_units = B25002_003E,
                      race_pop = B02001_001E, white = B02001_002E, black = B02001_003E,
                      hispan_pop = B03001_001E, nonhispan = B03001_002E, hispan = B03001_003E,
                      nativity_pop = B05002_001E, foreign_born = B05002_009E,
                      med_rooms = B25018_001E)
}

mutate_acs <- function(data) {
        data %>% dplyr::mutate(renter_share = scales::label_percent()(renter_pop/pop),
                                black_share = scales::label_percent()((black/race_pop)),
                                nonwhite_share = scales::label_percent()((race_pop-white)/race_pop),
                                hispan_share = scales::label_percent()(hispan/hispan_pop),
                                foreign_born_share = scales::label_percent()(foreign_born/nativity_pop),
                                vac_rate = scales::label_percent()(vacant_units/total_units),
                               pop = scales::label_comma(accuracy = 1)(pop),
                               renter_pop = scales::label_comma(accuracy = 1)(renter_pop),
                               med_inc = scales::label_dollar(accuracy = 1)(med_inc),
                               med_gross_rent = scales::label_dollar(accuracy = 1)(med_gross_rent))



}

#now put this in the master.

