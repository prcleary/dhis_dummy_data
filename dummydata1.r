library(data.table)

dhis_period <- function(datevar) {
  with_hyphen <- ISOweek::ISOweek(datevar)
  gsub('-', '', with_hyphen)
}

sim_surv_for_district <- function(n_obs) {
  # approx 10% of districts will have all zero observations
  # the remainder will have simulated baseline data plus outbreaks
  if (runif(1) > 0.9) {
    rep(0, n_obs)
  } else {
    K <- runif(1, 0, 4)
    surveillance::sim.pointSource(length = n_obs, K = K)$observed
  }
}

gen_district_data_for_one_dataelement <- function(dataelement,
                                                  orgunit,
                                                  periods,
                                                  categoryoptioncombo = 'HllvX50cXC0',
                                                  attributeoptioncombo = 'HllvX50cXC0',
                                                  storedby = 'admin',
                                                  lastupdated = Sys.Date()) {
  output <-
    data.table::data.table(dataelement = dataelement,
                           period = periods,
                           orgunit = orgunit
                           )
  output[, `:=`(
    categoryoptioncombo = categoryoptioncombo,
    attributeoptioncombo = attributeoptioncombo
  )][]
  n_obs <- length(periods)
  output[, value := sim_surv_for_district(n_obs)]
  output[, `:=`(
    storedby = storedby,
    lastupdated = lastupdated
  )][]
}

org_units <- fread('data/pakistan_organisational_unitsv3.csv')
head(org_units)

dhis_export <- fread('data/dhis_export.csv')
head(dhis_export)

dataelement <- dhis_export[, unique(dataelement)]
dataelement_names <- c(
  "IDSR Acute Haemorrhagic Fever (new case)",
  "IDSR Acute Haemorrhagic Fever (new death)",
  "IDSR Acute Respiratory Infection (new case)",
  "IDSR Acute Respiratory Infection (new death)",
  "IDSR Acute Watery Diarrhoea < 5 years (new case)",
  "IDSR Acute Watery Diarrhoea < 5 years (new death)",
  "IDSR Acute Watery Diarrhoea > 5 years (new case)",
  "IDSR Acute Watery Diarrhoea > 5 years (new death)",
  "IDSR Diphtheria (new case)",
  "IDSR Diphtheria (new death)",
  "IDSR Influenza-Like Illness (new case)",
  "IDSR Influenza-Like Illness (new death)",
  "IDSR Measles (new case)",
  "IDSR Measles (new death)",
  "IDSR Severe Acute Respiratory Infection (new case)",
  "IDSR Severe Acute Respiratory Infection (new death)"
)
names(dataelement) <- dataelement_names

orgunit <- org_units[description %in% 'District', uid]
orgunit_names <- org_units[description %in% 'District', name]
names(orgunit) <- orgunit_names

lastupdated <- Sys.Date()

start_date <- as.Date('2010-01-01')

end_date <- Sys.Date() - 7

periods <- unique(dhis_period(seq(start_date, end_date, 1)))

final_run <- TRUE  # Only set to TRUE when ready to save data

# Simulated measles data

measles_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='g4G2ZRwnKYy', orgunit=x, periods = periods))

measles_sim <- rbindlist(measles_sim_list)

if (final_run) {  
  write.csv(measles_sim, 
            paste('data/', Sys.Date(), 'sim_measles_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

measles_deaths <- copy(measles_sim)
measles_deaths[, dataelement := 'Mcrab8t8sxN']
measles_deaths[, rowid := 1:nrow(measles_deaths)]
measles_deaths[, value := as.double(rbinom(1, value, 0.1)), by=rowid]  
# Approximate 10% mortality 
measles_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(measles_deaths, 
            paste('data/', Sys.Date(), 'sim_measles_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated ILI data

ili_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='kLbl3xxK0I5', orgunit=x, periods = periods))

ili_sim <- rbindlist(ili_sim_list)

if (final_run) {  
  write.csv(ili_sim, 
            paste('data/', Sys.Date(), 'sim_ili_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

ili_deaths <- copy(ili_sim)
ili_deaths[, dataelement := 'RN6Xa082AV2']
ili_deaths[, rowid := 1:nrow(ili_deaths)]
ili_deaths[, value := as.double(rbinom(1, value, 0.05)), by=rowid]  
# Approximate 5% mortality (actually much lower than this)
ili_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(ili_deaths, 
            paste('data/', Sys.Date(), 'sim_ili_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated diphtheria data

dip_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='IJUQrnoj15G', orgunit=x, periods = periods))

dip_sim <- rbindlist(dip_sim_list)

if (final_run) {  
  write.csv(dip_sim, 
            paste('data/', Sys.Date(), 'sim_dip_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

dip_deaths <- copy(dip_sim)
dip_deaths[, dataelement := 'EiAu6nCiC0G']
dip_deaths[, rowid := 1:nrow(dip_deaths)]
dip_deaths[, value := as.double(rbinom(1, value, 0.3)), by=rowid]  
# Approximate 30% mortality
dip_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(dip_deaths, 
            paste('data/', Sys.Date(), 'sim_dip_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated AHF data

ahf_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='J9U5e9dm6V3', orgunit=x, periods = periods))

ahf_sim <- rbindlist(ahf_sim_list)

if (final_run) {  
  write.csv(ahf_sim, 
            paste('data/', Sys.Date(), 'sim_ahf_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

ahf_deaths <- copy(ahf_sim)
ahf_deaths[, dataelement := 'ZQqkwfDGbo0']
ahf_deaths[, rowid := 1:nrow(ahf_deaths)]
ahf_deaths[, value := as.double(rbinom(1, value, 0.5)), by=rowid]  
# Approximate 50% mortality
ahf_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(ahf_deaths, 
            paste('data/', Sys.Date(), 'sim_ahf_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated ARI data

ari_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='M9GaoM7pGjq', orgunit=x, periods = periods))

ari_sim <- rbindlist(ari_sim_list)

if (final_run) {  
  write.csv(ari_sim, 
            paste('data/', Sys.Date(), 'sim_ari_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

ari_deaths <- copy(ari_sim)
ari_deaths[, dataelement := 'Wp5V5QRzXKK']
ari_deaths[, rowid := 1:nrow(ari_deaths)]
ari_deaths[, value := as.double(rbinom(1, value, 0.1)), by=rowid]  
# Approximate 10% mortality
ari_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(ari_deaths, 
            paste('data/', Sys.Date(), 'sim_ari_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated AWD1 data

awd1_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='rirjWpxXlxl', orgunit=x, periods = periods))

awd1_sim <- rbindlist(awd1_sim_list)

if (final_run) {  
  write.csv(awd1_sim, 
            paste('data/', Sys.Date(), 'sim_awd1_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

awd1_deaths <- copy(awd1_sim)
awd1_deaths[, dataelement := 'WRGXdSY2E2h']
awd1_deaths[, rowid := 1:nrow(awd1_deaths)]
awd1_deaths[, value := as.double(rbinom(1, value, 0.1)), by=rowid]  
# Approximate 10% mortality
awd1_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(awd1_deaths, 
            paste('data/', Sys.Date(), 'sim_awd1_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated AWD2 data

awd2_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='CwCFvylPuBv', orgunit=x, periods = periods))

awd2_sim <- rbindlist(awd2_sim_list)

if (final_run) {  
  write.csv(awd2_sim, 
            paste('data/', Sys.Date(), 'sim_awd2_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

awd2_deaths <- copy(awd2_sim)
awd2_deaths[, dataelement := 'WbjcK9jM2a7']
awd2_deaths[, rowid := 1:nrow(awd2_deaths)]
awd2_deaths[, value := as.double(rbinom(1, value, 0.2)), by=rowid]  
# Approximate 20% mortality
awd2_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(awd2_deaths, 
            paste('data/', Sys.Date(), 'sim_awd2_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

# Simulated SARI data

sari_sim_list <-
  lapply(orgunit, function(x)
    gen_district_data_for_one_dataelement(dataelement='l0MZoxYBok2', orgunit=x, periods = periods))

sari_sim <- rbindlist(sari_sim_list)

if (final_run) {  
  write.csv(sari_sim, 
            paste('data/', Sys.Date(), 'sim_sari_data.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

sari_deaths <- copy(sari_sim)
sari_deaths[, dataelement := 'HOxJonDsB8Y']
sari_deaths[, rowid := 1:nrow(sari_deaths)]
sari_deaths[, value := as.double(rbinom(1, value, 0.6)), by=rowid]  
# Approximate 60% mortality
sari_deaths[, rowid := NULL]

if (final_run) {  
  write.csv(sari_deaths, 
            paste('data/', Sys.Date(), 'sim_sari_deaths.csv'), 
            row.names = FALSE, 
            fileEncoding = 'UTF-8')
}

