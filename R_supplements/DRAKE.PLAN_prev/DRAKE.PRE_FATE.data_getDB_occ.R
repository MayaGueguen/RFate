
getDB_CBNA = function(x.min, x.max, y.min, y.max)
{
  ## Create a connection : save username and password
  username <- { "r_user_cbna" }
  pw <- { "r_user_cbna" }
  
  ## Load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  ## Create a connection to the postgres database
  ## note that "con" will be used later in each connection to the database
  con <- dbConnect(drv,
                   dbname = "cbna",
                   host = "152.77.214.210",
                   port = 5432,
                   user = username,
                   password = pw
  )
  rm(pw) # removes the password
  
  ## Create dataframe with the stations table
  stations = dbGetQuery(con, paste0("SELECT * from stations where longitudel93_rel>", x.min, " 
                                                            and latitudel93_rel>", y.min, " 
                                                            and longitudel93_rel<", x.max, " 
                                                            and latitudel93_rel<", y.max))
  
  ## Create dataframe with the observations table
  observations = dbGetQuery(con, "SELECT * from observations")
  observations = observations[which(observations$numchrono %in% stations$numchrono), ]
  
  ## Create dataframe with the species list table
  species = dbGetQuery(con, "SELECT * from splist")
  
  # Important fields:
  #   Stations:
  #     stations$numchrono:Unique identifier of the releve
  #     stations$date:Date of the releve(aaammjj)
  #     stations$coderqualif:releve type (consider everything if you want occurences, just R06 & R07 if you want community data)
  #     stations$imprecision_rel:precision of the releve (in m)
  #     stations$longitudel93_rel:X coordinates of the point (Lambert 93 EPSG:2154)
  #     stations$latitudel93_rel:Y coordinates of the point (Lambert 93 EPSG:2154)
  #   
  #   Observations:
  #     observations$numchrono:releve where the observation has benn made (related to stations$numchrono)
  #     observations$numtaxon:species id (related to species$numtaxon)
  #     observations$codestrate:vegetative strata of the occurence (when Braun Blanquet type)
  #     observations$codecover:Braun Blanquet abondance code (+,1,2,3,4,5) when relevant
  #     
  #   Species (the taxonomy is based on the own cbna taxo...):
  #     species$numtaxon:Unique species id
  #     species$libcbna:Latin name
  #     species$cd_ref:Accepted id in the INPN taxo (French reference)
  
  return(list(stations = stations
              , observations = observations
              , species = species))
}
