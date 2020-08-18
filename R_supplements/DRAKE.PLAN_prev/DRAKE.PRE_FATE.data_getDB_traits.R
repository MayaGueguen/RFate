
getDB_ANDROSACE = function()
{
  ## Create a connection : save username and password
  username <- { "r_user_androsace" }
  pw <- { "r_user_androsace" }
  
  ## Load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  ## Create a connection to the postgres database
  ## note that "con" will be used later in each connection to the database
  con <- dbConnect(drv,
                   dbname = "androsace_v3",
                   # host = "152.77.214.210",
                   host = "129.88.201.97",
                   port = 5432,
                   user = username,
                   password = pw
  )
  rm(pw) # removes the password
  
  ## Create dataframe with the measured traits
  traits = dbGetQuery(con, " SELECT clean_mesure_traits.id,
                                  clean_mesure_traits.code_cbna,
                                  taxons.inpn,
                                  clean_mesure_traits.type,
                                  clean_mesure_traits.valeur,
                                  clean_modalite.nom,
                                  clean_modalite.description,
                                  clean_p_mesure_groupe.code,
                                  clean_mesure_traits.date_saisie,
                                  clean_personne.nom AS expert,
                                  clean_bibliographie.reference AS bibliographie,
                                  clean_mesure_traits.commentaire,
                                  taxons.libelle,
                                  taxons.rattach
                          FROM taxons
                          JOIN clean_mesure_traits ON clean_mesure_traits.code_cbna = taxons.code_cbna
                          JOIN clean_modalite ON clean_mesure_traits.ref_modalite = clean_modalite.id
                          JOIN clean_p_mesure_groupe ON clean_mesure_traits.ref_p_m_groupe = clean_p_mesure_groupe.id
                          JOIN clean_ressource ON clean_ressource.id = clean_mesure_traits.source_id
                          LEFT JOIN clean_bibliographie ON clean_ressource.ref_biblio = clean_bibliographie.id
                          LEFT JOIN clean_personne ON clean_ressource.ref_personne = clean_personne.id
                          ORDER BY taxons.libelle,clean_p_mesure_groupe.code;")
  
  return(traits)
}
