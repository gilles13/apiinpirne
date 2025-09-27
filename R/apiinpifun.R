globalVariables(c("simp", "dateEffet",
									"keyword",
									"sirene",
									"siren",
									"nom",
									"prenoms",
									"genre",
									"nafn5",
									"code",
									"libelle",
									"codeInseeCommune",
									"libape",
									"codeApe",
									"representantId",
									"name",
									"roleEntreprise",
									"typeDePersonne",
									"typePers",
									"inpi_codeRole",
									"libelle",
									"librole",
									"roleEnt",
									"ind.prenoms",
									"numVoie",
									"typeVoie",
									"codePostal",
									"voie",
									"urlTot",
									"commune"))

# FONCTIONS POUR LE PACKAGE apiinpirne

#' @title inpi_get_token
#' @description retourne le token necessaire aux requetes de l'API
#' @param user le mail renseigne lors de l'inscription
#' @param varenv le nom de la variable d'environnement stockant le mot de passe
#' @return string le token
#' @examples
#' \dontrun{
#' inpi_get_token(user = "gillesfidani@gmail.com", varenv = "INPI_CODE")
#' }
#' @export
inpi_get_token <- function(user = "gillesfidani@gmail.com", varenv = "INPI_CODE") {
	token <- 
		"https://registre-national-entreprises.inpi.fr/api/sso/login" |> 
		httr2::request() |> 
		httr2::req_body_json(list(username = user,
															password = Sys.getenv(varenv))) |> 
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = TRUE) |> 
		purrr::pluck("token")
	return(token)
}

#' @title inpi_get_siren
#' @description retourne le json de reponse de la requete sur un siren
#' @param siren le siren a interroger
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_get_siren(siren = "")
#' }
#' @export
inpi_get_siren <- function(siren = "", simp = TRUE) {
	siren <- gsub(" ", "", siren)
	urlBase <- "https://registre-national-entreprises.inpi.fr/api/companies/"
	urlTot <- paste0(urlBase, siren)
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
		httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = simp)
	return(res)
}

#' @title inpi_get_sirens
#' @description retourne le json de reponse de la requete sur plusieurs sirens
#' @param listesiren un vecteur contenant les sirens a interroger
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete sur plusieurs sirens
#' @examples
#' \dontrun{
#' inpi_get_sirens(listesiren = c("123456789", "987654321"))
#' }
#' @export
inpi_get_sirens <- function(listesiren = c("", ""), simp = TRUE) {
	listesiren <- gsub(" ", "", listesiren)
	temp <- paste0("siren[]=", listesiren)
	temp <- paste(temp, collapse = "&", sep = "")
	urlBase <- "https://registre-national-entreprises.inpi.fr/api/companies?pageSize=100&"
	urlTot <- paste0(urlBase, temp)
	message(urlTot)
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = simp)
	return(res)
}

#' @title inpi_get_by_acti
#' @description retourne le json de reponse de la requete sur un secteur d'activite
#' @param activite le nom de l'activite a interroger
#' @param simp if TRUE (default) simplifyVector
#' @return json non parse de la reponse a la requete sur activite
#' @examples
#' \dontrun{
#' inpi_get_by_acti(activite = "ARTISANALE", simp = TRUE)
#' }
#' @export
inpi_get_by_acti <- function(activite = "COMMERCIALE", simp = TRUE) {
	test_acti <- ifelse(!activite %in% c("COMMERCIALE",
																		 "AGENT_COMMERCIAL",
																		 "AGRICOLE_NON_ACTIF",
																		 "ARTISANALE",
																		 "ARTISANALE_REGLEMENTEE",
																		 "LIBERALE_REGLEMENTEE",
																		 "LIBERALE_NON_REGLEMENTEE",
																		 "GESTION_DE_BIENS",
																		 "SANS_ACTIVITE"),
											0,
											1)
	if(test_acti != 1) stop("Il faut choisir une activite parmi celles autorisees")
	urlBase <- "https://registre-national-entreprises.inpi.fr/api/companies?pageSize=100&activitySectors="
	ulrTot <- paste0(urlBase, activite)
	message(urlTot)
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = simp)
	return(res)
}

#' @title inpi_get_by_name
#' @description retourne le json de reponse de la requete sur un nom d'entreprise (requete type content)
#' @param nom le nom a chercher
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete sur nom
#' @examples
#' \dontrun{
#' inpi_get_by_name(nom = "FIDANI")
#' }
#' @export
inpi_get_by_name <- function(nom = "", simp = TRUE) {
	if(nom == "") stop("Il faut passer un nom a la fonction")
	urlBase <- "https://registre-national-entreprises.inpi.fr/api/companies?pageSize=100&companyName="
	urlTot <- paste0(urlBase, nom)
	message(urlTot)
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = simp)
	return(res)
}

#' @title inpi_test_persoju
#' @description retourne la personnalite juridique d'une requete sur siren(s)
#' @param x la requete a analyser
#' @return vecteur de la (ou des) personnalite(s) juridique(s) de la requete
#' @examples
#' \dontrun{
#' inpi_test_persoju(x = resultat1)
#' }
#' @export
inpi_test_persoju <- function(x) {
	perso <- names(x$formality$content)
	perso <- perso[!perso %in% c("succursaleOuFiliale",
															 "formeExerciceActivitePrincipale",
															 "natureCreation",
															 "natureCessationEntreprise",
															 "natureCessation",
															 "registreAnterieur")]
	return(perso)
}

#' @title inpi_test_type_pers
#' @description retourne le type de personne : P / M / E
#' @param x la requete a analyser
#' @return vecteur du ou des types de personne
#' @examples
#' \dontrun{
#' inpi_test_type_pers(x = resultat1)
#' }
#' @export
inpi_test_type_pers <- function(x) {
	typeperso <- x |> 
		purrr::pluck("formality", "typePersonne")
	return(typeperso)
}


#' @title inpi_test_var
#' @description retourne la valeur d'une variable si elle existe, sinon un message d'erreur
#' @param x l'objet contenant des variables
#' @param var le nom de la var a tester
#' @return tibble des moda et val de la var cherchee (si elle existe)
#' @examples
#' \dontrun{
#' inpi_test_var(x)
#' }
#' @export
inpi_test_var <- function(x, var = "") {
  if(var == "") {
    nomsvar <- x |> 
      purrr::pluck("formality", "content") |> 
      names()
    return(nomsvar)
    } else {
      nomsvar <- x |> 
        purrr::pluck("formality") |> 
        names()
      res <- any(nomsvar %in% var)
      return(res)
  }
}

#' @title inpi_parse_pe
#' @description retourne le json parse de reponse d'une requete sur personne exploitant
#' @param x la reponse json a parser
#' @return json parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_parse_pe(x = resultat1)
#' }
#' @export
inpi_parse_pe <- function(x) {
	tryCatch({
		nomExploit <-
			x |> 
			purrr::pluck("formality", "content",
									 "exploitation", "identite",
									 "entreprise", "nomExploitation", .default = NA)
		siren <- 
			x |> 
			purrr::pluck("formality", "content",
									 "exploitation", "identite",
									 "entreprise", "siren", .default = NA)
		natureCrea <- 
			x |> 
			purrr::pluck("formality", "content",
									 "natureCreation",
									 .default = NA)
		nicSiege <- 
			x |> 
			purrr::pluck("formality", "content",
									 "exploitation", "identite",
									 "entreprise", "nicSiege", .default = NA)
		descrEnt <- 
			x |> 
			purrr::pluck("formality", "content",
									 "exploitation", "etablissementPrincipal",
									 "descriptionEtablissement",
									  .default = NA)
		adresse <- 
			x |> 
			purrr::pluck("formality", "content",
									 "exploitation", "etablissementPrincipal",
									 "adresse",
									  .default = NA)
	}, error = function(e) {
		return(NULL)
	})
	resPE <- data.frame(siren,
											nomExploit,
											natureCrea,
											nicSiege,
											descrEnt,
											adresse)
	resPE <- 
		resPE |>
		dplyr::select(-tidyselect::ends_with("Present"))
	return(resPE)
}

#' @title inpi_parse_pm
#' @description retourne le json parse de reponse d'une requete sur personne morale
#' @param x la reponse json a parser
#' @return json parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_parse_pm(x = resultat1)
#' }
#' @export
inpi_parse_pm <- function(x) {
	tryCatch({
		siren <-
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "entreprise", "siren", .default = NA)
		denom <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "entreprise", "denomination", .default = NA)
		nomCom <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "etablissementPrincipal",
									 "descriptionEtablissement",
									 "nomCommercial", .default = NA)
		formeju <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "entreprise", "formeJuridique", .default = NA)
		codeApe <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "entreprise", "codeApe", .default = NA)
		montantCapital <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "description", "montantCapital", .default = NA)
		deviseCapital <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "description", "deviseCapital", .default = NA)
		objetEntr <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "identite",
									 "description", "objet", .default = NA)
		adresseEnt <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "adresseEntreprise",
									 "adresse", .default = NA)
	}, error = function(e) {
		return(NULL)
	})
	resPM <- data.frame(
								siren,
								denom, nomCom, formeju,
								objetEntr,
								codeApe, 
								montantCapital, deviseCapital,
								adresseEnt)
	myvars <- c("depcom"="codeInseeCommune")
	resPM <- 
		resPM |>
		dplyr::select(-tidyselect::ends_with("Present"))
	resPM <- 
	  resPM |> 
	  dplyr::rename(tidyselect::any_of(myvars))
# 		tidyr::drop_na(siren) |> 
# 		dplyr::arrange(siren)
	return(resPM)
}

#' @title inpi_parse_pp
#' @description retourne le json parse de reponse d'une requete sur personne physique
#' @param x la reponse json a parser
#' @return json parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_parse_pp(x = resultat1)
#' }
#' @export
inpi_parse_pp <- function(x) {
	res2 <- x |> 
		purrr::pluck("formality", "content", "personnePhysique",
								 "identite", "entreprise", .default = NA) |> 
		data.frame()
	res3 <- x |> 
		purrr::pluck("formality", "content", "personnePhysique",
								 "identite", "entrepreneur",
								 "descriptionPersonne", .default = NA) |> 
		data.frame()
	# ADRESSE ENTREPRISE
	res4 <- x |> 
		purrr::pluck("formality", "content", "personnePhysique",
								 "adresseEntreprise", "adresse", .default = NA) |> 
		data.frame()
	# DESCRIPTION ETAB
	rolePourEnt <- x |> 
		purrr::pluck("formality", "content", "personnePhysique",
								 "etablissementPrincipal", "descriptionEtablissement",
								 "rolePourEntreprise",
								 .default = NA)
	nomCom <- x |> 
		purrr::pluck("formality", "content", "personnePhysique",
								 "etablissementPrincipal", "descriptionEtablissement",
								 "nomCommercial",
								 .default = NA)
	resPP <- data.frame(res2, res3, res4, role=rolePourEnt, nomCo=nomCom)
	resPP <- resPP[, !duplicated(colnames(resPP))]
	resPP <- resPP |> dplyr::select(-tidyselect::ends_with("Present"))
	resPP <- resPP |> tidyr::drop_na(siren)
	# REMOVE DUPLICATED LINES
	resPP <- resPP[!duplicated(resPP), ]
	resPP <- 
		resPP |> 
		dplyr::relocate(siren) |>
		dplyr::relocate(nom, .after = siren) |>
		dplyr::relocate(prenoms, .after = nom) |>
		dplyr::relocate(genre, .after = prenoms)
	resPP <- 
		resPP |> 
		dplyr::left_join(y = nafn5 |> dplyr::mutate(code = gsub("\\.", "", code)),
										 by = c("codeApe" = "code")) |> 
		dplyr::rename(libape=libelle) |> 
		dplyr::rename(codeInsee=codeInseeCommune) |> 
		dplyr::relocate(libape, .after = codeApe) |> 
		dplyr::arrange(siren)
	return(resPP)
}

#' @title inpi_parse_meta
#' @description retourne le json parse de reponse d'une requete QUELCONQUE
#' @param x la reponse json a parser
#' @return json parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_parse_meta(x = resultat1)
#' }
#' @export
inpi_parse_meta <- function(x) {
	system2("clear")
	persoju <- x |> inpi_test_persoju()
	if(length(persoju) > 1) {
		res0 <- x |> inpi_parse_pm()
		res1 <- x |> inpi_parse_pp()
		resFinal <- list("PERSO_MORALE" = res0,
										 "PERSO_PHYSIQ" = res1)
		return(resFinal)
	} else {
		if(persoju == "personnePhysique") {
			resPP <- x |> inpi_parse_pp()
			return(resPP)
		} else {
			if(persoju == "personneMorale") {
			# PM
			resPM <- x |> inpi_parse_pm()
			return(resPM)
			}
			if(persoju == "exploitation") {
				resPE <- x |> inpi_parse_pe()
				return(resPE)
			}
		}
	}
}

#' @title inpi_vital
#' @description retourne les colonnes vitales
#' @param x la reponse json a parser
#' @return data.frame avec qques colonnes
#' @examples
#' \dontrun{
#' inpi_vital(x = resultat1)
#' }
#' @export
inpi_vital <- function(x) {
	x |> 
				dplyr::select(
					-tidyselect::any_of(c(
						"representantId",
						"indicateurActifAgricole",
						"actif",
						"metierArt",
						"secondRoleEntreprise",
						"libelleSecondRoleEntreprise",
						"mentionDemissionOrdre",
						"indicateurSecondRoleEntreprise",
						"ind.metierArt",
						"ind.codePays",
						"ind.cedex",
						"ind.pays",
						"ind.codePostal",
						"ind.commune",
						"ind.codeInseeCommune",
						"ind.numUsage",
						"ind.genre",
						"ind.role",
						"ent.roleEntreprise",
						"ent.pays",
						"ent.lieuRegistre",
						"ent.formeJuridique",
						"rep.nom",
						"rep.prenoms",
						"rep.dateDeNaissance",
						"rep.indicateurActifAgricole",
						"rep.genre",
						"rep.nomUsage",
						"rep.pays",
						"rep.codePays",
						"rep.codePostal",
						"rep.commune",
						"rep.codeInseeCommune",
						"pays",
						"codePays",
						"cedex",
						"codePostal",
						"commune",
						"voie",
						"complementLocalisation",
						"")))
}

#' @title inpi_relocate
#' @description retourne les colonnes de l'adresse dans le bon ordre
#' @param x la reponse json a parser
#' @return data.frame avec les colonnes d'adresse dans le bon ordre
#' @examples
#' \dontrun{
#' inpi_relocate(x = resultat1)
#' }
#' @export
inpi_relocate <- function(x) {
	x |> 
		dplyr::relocate(numVoie, .before = typeVoie) |>
		dplyr::relocate(codePostal, .after = voie) |> 
		dplyr::relocate(commune, .after = codePostal)
}

#' @title inpi_pouvoirs
#' @description retourne les colonnes du pouvoir !
#' @param x la reponse json a parser
#' @return data.frame avec les infos sur les pouvoirs des PM
#' @examples
#' \dontrun{
#' inpi_pouvoirs(x = resultat1)
#' }
#' @export
inpi_pouvoirs <- function(x) {
	tryCatch({
	  myvars <- c("depcom"="codeInseeCommune",
								"naiss"="ind.dateDeNaissance",
								"siren"="ent.siren",
								"denom"="ent.denomination")
		resPouvoirs <- 
			x |> 
			purrr::pluck("formality", "content",
									 "personneMorale", "composition",
									 "pouvoirs", .default = NULL) |> 
			data.frame() |> 
			tibble::as_tibble() |> 
			jsonlite::flatten() |> 
			dplyr::select(-tidyselect::ends_with("Present")) |> 
			tidyr::pivot_longer(cols = -representantId,
													values_transform = list(value = as.character)) |> 
			dplyr::mutate(
										name = gsub("individu.", "ind.", name),
										name = gsub("representant.", "rep.", name),
										name = gsub("entreprise.", "ent.", name),
										name = gsub("descriptionPersonne.", "", name),
										name = gsub("adresseDomicile.", "", name),
										name = gsub("adresseEntreprise.", "", name)
										) |> 
			tidyr::pivot_wider(id_cols = representantId) |> 
			dplyr::rename(roleEnt=roleEntreprise) |> 
			dplyr::rename(typePers=typeDePersonne) |> 
			dplyr::left_join(y = inpi_codeRole, by = c("roleEnt" = "code")) |> 
			dplyr::rename(librole=libelle) |> 
			dplyr::relocate(librole, .after = roleEnt) |> 
		  dplyr::arrange(dplyr::desc(typePers), roleEnt) |> 
		  dplyr::rename(tidyselect::any_of(myvars)) |> 
		  dplyr::select(-tidyselect::any_of(c("numVoie", "typeVoie"))) |> 
			inpi_vital()
		if(exists("ind.prenoms", where = resPouvoirs)) {
		  resPouvoirs <- 
		    resPouvoirs |>
				dplyr::mutate(ind.prenoms = gsub("\\\"", "", ind.prenoms),
											ind.prenoms = gsub("(c\\(|\\))", "", ind.prenoms)) |> 
				dplyr::rename(prenoms = ind.prenoms)
		}
# 		if(exists("ent.denomination", where = resPouvoirs)) {
# 		  resPouvoirs <- 
# 		    resPouvoirs |>
# 		    tidyr::unite(col = "identif", c(ind.nom, ent.denomination), na.rm = TRUE)
# 		}
		return(resPouvoirs)
	}, error = function(e) {
		return(NULL)
	})
}

#' @title inpi_histo
#' @description retourne les colonnes de l'historique
#' @param x la reponse json a parser
#' @return data.frame avec les infos sur l'historique
#' @examples
#' \dontrun{
#' inpi_histo(x = resultat1)
#' }
#' @export
inpi_histo <- function(x) {
  tryCatch({
    resHisto <- x |> 
      purrr::pluck("formality", "historique") |> 
      dplyr::select(tidyselect::any_of(c(
        "dateEffet",
        "numeroLiasse",
        "codeEvenement",
        "libelleEvenement"))) |> 
      dplyr::mutate(dateEffet = as.Date(dateEffet, format = "%Y-%m-%dT%H:%M:%S+%H:%M")) |> 
      dplyr::arrange(dplyr::desc(dateEffet))
    return(resHisto)
  }, error = function(x) return(NULL)
  )
}
  
#' @title inpi_meta_px
#' @description retourne les colonnes des px
#' @param x la reponse json a parser
#' @return data.frame avec les infos sur les PX
#' @examples
#' \dontrun{
#' inpi_meta_px(x = resultat1)
#' }
#' @export
inpi_meta_px <- function(x) {
	typePers <- x |> inpi_test_type_pers()
	nbPers <- length(typePers)
	maliste <- vector(mode = "list", length = length(typePers))
# 	return(typePers)
	if(length(typePers) == 1) {
			resPX <- switch(typePers,
											P = inpi_parse_pp(x),
											M = inpi_parse_pm(x),
											E = inpi_parse_pe(x)
											)
			return(resPX)
	} else {
		if(length(typePers) > 1) {
			malist <- vector(mode = "list", length = 0)
			for(i in seq_len(nbPers)) {
				tp <- typePers[i]
				y <- x[i, ]
				res <- switch(tp,
											M = inpi_parse_pm(y),
											P = inpi_parse_pp(y),
											E = inpi_parse_pe(y)
											)
				res <- list(res)
				malist <- append(x = malist, values = res)
			}
			return(malist)
		}
	}
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ACTES

#' @title inpi_get_acte_infos
#' @description retourne le json de reponse de la requete sur un acte
#' @param siren le siren a interroger
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_get_acte_infos(siren = "")
#' }
#' @export
inpi_get_acte_infos <- function(siren = "", simp = TRUE) {
  siren <- gsub(" ", "", siren)
  urlBase <- "https://registre-national-entreprises.inpi.fr/api/companies/"
  urlTot <- paste0(urlBase, siren, "/attachments")
  res <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = simp)
  return(res)
}

#' @title inpi_get_acte
#' @description retourne le json de reponse de la requete sur un acte
#' @param id l'identifiant a interroger
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_get_acte(id = "")
#' }
#' @export
inpi_get_acte <- function(id = "", simp = TRUE) {
  urlBase <- "https://registre-national-entreprises.inpi.fr/api/actes/"
  urlTot <- paste0(urlBase, id)
  res <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = simp)
  return(res)
}

# ------------------------------------------------------------------------------

#' @title inpi_get_acte_pdf
#' @description retourne le pdf de l'acte demande
#' @param id l'identifiant a interroger
#' @return le fichier pdf de l'acte
#' @examples
#' \dontrun{
#' inpi_get_acte_pdf(id = "")
#' }
#' @export
inpi_get_acte_pdf <- function(id = "") {
	pdfpath <- tempfile(fileext = ".pdf")
  urlBase <- "https://registre-national-entreprises.inpi.fr/api/actes/"
  urlTot <- paste0(urlBase, id, "/download")
  message(urlTot)
  pdfraw <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/pdf') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
    httr2::req_perform() |> 
		httr2::resp_body_raw()
	writeBin(pdfraw, pdfpath)
	system2("xdg-open", pdfpath)
	message(paste0("Le fichier pdf : ", pdfpath))
  return(pdfraw)
}

#' @title inpi_get_bilan_pdf
#' @description retourne le pdf du bilan demande
#' @param id l'identifiant du bilan a consulter
#' @return le fichier pdf du bilan
#' @examples
#' \dontrun{
#' inpi_get_bilan_pdf(id = "")
#' }
#' @export
inpi_get_bilan_pdf <- function(id = "") {
	pdfpath <- tempfile(fileext = ".pdf")
  urlBase <- "https://registre-national-entreprises.inpi.fr/api/bilans/"
  urlTot <- paste0(urlBase, id, "/download")
  message(urlTot)
  pdfraw <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/pdf') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
    httr2::req_perform() |> 
		httr2::resp_body_raw()
	writeBin(pdfraw, pdfpath)
	system2("xdg-open", pdfpath)
	message(paste0("Le fichier pdf : ", pdfpath))
  return(pdfraw)
}

# NE FONCTIONNE PAS
#' @title inpi_get_bilan_saisis
#' @description retourne le json de reponse de la requete sur un bilan saisis
#' @param id l'identifiant a interroger
#' @param simp simplifier le json ? default = TRUE
#' @return json non parse de la reponse a la requete
#' @examples
#' \dontrun{
#' inpi_get_bilan_saisis(id = "")
#' }
#' @export
inpi_get_bilan_saisis <- function(id = "", simp = TRUE) {
  id <- gsub(" ", "", id)
  urlBase <- "https://registre-national-entreprises.inpi.fr/api/bilans-saisis/"
  urlTot <- paste0(urlBase, id)
  message(urlTot)
  res <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_headers(Authorization = paste("Bearer", inpi_get_token())) |>
    httr2::req_perform()
  return(res)
}

#' @title inpi_get_extrait
#' @description retourne l'extrait inpi du siren demande
#' @param siren le siren a interroger
#' @param url si true la fonction ne retourne que le lien, pas le doc
#' @return un pdf de l'extrait inpi ou un lien
#' @examples
#' \dontrun{
#' inpi_get_extrait(siren = "123456789", url = FALSE)
#' }
#' @export
inpi_get_extrait <- function(siren, url = FALSE) {
  siren <- gsub(" ", "", siren)
  pdfpath <- tempfile(fileext = ".pdf")
  urlBase <- "https://data.inpi.fr/export/companies?format=pdf&ids="
  urlTot <- 
    paste0(urlBase,
           "[%22",
           siren,
           "%22]")
	if(url) {
		return(urlTot)
	} else {
    pdfraw <- 
      urlTot |> 
      httr2::request() |> 
      httr2::req_headers("Accept" = 'application/pdf') |>  
      httr2::req_perform() |> 
      httr2::resp_body_raw()
  writeBin(pdfraw, pdfpath)
  # system2("xdg-open", pdfpath)
  utils::browseURL(pdfpath)
  message(paste0("Le fichier pdf : ", pdfpath))
	}
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

