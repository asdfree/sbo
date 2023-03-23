# butchers, chandlers, baked
# sea shanty, filial pie
# call your mom and pop
#' dual design calculations for the survey of business owners
#'
#' the code{mitools::sbo_MIcombine} variant includes a 2007-specific variance adjustment. this will change in other years.
#' this adjustment statistic was pulled from the middle of page 8
#' url{https://www2.census.gov/econ/sbo/07/pums/2007_sbo_pums_users_guide.pdf#page=8}
#'
#' each of these sbo-specific functions contain a variant of some other code{library(survey)} function that also maintains the census bureau's dual design calculation.
#' these functions expect both the coefficient and the variance survey objects
#'
sbo_MIcombine <-
	function( x , adjustment = 1.992065 ){
	
		# just pull the structure of a variance-covariance matrix
		variance.shell <- suppressWarnings( vcov( x$var[[1]] ) )
		
		# initiate a function that will overwrite the diagonals.
		diag.replacement <-	
			function( z ){
				diag( variance.shell ) <- coef( z )
				variance.shell
			}
			
		# overwrite all the diagonals in the variance svy.sbo object
		coef.variances <- lapply( x$var , diag.replacement )
	
		# add 'em all together and divide by ten.
		midpoint <- Reduce( '+' , coef.variances ) / 10
	
		# initiate another function that takes some object,
		# subtracts the midpoint, squares it, and divides by ninety
		midpoint.var <- function( z ){ 1/10 * ( ( midpoint - z )^2 / 9 ) }
	
		# sum up all the differences into a single object
		variance <- Reduce( '+' , lapply( coef.variances , midpoint.var ) )
		
		# adjust every. single. number.
		adj_var <- adjustment * variance

		# construct a result that looks a lot like
		# other sbo_MIcombine methods.
		rval <-
			list( 
				coefficients = coef( x$coef ) ,
				variance = adj_var
			)
		
		# call it an MIresult class, just like all other sbo_MIcombine results.
		class( rval ) <- 'MIresult'
		
		# return it at the end of the function.
		rval
	}

sbo_with <-
	function ( sbo.svy , expr , ... ){
	
		pf <- parent.frame()
		
		expr <- substitute( expr )
		
		expr$design <- as.name(".design")

		# this pulls in means, medians, totals, etc.
		# notice it uses sbo.svy$coef
		results <- eval( expr , list( .design = sbo.svy$coef ) )
		
		gc()
		
		# this is used to calculate the variance, adjusted variance, standard error
		# notice it uses the sbo.svy$var object
		variances <- 
			lapply( 
				sbo.svy$var$designs , 
				function( .design ){ 
					eval( expr , list( .design = .design ) , enclos = pf ) 
				} 
			)
		
		gc()
		
		# combine both results..
		rval <- list( coef = results , var = variances )
		
		# ..into a brand new object class
		class( rval ) <- 'imputationResultList'
		
		gc()
		
		# and return it.
		rval
	}

sbo_subset <-
	function( x , ... ){
		
		# subset the survey object that's going to be used for
		# means, medians, totals, etc.
		coef.sub <- subset( x$coef , ... )
		
		gc()
		
		# replicate `var.sub` so it's got all the same attributes as `x$var`
		var.sub <- x$var
		
		# but then overwrite the `designs` attribute with a subset
		var.sub$designs <- lapply( x$var$designs , subset , ... )
		
		gc()
		
		# now re-create the `sbosvyimputationList` just as before
		sub.svy <-
			list(
				coef = coef.sub ,
				var = var.sub
			)
		
		# ..class it..
		sub.svy$call <- sys.call(-1)
		
		gc()
		
		# ..return it. done.
		sub.svy
	}

sbo_update <-
	function( x , ... ){
		
		# update the survey object that's going to be used for
		# means, medians, totals, etc.
		coef.upd <- update( x$coef , ... )
		
		gc()
		
		# replicate `var.upd` so it's got all the same attributes as `x$var`
		var.upd <- x$var
		
		# but then overwrite the `designs` attribute with an update
		var.upd$designs <- lapply( x$var$designs , update , ... )
		
		gc()
		
		# now re-create the `sbosvyimputationList` just as before
		upd.svy <-
			list(
				coef = coef.upd ,
				var = var.upd
			)
			
		gc()
		
		# ..return it. done.
		upd.svy
	}

sbo_degf <- function( x ) survey:::degf( x$coef )

library(readr)

tf <- tempfile()

this_url <- "https://www2.census.gov/programs-surveys/sbo/datasets/2007/pums_csv.zip"

download.file( this_url , tf , mode = 'wb' )

sbo_tbl <- read_csv( tf )

sbo_df <- data.frame( sbo_tbl )

names( sbo_df ) <- tolower( names( sbo_df ) )

sbo_df[ , 'one' ] <- 1

# and use the weights displayed in the census bureau's technical documentation
sbo_df[ , 'newwgt' ] <- 10 * sbo_df[ , 'tabwgt' ] * sqrt( 1 - 1 / sbo_df[ , 'tabwgt' ] )
# https://www2.census.gov/econ/sbo/07/pums/2007_sbo_pums_users_guide.pdf#page=7

# sbo_fn <- file.path( path.expand( "~" ) , "SBO" , "this_file.rds" )
# saveRDS( sbo_df , file = sbo_fn , compress = FALSE )
# sbo_df <- readRDS( sbo_fn )
library(survey)
library(mitools)

var_list <- NULL

for( i in 1:10 ) { var_list <- c( var_list , list( subset( sbo_df , rg == i ) ) ) }

#####################################################
# survey design for a hybrid database-backed object #
#####################################################

# create a survey design object with the SBO design
# to use for the coefficients: means, medians, totals, etc.
sbo_coef <-
	svydesign(
		id = ~ 1 ,
		weight = ~ tabwgt ,
		data = sbo_df
	)
# this one just uses the original table `x`

# create a survey design object with the SBO design
# to use for the variance and standard error
sbo_var <-
	svydesign(
		id = ~ 1 ,
		weight = ~ newwgt ,
		data = mitools::imputationList( var_list )
	)

# rm( var_list ) ; gc()
# this one uses the ten `x1` thru `x10` tables you just made.

# slap 'em together into a single list object..
sbo_design <- list( coef = sbo_coef , var = sbo_var )
class( sbo_design ) <- 'sbosvyimputationList'

# # keep only the variables you need
# variables_to_keep <- 
	# c( 
		# "one" , 
		# "newwgt" , 
		# "tabwgt" , 
		# "receipts_noisy" ,
		# "employment_noisy" ,
		# "n07_employer" ,
		# "established" ,
		# "healthins" ,
		# "husbwife"
	# )

# # keep only columns used in this analysis
# sbo_design$coef$variables <-
	# sbo_design$coef$variables[ variables_to_keep ]
	
# sbo_design$var <-
	# lapply( 
		# sbo_design$var , 
		# function( w ){
			# w$variables <- w$variables[ variables_to_keep ]
			# w
		# }
	# )
	
# gc()
# # this step conserves RAM
sbo_design <- 
	sbo_update( 
		sbo_design , 
		established_before_2000 =
			ifelse( established %in% c( '0' , 'A' ) , NA , as.numeric( established < 4 ) ) ,
			
		healthins =
			factor( healthins , levels = 1:2 ,
				labels = c( "offered health insurance" , "did not offer health insurance" )
			)
	)

gc()
sbo_MIcombine( sbo_with( sbo_design , svyby( ~ one , ~ one , unwtd.count ) ) )

sbo_MIcombine( sbo_with( sbo_design , svyby( ~ one , ~ healthins , unwtd.count ) ) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ one ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ one , ~ healthins , svytotal )
) )
sbo_MIcombine( sbo_with( sbo_design , svymean( ~ receipts_noisy ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ receipts_noisy , ~ healthins , svymean )
) )
sbo_MIcombine( sbo_with( sbo_design , svymean( ~ n07_employer , na.rm = TRUE ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ n07_employer , ~ healthins , svymean , na.rm = TRUE )
) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ receipts_noisy ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ receipts_noisy , ~ healthins , svytotal )
) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ n07_employer , na.rm = TRUE ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ n07_employer , ~ healthins , svytotal , na.rm = TRUE )
) )
sbo_MIcombine( sbo_with( sbo_design ,
	svyquantile(
		~ receipts_noisy ,
		0.5 , se = TRUE 
) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby(
		~ receipts_noisy , ~ healthins , svyquantile ,
		0.5 , se = TRUE ,
		ci = TRUE 
) ) )
sbo_MIcombine( sbo_with( sbo_design ,
	svyratio( numerator = ~ receipts_noisy , denominator = ~ employment_noisy )
) )
sub_sbo_design <- sbo_subset( sbo_design , husbwife %in% 1:3 )
sbo_MIcombine( sbo_with( sub_sbo_design , svymean( ~ receipts_noisy ) ) )
this_result <-
	sbo_MIcombine( sbo_with( sbo_design ,
		svymean( ~ receipts_noisy )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	sbo_MIcombine( sbo_with( sbo_design ,
		svyby( ~ receipts_noisy , ~ healthins , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
sbo_degf( sbo_design )
sbo_MIcombine( sbo_with( sbo_design , svyvar( ~ receipts_noisy ) ) )
# SRS without replacement
sbo_MIcombine( sbo_with( sbo_design ,
	svymean( ~ receipts_noisy , deff = TRUE )
) )

# SRS with replacement
sbo_MIcombine( sbo_with( sbo_design ,
	svymean( ~ receipts_noisy , deff = "replace" )
) )
# # sbo_MIsvyciprop( ~ established_before_2000 , sbo_design ,
# 	method = "likelihood" , na.rm = TRUE )
# # sbo_MIsvyttest( receipts_noisy ~ established_before_2000 , sbo_design )
# # sbo_MIsvychisq( ~ established_before_2000 + n07_employer , sbo_design )
glm_result <- 
	sbo_MIcombine( sbo_with( sbo_design ,
		svyglm( receipts_noisy ~ established_before_2000 + n07_employer )
	) )
	
glm_result

