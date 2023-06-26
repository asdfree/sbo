# butchers, chandlers, baked
# sea shanty, filial pie
# call your mom and pop
sbo_MIcombine <-
	function( x , adjustment = 1.992065 ){
	
		# pull the structure of a variance-covariance matrix
		variance.shell <- suppressWarnings( vcov( x$var[[1]] ) )
		
		# initiate a function that will overwrite the diagonals.
		diag.replacement <-	
			function( z ){
				diag( variance.shell ) <- coef( z )
				variance.shell
			}
			
		# overwrite all the diagonals in the variance this_design object
		coef.variances <- lapply( x$var , diag.replacement )
	
		# add then divide by ten
		midpoint <- Reduce( '+' , coef.variances ) / 10
	
		# initiate another function that takes some object,
		# subtracts the midpoint, squares it, divides by ninety
		midpoint.var <- function( z ){ 1/10 * ( ( midpoint - z )^2 / 9 ) }
	
		# sum up all the differences into a single object
		variance <- Reduce( '+' , lapply( coef.variances , midpoint.var ) )
		
		# adjust every number with the factor in the user guide
		adj_var <- adjustment * variance

		# construct a result that looks like other sbo_MIcombine methods
		rval <-
			list( 
				coefficients = coef( x$coef ) ,
				variance = adj_var
			)
		
		# call it an MIresult class, like other sbo_MIcombine results
		class( rval ) <- 'MIresult'
		
		rval
	}

sbo_with <-
	function ( this_design , expr , ... ){
	
		pf <- parent.frame()
		
		expr <- substitute( expr )
		
		expr$design <- as.name(".design")

		# this pulls in means, medians, totals, etc.
		# notice it uses this_design$coef
		results <- eval( expr , list( .design = this_design$coef ) )
		
		# this is used to calculate the variance, adjusted variance, standard error
		# notice it uses the this_design$var object
		variances <- 
			lapply( 
				this_design$var$designs , 
				function( .design ){ 
					eval( expr , list( .design = .design ) , enclos = pf ) 
				} 
			)
		
		# combine both results..
		rval <- list( coef = results , var = variances )
		
		# ..into a brand new object class
		class( rval ) <- 'imputationResultList'
		
		rval
	}

sbo_subset <-
	function( x , ... ){
		
		# subset the survey object
		coef.sub <- subset( x$coef , ... )
		
		# replicate `var.sub` so it's got all the same attributes as `x$var`
		var.sub <- x$var
		
		# but then overwrite the `designs` attribute with a subset
		var.sub$designs <- lapply( x$var$designs , subset , ... )
		
		# now re-create the `sbosvyimputationList` just as before..
		sub.svy <-
			list(
				coef = coef.sub ,
				var = var.sub
			)
		
		# ..and give it the same class
		sub.svy$call <- sys.call(-1)

		sub.svy
	}

sbo_update <-
	function( x , ... ){
		
		# update the survey object that's going to be used for
		# means, medians, totals, etc.
		coef.upd <- update( x$coef , ... )
		
		# replicate `var.upd` so it's got all the same attributes as `x$var`
		var.upd <- x$var
		
		# but then overwrite the `designs` attribute with an update
		var.upd$designs <- lapply( x$var$designs , update , ... )
		
		# now re-create the `sbosvyimputationList` just as before
		upd.svy <-
			list(
				coef = coef.upd ,
				var = var.upd
			)
		
		upd.svy
	}

sbo_degf <- function( x ) degf( x$coef )

library(httr)
library(readr)

tf <- tempfile()

this_url <- "https://www2.census.gov/programs-surveys/sbo/datasets/2007/pums_csv.zip"

GET( this_url , write_disk( tf ) , progress() )

sbo_tbl <- read_csv( tf )

sbo_df <- data.frame( sbo_tbl )

names( sbo_df ) <- tolower( names( sbo_df ) )

sbo_df[ , 'one' ] <- 1
sbo_df[ , 'newwgt' ] <- 10 * sbo_df[ , 'tabwgt' ] * sqrt( 1 - 1 / sbo_df[ , 'tabwgt' ] )
# replace percent missings with zeroes
for( i in 1:4 ) sbo_df[ is.na( sbo_df[ , paste0( 'pct' , i ) ] ) , paste0( 'pct' , i ) ] <- 0

# sum up ownership ethnicity and gender
sbo_df[ , 'hispanic_pct' ] <- sbo_df[ , 'nonhispanic_pct' ] <- 0
sbo_df[ , 'male_pct' ] <- sbo_df[ , 'female_pct' ] <- 0

# loop through the first four owners' ethnicity and sex variables
for( i in 1:4 ) {

	sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'H' , 'hispanic_pct' ] <-
		sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'H' , 'hispanic_pct' ] +
		sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'H' , paste0( 'pct' , i ) ]
		
	sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'N' , 'nonhispanic_pct' ] <-
		sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'N' , 'nonhispanic_pct' ] +
		sbo_df[ sbo_df[ , paste0( 'eth' , i ) ] %in% 'N' , paste0( 'pct' , i ) ]
		
	sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'M' , 'male_pct' ] <-
		sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'M' , 'male_pct' ] +
		sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'M' , paste0( 'pct' , i ) ]
		
	sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'F' , 'female_pct' ] <-
		sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'F' , 'female_pct' ] +
		sbo_df[ sbo_df[ , paste0( 'sex' , i ) ] %in% 'F' , paste0( 'pct' , i ) ]
		
}
# sbo_fn <- file.path( path.expand( "~" ) , "SBO" , "this_file.rds" )
# saveRDS( sbo_df , file = sbo_fn , compress = FALSE )
# sbo_df <- readRDS( sbo_fn )
library(survey)
library(mitools)

# break random groups into ten separate data.frame objects within a list
var_list <- NULL

for( i in 1:10 ) { var_list <- c( var_list , list( subset( sbo_df , rg == i ) ) ) }

sbo_coef <-
	svydesign(
		id = ~ 1 ,
		weight = ~ tabwgt ,
		data = sbo_df
	)

sbo_var <-
	svydesign(
		id = ~ 1 ,
		weight = ~ newwgt ,
		data = imputationList( var_list )
	)

sbo_design <- list( coef = sbo_coef , var = sbo_var )

class( sbo_design ) <- 'sbosvyimputationList'
sbo_design <- 
	sbo_update( 
		sbo_design , 
		established_before_2000 =
			ifelse( established %in% c( '0' , 'A' ) , NA , as.numeric( established < 4 ) ) ,
			
		healthins =
			factor( healthins , levels = 1:2 ,
				labels = c( "offered health insurance" , "did not offer health insurance" )
			) ,
			
		hispanic_ownership =
			factor(
				ifelse( hispanic_pct == nonhispanic_pct , 2 ,
				ifelse( hispanic_pct > nonhispanic_pct , 1 , 
				ifelse( nonhispanic_pct > hispanic_pct , 3 , NA ) ) ) ,
				levels = 1:3 ,
				labels = c( 'hispanic' , 'equally hisp/non' , 'non-hispanic' )
			) ,
			
		gender_ownership =
			factor(
				ifelse( male_pct == female_pct , 2 ,
				ifelse( male_pct > female_pct , 1 , 
				ifelse( female_pct > male_pct , 3 , NA ) ) ) ,
				levels = 1:3 ,
				labels = c( 'male' , 'equally male/female' , 'female' )
			)
		
	)
sbo_MIcombine( sbo_with( sbo_design , svyby( ~ one , ~ one , unwtd.count ) ) )

sbo_MIcombine( sbo_with( sbo_design , svyby( ~ one , ~ gender_ownership , unwtd.count ) ) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ one ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ one , ~ gender_ownership , svytotal )
) )
sbo_MIcombine( sbo_with( sbo_design , svymean( ~ receipts_noisy ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ receipts_noisy , ~ gender_ownership , svymean )
) )
sbo_MIcombine( sbo_with( sbo_design , svymean( ~ n07_employer , na.rm = TRUE ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ n07_employer , ~ gender_ownership , svymean , na.rm = TRUE )
) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ receipts_noisy ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ receipts_noisy , ~ gender_ownership , svytotal )
) )
sbo_MIcombine( sbo_with( sbo_design , svytotal( ~ n07_employer , na.rm = TRUE ) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby( ~ n07_employer , ~ gender_ownership , svytotal , na.rm = TRUE )
) )
sbo_MIcombine( sbo_with( sbo_design ,
	svyquantile(
		~ receipts_noisy ,
		0.5 , se = TRUE 
) ) )

sbo_MIcombine( sbo_with( sbo_design ,
	svyby(
		~ receipts_noisy , ~ gender_ownership , svyquantile ,
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
		svyby( ~ receipts_noisy , ~ gender_ownership , svymean )
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

hispanic_receipts_result <-
	sbo_MIcombine( sbo_with( sbo_design , 
		svyby( ~ receipts_noisy , ~ hispanic_ownership , svytotal )
	) )

hispanic_payroll_result <-
	sbo_MIcombine( sbo_with( sbo_design , 
		svyby( ~ payroll_noisy , ~ hispanic_ownership , svytotal )
	) )

hispanic_employment_result <-
	sbo_MIcombine( sbo_with( sbo_design , 
		svyby( ~ employment_noisy , ~ hispanic_ownership , svytotal )
	) )
stopifnot( round( coef( hispanic_receipts_result )[ 'hispanic' ] , 0 ) == 350763923 )
stopifnot( round( coef( hispanic_receipts_result )[ 'equally hisp/non' ] , 0 ) == 56166354 )
stopifnot( round( coef( hispanic_receipts_result )[ 'non-hispanic' ] , 0 ) == 10540609303 )

stopifnot( round( coef( hispanic_payroll_result )[ 'hispanic' ] , 0 ) == 54367702 )
stopifnot( round( coef( hispanic_payroll_result )[ 'equally hisp/non' ] , 0 ) == 11083148 )
stopifnot( round( coef( hispanic_payroll_result )[ 'non-hispanic' ] , 0 ) == 1875353228 )

stopifnot( round( coef( hispanic_employment_result )[ 'hispanic' ] , 0 ) == 2026406 )
stopifnot( round( coef( hispanic_employment_result )[ 'equally hisp/non' ] , 0 ) == 400152 )
stopifnot( round( coef( hispanic_employment_result )[ 'non-hispanic' ] , 0 ) == 56889606 )

stopifnot( round( cv( hispanic_receipts_result )[ 'hispanic' ] , 2 ) == 0.02 )
stopifnot( round( cv( hispanic_receipts_result )[ 'equally hisp/non' ] , 2 ) == 0.06 )
stopifnot( round( cv( hispanic_receipts_result )[ 'non-hispanic' ] , 2 ) == 0 )

stopifnot( round( cv( hispanic_payroll_result )[ 'hispanic' ] , 2 ) == 0.01 )
stopifnot( round( cv( hispanic_payroll_result )[ 'equally hisp/non' ] , 2 ) == 0.06 )
stopifnot( round( cv( hispanic_payroll_result )[ 'non-hispanic' ] , 2 ) == 0 )

stopifnot( round( cv( hispanic_employment_result )[ 'hispanic' ] , 2 ) == 0.01 )
stopifnot( round( cv( hispanic_employment_result )[ 'equally hisp/non' ] , 2 ) == 0.05 )
stopifnot( round( cv( hispanic_employment_result )[ 'non-hispanic' ] , 2 ) == 0 )

