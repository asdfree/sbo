if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "sbo" , output_dir = file.path( getwd() ) )

gc()

options( survey.lonely.psu = "adjust" )

library(survey)
library(mitools)

sbo_design <- 
	readRDS( file.path( getwd() , "2007 main.rds" ) )
	
# keep only the variables you need
variables_to_keep <- 
	c( 
		"one" , 
		"newwgt" , 
		"tabwgt" , 
		"receipts_noisy" ,
		"employment_noisy" ,
		"n07_employer" ,
		"established" ,
		"healthins" ,
		"husbwife"
	)

# keep only columns used in this analysis
sbo_design$coef$variables <-
	sbo_design$coef$variables[ variables_to_keep ]
	
sbo_design$var <-
	lapply( 
		sbo_design$var , 
		function( w ){
			w$variables <- w$variables[ variables_to_keep ]
			w
		}
	)
	
gc()
# this step conserves RAM
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
		keep.var = TRUE , ci = TRUE 
) ) )
sbo_MIcombine( sbo_with( sbo_design ,
	svyratio( numerator = ~ receipts_noisy , denominator = ~ employment_noisy )
) )
sub_sbo_design <- sbo_subset( sbo_design , husbwife %in% 1:3 )
sbo_MIcombine( sbo_with( sub_sbo_design , svymean( ~ receipts_noisy ) ) ) ; rm( sub_sbo_design ) ; gc()
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
sbo_MIsvyciprop( ~ established_before_2000 , sbo_design ,
	method = "likelihood" , na.rm = TRUE ) ; gc()
# not implemented sbo_MIsvyttest( receipts_noisy ~ established_before_2000 , sbo_design )
# not implemented sbo_MIsvychisq( ~ established_before_2000 + n07_employer , sbo_design )
glm_result <- 
	sbo_MIcombine( sbo_with( sbo_design ,
		svyglm( receipts_noisy ~ established_before_2000 + n07_employer )
	) )
	
glm_result

