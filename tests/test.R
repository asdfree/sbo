if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available SBO microdata files
sbo_cat <-
	get_catalog( "sbo" ,
		output_dir = file.path( getwd() ) )

# 2015 only
sbo_cat <- subset( sbo_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( sbo_cat ) > 0 )

library(survey)

sbo_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

sbo_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = sbo_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
sbo_design <- 
	update( 
		sbo_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( sbo_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , sbo_design , unwtd.count )
svytotal( ~ one , sbo_design )

svyby( ~ one , ~ ever_smoked_marijuana , sbo_design , svytotal )
svymean( ~ bmipct , sbo_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , sbo_design , svymean , na.rm = TRUE )
svymean( ~ q2 , sbo_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , sbo_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , sbo_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , sbo_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , sbo_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , sbo_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , sbo_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	sbo_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	sbo_design ,
	na.rm = TRUE
)
sub_sbo_design <- subset( sbo_design , qn41 == 1 )
svymean( ~ bmipct , sub_sbo_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , sbo_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		sbo_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( sbo_design )
svyvar( ~ bmipct , sbo_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , sbo_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , sbo_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , sbo_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , sbo_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	sbo_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		sbo_design 
	)

summary( glm_result )
library(srvyr)
sbo_srvyr_design <- as_survey( sbo_design )
sbo_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

sbo_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

