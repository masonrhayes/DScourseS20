-- Creating SQL file to read data from fl_insurance_sample.csv

-- First, create the table that the CSV will be stored in
CREATE TABLE "FL_insurance" (
	"policyID" INTEGER,
	"statecode" CHAR,
	"county" CHAR,
	"eq_site_limit" DECIMAL,
	"hu_site_limit" DECIMAL,
	"fl_site_limit" INTEGER,
	"fr_site_limit" DECIMAL,
	"tiv_2011" DECIMAL,
    "tiv_2012" DECIMAL,
    "eq_site_deductible" DECIMAL, 
    "hu_site_deductible" DECIMAL,
    "fl_site_deductible" DECIMAL,
    "fr_site_deductible" DECIMAL, 
    "point_latitude" DECIMAL, 
    "point_longitude" DECIMAL, 
    "line" CHAR, 
    "construction" CHAR, 
    "point_granularity" INTEGER
    );
.mode csv
.import "FL_insurance_sample.csv" FL_insurance;

-- Print first 10 observations
SELECT * FROM FL_insurance LIMIT 10;

-- Select unique/distinct counties
SELECT DISTINCT county FROM FL_insurance;

-- Show average appreciation/depreciation from 2011 to 2012
SELECT AVG(tiv_2012-tiv_2011) FROM FL_insurance;

-- Select 
SELECT construction, COUNT(*) FROM FL_insurance GROUP BY construction;