
<!-- README.md is generated from README.Rmd. Please edit that file -->
ox
==

The goal of the `ox` package is to read OpenClinica odm 1.3 xml export files, and create dataframes for data and metadata (or an `ox_all` object, a list containing both) from parsed xml files. Also, tidy dataframes can be created for any ItemGroup, with optional definition of factors from codelists.

`ox` is currently under development.

Installation
------------

You can install the development version of `ox` from [GitHub](https://github.com/acobos/ox) with:

``` r
install_github("acobos/ox")
```

Example
-------

This is a basic example which shows you how to solve a common problem: create a tidy dataframe for a group of related items (an **ItemGroup** in OpenCinica terminology), e.g., for demographic data.

First, load packages `XML`and `ox`. Then parse the xml file using `xmlParse` from package `XML`, and use function `ox_all()` to create an `ox_all` object. This involves reading data and metadata, and is a slow process. It may take a lot of time with any real study!

With `ox_info()` you will see some basic information of this object.

``` r

# load packages 
library(XML)
library(ox)

# the xml file address/name
xml_file <- system.file("extdata",
                        "odm1.3_full_example.xml",
                        package = "ox",
                        mustWork = TRUE)

# parse the xml file
doc <- xmlParse(xml_file) 

# create an (ox_all) object 
d <- ox_all(doc)
> Getting ItemData nodes...
> Extracting data from ItemData nodes...
> Done
> Extracting metadata...
> Done

# get basic info
ox_info(d)
> $numbers
> datapoints   subjects      sites     events      forms     groups 
>        240          2          2          6         12         15 
>      items 
>        106 
> 
> $events
> [1] "SE_BASELINE"          "SE_RW1"               "SE_ENDOFRADIOTHERAPY"
> [4] "SE_ACUTETOXICITY"     "SE_FM1"               "SE_FM3"              
> 
> $forms
>  [1] "F_INFORMEDCONS_1"  "F_IECRITERIA_11"   "F_COMORBIDITIE_11"
>  [4] "F_DEMO_1"          "F_CANCERHISTOL_11" "F_PREVMEDANTIN_11"
>  [7] "F_SURGERY_1"       "F_RANDOM_4"        "F_PHYSICALEXAM_11"
> [10] "F_RADIOTHERAPY_2"  "F_ACUTETOXICIT_3"  "F_SURVIVALANDR_21"
> 
> $groups
>  [1] "IG_INFOR_INFORMEDCONSENT"           
>  [2] "IG_IECRI_IECRITERIA"                
>  [3] "IG_COMOR_UNGROUPED"                 
>  [4] "IG_COMOR_COMORBIDITIES"             
>  [5] "IG_DEMO_DEMOGRAPHICDATA"            
>  [6] "IG_CANCE_CANCERHISTOLOGYANDRECEPTOR"
>  [7] "IG_PREVM_UNGROUPED"                 
>  [8] "IG_PREVM_PREVIOUSMEDICATIONANTINEOP"
>  [9] "IG_SURGE_TYPEOFSURGERY"             
> [10] "IG_RANDO_RANDOMIZATION_4899"        
> [11] "IG_PHYSI_PHYSICALEXAMINATION"       
> [12] "IG_RADIO_RADIOTHERAPYINTERVENTION"  
> [13] "IG_ACUTE_UNGROUPED"                 
> [14] "IG_ACUTE_ACUTETOXICITY"             
> [15] "IG_SURVI_SURVIVALANDDISEASERECURREN"
```

In the previous output, `$numbers` documents that this example data includes 240 datapoints on two subjects, from two sites, etc.

One of the ItemGroups in this study (`$groups` in the output above) is `IG_DEMO_DEMOGRAPHICDATA`. Let's get a tidy dataframe for this group, and see the result.

``` r
# get tidy dataframe for demographic data
demo <- ox_xtract_group(d, group = "IG_DEMO_DEMOGRAPHICDATA",
                        use_item_names = TRUE,
                        define_factors = TRUE)
> Joining, by = "codelist_oid"

demo
>    study_oid subject_key subject_id   event_oid event_repeat_key form_oid
> 1 S_CHU_SANT      SS_189        189 SE_BASELINE               NA F_DEMO_1
> 2 S_PARCSALU      SS_100        100 SE_BASELINE               NA F_DEMO_1
>                 group_oid group_repeat_key demo_age demo_menstrual
> 1 IG_DEMO_DEMOGRAPHICDATA                1       55  Postmenopause
> 2 IG_DEMO_DEMOGRAPHICDATA                1       72  Postmenopause
```

In the resulting dataframe, columns `study_oid` to `group_repeat_key` are keys for study site, subject, event, etc.; `demo_age` and `demo_menstrual` are the relevant variables, i.e., the two items included in the `IG_DEMO_DEMOGRAPHICDATA` ItemGroup.

See the vignette for more details.
