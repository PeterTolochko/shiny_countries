#### set country - alliance translation


#### speak for  Alliance (on behalf of alliance)
obo_africa <- c("Algeria", "African Group")
obo_g77 <- c("Palestine", "G77+China", "G77/China", "Egypt")
obo_clam <- c("Brazil", "Argentina", "Uruguay", "Chile", "Costa Rica", "El Salvador",
              "Guatemala", "Honduras", "Mexico", "Panama", "Peru", "Dominican Republic", "Colombia", "Clam")
obo_caricom <- c("Trinidad and Tobago", "Bahamas", "Barbados", "Jamaica", "Saint Lucia", "Caricom")
obo_aosis <- c("Belize")
obo_psids <- c("Fiji", "Federated States Of Micronesia", "Solomon Islands", "Tuvalu", "Nauru", "FSM")
obo_ldc <- c("Malawi", "Ldcs", "Bangladesh")
obo_landlocked <- c("Paraguay", "Landlocked")
obo_non <- c("Turkey", "Venezuela")

obo_africa <- str_to_lower(obo_africa)
obo_g77 <- str_to_lower(obo_g77)
obo_clam <- str_to_lower(obo_clam)
obo_caricom <- str_to_lower(obo_caricom)
obo_aosis <- str_to_lower(obo_aosis)
obo_psids <- str_to_lower(obo_psids)
obo_ldc <- str_to_lower(obo_ldc)
obo_landlocked <- str_to_lower(obo_landlocked)
obo_non <- str_to_lower(obo_non)

#### member in alliance

cari <- c("antigua and barbuda", "bahamas", "barbados", "dominica", "grenada", "guyana",
          "haiti", "jamaica", "montserrat", "saint lucia", "saint kitts", "sain vincent", "suriname",
          "trinidad and tobago") # belize

piss <- c("Fiji", "Federated States of Micronesia", "Solomon Islands", "Tuvalu", "Nauru", "FSM",
          "Samoa", "Tonga", "Vanuatu")		  

aos <- c("Cook Islands", "Fiji", "Kiribati", "Marshall Islands", "Federated States of Micronesia",
         "Nauru", "Niue", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands",
         "Timor-Leste", "Tonga", "Tuvalu", "Vanuatu", "Cabo Verde", "Comoros", 
         "Guinea-Bissau", "Maldives", "Mauritius", "Sao Tome and Principe", "Seychelles",
         "Singapore", "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
         "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica",
         "Saint Kitts", "Saint Lucia", "Sain Vincent", "Suriname", "Trinidad and Tobago")

afr <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cape Verde",
         "Cameroon", "Central African Republic", "Chad", "Comoros", "Republic of Congo", "cote divoire",
         "congo, democratic republic of", "Djibouti", "Egypt", "Equatorial Guinea", 
         "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
         "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali",
         "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", 
         "Nigeria", "Rwanda", "S???o Tom??? and Pr???ncipe", "Senegal", "Seychelles", 
         "Sierra Leone", "Somalia", "South Africa", "South Sudan", 
         "Sudan", "Swaziland", "Togo", "Tunisia", "Uganda", "Tanzania", 
         "Zambia", "Zimbabwe")

eu <- c("Austria", "Italy", "Belgium", "Latvia", "Bulgaria", "Lithuania", "Croatia", 
        "Luxembourg", "Cyprus", "Malta", "Czech Republic", "Netherlands", "Denmark", "Poland",
        "Estonia", "Portugal", "Finland", "Romania", "France", "Slovakia", "Germany", 
        "Slovenia", "Greece", "Spain", "Hungary", "Sweden", "Ireland", "United Kingdom")

least <- c("Afghanistan", "Malawi", "Angola" , "Mali",
           "Bangladesh", "Mauritania", "Benin", "Mozambique", "Bhutan",
           "Myanmar", "Burkina Faso", "Nepal","Burundi" , "Niger", "Cambodia",
           "Rwanda", "Central African Republic", "Sao Tome and Principe", "Chad",
           "Senegal", "Comoros", "Sierra Leone", "Democratic Republic of the Congo",
           "Solomon Islands",  "Djibouti", "Somalia", "Eritrea",
           "South Sudan", "Ethiopia", "Sudan", "Gambia", "Timor-Leste", "Guinea", "Togo",
           "Guinea-Bissau", "Tuvalu", "Haiti", "Uganda", "Kiribati", "United Republic of Tanzania",
           "Laos", "Vanuatu", "Lesotho", "Yemen", "Liberia", "Zambia", "Madagascar")  
land <- c("Afghanistan", "Armenia", "Azerbaijan", "Bhutan", "Bolivia", "Botswana", 
          "Burkina Faso", "Burundi" , "Central African Republic", "Chad" ,"Eswatini",
          "Ethiopia", "Kazakhstan", "Kyrgyzstan", "Laos", "Lesotho", "Macedonia", "Malawi",
          "Mali", "Mongolia", "Nepal", "Niger", "Paraguay", "Republic of Moldova", "Rwanda", 
          "South Sudan", "Tajikistan", "Turkmenistan", "Uganda", "Uzbekistan", "Zambia",
          "Zimbabwe")

nun <- c("Turkey", "Venezuela", "Syrian Arab Republic", "UZBEKISTAN",
         "TURKMENISTAN", "TAJIKISTAN")
library(readxl)
g77 <- read_excel("g77.xlsx", col_names = FALSE)
g77 <- unlist(t(g77))

aos <- str_to_lower(aos)
afr <- str_to_lower(afr)
eu <- str_to_lower(eu)
cari <- str_to_lower(cari)
piss <- str_to_lower(piss)
nun <- str_to_lower(nun)
least <- str_to_lower(least)
land <- str_to_lower(land)
clam <- str_to_lower(obo_clam)
ldc <- str_to_lower(obo_ldc)