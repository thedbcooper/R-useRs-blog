# create rand_names function with inputs
rand_names <- function(n = 1, seed = "1234", gender = NULL, nationality = NULL, key = NULL) {

  format_args <- function(arg_list) Filter(Negate(is.null), arg_list)

  args <- format_args(
    as.list(c(
        results = n,
        seed = seed,
        gender = gender,
        nat = nationality,
        key = key
      ))
    )

  if(n > 0) {
    # Make api call
    r <- httr::GET("http://api.randomuser.me/", query = args)

    # Check if it raised an error
    httr::stop_for_status(r)

    # Interpret it
    x <- jsonlite::fromJSON(httr::content(r, as = "text"), flatten = TRUE)
  }

  tibble::as_data_frame(x$results)

}

names <- rand_names(n = 1000, seed = "1234", nationality = "FR")

names_clean <- names |>
  dplyr::select(firstname = name.first, lastname = name.last, dob = dob.date) |>
  dplyr::mutate(
    patient_name_new = c(glue::glue("{lastname}, {firstname}")),
    dob = lubridate::as_date(dob)
  )

write.csv(names_clean, file = "posts/test/random_names.csv", row.names = FALSE)

new_names <- read.csv(file = "posts/test/random_names.csv")
dplyr::slice_sample(new_names, n = 15)

# we need to add str_to_lower() version of first and last name in both epi and lab datasets;
# joinby lowercase first and last name
# slice_sample(n = n_distinct_names) fake name dataset and bind_cols to joined data set
# use below code to change some patient names to uppercase

# Sample data (replace with your actual data)
data <- tibble::tibble(
  `Patient Name` = c("JOHN DOE", "Jane Smith", "PETER JONES", "ALICE BROWN"),
  patient_name_new = c("John Doe", "Jane Smith", "Peter Jones", "Alice Brown")
)

# Create the new variable patient_name2
data <- data |>
  dplyr::mutate(
    patient_name_new2 = dplyr::if_else(
      `Patient Name` == stringr::str_to_upper(`Patient Name`), # Check if `Patient Name` is identical to its all-uppercase version
      stringr::str_to_upper(patient_name_new), # If true, make patient_name_new all uppercase
      patient_name_new        # If false, keep patient_name_new
    )
  )

# View the result
print(data)


# Load necessary packages for date manipulation and data handling
# install.packages("lubridate") # Uncomment and run this line if you don't have lubridate installed
# install.packages("dplyr")     # Uncomment and run this line if you don't have dplyr installed
# install.packages("glue")      # Uncomment and run this line if you don't have glue installed
library(lubridate)
library(dplyr)
library(glue)

# Set seed for reproducibility of random dates
set.seed(123)

# Helper function to generate a random date between 1970 and 2005
generate_random_date <- function() {
  year <- sample(1970:2005, 1)
  month <- sample(1:12, 1)
  day <- sample(1:28, 1) # Use 1-28 to avoid issues with month-day combinations
  format(as.Date(paste(year, month, day, sep = "-")), "%B %d, %Y")
}

# Function to impute date if it falls outside 1970-2005 or is not a standard date
impute_date_if_needed <- function(lore_date_str) {
  if (is.na(lore_date_str)) {
      return(generate_random_date())
  }
  parsed_date <- suppressWarnings(mdy(lore_date_str))
  if (!is.na(parsed_date) && year(parsed_date) >= 1970 && year(parsed_date) <= 2005) {
    return(lore_date_str)
  } else {
    return(generate_random_date())
  }
}

# --- Define Character Data ---

# Harry Potter (Expanded)
hp_characters <- data.frame(
  firstname = c(
    "Harry", "Hermione", "Ron", "Albus", "Severus", "Tom", "Ginny", "Draco",
    "Neville", "Luna", "Rubeus", "Fred", "George", "Lily", "James", "Sirius",
    "Remus", "Minerva", "Molly", "Arthur", "Dudley", "Bellatrix", "Filius",
    "Dolores", "Percy", "Charlie", "Bill", "Pomona", "Horace", "Garrick",
    "Kingsley", "Nymphadora", "Alastor", "Aberforth", "Lucius", "Narcissa",
    "Peter", "Barty", "Igor", "Antonin", "Corban", "Cornelius", "Rufus",
    "Sybill", "Argus", "Poppy", "Rita", "Xenophilius", "Fleur", "Viktor",
    "Cedric", "Cho", "Seamus", "Dean", "Parvati", "Padma", "Gregory", "Vincent",
    "Pansy"
  ),
  lastname = c(
    "Potter", "Granger", "Weasley", "Dumbledore", "Snape", "Riddle", "Weasley", "Malfoy",
    "Longbottom", "Lovegood", "Hagrid", "Weasley", "Weasley", "Potter", "Potter", "Black",
    "Lupin", "McGonagall", "Weasley", "Weasley", "Dursley", "Lestrange", "Flitwick",
    "Umbridge", "Weasley", "Weasley", "Weasley", "Sprout", "Slughorn", "Ollivander",
    "Shacklebolt", "Tonks", "Moody", "Dumbledore", "Malfoy", "Malfoy", "Pettigrew",
    "Crouch", "Karkaroff", "Dolohov", "Yaxley", "Fudge", "Scrimgeour", "Trelawney",
    "Filch", "Pomfrey", "Skeeter", "Lovegood", "Delacour", "Krum", "Diggory",
    "Chang", "Finnigan", "Thomas", "Patil", "Patil", "Goyle", "Crabbe", "Parkinson"
  ),
  dob = c(
    "July 31, 1980", "September 19, 1979", "March 1, 1980", "August 1881", "January 9, 1960",
    "December 31, 1926", "August 11, 1981", "June 5, 1980", "July 30, 1980", "February 13, 1981",
    "December 6, 1928", "April 1, 1978", "April 1, 1978", "January 30, 1960", "March 27, 1960",
    "November 3, 1959", "March 10, 1960", "October 4, 1935", "October 30, 1949", "February 6, 1950",
    "June 23, 1980", "1951", "October 17, 1930", "August 26, 1965", "August 22, 1976",
    "December 12, 1972", "November 29, 1970", "May 15, 1931", "April 28, 1900", "September 25, 1890",
    NA, "1973", NA, NA, "1954", "1955", "1960", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1977",
    "1976", "1977", "1979", "1979", "1980", "1979", "1979", "1980", "1980", "1980"
  ),
  series_name = "Harry Potter",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Lord of the Rings (Expanded)
lotr_characters <- data.frame(
  firstname = c(
    "Frodo", "Legolas", "Samwise", "Meriadoc", "Peregrin", "Bilbo"
  ),
  lastname = c(
    "Baggins", "Greenleaf", "Gamgee", "Brandybuck", "Took", "Baggins"
  ),
  dob = c(
    "September 22, TA 2968", "Ageless", "April 6, TA 2980", "TA 2982", "TA 2990", "September 22, TA 2980"
  ),
  series_name = "Lord of the Rings",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Star Wars (Expanded)
sw_characters <- data.frame(
  firstname = c(
    "Luke", "Leia", "Han", "Anakin", "Obi-Wan", "Lando", "Boba", "Mace", "Qui-Gon", "Padmé",
    "Shmi", "Jango", "Count", "Bail", "Mon", "Nute", "Ki-Adi", "Plo", "Kit", "Aayla",
    "Shaak", "Luminara", "Barriss", "Zam", "Ben", "Armitage", "Poe", "Rose", "Ahsoka",
    "Hera", "Kanan", "Ezra", "Sabine", "Gar", "Pre", "Satine", "Asajj", "Cad", "Din",
    "Bo-Katan", "Moff", "Cara", "Greef", "Fennec"
  ),
  lastname = c(
    "Skywalker", "Organa", "Solo", "Skywalker", "Kenobi", "Calrissian", "Fett", "Windu", "Jinn", "Amidala",
    "Skywalker", "Fett", "Dooku", "Organa", "Mothma", "Gunray", "Mundi", "Koon", "Fisto", "Secura",
    "Ti", "Unduli", "Offee", "Wesell", "Solo", "Hux", "Dameron", "Tico", "Tano", "Syndulla",
    "Jarrus", "Bridger", "Wren", "Saxon", "Vizsla", "Kryze", "Ventress", "Bane", "Djarin",
    "Kryze", "Gideon", "Dune", "Karga", "Shand"
  ),
  dob = NA,
  series_name = "Star Wars",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# MCU (Expanded)
mcu_characters <- data.frame(
  firstname = c(
    "Tony", "Steve", "Thor", "Bruce", "Natasha", "Clint", "Wanda", "Sam", "Bucky", "Peter",
    "Stephen", "T'Challa", "Carol", "Scott", "Hope", "James", "Pepper", "Nick", "Maria",
    "Sharon", "Loki", "Erik", "Jane", "Johann", "Karl", "Christine", "Happy", "Odin",
    "Alexander", "Brock", "Jasper", "Arnim", "Justin", "Obadiah", "Aldrich", "Maya",
    "Trevor", "President", "Harley", "Cassie", "Agatha", "Billy", "Tommy", "John",
    "Isaiah", "Karli", "Yelena", "Alexei", "Antonia", "Kamala", "Marc", "Arthur", "Jen",
    "Emil", "Eric", "Jack", "Elsa", "Namor", "Riri", "Janet", "Hank", "Kraglin", "Peter",
    "Monica", "Jimmy", "Darcy", "Ned", "Michelle", "Flash", "Liz", "Adrian", "Quentin",
    "Helmut", "Taneleer", "Maria", "Howard", "Peggy", "Abraham", "Chester", "Gabe",
    "Jim", "Jacques", "Dum Dum", "Georges", "Ulysses", "Ava", "Darren", "Nathaniel",
    "Phil", "Melinda", "Daisy", "Leo", "Jemma", "Grant", "Alphonso", "Elena", "Lance",
    "Bobbi", "Jeffrey", "Cal", "Matt", "Jessica", "Luke", "Danny", "Frank", "Wilson",
    "Elektra", "Karen", "Foggy", "Claire", "Jeri", "Trish", "Malcolm", "Misty",
    "Colleen", "Everett", "Helen", "Laura", "Ivan"
  ),
  lastname = c(
    "Stark", "Rogers", "Odinson", "Banner", "Romanoff", "Barton", "Maximoff", "Wilson", "Barnes", "Parker",
    "Strange", "Udaku", "Danvers", "Lang", "van Dyne", "Rhodes", "Potts", "Fury", "Hill",
    "Carter", "Laufeyson", "Killmonger", "Foster", "Schmidt", "Mordo", "Palmer", "Hogan", "Borson",
    "Pierce", "Rumlow", "Sitwell", "Zola", "Hammer", "Stane", "Killian", "Hansen",
    "Slattery", "Ellis", "Keener", "Lang", "Harkness", "Maximoff", "Maximoff", "Walker",
    "Bradley", "Morgenthau", "Belova", "Shostakov", "Dreykov", "Khan", "Spector", "Harrow", "Walters",
    "Blonsky", "Brooks", "Russell", "Bloodstone", "McKenzie", "Williams", "van Dyne", "Pym",
    "Obfonteri", "Quill", "Rambeau", "Woo", "Lewis", "Leeds", "Jones", "Thompson",
    "Toomes", "Toomes", "Beck", "Zemo", "Tivan", "Rambeau", "Stark", "Carter", "Erskine",
    "Phillips", "Jones", "Morita", "Dernier", "Dugan", "Batroc", "Klaue", "Starr",
    "Cross", "Richards", "Coulson", "May", "Johnson", "Fitz", "Simmons", "Ward", "Mackenzie",
    "Rodriguez", "Hunter", "Morse", "Mace", "Zabo", "Murdock", "Jones", "Cage", "Rand",
    "Castle", "Fisk", "Natchios", "Page", "Nelson", "Temple", "Hogarth", "Walker",
    "Ducasse", "Knight", "Wing", "Ross", "Cho", "Barton", "Vanko"
  ),
  dob = NA,
  series_name = "Marvel Cinematic Universe",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Star Trek (Expanded)
st_characters <- data.frame(
  firstname = c(
    "James", "Leonard", "Nyota", "Hikaru", "Pavel", "Jean-Luc", "William", "Deanna",
    "Beverly", "Geordi", "Kathryn", "B'Elanna", "Tom", "Harry", "Benjamin", "Kira",
    "Julian", "Miles", "Michael", "Sylvia", "Paul", "Hugh", "Christopher", "Una",
    "La'an", "Christine", "Erica", "Joseph", "Wesley", "Tasha", "Ro", "Reginald",
    "Elim", "Amanda", "Will", "Khan", "Carol", "David", "Montgomery", "Janice",
    "Gabriel", "Ash", "Philippa", "Jet", "Raffi", "Agnes", "Cristobal", "Jonathan",
    "Charles", "Malcolm", "Hoshi", "Travis", "Garth", "Gary", "Elizabeth", "Naomi",
    "Jake", "Worf", "Data", "Lwaxana", "Katherine", "Jadzia", "Ezri", "Zefram"
  ),
  lastname = c(
    "Kirk", "McCoy", "Uhura", "Sulu", "Chekov", "Picard", "Riker", "Troi", "Crusher",
    "La Forge", "Janeway", "Torres", "Paris", "Kim", "Sisko", "Nerys", "Bashir",
    "O'Brien", "Burnham", "Tilly", "Stamets", "Culber", "Pike", "Chin-Riley",
    "Noonien-Singh", "Chapel", "Ortegas", "M'Benga", "Crusher", "Yar", "Laren", "Barclay",
    "Garak", "Grayson", "Decker", "Noonien Singh", "Marcus", "Marcus", "Scott", "Rand",
    "Lorca", "Tyler", "Georgiou", "Reno", "Musiker", "Jurati", "Rios", "Archer",
    "Tucker", "Reed", "Sato", "Mayweather", "of Izar", "Mitchell", "Dehner", "Wildman",
    "Sisko", "Rozhenko", "Soong", "Troi", "Pulaski", "Dax", "Dax", "Cochrane"
  ),
  dob = NA,
  series_name = "Star Trek",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# The Hunger Games
hg_characters <- data.frame(
  firstname = c(
    "Katniss", "Peeta", "Gale", "Haymitch", "Effie", "Coriolanus", "Primrose", "Finnick",
    "Johanna", "Beetee", "Plutarch", "Alma", "Madge", "Romulus", "Seneca", "Claudius",
    "Caesar", "Volumnia", "Sejanus", "Lucy", "Maude", "Clemensia", "Lysistrata",
    "Casca", "Lucky", "Strabo", "Billy", "Mayfair", "Arachne"
  ),
  lastname = c(
    "Everdeen", "Mellark", "Hawthorne", "Abernathy", "Trinket", "Snow", "Everdeen", "Odair",
    "Mason", "Latier", "Heavensbee", "Coin", "Undersee", "Thread", "Crane", "Templesmith",
    "Flickerman", "Gaul", "Plinth", "Gray Baird", "Ivory Baird", "Dovecote", "Vickers",
    "Highbottom", "Flickerman", "Plinth", "Taupe", "Lipp", "Crane"
  ),
  dob = NA,
  series_name = "The Hunger Games",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Pirates of the Caribbean
potc_characters <- data.frame(
  firstname = c(
    "Jack", "Will", "Elizabeth", "Hector", "James", "Joshamee", "Davy", "Bootstrap Bill",
    "Tia", "Cutler", "Sao", "Governor Weatherby", "Philip", "Edward", "Angelica",
    "Armando", "Henry", "Carina", "Teague", "Margaret", "Lana", "Margarita", "David",
    "Sarah", "William", "Elizabeth", "James", "Maria", "Thomas", "Hannah"
  ),
  lastname = c(
    "Sparrow", "Turner", "Swann", "Barbossa", "Norrington", "Gibbs", "Jones", "Turner",
    "Dalma", "Beckett", "Feng", "Swann", "Swift", "Teach", "Teach", "Salazar", "Turner",
    "Smyth", "Sparrow", "Smyth", "Turner", "Swann", "Jones", "Turner", "Turner Sr.",
    "Swann Jr.", "Gibbs", "Barbossa", "Norrington", "Gibbs"
  ),
  dob = NA,
  series_name = "Pirates of the Caribbean",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Dune
dune_characters <- data.frame(
  firstname = c(
    "Paul", "Jessica", "Leto", "Gurney", "Duncan", "Thufir", "Vladimir", "Glossu",
    "Piter", "Gaius Helen", "Irulan", "Feyd-Rautha", "Margot", "Hasimir", "Wellington", "Liet"
  ),
  lastname = c(
    "Atreides", "Atreides", "Atreides", "Halleck", "Idaho", "Hawat", "Harkonnen", "Rabban",
    "De Vries", "Mohiam", "Corrino", "Harkonnen", "Fenring", "Fenring", "Yueh", "Kynes"
  ),
  dob = NA,
  series_name = "Dune",
  series_type = "Movie",
  stringsAsFactors = FALSE
)

# Game of Thrones (Expanded)
got_characters <- data.frame(
  firstname = c(
    "Jon", "Daenerys", "Tyrion", "Cersei", "Jaime", "Arya", "Sansa", "Bran", "Ned",
    "Robert", "Joffrey", "Margaery", "Samwell", "Davos", "Petyr", "Theon", "Yara",
    "Ramsay", "Roose", "Oberyn", "Brienne", "Sandor", "Gregor", "Tywin", "Olenna",
    "Robb", "Rickon", "Benjen", "Lyanna", "Jorah", "Kevan", "Lancel", "Stannis",
    "Renly", "Shireen", "Viserys", "Rhaegar", "Aemon", "Balon", "Euron", "Doran",
    "Trystane", "Ellaria", "Loras", "Mace", "Walder", "Edmure", "Brynden", "Lysa",
    "Robin", "Gendry"
  ),
  lastname = c(
    "Snow", "Targaryen", "Lannister", "Lannister", "Lannister", "Stark", "Stark", "Stark", "Stark",
    "Baratheon", "Baratheon", "Tyrell", "Tarly", "Seaworth", "Baelish", "Greyjoy", "Greyjoy",
    "Bolton", "Bolton", "Martell", "Tarth", "Clegane", "Clegane", "Lannister", "Tyrell",
    "Stark", "Stark", "Stark", "Mormont", "Mormont", "Lannister", "Lannister", "Baratheon",
    "Baratheon", "Baratheon", "Targaryen", "Targaryen", "Targaryen", "Greyjoy", "Greyjoy",
    "Martell", "Martell", "Sand", "Tyrell", "Tyrell", "Frey", "Tully", "Tully", "Arryn",
    "Arryn", "Baratheon"
  ),
  dob = NA,
  series_name = "Game of Thrones",
  series_type = "TV",
  stringsAsFactors = FALSE
)

# The Witcher (Expanded)
witcher_characters <- data.frame(
  firstname = c(
    "Geralt", "Yennefer", "Triss", "Jaskier", "Vilgefortz", "Emhyr",
    "Ciri", "Fringilla", "Cahir", "Tissaia"
  ),
  lastname = c(
    "Rivia", "Vengerberg", "Merigold", "Pankratz", "Roggeveen", "Emreis",
    "Fiona", "Vigo", "Mawr", "Vries"
  ),
  dob = NA,
  series_name = "The Witcher",
  series_type = "Game",
  stringsAsFactors = FALSE
)

# Percy Jackson
pj_characters <- data.frame(
  firstname = c(
    "Percy", "Annabeth", "Grover", "Luke", "Thalia", "Clarisse", "Nico", "Jason",
    "Piper", "Leo", "Frank", "Hazel", "Will", "Rachel"
  ),
  lastname = c(
    "Jackson", "Chase", "Underwood", "Castellan", "Grace", "La Rue", "di Angelo", "Grace",
    "McLean", "Valdez", "Zhang", "Levesque", "Solace", "Dare"
  ),
  dob = NA,
  series_name = "Percy Jackson",
  series_type = "Book",
  stringsAsFactors = FALSE
)

# Final Fantasy VII
ff7_characters <- data.frame(
  firstname = c(
    "Cloud", "Tifa", "Aerith", "Barret", "Sephiroth", "Zack", "Vincent", "Reeve"
  ),
  lastname = c(
    "Strife", "Lockhart", "Gainsborough", "Wallace", "Crescent", "Fair", "Valentine", "Tuesti"
  ),
  dob = NA,
  series_name = "Final Fantasy VII",
  series_type = "Game",
  stringsAsFactors = FALSE
)

# Wheel of Time (New)
wot_characters <- data.frame(
  firstname = c(
    "Rand", "Mat", "Perrin", "Egwene", "Nynaeve", "Moiraine", "Lan", "Thom",
    "Siuan", "Elayne", "Min", "Faile", "Gawyn", "Galad", "Padan", "Logain",
    "Mazrim", "Verin", "Alanna", "Cadsuane"
  ),
  lastname = c(
    "al'Thor", "Cauthon", "Aybara", "al'Vere", "al'Meara", "Damodred", "Mandragoran", "Merrilin",
    "Sanche", "Trakand", "Farshaw", "Bashere", "Trakand", "Damodred", "Fain", "Ablar",
    "Taim", "Mathwin", "Mosvani", "Melaidhrin"
  ),
  dob = NA,
  series_name = "Wheel of Time",
  series_type = "Book",
  stringsAsFactors = FALSE
)

# Mass Effect (New)
me_characters <- data.frame(
  firstname = c(
    "John", "Jane", "Liara", "Garrus", "Tali", "Urdnot", "Mordin", "Kaidan",
    "Ashley", "Jeff", "David", "Saren", "Jack", "Miranda", "Jacob", "Thane",
    "Zaeed", "Kasumi", "James"
  ),
  lastname = c(
    "Shepard", "Shepard", "T'Soni", "Vakarian", "Zorah", "Wrex", "Solus", "Alenko",
    "Williams", "Moreau", "Anderson", "Arterius", "Harper", "Lawson", "Taylor", "Krios",
    "Massani", "Goto", "Vega"
  ),
  dob = NA,
  series_name = "Mass Effect",
  series_type = "Game",
  stringsAsFactors = FALSE
)

# --- Combine and Process Data ---

# Combine all characters into a single data frame
characters_data <- rbind(
  hp_characters,
  lotr_characters,
  sw_characters,
  mcu_characters,
  st_characters,
  hg_characters,
  potc_characters,
  dune_characters,
  got_characters,
  witcher_characters,
  pj_characters,
  ff7_characters,
  wot_characters, # Added
  me_characters   # Added
)

# List of series where dates should *always* be generated/imputed (NA or not)
# This includes all series *except* HP and LOTR which have some specific lore dates we might want to keep if in range
impute_always_series <- c(
  "Star Wars",
  "Marvel Cinematic Universe",
  "Star Trek",
  "The Hunger Games",
  "Pirates of the Caribbean",
  "Dune",
  "Game of Thrones",
  "The Witcher",
  "Percy Jackson",
  "Final Fantasy VII",
  "Wheel of Time",
  "Mass Effect"
)

# Apply the imputation logic
for (i in 1:nrow(characters_data)) {
  if (characters_data$series_name[i] %in% impute_always_series) {
    characters_data$dob[i] <- generate_random_date()
  } else {
    # For HP and LOTR, impute only if original date is NA, outside range, or unparseable
    characters_data$dob[i] <- impute_date_if_needed(characters_data$dob[i])
  }
}

# Define the file path for the CSV
csv_file_path <- "posts/test/fictional_characters.csv" # New filename

# Save the data frame to a CSV file
# Ensure dplyr and glue are available
characters_data2 <- characters_data |>
  dplyr::mutate(patient_name_new = glue::glue("{lastname}, {firstname}"))

write.csv(characters_data2, file = csv_file_path, row.names = FALSE, fileEncoding = "UTF-8")

cat(paste("Dataset successfully created at:", csv_file_path, "\n"))

# To verify the dates are parseable by lubridate, you can run this in R:
characters_data$Parsed_Date <- mdy(characters_data$dob)
print(head(characters_data))
print(summary(characters_data$Parsed_Date))

print(paste("Number of distinct last names:", dplyr::n_distinct(characters_data2$lastname)))
