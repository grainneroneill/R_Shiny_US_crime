df = read.csv("/Users/grainneoneill/Documents/NYCDSA/Projects/crime.csv")

# remove rows
df = na.omit(df)

# drop agency_code
df = df[ , !(names(df) %in% c("agency_code"))]

# create state column from last 2 letters of agency_jurisdiction
df$state = substr(df$agency_jurisdiction, start = nchar(df$agency_jurisdiction)-1, stop = nchar(df$agency_jurisdiction))

# create city column from agency_jurisdiction
df$city = substr(df$agency_jurisdiction, start = 1, stop = nchar(df$agency_jurisdiction)-4)

# drop agency_jurisdiction
df = df[ , !(names(df) %in% c("agency_jurisdiction"))]

write.csv(df,"crime_cleaned.csv", row.names = FALSE)
