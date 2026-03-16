library(tibble)
library(readr)
library(dplyr)

# ------------------------------------------------------------
# Create lightweight metadata tables that the app can use later
#
# These tables do not contain survey responses themselves.
# Instead, they describe:
# 1. what each selected question means
# 2. why each question made the final cut
# 3. how to translate country codes into readable country names
# ------------------------------------------------------------

# Basic metadata for each selected survey question:
# - the original question id
# - the wording shown in the survey
# - the kind of response scale it uses
# - the available answer choices
# - the broader theme the question belongs to
question_meta <- tibble::tribble(
  ~question_id, ~question_text, ~response_type, ~choices, ~theme,
  
  "q6",   "How important is religion in your life?",
  "ordinal_4",
  "1=Very important; 2=Rather important; 3=Not very important; 4=Not at all important",
  "religion",
  
  "q164", "How important is God in your life?",
  "scale_1_10",
  "1=Not at all important; 10=Very important",
  "religion",
  
  "q169", "Whenever science and religion conflict, religion is always right.",
  "ordinal_4",
  "1=Strongly agree; 2=Agree; 3=Disagree; 4=Strongly disagree",
  "religion_science",
  
  "q186", "Is sex before marriage ever justifiable?",
  "scale_1_10",
  "1=Never justifiable; 10=Always justifiable",
  "sexual_norms",
  
  "q182", "Is homosexuality ever justifiable?",
  "scale_1_10",
  "1=Never justifiable; 10=Always justifiable",
  "sexual_norms",
  
  "q172", "How often do you pray (apart from weddings and funerals)?",
  "ordinal_8",
  "1=Several times a day; 2=Once a day; 3=Several times each week; 4=Only when attending religious services; 5=Only on special holy days; 6=Once a year; 7=Less often; 8=Never",
  "religious_practice",
  
  "q170", "The only acceptable religion is my religion.",
  "ordinal_4",
  "1=Strongly agree; 2=Agree; 3=Disagree; 4=Strongly disagree",
  "religious_exclusivity",
  
  "q25",  "Would you mind having unmarried couples living together as neighbors?",
  "binary_mentioned",
  "1=Yes; 2=No",
  "social_tolerance",
  
  "q22",  "Would you mind having homosexual neighbors?",
  "binary_mentioned",
  "1=Yes; 2=No",
  "social_tolerance",
  
  "q165", "Do you believe in God?",
  "binary_yes_no",
  "1=Yes; 2=No",
  "religious_belief",
  
  "q148", "How worried are you about the possibility of a civil war?",
  "ordinal_4",
  "1=Very much; 2=A good deal; 3=Not much; 4=Not at all",
  "security_conflict",
  
  "q33",  "When jobs are scarce, men should have more right to a job than women.",
  "ordinal_5",
  "1=Agree strongly; 2=Agree; 3=Neither agree nor disagree; 4=Disagree; 5=Disagree strongly",
  "gender_roles",
  
  "q193", "Is casual sex ever justifiable?",
  "scale_1_10",
  "1=Never justifiable; 10=Always justifiable",
  "sexual_norms",
  
  "q209", "How likely are you to sign a petition?",
  "ordinal_3",
  "1=Have done; 2=Might do; 3=Would never do",
  "political_action",
  
  "q38",  "Adult children have a duty to provide long-term care for their parents.",
  "ordinal_5",
  "1=Agree strongly; 2=Agree; 3=Neither agree nor disagree; 4=Disagree; 5=Disagree strongly",
  "family_duty"
)

# This table adds interpretation for each question.
# The idea is to make downstream app text more understandable:
# - why this question was selected
# - what a "higher" response tends to mean
# - what a "lower" response tends to mean
# - a friendlier plain-English explanation for users
question_explanations <- tribble(
  ~question_id, ~theme, ~why_selected, ~high_meaning, ~low_meaning, ~friendly_explanation,
  "q6", "religion", "This question strongly separates countries on average because religiosity varies a lot across national profiles.", "Religion is more central in daily life.", "Religion is less central in daily life.", "This helps place you on a spectrum from more secular to more religious societies.",
  "q164", "religion", "Belief salience around God is one of the clearest cross-country separators in the selected set.", "God feels highly important in life.", "God feels less important in life.", "This is one of the clearest signals about religious intensity.",
  "q169", "religion_science", "Views on religion versus science help distinguish countries with different moral and epistemic traditions.", "Religion should win when it conflicts with science.", "Religion should not automatically override science.", "This helps capture whether someone leans toward faith-centered or more secular-scientific authority.",
  "q186", "sexual_norms", "Attitudes toward sex before marriage are highly differentiating across countries.", "More acceptance of premarital sex.", "Less acceptance of premarital sex.", "This helps place you on a more permissive versus more traditional social-values axis.",
  "q182", "sexual_norms", "Acceptance of homosexuality is one of the strongest differentiators in the shortlist.", "More accepting of homosexuality.", "Less accepting of homosexuality.", "This is a major signal of broader social permissiveness.",
  "q172", "religious_practice", "Religious practice frequency adds behavioral depth beyond belief alone.", "More frequent prayer.", "Less frequent or no prayer.", "This captures lived religious practice, not just stated belief.",
  "q170", "religious_exclusivity", "Religious exclusivity helps separate pluralist from exclusivist orientations.", "More likely to see one religion as uniquely acceptable.", "More open to religious pluralism.", "This helps distinguish exclusive versus pluralist religious outlooks.",
  "q25", "social_tolerance", "Neighbor preferences capture everyday social tolerance.", "More discomfort with unmarried couples living together.", "More comfort with unmarried couples living together.", "This helps measure tolerance around nontraditional family arrangements.",
  "q22", "social_tolerance", "Neighbor preferences around homosexuals are strongly differentiating.", "More discomfort with homosexual neighbors.", "More comfort with homosexual neighbors.", "This gives a practical signal of social tolerance.",
  "q165", "religious_belief", "Simple belief in God still carries strong cross-country signal.", "Believes in God.", "Does not believe in God.", "This is a straightforward belief marker that works well with the other religion questions.",
  "q148", "security_conflict", "Worry about civil war captures perceived instability and conflict sensitivity.", "More worried about civil war.", "Less worried about civil war.", "This helps capture how much instability and conflict loom in someone’s worldview.",
  "q33", "gender_roles", "Gender-role views remain highly variable across countries.", "More likely to favor men in job scarcity.", "More likely to reject that preference.", "This helps place you on a more traditional versus egalitarian gender-values axis.",
  "q193", "sexual_norms", "Views on casual sex add extra resolution within the permissive-traditional cluster.", "More accepting of casual sex.", "Less accepting of casual sex.", "This sharpens the social-liberal versus traditional profile.",
  "q209", "political_action", "Willingness to sign petitions helps distinguish civic participation styles.", "More open to petition-based civic action.", "Less open to that form of participation.", "This adds a civic-engagement dimension beyond moral and religious values.",
  "q38", "family_duty", "Family obligation norms are strongly patterned across countries.", "Stronger belief that adult children owe long-term care to parents.", "Weaker belief in that duty.", "This helps capture collectivist family-duty expectations."
)

# Country lookup table so the app does not have to show numeric country codes.
# This is especially useful after ranking or scoring countries, since users
# should see readable names rather than internal IDs.
country_lookup <- tribble(
  ~b_country, ~country_name,
  "20", "Andorra",
  "32", "Argentina",
  "36", "Australia",
  "51", "Armenia",
  "50", "Bangladesh",
  "68", "Bolivia",
  "76", "Brazil",
  "124", "Canada",
  "152", "Chile",
  "156", "China",
  "170", "Colombia",
  "196", "Cyprus",
  "203", "Czechia",
  "218", "Ecuador",
  "818", "Egypt",
  "231", "Ethiopia",
  "276", "Germany",
  "300", "Greece",
  "826", "Great Britain",
  "320", "Guatemala",
  "344", "Hong Kong SAR",
  "356", "India",
  "360", "Indonesia",
  "364", "Iran",
  "368", "Iraq",
  "392", "Japan",
  "400", "Jordan",
  "398", "Kazakhstan",
  "404", "Kenya",
  "417", "Kyrgyzstan",
  "422", "Lebanon",
  "434", "Libya",
  "446", "Macau SAR",
  "458", "Malaysia",
  "462", "Maldives",
  "484", "Mexico",
  "496", "Mongolia",
  "504", "Morocco",
  "104", "Myanmar",
  "528", "Netherlands",
  "554", "New Zealand",
  "558", "Nicaragua",
  "566", "Nigeria",
  "909", "Northern Ireland",
  "586", "Pakistan",
  "604", "Peru",
  "608", "Philippines",
  "630", "Puerto Rico",
  "642", "Romania",
  "643", "Russia",
  "688", "Serbia",
  "702", "Singapore",
  "703", "Slovakia",
  "410", "South Korea",
  "158", "Taiwan ROC",
  "762", "Tajikistan",
  "764", "Thailand",
  "788", "Tunisia",
  "792", "Turkey",
  "804", "Ukraine",
  "840", "United States",
  "858", "Uruguay",
  "860", "Uzbekistan",
  "862", "Venezuela",
  "704", "Vietnam",
  "716", "Zimbabwe"
)

# Save all three metadata tables so the rest of the project can load them easily
# without rebuilding them each time
write_csv(question_meta, "data/processed/question_meta.csv")
write_csv(question_explanations, "data/processed/question_explanations.csv")
write_csv(country_lookup, "data/processed/country_lookup.csv")

message("Saved metadata CSVs to data/processed/")
