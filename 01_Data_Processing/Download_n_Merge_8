rm(list = ls())

library(dplyr)

# Load your recovered dataset
final_combined <- readRDS(list.files(pattern = "arctic_pelagic_recovered_2025-06-07.rds$")[1])

cat("Starting with", nrow(final_combined), "observations\n")
cat("Starting with", length(unique(final_combined$`COMMON NAME`)), "species\n\n")

# Get all species
all_species <- unique(final_combined$`COMMON NAME`)

# MANUAL LIST: Obvious terrestrial birds to remove
# No pattern matching, no automation - just clear terrestrial species

terrestrial_to_remove <- c(
  # Songbirds
  "European Starling", "Redpoll", "Redwing", "Brambling", "Cedar Waxwing",
  "Eurasian Blackcap", "Common Chiffchaff", "Fieldfare", "Lesser Whitethroat",
  "Bohemian Waxwing", "Eurasian Blackbird", "Eurasian Skylark", "Lapland Longspur",
  "Common Nightingale", "Common Redstart", "Eurasian Siskin", "Whinchat",
  "Western House-Martin", "Greater Whitethroat", "Ring Ouzel", "Common Swift",
  "Dunnock", "Goldcrest", "Black Redstart", "Common Yellowthroat", "Alpine Swift",
  "Bluethroat", "Red-backed Shrike", "Siberian Stonechat", "Parrot Crossbill",
  "Red-flanked Bluetail", "Red Crossbill", "White-winged Crossbill", "Rosy Starling",
  "Eurasian Jackdaw", "Yellowhammer", "Siberian Rubythroat", "Amur Stonechat",
  "Rusty Blackbird", "Common Cuckoo", "Eurasian Wryneck", "Siberian Accentor",
  "Dark-eyed Junco", "Siberian House-Martin", "Pine Grosbeak", "Horned Lark",
  "Brown-headed Cowbird", "Northern Shrike", "Black-headed Grosbeak", "Oriental Cuckoo",
  "Northern Flicker", "Say's Phoebe", "Philadelphia Vireo", "Brown Shrike",
  "Pacific Swift", "American Redstart", "Bullock's Oriole", "Ovenbird",
  "Red-breasted Sapsucker", "Townsend's Solitaire", "Northern Mockingbird",
  "Red-eyed Vireo", "Blue-headed Vireo", "Eastern Phoebe", "Anna's Hummingbird",
  "Purple Martin", "Mountain Bluebird", "Yellow-headed Blackbird", "Western Tanager",
  "Great Tit", "Twite", "Willow Tit", "Coal Tit", "European Stonechat",
  "White-winged Lark", "Eurasian Blue Tit", "Long-tailed Tit", "Eurasian Linnet",
  "White-throated Dipper", "Eurasian Treecreeper", "Siberian Jay", "Great Gray Shrike",
  "Eurasian Jay", "Common Quail", "Rook", "Isabelline Wheatear", "European Bee-eater",
  "Eurasian Golden Oriole", "Marsh Tit", "Woodchat Shrike", "Eurasian Nightjar",
  "Lesser Gray Shrike", "Crested Tit", "Wood Lark", "Northern Nutcracker",
  "Northern Wheatear", "Rufous Hummingbird",
  
  # Raptors
  "Merlin", "Hen Harrier", "Western Marsh Harrier", "Eurasian Kestrel", "Northern Harrier",
  "Osprey", "Eurasian Hobby", "Black-billed Magpie", "Canada Jay", "Steller's Jay",
  "American Dipper", "Brown Creeper", "Belted Kingfisher", "Pine Siskin",
  "Black Kite", "Common Buzzard", "Oriental Honey-buzzard", "Western Capercaillie",
  "Pallid Harrier", "Montagu's Harrier", "European Honey-buzzard",
  
  # Shorebirds
  "Eurasian Oystercatcher", "Common Redshank", "Dunlin", "Pied Avocet", "Sanderling",
  "Long-billed Dowitcher", "Northern Lapwing", "Little Stint", "Ruff", "Temminck's Stint",
  "Water Rail", "Common Greenshank", "Red-necked Stint", "Black-winged Stilt",
  "Eurasian Dotterel", "Black Oystercatcher", "Long-toed Stint", "Short-billed Dowitcher",
  "Surfbird", "Wandering Tattler", "Gray-tailed Tattler", "American Avocet", "Killdeer",
  "Spotted Redshank", "Whimbrel",
  
  # Freshwater ducks and waterfowl
  "Greater Scaup", "Gadwall", "Eurasian Coot", "Common Pochard", "Brant", "Garganey",
  "Lesser Scaup", "Velvet Scoter", "Eurasian Moorhen", "Bufflehead", "Canvasback",
  "Redhead", "Smew", "Ring-necked Pheasant", "Green Pheasant", "Eurasian Bittern",
  
  # Game birds
  "Western Capercaillie", "Common Quail", "Ring-necked Pheasant", "Green Pheasant",
  
  # Other obvious terrestrials
  "Eurasian Spoonbill", "Eurasian Woodcock", "Collared Pratincole", "Black-winged Pratincole",
  "Corn Crake", "Eurasian Hoopoe"
)

# Filter out terrestrial birds
marine_data <- final_combined %>%
  filter(!`COMMON NAME` %in% terrestrial_to_remove)

# Results
removed_obs <- nrow(final_combined) - nrow(marine_data)
removed_species <- length(setdiff(unique(final_combined$`COMMON NAME`), unique(marine_data$`COMMON NAME`)))

cat("REMOVAL RESULTS:\n")
cat("Removed", removed_species, "terrestrial species\n")
cat("Removed", format(removed_obs, big.mark = ","), "observations\n")
cat("Remaining", format(nrow(marine_data), big.mark = ","), "observations\n")
cat("Remaining", length(unique(marine_data$`COMMON NAME`)), "species\n\n")

# Show what was removed
removed_species_list <- setdiff(unique(final_combined$`COMMON NAME`), unique(marine_data$`COMMON NAME`))
cat("REMOVED SPECIES (first 20):\n")
print(head(removed_species_list, 20))

unique(marine_data$`COMMON NAME`)



cat("Second round cleanup starting with", length(unique(marine_data$`COMMON NAME`)), "species\n")

# SECOND ROUND: Remove remaining obvious terrestrials
additional_terrestrial <- c(
  # Ravens, crows, corvids
  "Common Raven", "American Crow", "Carrion Crow", "Hooded Crow", "Canada Jay", 
  "Steller's Jay", "Black-billed Magpie", "Eurasian Magpie", "Eurasian Jackdaw", 
  "Eurasian Jay", "Siberian Jay", "Rook",
  
  # Ptarmigans, grouse, game birds  
  "Rock Ptarmigan", "Willow Ptarmigan", "Spruce Grouse", "Black Grouse", 
  "Western Capercaillie",
  
  # Raptors and owls
  "White-tailed Eagle", "Bald Eagle", "Golden Eagle", "Steller's Sea-Eagle",
  "Red-tailed Hawk", "Rough-legged Hawk", "American Goshawk", "Eurasian Goshawk",
  "Sharp-shinned Hawk", "Eurasian Sparrowhawk", "Peregrine Falcon", "Snowy Owl",
  "Long-eared Owl", "Short-eared Owl", "Boreal Owl", "Northern Saw-whet Owl",
  "Great Horned Owl", "Western Screech-Owl", "Eurasian Eagle-Owl", "Eurasian Pygmy-Owl",
  
  # Sandpipers and shorebirds
  "Common Sandpiper", "Purple Sandpiper", "Red Knot", "Ruddy Turnstone", 
  "Black Turnstone", "Common Ringed Plover", "Semipalmated Plover", 
  "European Golden-Plover", "American Golden-Plover", "Pacific Golden-Plover",
  "Black-bellied Plover", "Bar-tailed Godwit", "Black-tailed Godwit", 
  "Hudsonian Godwit", "Marbled Godwit", "Greater Yellowlegs", "Lesser Yellowlegs",
  "Spotted Sandpiper", "Pectoral Sandpiper", "Least Sandpiper", "Semipalmated Sandpiper",
  "Western Sandpiper", "White-rumped Sandpiper", "Baird's Sandpiper", 
  "Buff-breasted Sandpiper", "Stilt Sandpiper", "Curlew Sandpiper", 
  "Broad-billed Sandpiper", "Sharp-tailed Sandpiper", "Rock Sandpiper",
  "Terek Sandpiper", "Green Sandpiper", "Wood Sandpiper", "Solitary Sandpiper",
  "Marsh Sandpiper", "Common Snipe", "Wilson's Snipe", "Jack Snipe", 
  "Pin-tailed Snipe", "Great Snipe", "Bristle-thighed Curlew", "Eurasian Curlew",
  "Far Eastern Curlew", "Little Curlew", "Willet", "Siberian Sand-Plover",
  "Kentish Plover", "Little Ringed Plover", "Snowy Plover", "Oriental Plover",
  
  # Freshwater ducks and waterfowl
  "Mallard", "Northern Pintail", "American Wigeon", "Eurasian Wigeon", 
  "Green-winged Teal", "Blue-winged Teal", "Cinnamon Teal", "Northern Shoveler",
  "Wood Duck", "American Black Duck", "Ring-necked Duck", "Tufted Duck",
  "Common Goldeneye", "Barrow's Goldeneye", "Common Merganser", "Red-breasted Merganser",
  "Hooded Merganser", "Falcated Duck", "Baikal Teal", "Mandarin Duck",
  "Ferruginous Duck", "Eastern Spot-billed Duck", "Muscovy Duck",
  
  # Geese and swans
  "Graylag Goose", "Greater White-fronted Goose", "Lesser White-fronted Goose",
  "Pink-footed Goose", "Taiga/Tundra Bean-Goose", "Tundra Bean-Goose", 
  "Taiga Bean-Goose", "Snow Goose", "Cackling Goose", "Canada Goose", 
  "Barnacle Goose", "Red-breasted Goose", "Bar-headed Goose", "Emperor Goose",
  "Whooper Swan", "Tundra Swan", "Trumpeter Swan", "Mute Swan",
  "Common Shelduck", "Ruddy Shelduck",
  
  # Herons, egrets, cranes
  "Gray Heron", "Great Blue Heron", "Great Egret", "Little Egret", "Medium Egret",
  "Western Cattle-Egret", "Black-crowned Night Heron", "Chinese Pond-Heron",
  "Sandhill Crane", "Common Crane",
  
  # Grebes
  "Red-necked Grebe", "Horned Grebe", "Great Crested Grebe", "Eared Grebe", "Little Grebe",
  
  # Pigeons and doves
  "Rock Pigeon", "Common Wood-Pigeon", "Stock Dove", "European Turtle-Dove",
  "Oriental Turtle-Dove", "Eurasian Collared-Dove", "Mourning Dove",
  
  # Songbirds (remaining ones)
  "Meadow Pipit", "Tree Pipit", "Rock Pipit", "American Pipit", "Red-throated Pipit",
  "Olive-backed Pipit", "Pechora Pipit", "Siberian Pipit", "White Wagtail", 
  "Gray Wagtail", "Eastern Yellow Wagtail", "Western Yellow Wagtail", "Citrine Wagtail",
  "Snow Bunting", "McKay's Bunting", "Little Bunting", "Rustic Bunting", 
  "Pine Bunting", "Yellow-browed Bunting", "Yellow-breasted Bunting", "Pallas's Bunting",
  "Lazuli Bunting", "Gray Bunting", "Reed Bunting", "Corn Bunting", "Black-headed Bunting",
  "Black-faced Bunting", "Ortolan Bunting", "European Robin", "Barred Warbler",
  "Blyth's Reed Warbler", "Black-throated Blue Warbler", "Common Rosefinch",
  "Willow Warbler", "Yellow-browed Warbler", "Icterine Warbler", "Cliff Swallow",
  "Eurasian Bullfinch", "Red-breasted Flycatcher", "Barn Swallow", "Garden Warbler",
  "Song Thrush", "Eurasian Wren", "Blackpoll Warbler", "Common Chaffinch",
  "Hawfinch", "Hermit Thrush", "Spotted Flycatcher", "Swainson's Thrush",
  "European Goldfinch", "Sedge Warbler", "White-throated Sparrow", 
  "European Pied Flycatcher", "Bank Swallow", "Yellow-rumped Warbler",
  "Tennessee Warbler", "Orange-crowned Warbler", "Song Sparrow", "American Robin",
  "Black-capped Chickadee", "Fox Sparrow", "Golden-crowned Sparrow", 
  "Red-breasted Nuthatch", "Ruby-crowned Kinglet", "Tree Swallow", 
  "Varied Thrush", "Violet-green Swallow", "American Tree Sparrow", 
  "Arctic Warbler", "Gray-cheeked Thrush", "Northern Waterthrush", 
  "White-crowned Sparrow", "Wilson's Warbler", "Yellow Warbler", 
  "Chestnut-backed Chickadee", "Pacific Wren", "Townsend's Warbler",
  "Clay-colored Sparrow", "Dark-sided Flycatcher", "Gray-crowned Rosy-Finch",
  "Gray-streaked Flycatcher", "Middendorff's Grasshopper Warbler", 
  "Alder Flycatcher", "Boreal Chickadee", "Lincoln's Sparrow", 
  "Dusky Warbler", "Nashville Warbler", "Purple Finch", "Warbling Vireo",
  "Golden-crowned Kinglet", "Asian Brown Flycatcher", "Great Spotted Woodpecker",
  "Olive-sided Flycatcher", "Oriental Greenfinch", "Common Nighthawk",
  "Chipping Sparrow", "Pallas's Leaf Warbler", "Lanceolated Warbler",
  "Western Flycatcher", "Dusky Thrush", "Willow Flycatcher", "Mourning Warbler",
  "Yellow-bellied Flycatcher", "Magnolia Warbler", "Cape May Warbler",
  "Siberian Blue Robin", "Least Flycatcher", "MacGillivray's Warbler",
  "Bay-breasted Warbler", "Rufous-tailed Robin", "Palm Warbler",
  "Scissor-tailed Flycatcher", "Cassin's Finch", "Booted Warbler",
  "Common Reed Warbler", "Thrush Nightingale", "Gray-headed Chickadee",
  "Lesser Spotted Woodpecker", "Great Reed Warbler", "European Red-rumped Swallow",
  "Hume's Warbler", "Western Bonelli's Warbler", "White's Thrush", 
  "Gray-headed Woodpecker", "Eurasian Three-toed Woodpecker", "Greater Short-toed Lark",
  "Red-footed Falcon", "Steppe Eagle", "Paddyfield Warbler", "Green Warbler",
  "Eurasian Nuthatch", "Eurasian Green Woodpecker", "Tawny Pipit",
  "House Sparrow", "Eurasian Tree Sparrow",
  
  # Woodpeckers
  "Downy Woodpecker", "American Three-toed Woodpecker", "Black-backed Woodpecker",
  "Hairy Woodpecker",
  
  # Generic terrestrial groups
  "passerine sp.", "new world warbler sp.", "new world sparrow sp.", 
  "new world flycatcher sp.", "ptarmigan sp.", "pipit sp.", "wagtail sp.",
  "swallow sp.", "finch sp.", "crossbill sp.", "raven sp.", "crow/raven sp.",
  "woodpecker sp.", "owl sp.", "hawk sp.", "falcon sp.", "large falcon sp.",
  "harrier sp.", "diurnal raptor sp.", "chickadee sp.", "hummingbird sp.",
  "Empidonax sp.", "Phylloscopus sp.", "Catharus sp.", "Turdus sp.", 
  "Acanthis/Spinus sp.", "Emberiza sp.", "Cuculus sp.", "Acrocephalus sp.",
  "Anser sp.", "Branta sp.", "Aythya sp.", "Tringa sp.", "Calidris sp.",
  "Scolopacidae sp.", "Accipitrine hawk sp. (former Accipiter sp.)",
  
  # Shorebird groups
  "shorebird sp.", "large shorebird sp.", "peep sp.", "godwit sp.", 
  "curlew sp.", "plover sp.", "golden-plover sp.", "small plover sp.",
  "snipe sp.", "Parrot/Red Crossbill",
  
  # Duck/waterfowl groups
  "duck sp.", "dabbling duck sp.", "teal sp.", "merganser sp.", "goose sp.",
  "swan sp.", "grebe sp.", "waterfowl sp.", "white egret sp.",
  
  # Hybrids of terrestrial species
  "Northern Shoveler x Mallard (hybrid)", "Tufted Duck x Greater Scaup (hybrid)",
  "Snow x McKay's Bunting (hybrid)", "Common/Barrow's Goldeneye", 
  "Common x Barrow's Goldeneye (hybrid)", "Eurasian/American Wigeon",
  "Eurasian x American Wigeon (hybrid)", "Mallard x Northern Pintail (hybrid)",
  "Gadwall x Mallard (hybrid)", "Northern Pintail x Green-winged Teal (hybrid)",
  "House x Eurasian Tree Sparrow (hybrid)", "Eurasian Wigeon x Mallard (hybrid)",
  "House/Eurasian Tree Sparrow",
  
  # Generic bird
  "bird sp."
)

# Remove second round terrestrials
final_marine_data <- marine_data %>%
  filter(!`COMMON NAME` %in% additional_terrestrial)

# Results
removed_obs_round2 <- nrow(marine_data) - nrow(final_marine_data)
removed_species_round2 <- length(setdiff(unique(marine_data$`COMMON NAME`), unique(final_marine_data$`COMMON NAME`)))

cat("\nSECOND ROUND REMOVAL RESULTS:\n")
cat("Removed", removed_species_round2, "additional terrestrial species\n")
cat("Removed", format(removed_obs_round2, big.mark = ","), "additional observations\n")
cat("Final dataset:", format(nrow(final_marine_data), big.mark = ","), "observations\n")
cat("Final species count:", length(unique(final_marine_data$`COMMON NAME`)), "species\n\n")

unique(final_marine_data$`COMMON NAME`)




cat("Third round cleanup starting with", length(unique(final_marine_data$`COMMON NAME`)), "species\n")

# THIRD ROUND: Remove remaining obvious terrestrials from the 212 species list
third_round_terrestrial <- c(
  # Clear songbirds/terrestrials still remaining
  "Mistle Thrush", "Marsh Warbler", "European Greenfinch", "Wood Warbler",
  "Black-throated Thrush", "Eyebrowed Thrush", "Savannah Sparrow", 
  "Kamchatka Leaf Warbler", "Taiga Flycatcher", "Western Wood-Pewee",
  "Naumann's Thrush", "Thick-billed Warbler", "River Warbler", 
  "Japanese Leaf/Arctic/Kamchatka Leaf Warbler", "Snow/McKay's Bunting",
  "Pallas's Grasshopper Warbler", "Western/Eastern Wood-Pewee", 
  "Bohemian/Cedar Waxwing", "Tree/Violet-green Swallow", "Richard's Pipit",
  "Eastern Orphean Warbler", "Common Grasshopper Warbler", 
  "Eastern Subalpine Warbler", "Willow Warbler/Common Chiffchaff",
  "Meadow/Tree Pipit", "Siberian/Amur Stonechat",
  
  # Shorebirds and plovers still remaining
  "Black-bellied Plover/golden-plover sp.", "Great Knot", 
  "Short-billed/Long-billed Dowitcher", "Common/Wilson's Snipe",
  "American/Pacific Golden-Plover (Lesser Golden-Plover)",
  "Lesser/Greater Yellowlegs", "Western/Semipalmated Sandpiper",
  "Sharp-tailed/Pectoral Sandpiper", "Common Ringed/Semipalmated Plover",
  "Gray-tailed/Wandering Tattler", "Whimbrel/Eurasian Curlew",
  
  # Freshwater waterfowl still remaining  
  "Cackling/Canada Goose", "Greater/Lesser Scaup", "Horned/Eared Grebe",
  "Trumpeter/Tundra Swan", "Snow/Ross's Goose", "Tundra/Whooper Swan",
  "Common/Red-breasted Merganser",
  
  # Raptors still remaining
  "Hen/Northern Harrier",
  
  # Pipit remaining
  "Siberian/American Pipit"
)

# Remove third round terrestrials
truly_marine_data <- final_marine_data %>%
  filter(!`COMMON NAME` %in% third_round_terrestrial)

# Results
removed_obs_round3 <- nrow(final_marine_data) - nrow(truly_marine_data)
removed_species_round3 <- length(setdiff(unique(final_marine_data$`COMMON NAME`), unique(truly_marine_data$`COMMON NAME`)))

cat("\nTHIRD ROUND REMOVAL RESULTS:\n")
cat("Removed", removed_species_round3, "additional terrestrial species\n")
cat("Removed", format(removed_obs_round3, big.mark = ","), "additional observations\n")
cat("FINAL dataset:", format(nrow(truly_marine_data), big.mark = ","), "observations\n")
cat("FINAL species count:", length(unique(truly_marine_data$`COMMON NAME`)), "species\n\n")


unique(truly_marine_data$`COMMON NAME`)


cat("Assigning pelagic categories to", length(unique(truly_marine_data$`COMMON NAME`)), "marine species\n\n")

# TRUE PELAGIC - spend majority of life in open ocean, far from shore
true_pelagic_species <- c(
  # ALBATROSSES - extremely oceanic, only come to land to breed
  "Black-footed Albatross", "Laysan Albatross", "Short-tailed Albatross", 
  "Black-browed Albatross", "Atlantic Yellow-nosed Albatross", "Salvin's Albatross",
  "albatross sp.", "Laysan x Black-footed Albatross (hybrid)",
  
  # PETRELS & SHEARWATERS - highly pelagic, feed far offshore
  "Northern Fulmar", "Sooty Shearwater", "Great Shearwater", "Manx Shearwater",
  "Short-tailed Shearwater", "Buller's Shearwater", "Pink-footed Shearwater",
  "Flesh-footed Shearwater", "Mottled Petrel", "Providence Petrel",
  "shearwater sp.", "small shearwater sp.", "Sooty/Short-tailed Shearwater", 
  "procellariid sp.",
  
  # STORM-PETRELS - extremely pelagic, only seen from shore during storms
  "Wilson's Storm-Petrel", "Leach's Storm-Petrel", "Fork-tailed Storm-Petrel", 
  "European Storm-Petrel", "storm-petrel sp.", "storm-petrel sp. (white-rumped)",
  "Oceanites sp.", "Hydrobates sp.",
  
  # JAEGERS & SKUAS - pursue other seabirds far offshore
  "Pomarine Jaeger", "Parasitic Jaeger", "Long-tailed Jaeger", "Great Skua", 
  "South Polar Skua", "jaeger sp.", "skua sp.", "jaeger/skua sp.",
  "Long-tailed/Parasitic Jaeger", "Parasitic/Pomarine Jaeger",
  
  # ALCIDS - deep water specialists, dive in offshore waters
  "Thick-billed Murre", "Common Murre", "Razorbill", "Dovekie",
  "Atlantic Puffin", "Horned Puffin", "Tufted Puffin",
  "Ancient Murrelet", "Marbled Murrelet", "Kittlitz's Murrelet", "Long-billed Murrelet",
  "Cassin's Auklet", "Parakeet Auklet", "Least Auklet", "Crested Auklet",
  "Whiskered Auklet", "Rhinoceros Auklet", "Black Guillemot", "Pigeon Guillemot",
  "alcid sp.", "large alcid sp.", "murrelet sp.", "auklet sp.", "puffin sp.",
  "Thick-billed/Common Murre", "Black/Pigeon Guillemot",
  
  # TRULY PELAGIC GULLS - spend most time far offshore
  "Black-legged Kittiwake", "Red-legged Kittiwake", "Ivory Gull", "Ross's Gull", 
  "Sabine's Gull", "Black-legged/Red-legged Kittiwake",
  
  # NORTHERN GANNET - dives in deep offshore waters
  "Northern Gannet",
  
  # RED PHALAROPE - highly pelagic during non-breeding season
  "Red Phalarope"
)

# PARTIALLY PELAGIC - can be offshore but also coastal/nearshore
partially_pelagic_species <- c(
  # LARGE GULLS - forage both offshore and coastal
  "Great Black-backed Gull", "Glaucous Gull", "Lesser Black-backed Gull",
  "Iceland Gull", "American Herring Gull", "European Herring Gull",
  "Glaucous-winged Gull", "Slaty-backed Gull", "Vega Gull", "California Gull",
  "Ring-billed Gull", "Short-billed Gull", "Common Gull", "Caspian Gull",
  "Yellow-legged Gull", "Black-tailed Gull", "Heermann's Gull", "Pallas's Gull",
  
  # SMALLER GULLS - more coastal but can be offshore
  "Black-headed Gull", "Little Gull", "Bonaparte's Gull", "Franklin's Gull",
  "Laughing Gull", "Mediterranean Gull",
  
  # TERNS - mostly coastal but venture offshore
  "Arctic Tern", "Common Tern", "Aleutian Tern", "Caspian Tern", "Little Tern",
  "Black Tern", "White-winged Tern", "Sandwich Tern", "Roseate Tern", 
  "Gull-billed Tern",
  
  # CORMORANTS - dive in both coastal and offshore waters
  "Great Cormorant", "Double-crested Cormorant", "Pelagic Cormorant",
  "Red-faced Cormorant", "European Shag",
  
  # SEA DUCKS - found in both nearshore and offshore waters
  "Common Eider", "King Eider", "Spectacled Eider", "Steller's Eider",
  "Long-tailed Duck", "Harlequin Duck", "Surf Scoter", "White-winged Scoter",
  "Black Scoter", "Velvet Scoter", "Stejneger's Scoter", "Common Scoter",
  
  # LOONS - use both coastal and offshore marine waters
  "Common Loon", "Red-throated Loon", "Arctic Loon", "Pacific Loon", "Yellow-billed Loon",
  
  # OTHER PHALAROPES - less pelagic than Red Phalarope
  "Red-necked Phalarope", "Wilson's Phalarope",
  
  # BOOBIES - can be both coastal and pelagic
  "Brown Booby", "Red-footed Booby", "Cocos Booby",
  
  # GULL/TERN COMBINATIONS AND SPECIES GROUPS
  "gull sp.", "tern sp.", "cormorant sp.", "eider sp.", "scoter sp.", 
  "loon sp.", "phalarope sp.", "white-winged gull sp.", "gull/tern sp.",
  "Larus sp.", "Sterna sp.", "King/Common Eider", "Red/Red-necked Phalarope",
  "Surf/Black Scoter", "Arctic/Pacific Loon", "Common/Yellow-billed Loon",
  "White-winged/Stejneger's Scoter", "Velvet/White-winged/Stejneger's Scoter",
  "Common/Arctic Tern", "Common/Short-billed Gull", "Little/Least Tern",
  
  # GULL HYBRIDS
  "American Herring x Glaucous-winged Gull (hybrid)", "American Herring x Glaucous Gull (hybrid)",
  "Glaucous-winged x Slaty-backed Gull (hybrid)", "Glaucous x Glaucous-winged Gull (hybrid)",
  "European Herring x Glaucous Gull (hybrid)", "American Herring/Vega Gull",
  "American Herring/Iceland Gull", "American Herring/Glaucous-winged Gull",
  
  # EIDER HYBRIDS
  "King x Common Eider (hybrid)",
  
  # CORMORANT COMBINATIONS
  "Great Cormorant/European Shag", "Double-crested/Neotropic Cormorant"
)

# Assign pelagic categories
truly_marine_data$pelagic_type <- case_when(
  truly_marine_data$`COMMON NAME` %in% true_pelagic_species ~ "True Pelagic",
  truly_marine_data$`COMMON NAME` %in% partially_pelagic_species ~ "Partially Pelagic",
  TRUE ~ "UNCLASSIFIED"
)

# Check results
classification_summary <- truly_marine_data %>%
  st_drop_geometry() %>%
  count(pelagic_type, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

cat("PELAGIC CLASSIFICATION RESULTS:\n")
print(classification_summary)

# Check for unclassified species
unclassified_species <- truly_marine_data %>%
  st_drop_geometry() %>%
  filter(pelagic_type == "UNCLASSIFIED") %>%
  count(`COMMON NAME`, sort = TRUE)

if(nrow(unclassified_species) > 0) {
  cat("\nUNCLASSIFIED SPECIES (need manual assignment):\n")
  print(unclassified_species)
} else {
  cat("\n✅ ALL SPECIES SUCCESSFULLY CLASSIFIED!\n")
}

# Species breakdown by category
cat("\nSPECIES COUNT BY CATEGORY:\n")
species_by_category <- truly_marine_data %>%
  st_drop_geometry() %>%
  group_by(pelagic_type) %>%
  summarise(species_count = n_distinct(`COMMON NAME`), .groups = 'drop')
print(species_by_category)

# Save final dataset with pelagic classifications
final_output <- paste0("marine_with_pelagic_types_", Sys.Date(), ".rds")
saveRDS(truly_marine_data, final_output)
cat("\n✅ FINAL DATASET WITH PELAGIC TYPES SAVED:", final_output, "\n")

