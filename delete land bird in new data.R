rm(list = ls())
library(dplyr)

cat("=== UNIFIED ROBUST PELAGIC CLASSIFICATION ===\n")

# Load dataset
input_files <- list.files(pattern = "arctic_pelagic_marine_only\\.rds$")
if (length(input_files) == 0) {
  input_files <- list.files(pattern = "arctic_pelagic_master_2025-06-06.rds$")
}
if (length(input_files) == 0) {
  stop("No dataset found!")
}

pelagic_birds <- readRDS(input_files[1])
cat("Original dataset:", format(nrow(pelagic_birds), big.mark = ","), "observations\n")
cat("Original species:", length(unique(pelagic_birds$`COMMON NAME`)), "\n\n")

# TRUE PELAGIC - spend majority of life far from shore, highly oceanic
# Based on your old classification + biological principles
true_pelagic <- c(
  # ALBATROSSES - truly oceanic
  "Black-browed Albatross", "Black-footed Albatross", "Laysan Albatross", 
  "Short-tailed Albatross", "Atlantic Yellow-nosed Albatross",
  
  # PETRELS & SHEARWATERS - highly pelagic
  "Northern Fulmar", "Sooty Shearwater", "Great Shearwater", "Manx Shearwater",
  "Short-tailed Shearwater", "Buller's Shearwater", "Pink-footed Shearwater",
  "Flesh-footed Shearwater", "Mottled Petrel", "Providence Petrel",
  
  # STORM-PETRELS - extremely pelagic
  "Wilson's Storm-Petrel", "Leach's Storm-Petrel", "European Storm-Petrel",
  "Fork-tailed Storm-Petrel",
  
  # JAEGERS & SKUAS - pursue other seabirds far offshore
  "Great Skua", "South Polar Skua", "Pomarine Jaeger", "Parasitic Jaeger", 
  "Long-tailed Jaeger",
  
  # ALCIDS - deep water specialists (from your old TRUE PELAGIC list)
  "Thick-billed Murre", "Common Murre", "Razorbill", "Dovekie",
  "Black Guillemot", "Pigeon Guillemot", "Atlantic Puffin", "Horned Puffin", 
  "Tufted Puffin", "Ancient Murrelet", "Marbled Murrelet", "Kittlitz's Murrelet",
  "Long-billed Murrelet", "Cassin's Auklet", "Parakeet Auklet", "Least Auklet", 
  "Crested Auklet", "Whiskered Auklet", "Rhinoceros Auklet",
  
  # NORTHERN GANNET - dives in deep water, highly pelagic
  "Northern Gannet",
  
  # KITTIWAKES - truly pelagic gulls (resolving your overlap - these are TRUE PELAGIC)
  "Black-legged Kittiwake", "Red-legged Kittiwake",
  
  # RED PHALAROPE - highly pelagic when not breeding (resolving overlap)
  "Red Phalarope",
  
  # SPECIALIZED ARCTIC GULLS - when far from shore
  "Ivory Gull", "Ross's Gull", "Sabine's Gull",
  
  # Generic groups that are truly pelagic
  "shearwater sp.", "storm-petrel sp.", "jaeger sp.", "skua sp.", 
  "jaeger/skua sp.", "albatross sp.", "procellariid sp.", "alcid sp.",
  "large alcid sp.", "murrelet sp.", "auklet sp.", "puffin sp.",
  "storm-petrel sp. (white-rumped)", "Oceanites sp.", "Hydrobates sp.",
  "Black/Pigeon Guillemot", "Thick-billed/Common Murre", "Black-legged/Red-legged Kittiwake",
  "Sooty/Short-tailed Shearwater", "Parasitic/Pomarine Jaeger", "Long-tailed/Parasitic Jaeger"
)

# PARTIALLY PELAGIC - can be found offshore but also coastal/nearshore
# Based on your old PARTIALLY PELAGIC list + biological knowledge
partially_pelagic <- c(
  # LARGE GULLS - coastal but venture offshore
  "Great Black-backed Gull", "Glaucous Gull", "Lesser Black-backed Gull",
  "Iceland Gull", "American Herring Gull", "European Herring Gull", 
  "Glaucous-winged Gull", "Slaty-backed Gull", "Vega Gull",
  "California Gull", "Ring-billed Gull", "Short-billed Gull",
  
  # SMALLER GULLS - more coastal
  "Common Gull", "Black-headed Gull", "Little Gull", "Bonaparte's Gull",
  "Laughing Gull", "Franklin's Gull", "Mediterranean Gull", "Yellow-legged Gull",
  "Black-tailed Gull", "Heermann's Gull", "Caspian Gull",
  
  # TERNS - mostly coastal but can be offshore
  "Arctic Tern", "Common Tern", "Aleutian Tern", "Sandwich Tern", 
  "Caspian Tern", "Little Tern", "Black Tern", "White-winged Tern",
  "Gull-billed Tern", "Roseate Tern",
  
  # CORMORANTS - coastal but can venture offshore
  "Great Cormorant", "Double-crested Cormorant", "Pelagic Cormorant",
  "Red-faced Cormorant", "European Shag",
  
  # SEA DUCKS - when in marine environment
  "King Eider", "Common Eider", "Spectacled Eider", "Steller's Eider",
  "Long-tailed Duck", "Harlequin Duck", "Surf Scoter", "White-winged Scoter",
  "Black Scoter", "Velvet Scoter", "Common Scoter", "Stejneger's Scoter",
  
  # LOONS - when in marine environment
  "Common Loon", "Red-throated Loon", "Arctic Loon", "Pacific Loon", 
  "Yellow-billed Loon",
  
  # RED-NECKED PHALAROPE - more coastal than Red Phalarope
  "Red-necked Phalarope", "Wilson's Phalarope",
  
  # Generic groups
  "gull sp.", "tern sp.", "cormorant sp.", "eider sp.", "scoter sp.", 
  "loon sp.", "phalarope sp.", "white-winged gull sp.", "gull/tern sp.",
  "Common/Arctic Tern", "Red/Red-necked Phalarope", "King/Common Eider",
  "Surf/Black Scoter", "Arctic/Pacific Loon", "Common/Yellow-billed Loon",
  "Velvet/White-winged/Stejneger's Scoter", "White-winged/Stejneger's Scoter",
  "Great Cormorant/European Shag", "Common/Short-billed Gull",
  
  # Gull hybrids
  "European Herring x Glaucous Gull (hybrid)", "American Herring x Glaucous Gull (hybrid)",
  "Glaucous x Glaucous-winged Gull (hybrid)", "American Herring x Glaucous-winged Gull (hybrid)",
  "Glaucous-winged x Slaty-backed Gull (hybrid)", "American Herring/Vega Gull",
  "American Herring/Iceland Gull", "American Herring/Glaucous-winged Gull",
  "Laysan x Black-footed Albatross (hybrid)",
  
  # Duck hybrids and species groups
  "King x Common Eider (hybrid)", "Velvet/White-winged/Stejneger's Scoter"
)

# DEFINITELY NOT PELAGIC - comprehensive terrestrial bird removal
# All songbirds, raptors, gamebirds, etc. that should never be far offshore
definitely_not_pelagic <- c(
  # CORVIDS
  "Common Raven", "Hooded Crow", "Carrion Crow", "Eurasian Jackdaw", "Rook",
  "Eurasian Magpie", "Black-billed Magpie", "Canada Jay", "Steller's Jay", 
  "American Crow", "Siberian Jay", "crow/raven sp.",
  
  # BUNTINGS & LONGSPURS
  "Lapland Longspur", "Smith's Longspur", "Snow Bunting", "McKay's Bunting",
  "Reed Bunting", "Little Bunting", "Rustic Bunting", "Yellow-browed Bunting",
  "Corn Bunting", "Black-headed Bunting", "Black-faced Bunting", "Pallas's Bunting",
  "Pine Bunting", "Lazuli Bunting", "Snow/McKay's Bunting", "Snow x McKay's Bunting (hybrid)",
  "longspur sp.",
  
  # FINCHES
  "Common Redpoll", "Hoary Redpoll", "European Greenfinch", "European Goldfinch",
  "American Goldfinch", "Eurasian Siskin", "Pine Siskin", "Twite", "Eurasian Linnet",
  "Common Rosefinch", "Purple Finch", "Pine Grosbeak", "Evening Grosbeak",
  "Brambling", "Common Chaffinch", "Hawfinch", "Parrot Crossbill", "Red Crossbill",
  "White-winged Crossbill", "Eurasian Bullfinch", "Oriental Greenfinch",
  "Gray-crowned Rosy-Finch", "Rosy-cheeked Finch", "finch sp.", "crossbill sp.",
  "Parrot/Red Crossbill", "Acanthis/Spinus sp.",
  
  # THRUSHES & CHATS
  "European Robin", "Bluethroat", "Common Redstart", "Black Redstart", "Whinchat",
  "European Stonechat", "Amur Stonechat", "Northern Wheatear", "Isabelline Wheatear",
  "Redwing", "Fieldfare", "Song Thrush", "Mistle Thrush", "Eurasian Blackbird",
  "Ring Ouzel", "American Robin", "Varied Thrush", "Hermit Thrush", "Swainson's Thrush",
  "Gray-cheeked Thrush", "Dusky Thrush", "Eyebrowed Thrush", "Naumann's Thrush",
  "Black-throated Thrush", "White's Thrush", "Red-flanked Bluetail", "Siberian Rubythroat",
  "Rufous-tailed Robin", "Turdus sp.", "Catharus sp.",
  
  # OLD WORLD WARBLERS
  "Willow Warbler", "Common Chiffchaff", "Wood Warbler", "Eurasian Blackcap",
  "Garden Warbler", "Greater Whitethroat", "Lesser Whitethroat", "Barred Warbler",
  "Western Bonelli's Warbler", "Eastern Subalpine Warbler", "Arctic Warbler",
  "Greenish Warbler", "Green Warbler", "Yellow-browed Warbler", "Hume's Warbler",
  "Pallas's Leaf Warbler", "Kamchatka Leaf Warbler", "Dusky Warbler", "Sedge Warbler",
  "Marsh Warbler", "Common Reed Warbler", "Blyth's Reed Warbler", "Paddyfield Warbler",
  "Icterine Warbler", "Middendorff's Grasshopper Warbler", "Pallas's Grasshopper Warbler",
  "Common Grasshopper Warbler", "Lanceolated Warbler", "Phylloscopus sp.",
  "Willow Warbler/Common Chiffchaff",
  
  # NEW WORLD WARBLERS
  "Yellow Warbler", "Yellow-rumped Warbler", "Blackpoll Warbler", "Black-throated Blue Warbler",
  "Tennessee Warbler", "Orange-crowned Warbler", "MacGillivray's Warbler", "Mourning Warbler",
  "Common Yellowthroat", "Wilson's Warbler", "Townsend's Warbler", "Magnolia Warbler",
  "Palm Warbler", "Bay-breasted Warbler", "Chestnut-sided Warbler", "American Redstart",
  "Ovenbird", "Northern Waterthrush", "Yellow-throated Warbler", "Nashville Warbler",
  "Black-and-white Warbler", "new world warbler sp.",
  
  # PIPITS & WAGTAILS  
  "American Pipit", "Meadow Pipit", "Tree Pipit", "Rock Pipit", "Red-throated Pipit",
  "Olive-backed Pipit", "Pechora Pipit", "Richard's Pipit", "Tawny Pipit", 
  "Siberian Pipit", "Siberian/American Pipit", "White Wagtail", "Western Yellow Wagtail",
  "Eastern Yellow Wagtail", "Citrine Wagtail", "Gray Wagtail", "pipit sp.", "wagtail sp.",
  "Meadow/Tree Pipit",
  
  # SWALLOWS
  "Barn Swallow", "Cliff Swallow", "Tree Swallow", "Violet-green Swallow", 
  "Bank Swallow", "Western House-Martin", "Siberian House-Martin", "swallow sp.",
  "Tree/Violet-green Swallow",
  
  # FLYCATCHERS
  "European Pied Flycatcher", "Red-breasted Flycatcher", "Spotted Flycatcher",
  "Taiga Flycatcher", "Asian Brown Flycatcher", "Gray-streaked Flycatcher",
  "Dark-sided Flycatcher", "Alder Flycatcher", "Least Flycatcher", "Olive-sided Flycatcher",
  "Western Wood-Pewee", "Western/Eastern Wood-Pewee", "Say's Phoebe", "Eastern Kingbird", 
  "Western Kingbird", "Willow Flycatcher", "Western Flycatcher", "new world flycatcher sp.",
  "Empidonax sp.",
  
  # TITS & CHICKADEES
  "Great Tit", "Eurasian Blue Tit", "Coal Tit", "Marsh Tit", "Willow Tit", 
  "Long-tailed Tit", "Black-capped Chickadee", "Boreal Chickadee", "Gray-headed Chickadee", 
  "Chestnut-backed Chickadee", "chickadee sp.",
  
  # NUTHATCHES & CREEPERS
  "Red-breasted Nuthatch", "Eurasian Treecreeper", "Brown Creeper",
  
  # WRENS
  "Eurasian Wren", "Pacific Wren", "Winter Wren",
  
  # KINGLETS
  "Goldcrest", "Golden-crowned Kinglet", "Ruby-crowned Kinglet",
  
  # SHRIKES
  "Great Gray Shrike", "Red-backed Shrike", "Brown Shrike", "Woodchat Shrike", "Northern Shrike",
  
  # VIREOS
  "Red-eyed Vireo", "Philadelphia Vireo", "Warbling Vireo", "Blue-headed Vireo",
  
  # SPARROWS
  "House Sparrow", "Eurasian Tree Sparrow", "American Tree Sparrow", "Chipping Sparrow",
  "Clay-colored Sparrow", "Savannah Sparrow", "Fox Sparrow", "Song Sparrow", "Lincoln's Sparrow",
  "Swamp Sparrow", "White-throated Sparrow", "White-crowned Sparrow", "Golden-crowned Sparrow",
  "Harris's Sparrow", "Dark-eyed Junco", "Lark Sparrow", "Nelson's Sparrow", "LeConte's Sparrow",
  "new world sparrow sp.", "House/Eurasian Tree Sparrow", "House x Eurasian Tree Sparrow (hybrid)",
  
  # BLACKBIRDS & ORIOLES
  "Red-winged Blackbird", "Yellow-headed Blackbird", "Rusty Blackbird", "Brewer's Blackbird",
  "Common Grackle", "Brown-headed Cowbird", "Bullock's Oriole",
  
  # OTHER SONGBIRDS
  "European Starling", "Rosy Starling", "Northern Mockingbird", "Gray Catbird", 
  "Brown Thrasher", "Sage Thrasher", "American Dipper", "White-throated Dipper", 
  "Cedar Waxwing", "Bohemian Waxwing", "Bohemian/Cedar Waxwing", "passerine sp.",
  "Dunnock", "Common Nightingale", "Townsend's Solitaire", "Mountain Bluebird", "Eastern Bluebird",
  
  # GROUSE & PTARMIGANS
  "Rock Ptarmigan", "Willow Ptarmigan", "Spruce Grouse", "Black Grouse", 
  "Western Capercaillie", "Sharp-tailed Grouse", "ptarmigan sp.",
  
  # OWLS
  "Snowy Owl", "Great Horned Owl", "Eurasian Eagle-Owl", "Short-eared Owl", 
  "Long-eared Owl", "Boreal Owl", "Northern Saw-whet Owl", "Eurasian Pygmy-Owl", "owl sp.",
  
  # RAPTORS
  "Golden Eagle", "White-tailed Eagle", "Steller's Sea-Eagle", "Bald Eagle", "Gyrfalcon",
  "Peregrine Falcon", "Merlin", "American Kestrel", "Eurasian Kestrel", "Eurasian Hobby",
  "Red-footed Falcon", "Rough-legged Hawk", "Red-tailed Hawk", "American Goshawk",
  "Eurasian Goshawk", "Sharp-shinned Hawk", "Eurasian Sparrowhawk", "Northern Harrier",
  "Hen Harrier", "Hen/Northern Harrier", "Western Marsh Harrier", "Pallid Harrier", 
  "Common Buzzard", "Steppe Eagle", "Black Kite", "falcon sp.", "harrier sp.",
  "diurnal raptor sp.", "large falcon sp.", "Accipitrine hawk sp. (former Accipiter sp.)",
  
  # WOODPECKERS
  "Great Spotted Woodpecker", "Downy Woodpecker", "Hairy Woodpecker", 
  "American Three-toed Woodpecker", "Eurasian Three-toed Woodpecker", "Black-backed Woodpecker",
  "Northern Flicker", "Red-breasted Sapsucker", "Lesser Spotted Woodpecker", 
  "Gray-headed Woodpecker", "Eurasian Wryneck", "woodpecker sp.",
  
  # GAMEBIRDS (excluding waterfowl/seabirds)
  "Common Quail", "Ring-necked Pheasant", "Common/Wilson's Snipe", "snipe sp.",
  
  # OTHER TERRESTRIALS
  "Common Cuckoo", "Oriental Cuckoo", "Cuculus sp.", "Common Swift", "Alpine Swift", 
  "Pacific Swift", "Eurasian Nightjar", "Common Nighthawk", "Eurasian Hoopoe", 
  "European Bee-eater", "Eurasian Golden Oriole", "Rock Pigeon", "European Turtle-Dove",
  "Oriental Turtle-Dove", "Eurasian Collared-Dove", "Mourning Dove", "White-winged Dove",
  "Stock Dove", "Common Wood-Pigeon", "Eurasian Jay", "Belted Kingfisher", "Ruby-throated Hummingbird",
  "Rufous Hummingbird", "Anna's Hummingbird", "hummingbird sp.", "Rose-breasted Grosbeak",
  "Black-headed Grosbeak", "Western Tanager", "Horned Lark"
)

# Apply classification
cat("=== APPLYING UNIFIED CLASSIFICATION ===\n")

# Get species counts
original_species <- unique(pelagic_birds$`COMMON NAME`)
true_pelagic_found <- original_species[original_species %in% true_pelagic]
partially_pelagic_found <- original_species[original_species %in% partially_pelagic]
definitely_not_found <- original_species[original_species %in% definitely_not_pelagic]
unclassified <- original_species[!original_species %in% c(true_pelagic, partially_pelagic, definitely_not_pelagic)]

cat("Classification results:\n")
cat("  True pelagic species found:", length(true_pelagic_found), "\n")
cat("  Partially pelagic species found:", length(partially_pelagic_found), "\n")
cat("  Terrestrial species to remove:", length(definitely_not_found), "\n")
cat("  Unclassified species:", length(unclassified), "\n\n")

# Show top unclassified for manual review
if (length(unclassified) > 0) {
  cat("TOP UNCLASSIFIED SPECIES (manual review needed):\n")
  unclassified_counts <- pelagic_birds %>%
    st_drop_geometry() %>%
    filter(`COMMON NAME` %in% unclassified) %>%
    count(`COMMON NAME`, sort = TRUE)
  
  for (i in 1:min(20, nrow(unclassified_counts))) {
    cat("  -", unclassified_counts$`COMMON NAME`[i], 
        "(", format(unclassified_counts$n[i], big.mark = ","), "obs )\n")
  }
  cat("\n")
}

# Remove terrestrial species
cat("Removing terrestrial species...\n")
terrestrial_obs <- sum(pelagic_birds$`COMMON NAME` %in% definitely_not_pelagic)
cat("Terrestrial observations to remove:", format(terrestrial_obs, big.mark = ","), "\n")

# Filter to keep only marine species
marine_only <- pelagic_birds %>%
  filter(!`COMMON NAME` %in% definitely_not_pelagic)

# Add unified classification
marine_only$pelagic_category <- case_when(
  marine_only$`COMMON NAME` %in% true_pelagic ~ "True Pelagic",
  marine_only$`COMMON NAME` %in% partially_pelagic ~ "Partially Pelagic",
  TRUE ~ "Unclassified"
)

# Final summary
cat("\n=== FINAL RESULTS ===\n")
cat("Original dataset:", format(nrow(pelagic_birds), big.mark = ","), "obs,", 
    length(unique(pelagic_birds$`COMMON NAME`)), "species\n")
cat("Marine-only dataset:", format(nrow(marine_only), big.mark = ","), "obs,", 
    length(unique(marine_only$`COMMON NAME`)), "species\n")

# Classification summary
final_summary <- marine_only %>%
  st_drop_geometry() %>%
  group_by(pelagic_category) %>%
  summarise(
    species = n_distinct(`COMMON NAME`),
    observations = n(),
    .groups = 'drop'
  )

cat("\nFinal classification breakdown:\n")
print(final_summary)

cat("\nSpecies removed:", length(unique(pelagic_birds$`COMMON NAME`)) - length(unique(marine_only$`COMMON NAME`)), "\n")
cat("Observations removed:", format(nrow(pelagic_birds) - nrow(marine_only), big.mark = ","), "\n")



unclassified_species <- marine_only %>%
  st_drop_geometry() %>%
  filter(pelagic_category == "Unclassified") %>%
  count(`COMMON NAME`, sort = TRUE)

cat("=== ALL 212 UNCLASSIFIED SPECIES ===\n")
cat("(These need to be manually classified or removed)\n\n")

# Show all unclassified species
for (i in 1:nrow(unclassified_species)) {
  cat(sprintf("%3d. %-50s (%s obs)\n", 
              i, 
              unclassified_species$`COMMON NAME`[i],
              format(unclassified_species$n[i], big.mark = ",")))
}



# Save cleaned dataset
output_file <- "arctic_pelagic_classified.rds"
saveRDS(marine_only, output_file)
cat("✓ Saved:", output_file, "\n")

