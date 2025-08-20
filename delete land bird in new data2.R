rm(list = ls())
library(dplyr)

# Load your classified dataset
marine_only <- readRDS("arctic_pelagic_classified.rds")

cat("=== REMOVING OBVIOUS TERRESTRIAL & sp. GROUPS ===\n")
cat("Current unclassified species:", sum(marine_only$pelagic_category == "Unclassified"), "obs\n\n")

# OBVIOUS TERRESTRIAL BIRDS TO REMOVE
obvious_terrestrial <- c(
  # Songbirds that somehow got through
  "Redpoll", "Eurasian Skylark", "Siberian Accentor", "Yellowhammer",
  
  # Strictly coastal/terrestrial birds
  "Eurasian Oystercatcher", "Black Oystercatcher",  # Coastal only
  "Common Snipe", "Wilson's Snipe", "Jack Snipe",   # Freshwater/marsh
  "Eurasian Woodcock",                               # Forest bird
  "Sandhill Crane", "Common Crane",                 # Terrestrial
  "Sora", "American Coot", "Eurasian Coot", "Water Rail", "Yellow Rail", "Eurasian Moorhen", "Corn Crake",  # Marsh birds
  "American Bittern", "Eurasian Bittern",           # Marsh birds
  "Osprey",                                          # Freshwater fish-eater
  "Killdeer", "Northern Lapwing",                   # Terrestrial plovers
  "Eurasian Dotterel", "Oriental Plover",           # Terrestrial plovers
  "Black-winged Pratincole",                        # Terrestrial
  "Pied Avocet", "American Avocet",                 # Shallow water specialists
  
  # Small sandpipers (shouldn't be 25km offshore)
  "Dunlin", "Sanderling", "Purple Sandpiper", "Rock Sandpiper", 
  "Western Sandpiper", "Semipalmated Sandpiper", "Least Sandpiper",
  "Pectoral Sandpiper", "White-rumped Sandpiper", "Baird's Sandpiper",
  "Stilt Sandpiper", "Buff-breasted Sandpiper", "Red-necked Stint",
  "Temminck's Stint", "Little Stint", "Long-toed Stint", "Sharp-tailed Sandpiper",
  "Curlew Sandpiper", "Broad-billed Sandpiper",
  
  # Turnstones (coastal)
  "Ruddy Turnstone", "Black Turnstone",
  
  # Small plovers (coastal)
  "Common Ringed Plover", "Semipalmated Plover", "Little Ringed Plover",
  "Kentish Plover", "Snowy Plover", "Siberian Sand-Plover",
  
  # Yellowlegs and tattlers (wading birds)
  "Greater Yellowlegs", "Lesser Yellowlegs", "Common Greenshank",
  "Spotted Redshank", "Common Redshank", "Wood Sandpiper", 
  "Green Sandpiper", "Solitary Sandpiper", "Spotted Sandpiper",
  "Wandering Tattler", "Gray-tailed Tattler", "Terek Sandpiper",
  "Marsh Sandpiper",
  
  # Dowitchers and godwits (wading birds)
  "Long-billed Dowitcher", "Short-billed Dowitcher", "Bar-tailed Godwit",
  "Black-tailed Godwit", "Hudsonian Godwit", "Marbled Godwit",
  
  # Knots and other shorebirds
  "Red Knot", "Great Knot", "Surfbird", "Ruff", "Bristle-thighed Curlew",
  
  # Curlews and whimbrels (large shorebirds, but still coastal)
  "Whimbrel", "Eurasian Curlew", "Far Eastern Curlew", "Little Curlew",
  
  # Golden plovers (terrestrial)
  "European Golden-Plover", "American Golden-Plover", "Pacific Golden-Plover",
  "Black-bellied Plover",
  
  # Freshwater ducks
  "Mallard", "Northern Pintail", "Green-winged Teal", "Blue-winged Teal",
  "American Wigeon", "Eurasian Wigeon", "Northern Shoveler", "Gadwall",
  "Wood Duck", "American Black Duck", "Garganey", "Baikal Teal",
  "Falcated Duck", "Mandarin Duck",
  
  # Diving ducks (mostly freshwater/coastal)
  "Tufted Duck", "Common Pochard", "Canvasback", "Redhead", "Ring-necked Duck",
  "Greater Scaup", "Lesser Scaup", "Bufflehead", "Ruddy Duck",
  
  # Mergansers (mostly freshwater)
  "Red-breasted Merganser", "Common Merganser", "Hooded Merganser", "Smew",
  
  # Goldeneyes (mostly freshwater)
  "Common Goldeneye", "Barrow's Goldeneye",
  
  # Grebes (mostly freshwater)
  "Red-necked Grebe", "Horned Grebe", "Little Grebe", "Great Crested Grebe",
  
  # Geese (mostly terrestrial when not migrating)
  "Canada Goose", "Cackling Goose", "Snow Goose", "Ross's Goose",
  "Greater White-fronted Goose", "Lesser White-fronted Goose",
  "Graylag Goose", "Pink-footed Goose", "Taiga Bean-Goose", "Tundra Bean-Goose",
  "Bar-headed Goose", "Emperor Goose", "Barnacle Goose", "Brant",
  
  # Swans (mostly freshwater)
  "Tundra Swan", "Trumpeter Swan", "Whooper Swan", "Mute Swan",
  
  # Shelducks (coastal but not pelagic)
  "Common Shelduck", "Ruddy Shelduck",
  
  # Herons and egrets (wading birds)
  "Gray Heron", "Great Blue Heron", "Great Egret", "Little Egret",
  "Western Cattle-Egret", "Medium Egret", "Black-crowned Night Heron",
  "Chinese Pond-Heron",
  
  # Pelicans and boobies (when not truly pelagic)
  "American White Pelican", "Red-footed Booby", "Cocos Booby",
  
  # Other waterbirds
  "Eurasian Spoonbill",
  
  # Hybrid combinations and groups with hybrids
  "Eurasian Wigeon x Mallard (hybrid)", "Northern Shoveler x Mallard (hybrid)",
  "Mallard x American Black Duck (hybrid)", "Mallard x Northern Pintail (hybrid)",
  "Northern Pintail x Green-winged Teal (hybrid)", "Eurasian x American Wigeon (hybrid)",
  "Snow x Ross's Goose (hybrid)", "Snow x Cackling Goose (hybrid)",
  "Common x Barrow's Goldeneye (hybrid)"
)

# GENERIC sp. GROUPS TO REMOVE (impossible to classify)
sp_groups <- c(
  "bird sp.", "waterfowl sp.", "duck sp.", "goose sp.", "swan sp.", "teal sp.",
  "merganser sp.", "grebe sp.", "shorebird sp.", "large shorebird sp.", 
  "peep sp.", "small plover sp.", "plover sp.", "golden-plover sp.",
  "curlew sp.", "godwit sp.", "Calidris sp.", "Tringa sp.", "Scolopacidae sp.",
  "Western/Semipalmated Sandpiper", "Lesser/Greater Yellowlegs", 
  "Short-billed/Long-billed Dowitcher", "Sharp-tailed/Pectoral Sandpiper",
  "Black-bellied Plover/golden-plover sp.", "American/Pacific Golden-Plover (Lesser Golden-Plover)",
  "Greater/Lesser Scaup", "Common/Barrow's Goldeneye", "Common/Red-breasted Merganser",
  "Trumpeter/Tundra Swan", "Cackling/Canada Goose", "Snow/Ross's Goose",
  "Taiga/Tundra Bean-Goose", "Eurasian/American Wigeon", "Mallard/American Black Duck",
  "Horned/Eared Grebe", "Whimbrel/Eurasian Curlew", "dabbling duck sp.",
  "small shearwater sp.",  # This one might actually be marine, but too vague
  "Anser sp.", "Branta sp.", "Aythya sp.", "Emberiza sp.",
  
  # Complex gull hybrids (keep the simpler ones)
  "American Herring/Vega/European Herring x Glaucous Gull (hybrid)",
  "American/European Herring Gull", "Vega/Mongolian Gull",
  
  # Cormorant hybrid (keep simple ones)
  "Double-crested/Neotropic Cormorant"
)

# ALL SPECIES TO REMOVE
species_to_remove <- c(obvious_terrestrial, sp_groups)

cat("Removing", length(species_to_remove), "obvious terrestrial/sp. group species...\n")

# Count observations being removed
unclassified_to_remove <- marine_only %>%
  filter(pelagic_category == "Unclassified" & `COMMON NAME` %in% species_to_remove) %>%
  st_drop_geometry() %>%
  count(`COMMON NAME`, sort = TRUE)

if (nrow(unclassified_to_remove) > 0) {
  cat("Top species being removed:\n")
  for (i in 1:min(10, nrow(unclassified_to_remove))) {
    cat("  -", unclassified_to_remove$`COMMON NAME`[i], 
        "(", format(unclassified_to_remove$n[i], big.mark = ","), "obs )\n")
  }
  
  total_removed_obs <- sum(unclassified_to_remove$n)
  cat("Total observations removed:", format(total_removed_obs, big.mark = ","), "\n\n")
}

# Filter out the obvious terrestrial and sp. groups
cleaned_marine <- marine_only %>%
  filter(!`COMMON NAME` %in% species_to_remove)

# Show remaining unclassified
remaining_unclassified <- cleaned_marine %>%
  st_drop_geometry() %>%
  filter(pelagic_category == "Unclassified") %>%
  count(`COMMON NAME`, sort = TRUE)

cat("=== REMAINING UNCLASSIFIED SPECIES ===\n")
cat("(These need manual review)\n\n")

if (nrow(remaining_unclassified) > 0) {
  for (i in 1:nrow(remaining_unclassified)) {
    cat(sprintf("%3d. %-40s (%s obs)\n", 
                i, 
                remaining_unclassified$`COMMON NAME`[i],
                format(remaining_unclassified$n[i], big.mark = ",")))
  }
} else {
  cat("No unclassified species remaining!\n")
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Original dataset:", format(nrow(marine_only), big.mark = ","), "obs\n")
cat("After removing obvious terrestrial:", format(nrow(cleaned_marine), big.mark = ","), "obs\n")
cat("Observations removed:", format(nrow(marine_only) - nrow(cleaned_marine), big.mark = ","), "\n")
cat("Species removed:", length(unique(marine_only$`COMMON NAME`)) - length(unique(cleaned_marine$`COMMON NAME`)), "\n")
cat("Remaining unclassified species:", nrow(remaining_unclassified), "\n")

# Save cleaned dataset
saveRDS(cleaned_marine, "arctic_pelagic_cleaned_v2.rds")
cat("✓ Saved: arctic_pelagic_cleaned_v2.rds\n")

# Final classification summary
final_summary <- cleaned_marine %>%
  st_drop_geometry() %>%
  group_by(pelagic_category) %>%
  summarise(
    species = n_distinct(`COMMON NAME`),
    observations = n(),
    .groups = 'drop'
  )

cat("\nFinal classification:\n")
print(final_summary)
