#!/bin/bash
# Font Awesome subset generator

set -e

# Configuration
REPO_DIR="${GW_REPO_DIR:-$HOME/gw}"
SCAN_DIRS=("${REPO_DIR}/lib" "${REPO_DIR}/hd/etc")

# File names
DETECTED_SOLID="icons-detected-solid.txt"
DETECTED_REGULAR="icons-detected-regular.txt"
DETECTED_BRANDS="icons-detected-brands.txt"
WHITELIST_SOLID="icons-whitelist-solid.txt"
WHITELIST_REGULAR="icons-whitelist-regular.txt"
WHITELIST_BRANDS="icons-whitelist-brands.txt"
BLACKLIST="icons-blacklist.txt"
CONFIG_JSON="fontawesome-subset-config.json"

# Known brand icons
KNOWN_BRANDS="github markdown wikipedia-w"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}=== Enhanced Font Awesome Subset Generator (Fixed) ===${NC}"
echo ""

# Step 1: Create whitelist files if they don't exist
create_whitelist_file() {
    local file=$1
    local style=$2
    
    if [ ! -f "$file" ]; then
        echo -e "${YELLOW}Creating $file...${NC}"
        cat > "$file" << EOF
# Icons Whitelist - $style style
# Add icons here that aren't automatically detected
# One icon name per line (without fa- prefix)
# This file is preserved between runs
EOF
        echo -e "  Created new $file"
    else
        local count=$(grep -v '^#' "$file" | grep -v '^$' | wc -l)
        echo -e "${GREEN}✓ Using existing $file ($count icons)${NC}"
    fi
}

create_whitelist_file "$WHITELIST_SOLID" "Solid"
create_whitelist_file "$WHITELIST_REGULAR" "Regular"
create_whitelist_file "$WHITELIST_BRANDS" "Brands"

# Create blacklist if it doesn't exist
if [ ! -f "$BLACKLIST" ]; then
    echo -e "${YELLOW}Creating blacklist file...${NC}"
    cat > "$BLACKLIST" << 'EOF'
# Icons Blacklist - Add false positives here
# One icon name per line (without fa- prefix)
# Common false positives:
2x
2xs
lg
xl
2xl
sm
xs
fw
fa
fas
far
fab
solid
regular
brands
EOF
else
    echo -e "${GREEN}✓ Using existing blacklist${NC}"
fi

# Load blacklist into memory for efficient filtering
echo -e "${BLUE}Loading blacklist...${NC}"
declare -A blacklist_map
while IFS= read -r line; do
    # Skip comments and empty lines
    [[ "$line" =~ ^#.*$ ]] && continue
    [[ -z "$line" ]] && continue
    # Trim whitespace
    line=$(echo "$line" | xargs)
    blacklist_map["$line"]=1
done < "$BLACKLIST"
echo -e "  Loaded ${#blacklist_map[@]} blacklist entries"

# Function to check if an icon is blacklisted
is_blacklisted() {
    local icon="$1"
    [[ -n "${blacklist_map[$icon]}" ]] && return 0
    return 1
}

# Step 2: Scan repository with style detection
echo ""
echo -e "${BLUE}Scanning for Font Awesome icons...${NC}"

# Create temporary files
> "$DETECTED_SOLID.tmp"
> "$DETECTED_REGULAR.tmp"
> "$DETECTED_BRANDS.tmp"

for dir in "${SCAN_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo -e "  Scanning: $dir"
        
        # Find all files
        find "$dir" -type f \( -name "*.txt" -o -name "*.ml" \) | while read -r file; do
            # Extract all icon references and filter immediately
            
            # Solid icons (fas)
            grep -Po 'class="[^"]*\bfas\s+fa-([a-z0-9-]+)' "$file" 2>/dev/null | \
                grep -Po 'fa-\K[a-z0-9-]+' | while read -r icon; do
                    if ! is_blacklisted "$icon"; then
                        echo "$icon" >> "$DETECTED_SOLID.tmp"
                    fi
                done || true
            
            # Regular icons (far)
            grep -Po 'class="[^"]*\bfar\s+fa-([a-z0-9-]+)' "$file" 2>/dev/null | \
                grep -Po 'fa-\K[a-z0-9-]+' | while read -r icon; do
                    if ! is_blacklisted "$icon"; then
                        echo "$icon" >> "$DETECTED_REGULAR.tmp"
                    fi
                done || true
            
            # Brand icons (fab)
            grep -Po 'class="[^"]*\bfab\s+fa-([a-z0-9-]+)' "$file" 2>/dev/null | \
                grep -Po 'fa-\K[a-z0-9-]+' | while read -r icon; do
                    if ! is_blacklisted "$icon"; then
                        echo "$icon" >> "$DETECTED_BRANDS.tmp"
                    fi
                done || true
            
            # Generic pattern (class="fa fa-xxx")
            grep -Po 'class="[^"]*\bfa\s+fa-([a-z0-9-]+)' "$file" 2>/dev/null | \
                grep -Po 'fa-\K[a-z0-9-]+' | while read -r icon; do
                    if ! is_blacklisted "$icon"; then
                        # Check if it's a known brand
                        if echo "$KNOWN_BRANDS" | grep -w -q "$icon"; then
                            echo "$icon" >> "$DETECTED_BRANDS.tmp"
                        else
                            echo "$icon" >> "$DETECTED_SOLID.tmp"
                        fi
                    fi
                done || true
        done
    else
        echo -e "  ${YELLOW}Warning: Directory not found: $dir${NC}"
    fi
done

# Remove duplicates and sort
sort -u "$DETECTED_SOLID.tmp" > "$DETECTED_SOLID" 2>/dev/null || touch "$DETECTED_SOLID"
sort -u "$DETECTED_REGULAR.tmp" > "$DETECTED_REGULAR" 2>/dev/null || touch "$DETECTED_REGULAR"
sort -u "$DETECTED_BRANDS.tmp" > "$DETECTED_BRANDS" 2>/dev/null || touch "$DETECTED_BRANDS"

# Clean up temp files
rm -f "$DETECTED_SOLID.tmp" "$DETECTED_REGULAR.tmp" "$DETECTED_BRANDS.tmp"

# Count detections
SOLID_COUNT=$(wc -l < "$DETECTED_SOLID")
REGULAR_COUNT=$(wc -l < "$DETECTED_REGULAR")
BRANDS_COUNT=$(wc -l < "$DETECTED_BRANDS")

echo -e "${GREEN}✓ Found (after blacklist filtering):${NC}"
echo -e "  Solid:   $SOLID_COUNT icons"
echo -e "  Regular: $REGULAR_COUNT icons"
echo -e "  Brands:  $BRANDS_COUNT icons"

# Step 3: Build final lists with whitelists
echo ""
echo -e "${BLUE}Building final icon lists...${NC}"

build_final_list() {
    local detected=$1
    local whitelist=$2
    local style=$3
    local output="final-$style.txt"
    
    # Start with detected icons
    cp "$detected" "$output"
    
    # Add whitelist entries (but still filter through blacklist)
    if [ -f "$whitelist" ]; then
        grep -v '^#' "$whitelist" | grep -v '^$' | while read -r icon; do
            # Trim whitespace
            icon=$(echo "$icon" | xargs)
            if [ -n "$icon" ] && ! is_blacklisted "$icon"; then
                echo "$icon" >> "$output"
            fi
        done || true
    fi
    
    # Sort and remove duplicates
    sort -u "$output" -o "$output"
    
    local count=$(wc -l < "$output")
    echo -e "  $style: $count icons (after whitelist additions)"
}

build_final_list "$DETECTED_SOLID" "$WHITELIST_SOLID" "solid"
build_final_list "$DETECTED_REGULAR" "$WHITELIST_REGULAR" "regular"
build_final_list "$DETECTED_BRANDS" "$WHITELIST_BRANDS" "brands"

# Step 4: Generate configuration JSON
echo ""
echo -e "${BLUE}Generating configuration...${NC}"

# Function to convert file to JSON array
file_to_json_array() {
    local file=$1
    local first=true
    
    if [ -s "$file" ]; then
        while IFS= read -r icon; do
            if [ -n "$icon" ]; then
                if [ "$first" = true ]; then
                    printf '    "%s"' "$icon"
                    first=false
                else
                    printf ',\n    "%s"' "$icon"
                fi
            fi
        done < "$file"
        [ "$first" = false ] && echo ""
    fi
}

# Generate JSON
{
    echo "{"
    
    # Solid
    echo '  "solid": ['
    file_to_json_array "final-solid.txt"
    echo "  ],"
    
    # Regular
    echo '  "regular": ['
    file_to_json_array "final-regular.txt"
    echo "  ],"
    
    # Brands
    echo '  "brands": ['
    file_to_json_array "final-brands.txt"
    echo "  ]"
    
    echo "}"
} > "$CONFIG_JSON"

# Clean up temporary files
rm -f final-solid.txt final-regular.txt final-brands.txt

echo -e "${GREEN}✓ Configuration saved to $CONFIG_JSON${NC}"

# Step 5: Summary
echo ""
echo -e "${BLUE}=== Summary ===${NC}"

# Re-count from JSON for accuracy
FINAL_SOLID=$(grep -o '"' "$CONFIG_JSON" | wc -l)
FINAL_SOLID=$(( (FINAL_SOLID / 2 - 6) / 3 ))  # Approximate count

echo -e "Icons after all filtering:"
echo -e "  Detected: $((SOLID_COUNT + REGULAR_COUNT + BRANDS_COUNT)) total"
echo -e "  Blacklist filtered: ${#blacklist_map[@]} patterns"
echo -e "  Final configuration: Check $CONFIG_JSON"
echo ""
echo -e "${GREEN}Ready to generate fonts!${NC}"
echo "Run: node generate-geneweb-subset.js"
# Step 6: Generate subset-icons.txt for dzuhang/subset-iconfont
echo ""
echo -e "${BLUE}Generating subset-icons.txt for subset-iconfont...${NC}"

# Function to extract icons from JSON and format for subset-iconfont
generate_subset_icons_list() {
    local config_file="$1"
    local output_file="$2"
    
    if [ ! -f "$config_file" ]; then
        echo -e "${RED}❌ Error: Configuration file $config_file not found${NC}"
        return 1
    fi
    
    # Extract all icons from JSON, remove duplicates, and format for subset-iconfont
    python3 << 'PYTHON_EOF' > "$output_file"
import json
import sys

try:
    # Read the JSON configuration
    with open('fontawesome-subset-config.json', 'r') as f:
        config = json.load(f)
    
    # Collect all icons from all styles
    all_icons = set()
    
    # Add icons from each style if they exist
    for style in ['solid', 'regular', 'brands']:
        if style in config and isinstance(config[style], list):
            all_icons.update(config[style])
    
    # Sort icons alphabetically for consistency
    sorted_icons = sorted(all_icons)
    
    # Format as single-quoted, comma-separated list on one line
    formatted_icons = ', '.join(f"'{icon}'" for icon in sorted_icons)
    
    # Output the formatted list
    print(formatted_icons)
    
except Exception as e:
    print(f"Error processing JSON: {e}", file=sys.stderr)
    sys.exit(1)
PYTHON_EOF
    
    # Check if Python processing was successful
    if [ $? -eq 0 ] && [ -s "$output_file" ]; then
        local icon_count=$(grep -o "'" "$output_file" | wc -l)
        icon_count=$((icon_count / 2))  # Divide by 2 since each icon has 2 quotes
        echo -e "  ✓ Generated subset-icons.txt with $icon_count unique icons"
        echo -e "  📄 File: subset-icons.txt"
    else
        echo -e "${RED}❌ Error generating subset-icons.txt${NC}"
        
        # Fallback: try with basic shell commands if Python fails
        echo -e "${YELLOW}Trying fallback method...${NC}"
        {
            # Extract icons using grep and sed
            grep -o '"[^"]*"' "$config_file" | \
            grep -v -E '"(solid|regular|brands)"' | \
            sed 's/"//g' | \
            sort -u | \
            sed "s/.*/'&'/" | \
            paste -sd ',' - | \
            sed 's/,/, /g'
        } > "$output_file"
        
        if [ -s "$output_file" ]; then
            echo -e "  ✓ Generated subset-icons.txt using fallback method"
        else
            echo -e "${RED}❌ Failed to generate subset-icons.txt${NC}"
            return 1
        fi
    fi
    
    return 0
}

# Generate the subset-icons.txt file
SUBSET_ICONS_FILE="subset-icons.txt"
if generate_subset_icons_list "$CONFIG_JSON" "$SUBSET_ICONS_FILE"; then
    echo -e "${GREEN}✓ subset-icons.txt ready for use with subset.mjs${NC}"
    
    # Show preview of generated content
    echo ""
    echo -e "${BLUE}Preview of generated content:${NC}"
    echo -e "${YELLOW}$(head -c 100 "$SUBSET_ICONS_FILE")...${NC}"
else
    echo -e "${RED}❌ Failed to generate subset-icons.txt${NC}"
fi

# Step 7: Final Summary with usage instructions
echo ""
echo -e "${BLUE}=== Enhanced Summary ===${NC}"
echo -e "Files generated:"
echo -e "  📋 $CONFIG_JSON - Configuration for omacranger/fontawesome-subset tool"
echo -e "  📄 $SUBSET_ICONS_FILE - Icon list for dzuhang/subset-iconfont tool"
echo ""
echo -e "${GREEN}Usage instructions:${NC}"
echo -e "  For omacranger/fontawesome-subset: node generate-geneweb-subset.js"
echo -e "  For dzuhang/subset-iconfont: node subset.mjs"
echo ""
echo -e "${BLUE}Note:${NC} subset.mjs can now be configured to automatically read $SUBSET_ICONS_FILE"
