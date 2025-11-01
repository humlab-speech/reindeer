#!/bin/bash

#==============================================================================
# validate-cmdi.sh
# 
# Validates CMDI (Component Metadata Infrastructure) XML files
# 
# Usage: ./validate-cmdi.sh <cmdi_file.xml>
#==============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored messages
print_error() {
    echo -e "${RED}ERROR: $1${NC}" >&2
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}INFO: $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}WARNING: $1${NC}"
}

# Check if file argument provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <cmdi_file.xml>"
    echo ""
    echo "Validates a CMDI XML file for:"
    echo "  - XML well-formedness"
    echo "  - CMDI namespace compliance"
    echo "  - Required elements"
    echo "  - Placeholder detection"
    exit 1
fi

CMDI_FILE="$1"

# Check if file exists
if [ ! -f "$CMDI_FILE" ]; then
    print_error "File not found: $CMDI_FILE"
    exit 1
fi

echo "=============================================="
echo "  CMDI VALIDATION REPORT"
echo "=============================================="
echo ""
echo "File: $CMDI_FILE"
echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""

ERRORS=0
WARNINGS=0

# Test 1: XML Well-formedness
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 1: XML Well-formedness"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if xmllint --noout "$CMDI_FILE" 2>/dev/null; then
    print_success "XML is well-formed"
else
    print_error "XML is NOT well-formed"
    xmllint --noout "$CMDI_FILE" 2>&1
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Test 2: CMDI Namespace Check
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 2: CMDI Namespace"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if grep -q 'xmlns:cmd="http://www.clarin.eu/cmd/1"' "$CMDI_FILE"; then
    print_success "CMDI namespace found"
else
    print_error "CMDI namespace not found"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Test 3: Required Elements
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 3: Required Elements"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

REQUIRED_ELEMENTS=(
    "cmd:Header"
    "cmd:MdCreator"
    "cmd:MdCreationDate"
    "cmd:MdProfile"
    "cmd:Resources"
    "cmd:ResourceProxyList"
    "cmd:Components"
)

for element in "${REQUIRED_ELEMENTS[@]}"; do
    if grep -q "<$element" "$CMDI_FILE"; then
        print_success "Found: $element"
    else
        print_error "Missing: $element"
        ERRORS=$((ERRORS + 1))
    fi
done
echo ""

# Test 4: Profile ID Check
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 4: Profile ID"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
PROFILE=$(grep -o '<cmd:MdProfile>[^<]*</cmd:MdProfile>' "$CMDI_FILE" | sed 's/<[^>]*>//g')
if [ -n "$PROFILE" ]; then
    print_success "Profile ID: $PROFILE"
    
    # Check if it's a known profile
    case "$PROFILE" in
        "clarin.eu:cr1:p_1387365569699")
            print_info "  → media-corpus profile"
            ;;
        "clarin.eu:cr1:p_1392642184799")
            print_info "  → SpeechCorpusWithParticipants profile"
            ;;
        "clarin.eu:cr1:p_1381926654456")
            print_info "  → SpeechCorpus-DLU profile"
            ;;
        *)
            print_warning "  → Unknown profile (may be custom)"
            ;;
    esac
else
    print_error "No Profile ID found"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Test 5: Placeholder Detection
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 5: Placeholder Detection"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
PLACEHOLDERS=$(grep -o 'PLACEHOLDER_[A-Z_]*' "$CMDI_FILE" | sort -u)
if [ -z "$PLACEHOLDERS" ]; then
    print_success "No placeholders found - all fields filled"
else
    print_warning "Found placeholders that should be replaced:"
    echo "$PLACEHOLDERS" | while read placeholder; do
        COUNT=$(grep -c "$placeholder" "$CMDI_FILE")
        echo "  • $placeholder ($COUNT occurrence(s))"
    done
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# Test 6: Metadata Completeness
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 6: Metadata Completeness"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check for corpus description
if grep -q '<cmd:Description>' "$CMDI_FILE"; then
    DESC=$(grep -o '<cmd:Description>[^<]*</cmd:Description>' "$CMDI_FILE" | sed 's/<[^>]*>//g')
    if [ ${#DESC} -gt 50 ]; then
        print_success "Corpus description present (${#DESC} characters)"
    else
        print_warning "Corpus description is very short (${#DESC} characters)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    print_error "No corpus description found"
    ERRORS=$((ERRORS + 1))
fi

# Check for contact information
if grep -q '<cmd:Email>' "$CMDI_FILE"; then
    EMAIL=$(grep -o '<cmd:Email>[^<]*</cmd:Email>' "$CMDI_FILE" | sed 's/<[^>]*>//g' | head -1)
    if [[ "$EMAIL" == *"@"* ]]; then
        print_success "Contact email present: $EMAIL"
    else
        print_warning "Contact email may be invalid: $EMAIL"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    print_warning "No contact email found"
    WARNINGS=$((WARNINGS + 1))
fi

# Check for license
if grep -q '<cmd:License>' "$CMDI_FILE"; then
    LICENSE=$(grep -o '<cmd:License>[^<]*</cmd:License>' "$CMDI_FILE" | sed 's/<[^>]*>//g')
    print_success "License specified: $LICENSE"
else
    print_warning "No license information found"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""

# Test 7: Resource References
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 7: Resource References"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
RESOURCE_COUNT=$(grep -c '<cmd:ResourceProxy' "$CMDI_FILE" || echo "0")
if [ "$RESOURCE_COUNT" -gt 0 ]; then
    print_success "Found $RESOURCE_COUNT resource reference(s)"
else
    print_warning "No resource references found"
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# Summary
echo "=============================================="
echo "  VALIDATION SUMMARY"
echo "=============================================="
echo ""
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    print_success "All tests passed!"
    echo ""
    echo "This CMDI file is valid and complete."
    exit 0
elif [ $ERRORS -eq 0 ]; then
    print_warning "Validation completed with $WARNINGS warning(s)"
    exit 0
else
    print_error "Validation failed with $ERRORS error(s) and $WARNINGS warning(s)"
    exit 1
fi
