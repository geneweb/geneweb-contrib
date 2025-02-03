#!/bin/bash
set -e

if [[ "$1" == "-s" ]]; then
    export no_source=1
    shift
else
    unset no_source
fi

[[ -z "$1" ]] && { echo "Usage: $0 [-s don't print source] search_term"; exit 1; }

latest_csv=$(ls -t deces_insee*.csv | head -1)
[[ -z "$latest_csv" ]] && { echo "No deces_insee CSV file found"; exit 1; }

result=$(grep -i "$1" "$latest_csv" | \
perl -CS -Mutf8 -pe '
s@^([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),(?<TEN>[^,]*),(?<ELEVEN>[^\n]*)@\3 \1 \2 °\4 \5 $+{TEN} [\6] \e[1;33m†\7 \8 $+{ELEVEN} \e[0m(\9)§\2,\9@g;
s@([0-9]{4})-([0-9]{2})-([0-9]{2})@\3-\2-\1@g;
# Background and text color for sex (blue = male/red = female)
s@^1@\e[44m\e[8m1\e[0m@g;
s@^2@\e[41m\e[8m2\e[0m@g;
#s@\s*Arrondissement@@g;
#s@\s*ARRONDISSEMENT@@g;
s@(?<= )\b(\w)(\w+)\b@\U\1\E\2@g;
s@ \(\)@@g;
# Normalized birth place comparison
s@(\d{5}) ([^[]+) \[([^\]]+)\]@ do {
    use Unicode::Normalize;
    my $code = $1;
    my $hist = $2;
    my $orig = $3;
    #warn sprintf("DEBUG: before norm hist=<%s> orig=<%s>\n", $hist, $orig);
    
    # Separate department from historical name if present
    my $dept = "";
    my $hist_base = $hist;
    if ($hist =~ /^(.*?)\s*( \([^)]+\))$/) {
        $hist_base = $1;
        $dept = $2;
    }
    
    my $hist_norm = lc(NFKD($hist_base));
    my $orig_norm = lc(NFKD($orig));
    $hist_norm =~ s/\p{Mn}//g;
    $orig_norm =~ s/\p{Mn}//g;
    $hist_norm =~ s/[-\s]+//g;
    $orig_norm =~ s/[-\s]+//g;
    #warn sprintf("DEBUG: after norm hist=<%s> orig=<%s>\n", $hist_norm, $orig_norm);
    
    "$code " . ($hist_norm eq $orig_norm ? "$hist_base$dept" : "$hist_base$dept [$orig]");
}@eg;' | \
sort -k2 | \
perl -CS -Mutf8 -pe '
s@§([^,]*),([^\n]*)@
    my $name = $1;
    my $acte = $2;
    if (exists $ENV{"no_source"}) {
        "";
    } else {
        my $cap_name = join(" ", map { ucfirst(lc($_)) } split(/\s+/, $name));
        my $suffix = $acte =~ /^0*$/ ? ", numéro d’acte manquant" : ", acte n<sup>o</sup> $acte";
        "\n\e[8mInsee ($cap_name$suffix)";
    }@eg;'
)

[[ -z "$result" ]] && { echo "No matches found."; exit 0; }

echo "$result" | tee /dev/clipboard