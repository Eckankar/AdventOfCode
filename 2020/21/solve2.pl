#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::Util qw(sum0);

chomp(my @input = <STDIN>);

my %ingredient_counts;
my %potential_allergen;

for my $line (@input) {
    next unless my ($ingredients, $allergens) = $line =~ m/^(.*) \(contains (.*)\)$/;

    my @ingredients = split(/ /, $ingredients);
    $ingredient_counts{$_} += 1 for @ingredients;

    my @allergens = split(/, /, $allergens);

    for my $allergen (@allergens) {
        $potential_allergen{$allergen} //= { map { $_ => 1 } @ingredients };
        $potential_allergen{$allergen} = { map { $_ => 1 } grep { $potential_allergen{$allergen}->{$_} } @ingredients };
    }
}

my %allergen;
while (my ($allergen) = grep { scalar $potential_allergen{$_}->%* == 1 } keys(%potential_allergen)) {
    my ($ingredient) = keys $potential_allergen{$allergen}->%*;
    $allergen{$allergen} = $ingredient;
    delete $potential_allergen{$allergen};

    delete $potential_allergen{$_}->{$ingredient} for keys(%potential_allergen);
}

say join(",", map { $allergen{$_} } sort keys %allergen);
