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

my %all_pot_allergens = map { %$_ } values(%potential_allergen);
say sum0( map { $ingredient_counts{$_} } grep { ! $all_pot_allergens{$_} } keys %ingredient_counts );
