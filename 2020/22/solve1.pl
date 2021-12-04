#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::MoreUtils qw(zip_unflatten);
use List::Util qw(sum0);

chomp(my $input = do { local $/; <STDIN> });

my ($deck1, $deck2) = map { [ split(/\n/, $_) ] } split(/\n\s*?\n/, $input);
shift @$deck1;
shift @$deck2;

while (@$deck1 && @$deck2) {
    my ($c1, $c2) = (shift @$deck1, shift @$deck2);

    if ($c1 > $c2) {
        push(@$deck1, $c1, $c2);
    } elsif ($c2 > $c1) {
        push(@$deck2, $c2, $c1);
    }
}

my @deck = (@$deck1, @$deck2);
my @ids = reverse (1 .. @deck);
say sum0(map { $_->[0] * $_->[1] } zip_unflatten @deck, @ids);
