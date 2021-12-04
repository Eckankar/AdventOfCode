#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my $input = do { local $/; <STDIN> });
my @declarations = split(/\n\s*?\n/, $input);

my $sum = 0;

for my $declaration (@declarations) {
    my ($first, @rest) = map { [split(//, $_)] } split(/\n/, $declaration);
    my %group = map { $_ => 1 } @$first;
    for my $current (@rest) {
        %group = map { $_ => 1 } grep { $group{$_} } @$current;
    }
    $sum += scalar %group;
}

say $sum;
