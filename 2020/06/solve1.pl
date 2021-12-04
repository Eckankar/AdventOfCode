#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my $input = do { local $/; <STDIN> });
my @declarations = split(/\n\s*?\n/, $input);

my $sum = 0;

for my $declaration (@declarations) {
    my %group = map { $_ => 1 } grep { /[a-z]/ } split(//, $declaration);
    $sum += scalar %group;
}

say $sum;
