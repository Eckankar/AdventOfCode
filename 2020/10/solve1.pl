#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

@input = sort { $a <=> $b } @input;

my %counts = (3 => 1);
my $val = 0;

for my $v (@input) {
    $counts{$v - $val} += 1;
    $val = $v;
}

say $counts{1} * $counts{3};
