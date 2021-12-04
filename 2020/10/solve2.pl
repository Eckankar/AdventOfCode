#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my ($first, @rest) = (sort { $b <=> $a } @input, 0);

my %counts = ( $first => 1 );

for my $v (@rest) {
    $counts{$v} = ($counts{$v+1} // 0) + ($counts{$v+2} // 0) + ($counts{$v+3} // 0);
}

say $counts{0};
