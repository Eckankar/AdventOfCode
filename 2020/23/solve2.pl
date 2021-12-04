#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::MoreUtils qw(before after);

chomp(my $input = do { local $/; <STDIN> });

my @cups = (split(//, $input), 10 .. 1_000_000);
my %cups = map { $cups[$_] => $cups[ ($_ + 1) % @cups ] } (0 .. $#cups);

local $| = 1;

sub previous($n) {
    my $res = $n-1;
    $res = scalar @cups unless $res;
    return $res;
}

my ($active) = @cups;

for my $i (1 .. 10_000_000) {
    print "\rIteration $i...";

    # Pull out the 3 elements following the active element
    my $p1 = $cups{$active};
    my $p2 = $cups{$p1};
    my $p3 = $cups{$p2};

    $cups{$active} = $cups{$p3};

    my %taken = map { $_ => 1 } ($active, $p1, $p2, $p3);

    my $destination = $active;
    $destination = previous($destination) while $taken{$destination};

    # Patch in p1, p2, p3 after the destination
    $cups{$p3} = $cups{$destination};
    $cups{$destination} = $p1;

    $active = $cups{$active};
}
say "";

my $first = $cups{1};
my $second = $cups{$first};

say $first*$second;
