#!/usr/bin/env perl
use 5.024;
use warnings;

chomp(my @input = <STDIN>);
my %nums = map { (2020 - $_) => 1} @input;

for my $i (0 .. $#input) {
    for my $j ( ($i+1) .. $#input ) {
        my ($ni, $nj) = @input[$i, $j];
        if ($nums{$ni + $nj}) {
            say $ni * $nj * (2020-$ni-$nj);
            exit(0);
        }
    }
}
