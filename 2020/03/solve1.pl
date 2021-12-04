#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);
@input = map { [split(//, $_)] } @input;

my ($incX, $incY) = (3, 1);

my $width = $input[0]->@*;

my $pos = [0, 0];

my $count = 0;
while ($pos->[1] <= $#input) {
    $count += 1 if $input[$pos->[1]]->[$pos->[0]] eq '#';
    $pos->[0] = ($pos->[0] + $incX) % $width;
    $pos->[1] += $incY;
}

say $count;
