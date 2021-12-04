#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

sub validate($line) {
    my ($min, $max, $char, $password) = $line =~ m/^(\d+)-(\d+) (.): (.*)$/;

    my @matches = $password =~ m/\Q$char\E/g;
    return @matches >= $min && @matches <= $max;
}

say scalar grep { validate($_) } @input;
