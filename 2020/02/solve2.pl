#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

sub validate($line) {
    my ($min, $max, $char, $password) = $line =~ m/^(\d+)-(\d+) (.): (.*)$/;

    return ((substr($password, $min-1, 1) eq $char) xor (substr($password, $max-1, 1) eq $char));
}

say scalar grep { validate($_) } @input;
