#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

chomp(my $input = <STDIN>);

my @initials = split(/,/, $input);

my $turn = 0;
my %last_spoken;

sub speak($n) {
    $turn += 1;

    if ($turn == 2020) {
        say $n;
        exit(0);
    }

    my $next = $last_spoken{$n} ? $turn - $last_spoken{$n} : 0;
    $last_spoken{$n} = $turn;

    return $next;
}

my $next;
$next = speak($_) for @initials;

while (1) {
    $next = speak($next);
}
