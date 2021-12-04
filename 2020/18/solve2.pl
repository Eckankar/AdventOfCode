#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::Util qw(sum0);

chomp(my @input = <STDIN>);

my $sum = 0;

sub evaluate($expr) {
    while ($expr =~ s/\(([^()]*)\)/evaluate("$1")/ge) { }

    while ($expr =~ s/(\d+)\s*\+\s*(\d+)/$1+$2/e) { }
    while ($expr =~ s/(\d+)\s*\*\s*(\d+)/$1*$2/e) { }

    return $expr;
}

$sum += evaluate($_) for @input;

say $sum;
