#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @publics = <STDIN>);

my %publics = map { $_ => 1 } @publics;

my $v = 1;
my $i = 0;
while (! $publics{$v}) {
    $v = ($v * 7) % 20201227;
    $i += 1;
}

my ($other_public) = grep { $_ != $v } @publics;
my $v2 = 1;
for (1 .. $i) {
    $v2 = ($v2 * $other_public) % 20201227;
}

say $v2;
