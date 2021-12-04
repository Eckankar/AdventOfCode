#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::MoreUtils qw(before after);

chomp(my $input = do { local $/; <STDIN> });

my @cups = split(//, $input);

for my $i (1 .. 100) {
    my ($active, $p1, $p2, $p3, @rest) = @cups;

    my %rest = map { $_ => 1 } @rest;
    my ($destination) = grep { $rest{$_} } reverse ( ($active+1) .. 9 , 1 .. ($active-1) );

    @cups = (map { $_ == $destination ? ($_, $p1, $p2, $p3) : ($_) } @rest, $active);
}

my @output = (after { $_ == 1 } @cups, before { $_ == 1 } @cups);

say join("", @output);
