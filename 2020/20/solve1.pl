#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::Util qw(uniq);

chomp(my $input = do { local $/; <STDIN> });

chomp(my @input_tiles = split(/\n\s*?\n/, $input));

my %side2id;

for my $input_tile (@input_tiles) {
    my ($id, $contents) = $input_tile =~ m/^Tile (\d+):\s*(.*)$/s;

    my @contents = map { [ split(//, $_) ] } split(/\n/, $contents);
    my @sides = map { join('', @$_), join('', reverse @$_) } ( $contents[0], $contents[$#contents], [ map { $_->[0] } @contents ], [ map { $_->[$#$_] } @contents ] );

    for my $side (@sides) {
        $side2id{$side} //= [];
        push($side2id{$side}->@*, $id);
    }
}

my %unmatched_count;
$unmatched_count{$_}++ for map { @$_ } grep { scalar uniq(@$_) == 1 } values(%side2id);

my @corner_ids = grep { $unmatched_count{$_} == 4 } keys %unmatched_count;

my $result = 1;
$result *= $_ for @corner_ids;

say $result;
