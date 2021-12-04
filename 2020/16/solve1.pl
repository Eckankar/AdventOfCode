#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

use List::Util qw(sum0);

chomp(my $input = do { local $/; <STDIN> });

my ($notes, $my_ticket, $nearby_tickets) = $input =~ m/^(.*)\s*your ticket:\s*(\S*)\s*nearby tickets:\s*(.*)$/s;

my @notes = split(/\n/, $notes);
my @nearby_tickets = split(/\n/, $nearby_tickets);

my %valid;
for my $line (@notes) {
    my ($start1, $end1, $start2, $end2) = $line =~ m/^[^:]*: (\d+)-(\d+) or (\d+)-(\d+)$/;
    $valid{$_} = 1 for $start1 .. $end1, $start2 .. $end2;
}

my @invalid_numbers = grep { ! $valid{$_} } map { split(/,/, $_) } @nearby_tickets;

say sum0(@invalid_numbers);
