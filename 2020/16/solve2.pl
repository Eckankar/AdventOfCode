#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

use List::Util qw(sum0);

chomp(my $input = do { local $/; <STDIN> });

my ($notes, $my_ticket, $nearby_tickets) = $input =~ m/^(.*)\s*your ticket:\s*(\S*)\s*nearby tickets:\s*(.*)$/s;

my @notes = split(/\n/, $notes);
my @my_ticket = split(/,/, $my_ticket);
my @nearby_tickets = split(/\n/, $nearby_tickets);

my %valid;
my %fields;
for my $line (@notes) {
    my ($field, $start1, $end1, $start2, $end2) = $line =~ m/^([^:]*): (\d+)-(\d+) or (\d+)-(\d+)$/;
    my %field = map { $_ => 1 } $start1 .. $end1, $start2 .. $end2;
    %valid = (%valid, %field);

    $fields{$field} = \%field;
}

# filter out invalid tickets
@nearby_tickets = grep { ! grep { ! $valid{$_} } split(/,/, $_) } @nearby_tickets;

my @nearby_fields = map { [ split(/,/, $_) ] } @nearby_tickets;

my @possible_labels;
my %seen;
for my $i (0 .. scalar %fields - 1) {
    my @field_values = map { $_->[$i] } @nearby_fields;

    my @possible_fields;
    for my $field (keys %fields) {
        unless (grep { ! $fields{$field}->{$_} } @field_values) {
            push(@possible_fields, $field);
            $seen{$field}++;
        }
    }
    push(@possible_labels, \@possible_fields);
}

my %labels;
my @unresolved_fields = keys %fields;
while (@unresolved_fields) {
    my ($trivial_field) = grep { $seen{$_} == 1 } @unresolved_fields;

    use Data::Dumper; die("No trivial field exists. " . Dumper(\%seen)) unless $trivial_field;

    my ($id) = grep { $trivial_field ~~ $possible_labels[$_] && !defined($labels{$_})  } (0 .. $#possible_labels);

    $labels{$id} = $trivial_field;
    @unresolved_fields = grep { $_ ne $trivial_field } @unresolved_fields;
    $seen{$_}-- for $possible_labels[$id]->@*;
}

%labels = reverse %labels;

my @departure_values = map { $my_ticket[ $labels{$_} ] } grep { m/^departure\b/ } keys %labels;

my $result = 1;
$result *= $_ for @departure_values;

say $result;
