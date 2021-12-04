#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my %bags;

for my $line (@input) {
    my ($type, $contents) = $line =~ m/^(.*) bags contain (.*)\.$/;

    my @val;
    if ($contents ne 'no other bags') {
        for my $cline (split(', ', $contents)) {
            my ($cnt, $type) = $cline =~ m/^(\d+) (.*) bags?$/;
            push(@val, { type => $type, count => $cnt });
        }
    }

    $bags{$type} = \@val;
}

my %bagcount;

sub bagcount($type) {
    return $bagcount{$type} if defined($bagcount{$type});

    my $sum = 0;

    for my $c ($bags{$type}->@*) {
        $sum += $c->{count} * (bagcount($c->{type}) + 1);
    }
    return $sum;
}

say bagcount('shiny gold');
