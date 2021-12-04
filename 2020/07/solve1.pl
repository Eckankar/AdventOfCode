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
        @val = map { s/^\d+ (.*) bags?$/$1/r } split(', ', $contents);
    }

    $bags{$type} = \@val;
}

my $count = 0;
for my $bag (keys %bags) {
    my @queue = $bags{$bag}->@*;

    while (my $type = shift @queue) {
        if ($type eq 'shiny gold') {
            $count += 1;
            @queue = ();
        } else {
            push(@queue, @{ $bags{$type} // [] });
        }
    }
}

say $count;
