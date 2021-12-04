#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use List::MoreUtils qw(zip_unflatten);
use List::Util qw(sum0);

chomp(my $input = do { local $/; <STDIN> });

my ($deck1, $deck2) = map { [ split(/\n/, $_) ] } split(/\n\s*?\n/, $input);
shift @$deck1;
shift @$deck2;

sub combat($deck1, $deck2) {
    my %seen;
    while (@$deck1 && @$deck2) {
        my $ident = join(',', @$deck1).'-'.join(',', @$deck2);
        return 'p1' if $seen{$ident}++;

        my $winner;
        my ($c1, $c2) = (shift @$deck1, shift @$deck2);

        if ($c1 <= @$deck1 && $c2 <= @$deck2) {
            $winner = combat([$deck1->@[0..$c1-1]], [$deck2->@[0..$c2-1]]);
        } else {
            $winner = $c1 > $c2 ? 'p1' : 'p2';
        }

        if ($winner eq 'p1') {
            push(@$deck1, $c1, $c2);
        } elsif ($winner eq 'p2') {
            push(@$deck2, $c2, $c1);
        }
    }

    return @$deck1 ? 'p1' : 'p2';
}

my $winner = combat($deck1, $deck2);

my @deck = $winner eq 'p1' ? @$deck1 : @$deck2;
my @ids = reverse (1 .. @deck);
say sum0(map { $_->[0] * $_->[1] } zip_unflatten @deck, @ids);
