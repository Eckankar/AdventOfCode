#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use Storable qw(dclone);

chomp(my @input = <STDIN>);
@input = map { [split(//, $_)] } @input;

my ($height, $width) = (scalar @input, scalar $input[0]->@*);

sub add($p1, $p2) {
    return [$p1->[0] + $p2->[0], $p1->[1] + $p2->[1]];
}

sub valid($p) {
    return ($p->[0] >= 0) && ($p->[0] < $height) && ($p->[1] >= 0) && ($p->[1] < $width);
}

sub neighbors($p, $data) {
    my @offsets = ([1,1],[1,0],[1,-1],[0,1],[0,-1],[-1,1],[-1,0],[-1,-1]);
    my @result;

    for my $offset (@offsets) {
        my $candidate = add($p, $offset);

        while (valid($candidate) && $data->[$candidate->[0]]->[$candidate->[1]] eq '.') {
            $candidate = add($candidate, $offset);
        }

        push(@result, $data->[$candidate->[0]]->[$candidate->[1]]) if valid($candidate);

    }

    return @result;
}

sub next_gen($data) {
    my $next_gen = dclone($data);

    for my $y (0 .. $height-1) {
        for my $x (0 .. $width-1) {
            my @values = neighbors([$y, $x], $data);
            my $current = $data->[$y]->[$x];

            if ($current eq 'L' && ! grep { $_ eq '#' } @values) {
                $next_gen->[$y]->[$x] = '#';
            } elsif ($current eq '#' && (grep { $_ eq '#' } @values) >= 5) {
                $next_gen->[$y]->[$x] = 'L';
            }
        }
    }

    return $next_gen;
}

sub render($data) {
    return join("\n", map { join("", @$_) } @$data);
}

my $old_data = [];
my $new_data = \@input;

while (render($new_data) ne render($old_data)) {
    $old_data = $new_data;
    $new_data = next_gen($old_data);
}

say scalar grep { $_ eq '#' } map { @$_ } @$new_data;
