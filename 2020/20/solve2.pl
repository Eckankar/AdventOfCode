#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use Carp qw(confess);
use List::Util qw(uniq);

chomp(my $input = do { local $/; <STDIN> });

chomp(my @input_tiles = split(/\n\s*?\n/, $input));

my %side2id;

my @pieces;

sub rotate($contents) {
    my @result;
    for my $i (0 .. $#$contents) {
        push(@result, [ reverse (map { $_->[$i] } @$contents) ]);
    }

    return \@result;
}

sub flip($contents) {
    return [ reverse @$contents ];
}

sub all_transformations($contents) {
    my @rotations = ($contents);
    for (1..3) {
        $contents = rotate($contents);
        push(@rotations, $contents);
    }

    my @results = (@rotations, map { flip($_) } @rotations);
    return @results;
}

sub align_to_constraint($piece, $constraint) {
    my ($aligned) = grep { $constraint->() } all_transformations($piece->{contents});
    confess("Could not align to constraint.") unless $aligned;
    $piece->{contents} = $aligned;
}

sub edge($side, $contents) {
    my $res;

    if ($side eq 'top')    { $res = $contents->[0]; }
    if ($side eq 'bottom') { $res = $contents->[$#$contents]; }
    if ($side eq 'left')   { $res = [ map { $_->[0] } @$contents ]; }
    if ($side eq 'right')  { $res = [ map { $_->[$#$_] } @$contents ]; }

    return join('', @$res);
}

for my $input_tile (@input_tiles) {
    my ($id, $contents) = $input_tile =~ m/^Tile (\d+):\s*(.*)$/s;

    my @contents = map { [ split(//, $_) ] } split(/\n/, $contents);
    my @sides = map { $_, scalar reverse $_ } ( map { edge($_, \@contents) } (qw(top bottom left right)) );

    for my $side (@sides) {
        $side2id{$side} //= [];
        push($side2id{$side}->@*, $id);
    }

    push(@pieces, { id => $id, contents => \@contents });
}


my %unmatched_count;
$unmatched_count{$_}++ for map { @$_ } grep { scalar uniq(@$_) == 1 } values(%side2id);

my @corner_ids = grep { $unmatched_count{$_} == 4 } keys %unmatched_count;
my %is_corner = map { $_ => 1 } @corner_ids;

sub find_constrained($constraint) {
    my ($piece) = grep { $constraint->() } @pieces;
    @pieces = grep { $_->{id} != $piece->{id} } @pieces;
    return $piece;
}

my @assembled_pieces;

# Find the top left corner
my $corner = find_constrained( sub { $is_corner{ $_->{id} } } );
align_to_constraint($corner, sub {
    $side2id{ edge('left', $_) }->@* == 1 &&
    $side2id{ edge('top', $_) }->@* == 1
});

push(@assembled_pieces, [$corner]);

while (@pieces) {
    my $current_row = $assembled_pieces[-1];
    if (@$current_row) {
        # The current row is not empty
        my $last_piece = $current_row->[-1];

        my $side = edge('right', $last_piece->{contents});
        my ($id) = grep { $_ != $last_piece->{id} } $side2id{$side}->@*;
        my $piece = find_constrained(sub { $_->{id} == $id });

        align_to_constraint($piece, sub { edge('left', $_) eq $side });

        push(@$current_row, $piece);

        push(@assembled_pieces, []) if $side2id{ edge('right', $piece->{contents}) }->@* == 1;
    } else {
        # Current row is empty - match with piece above instead
        my $above_piece = $assembled_pieces[-2]->[0];

        my $side = edge('bottom', $above_piece->{contents});
        my ($id) = grep { $_ != $above_piece->{id} } $side2id{$side}->@*;
        my $piece = find_constrained(sub { $_->{id} == $id });

        align_to_constraint($piece, sub { edge('top', $_) eq $side });

        push(@$current_row, $piece);
    }
}

pop(@assembled_pieces);

#for my $row (@assembled_pieces) {
#    for my $i (0 .. $row->[0]->{contents}->@* - 1) {
#        say join(' ', map { join('', $_->{contents}->[$i]->@*) } @$row);
#    }
#    say "";
#}

# Trim the edges off of the pieces
for my $row (@assembled_pieces) {
    for my $piece (@$row) {
        my @cs = $piece->{contents}->@*;
        $piece->{contents} = [ map { [ @$_[1 .. $#$_-1] ] } @cs[1 .. $#cs-1] ];
    }
}

#for my $row (@assembled_pieces) {
#    for my $i (0 .. $row->[0]->{contents}->@* - 1) {
#        say join('', map { join('', $_->{contents}->[$i]->@*) } @$row);
#    }
#}

my @map_lines;
for my $row (@assembled_pieces) {
    for my $i (0 .. $row->[0]->{contents}->@* - 1) {
        push(@map_lines, [ map { $_->{contents}->[$i]->@* } @$row ]);
    }
}

# Monster:
#   .    .    .    .
                  #
#    ##    ##    ###
 #  #  #  #  #  #

# width: 20

my $i=0;
for my $trans (all_transformations(\@map_lines)) {
    my $map = join("\n", map { join('', @$_) } @$trans);

    my $W = scalar $trans->[0]->@*;

    my $offset = $W + 1 - 20;

    my $fix_map = sub {
        return $map =~ s/
            ([^\n]{18}) \# ([^\n] .{$offset})

            \# ([^\n]{4}) \#\# ([^\n]{4}) \#\# ([^\n]{4}) \#\#\# (.{$offset}

            [^\n]) \# ([^\n]{2}) \# ([^\n]{2}) \# ([^\n]{2}) \# ([^\n]{2}) \# ([^\n]{2}) \# ([^\n]{3})
        / join("",$1,"O",$2,"O",$3,"OO",$4,"OO",$5,"OOO",$6,"O",$7,"O",$8,"O",$9,"O",$10,"O",$11,"O",$12) /gxse;
    };
    next unless scalar $fix_map->();

    while (scalar $fix_map->()) {}

    say $map;

    my @sea = $map =~ m/\#/g;
    say scalar @sea;
}
