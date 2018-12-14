#!/usr/bin/perl -w

use strict;

# my $goal = "793061";
my $goal = shift @ARGV;

sub report {
    print STDERR @_, "\n";
}

sub verbose {
    # print STDERR @_, "\n";
}

my $init = "37";
my $scoreboard = $init;
my @e = (0, 1);

while (length $scoreboard < ($goal + 10)) {

    my $newscore = substr($scoreboard, $e[0], 1) + substr($scoreboard, $e[1], 1);
    verbose("newscore $newscore, length ", length($newscore));

    for (my $i = 0; $i < length($newscore); $i++) {
        $scoreboard .= substr($newscore, $i, 1);
        verbose "scoreboard now $scoreboard"
    }

    for (my $i = 0; $i < scalar(@e); $i++) {
        verbose "elf $i at $e[$i]";
        my $cscore = substr($scoreboard, $e[$i], 1);
        $e[$i] = ($e[$i] + $cscore + 1) % length($scoreboard);
        verbose "now elf $i at $e[$i]";
    }
}

report "next ten: " . substr($scoreboard, $goal, 10);
