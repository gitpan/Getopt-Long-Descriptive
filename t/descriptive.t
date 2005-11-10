#!perl

use strict;
use warnings;

use Test::More 'no_plan';

use_ok("Getopt::Long::Descriptive");

# test constraints:
# (look at P::V for names, too)
# required => 1
# depends => [...]
# precludes => [...]
# sugar for only_one_of and all_or_none

# I thougt I had tests here, but I can't find them.
