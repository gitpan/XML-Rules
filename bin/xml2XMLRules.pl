use strict;
use XML::Rules;
use Data::Dumper;

print Dumper(XML::Rules::inferRulesFromExample( @ARGV));