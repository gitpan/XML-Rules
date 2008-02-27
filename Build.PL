use strict;
use warnings;
use Module::Build;
use Module::Build::Compat;

my $builder = Module::Build->new(
    module_name         => 'XML::Rules',
    license             => 'perl',
    dist_author         => 'Jan Krynicky <Jenda@CPAN.org>',
    dist_version_from   => 'lib/XML/Rules.pm',
	requires => {
                  'perl'          => '5.008',
                  'XML::Parser'  => '2.00',
                  'XML::Parser::Expat' => '2.00',
#                  'Encode' => '2.0', # in core
                 },
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'XML-Rules-*' ],
	dynamic_config  => 0,
	create_makefile_pl => 'passthrough',
);

$builder->create_build_script();
