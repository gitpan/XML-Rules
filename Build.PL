use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'XML::Rules',
    license             => 'perl',
    dist_author         => 'Jan Krynicky <Jenda@CPAN.org>',
    dist_version_from   => 'lib/XML/Rules.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'XML-Rules-*' ],
	dynamic_config  => 0,
);

$builder->create_build_script();
