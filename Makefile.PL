use strict;

use ExtUtils::MakeMaker;

WriteMakefile( NAME         => 'DateTime::Incomplete',
               VERSION_FROM => 'lib/DateTime/Incomplete.pm',
               AUTHOR       => 'Flavio S. Glock <fglock@pucrs.br>',
               ABSTRACT     => 'Incomplete DateTime objects',

               PREREQ_PM    => { 'DateTime' => 0,
                                 'Test::More'  => 0,
                               },
             );

