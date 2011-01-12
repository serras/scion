#!/bin/sh

cabal_flags="-fserver"
cpp_flags="-DWPINLINE"

# Figure out what version of GHC we've got to set cabal flags appropriately:

if ( ghc --version 2>&1 | grep '6.12' ); then
	cabal_flags="${cabal_flags} -fcabal_1_8"
	cpp_flags="${cpp_flags} -DCABAL_VERSION=108 -DHAVE_PACKAGE_DB_MODULES -DGHC_VERSION=612"
else
	cabal_flags="${cabal_flags} -fcabal_1_6"
	cpp_flags="${cpp_flags} -DCABAL_VERSION=106 -DHAVE_PACKAGE_DB_MODULES -DGHC_VERSION=610"
fi


# Have cabal build the package dependency list for us, by running configure in a
# temporary build directory:

build_dir=/tmp/scionsrv$$x

trap "rm -rf ${build_dir}" 0 1 2 3 15
[ -d ${build_dir} ] && rm -rf ${build_dir}
mkdir ${build_dir}

deppkgs=`cabal -v ${cabal_flags} -builddir=${build_dir} configure 2>&1 | \
	awk '/^Dependency / { pkgdep=sub(/^.*: using /, "-package ", $0); print $0; }'`

rm -rf ${build_dir}

# Extensions: These were extracted from scion.cabal, since there's no programmatic
# way to get them:
extensions="-XCPP -XPatternGuards -XDeriveDataTypeable -XOverlappingInstances -XIncoherentInstances \
	-XFlexibleInstances -XUndecidableInstances -XTypeSynonymInstances"

# And kick off our interactive session:
ghc --interactive ${deppkgs} ${cpp_flags} ${extensions} -iserver:lib -cpp Main
