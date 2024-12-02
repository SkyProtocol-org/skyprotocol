# Source this file into your shell environment to define important variables:
#   source $HOME/src/fare/skyprotocol/env.sh

if [ -n "${BASH_VERSION-}" ] ; then
    this=${BASH_SOURCE[0]}
elif [ -n "${ZSH_VERSION-}" ] ; then
    this=$0
elif [ -n "$this" ] ; then
    : Assuming the caller set the path to $this
else
    echo "Unknown shell and \$this not defined" ; unset this ; set -u ; return 1
fi

export SKYPROTOCOL_SRC="$(dirname $this)"
export SKYPROTOCOL_HOME GERBIL_APPLICATION_HOME
: ${SKYPROTOCOL_HOME:=${SKYPROTOCOL_SRC}}
GERBIL_APPLICATION_HOME=$SKYPROTOCOL_HOME
bindir=${SKYPROTOCOL_SRC}/.build_outputs
GERBIL_PACKAGE=gerbil-unstable

#srcdir="$(realpath "$SKYPROTOCOL_SRC/..")"
### export GERBIL_LOADPATH=$SKYPROTOCOL_SRC:$srcdir/gerbil-utils
# Don't change the GERBIL_LOADPATH, instead configure your gxpkg with:
#   gxpkg link github.com/mighty-gerbils/gerbil-utils $srcdir/gerbil-utils
#
# Then you can use this, which assumes the clan utilities were installed via gxpkg:
. "${GERBIL_PATH:-${HOME}/.gerbil}/pkg/github.com/mighty-gerbils/gerbil-utils/gerbil-nix-env.sh"

export GERBIL_ETHEREUM_SRC="${GERBIL_PATH:-${HOME}/.gerbil}/pkg/github.com/mighty-gerbils/gerbil-ethereum"
export GERBIL_LOADPATH="${SKYPROTOCOL_SRC}:${GERBIL_LOADPATH}:${GERBIL_ETHEREUM_SRC}"


# Manage the git submodule
subm_reset () {(cd $SKYPROTOCOL_SRC ; git submodule update --init )} # Reset to version pinned in git
subm_update () {(cd $SKYPROTOCOL_SRC ; git submodule update --remote )} # Update version from upstream
subm_remove () {(cd $SKYPROTOCOL_SRC ; git submodule deinit . )} # Remove contents altogether

#. $srcdir/gerbil-utils/gerbil-nix-env.sh

build_glo () {(
  cd "$SKYPROTOCOL_SRC" &&
  ./build.ss "$@"
)}

sky_dirs () { : ;}
sky_proxy () { sky_dirs ; exec "$bindir/sky" run-proxy-server > ~/data/proxy.log 2>&1 & }
_sky () { "$bindir/sky" "$@" ;}
sky () { build_sky build-only && _sky "$@" ;}
bsky () { build_sky && sky_unit_tests ;}

sky_unit_tests () {(
  cd "$srcdir" &&
  #gxi clan/utils/tests/run-unit-tests.ss &&
  gxi tests/run-unit-tests.ss
)}
sky_integration_tests () { (cd "$srcdir" && gxi tests/run-integration-tests.ss) }
sky_tests () { sky_unit_tests ; sky_integration_tests ; }
tf () { sky_tests ; }
wcssd () {( cd ${SKYPROTOCOL_SRC} ; cat $(find $@ -type f -name '*.ss') | wc -l )}
wcss () {
    a=$(wcssd) u=$(wcssd .) t=$(wcssd **/t/)
    echo "utils: $u"
    echo "tests: $t"
    echo "sky: $(($a-$u-$t))"
    echo "all: $a"
}
sky_gxi () { gxi ${SKYPROTOCOL_SRC}/all-sky.ss $GERBIL_HOME/lib/gxi-interactive - ;}
regeth () { $GERBIL_ETHEREUM_SRC/scripts/run-geth-test-net.ss ;}
buu () { ./build.ss && ./unit-tests.ss && ./unit-tests.ss integration ;}
