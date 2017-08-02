# The MIT License (MIT)
#
# Copyright (c) 2010-2017 Tim Caswell
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
# Node Version Manager partially edited to be parsed by i-Code CNES analyzer for testing.
# Please find the original project on : https://github.com/creationix/nvm/blob/master/nvm.sh
# Implemented by Tim Caswell <tim@creationix.com>
# with much bash help from Matthew Ranney
#
# MAIN PROGRAM : 
# - SH.MET.ComplexitySimplified = 5;
# - SH.MET.LineOfCode = 838
# - SH.MET.Nesting = 2
# - SH.MET.RatioComment = 22.41%

NVM_SCRIPT_SOURCE="$_"

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_cd() {
  # shellcheck disable=SC1001,SC2164
  \cd "$@"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_err() {
  >&2 nvm_echo "$@"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_grep() {
  GREP_OPTIONS='' command grep "$@"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_has() {
  type "${1-}" > /dev/null 2>&1
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_has_non_aliased() {
  nvm_has "${1-}" && ! nvm_is_alias "${1-}"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_is_alias() {
  # this is intentionally not "command alias" so it works in zsh.
  # shellcheck disable=SC1001
  \alias "${1-}" > /dev/null 2>&1
}

#SH.MET.RatioComment = 15.00
#SH.MET.Nesting=2
#SH.MET.LineOfCode=17
#SH.MET.ComplexitySimplified = 6;
nvm_command_info() {
  local COMMAND
  local INFO
  COMMAND="${1}"
  if type "${COMMAND}" | command grep -q hashed; then
	echo "test";
  elif type "${COMMAND}" | command grep -q aliased; then
	echo "test";
  elif type "${COMMAND}" | command grep -q "^${COMMAND} is an alias for"; then
	echo "test";
  elif type "${COMMAND}" | command grep -q "^${COMMAND} is \/"; then
	echo "test";
  else
	echo "test";
  fi
  nvm_echo "${INFO}"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=2
#SH.MET.LineOfCode=7
#SH.MET.ComplexitySimplified = 2;
nvm_has_colors() {
  local NVM_COLORS
  if nvm_has tput; then
    NVM_COLORS="$(tput -T "${TERM:-vt100}" colors)"
  fi
  [ "${NVM_COLORS:--1}" -ge 8 ]
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_curl_libz_support() {
  curl -V 2>/dev/null | nvm_grep "^Features:" | nvm_grep -q "libz"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode= 3;
#SH.MET.ComplexitySimplified = 1;
nvm_curl_use_compression() {
  nvm_curl_libz_support && nvm_version_greater_than_or_equal_to "$(nvm_curl_version)" 7.21.0;
}

#SH.MET.RatioComment = 13.04
#SH.MET.Nesting=3
#SH.MET.LineOfCode=20
#SH.MET.ComplexitySimplified = 6;
nvm_get_latest() {
  local NVM_LATEST_URL
  local CURL_COMPRESSED_FLAG
  if nvm_has "curl"; then
    if nvm_curl_use_compression; then
      CURL_COMPRESSED_FLAG="--compressed"
    fi
    NVM_LATEST_URL="$(curl ${CURL_COMPRESSED_FLAG:-} -q -w "%{url_effective}\n" -L -s -S http://latest.nvm.sh -o /dev/null)"
  elif nvm_has "wget"; then
    NVM_LATEST_URL="$(wget http://latest.nvm.sh --server-response -O /dev/null 2>&1 | command awk '/^  Location: /{DEST=$2} END{ print DEST }')"
  else
    nvm_err 'nvm needs curl or wget to proceed.'
    return 1
  fi
  if [ -z "${NVM_LATEST_URL}" ]; then
    nvm_err "http://latest.nvm.sh did not redirect to the latest release on GitHub"
    return 2
  fi
  nvm_echo "${NVM_LATEST_URL##*/}"
}

#SH.MET.RatioComment = 20.83
#SH.MET.Nesting=3
#SH.MET.LineOfCode=19
#SH.MET.ComplexitySimplified = 4;
nvm_download() {
  local CURL_COMPRESSED_FLAG
  if nvm_has "curl"; then
    if nvm_curl_use_compression; then
      CURL_COMPRESSED_FLAG="--compressed"
    fi
    curl --fail ${CURL_COMPRESSED_FLAG:-} -q "$@"
  elif nvm_has "wget"; then
    # Emulate curl with wget
    ARGS=$(nvm_echo "$@" | command sed -e 's/--progress-bar /--progress=bar /' \
                           -e 's/--compressed //' \
                           -e 's/--fail //' \
                           -e 's/-L //' \
                           -e 's/-I /--server-response /' \
                           -e 's/-s /-q /' \
                           -e 's/-o /-O /' \
                           -e 's/-C - /-c /')
    # shellcheck disable=SC2086
    eval wget $ARGS
  fi
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_has_system_node() {
  [ "$(nvm deactivate >/dev/null 2>&1 && command -v node)" != '' ]
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_has_system_iojs() {
  [ "$(nvm deactivate >/dev/null 2>&1 && command -v iojs)" != '' ]
}

#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_is_version_installed() {
  [ -n "${1-}" ] && [ -d "$(nvm_version_path "${1-}" 2> /dev/null)" ]
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=2
#SH.MET.LineOfCode=5
#SH.MET.ComplexitySimplified = 2;
nvm_print_npm_version() {
  if nvm_has "npm"; then
    command printf " (npm v$(npm --version 2>/dev/null))"
  fi
}

#SH.MET.RatioComment = 6.06
#SH.MET.Nesting=3
#SH.MET.LineOfCode=62
#SH.MET.ComplexitySimplified = 16;
nvm_install_latest_npm() {
  nvm_echo 'Attempting to upgrade to the latest working version of npm...'
  local NODE_VERSION
  NODE_VERSION="$(nvm_strip_iojs_prefix "$(nvm_ls_current)")"
  if [ "${NODE_VERSION}" = 'system' ]; then
    NODE_VERSION="$(node --version)"
  elif [ "${NODE_VERSION}" = 'none' ]; then
    nvm_echo "Detected node version ${NODE_VERSION}, npm version v${NPM_VERSION}"
    NODE_VERSION=''
  fi
  if [ -z "${NODE_VERSION}" ]; then
    nvm_err 'Unable to obtain node version.'
    return 1
  fi
  local NPM_VERSION
  NPM_VERSION="$(npm --version 2>/dev/null)"
  if [ -z "${NPM_VERSION}" ]; then
    nvm_err 'Unable to obtain npm version.'
    return 2
  fi

  local NVM_NPM_CMD
  NVM_NPM_CMD='npm'
  if [ "${NVM_DEBUG-}" = 1 ]; then
    nvm_echo "Detected node version ${NODE_VERSION}, npm version v${NPM_VERSION}"
    NVM_NPM_CMD='echo npm'
  fi

  local NVM_IS_0_6
  NVM_IS_0_6=0
  if nvm_version_greater_than_or_equal_to "${NODE_VERSION}" 0.6.0 && nvm_version_greater 0.7.0 "${NODE_VERSION}"; then
    NVM_IS_0_6=1
  fi
  local NVM_IS_0_9
  NVM_IS_0_9=0
  if nvm_version_greater_than_or_equal_to "${NODE_VERSION}" 0.9.0 && nvm_version_greater 0.10.0 "${NODE_VERSION}"; then
    NVM_IS_0_9=1
  fi

  if [ $NVM_IS_0_6 -eq 1 ]; then
    nvm_echo '* `node` v0.6.x can only upgrade to `npm` v1.3.x'
    $NVM_NPM_CMD install -g npm@1.3
  elif [ $NVM_IS_0_9 -eq 0 ]; then
    # node 0.9 breaks here, for some reason
    if nvm_version_greater_than_or_equal_to "${NPM_VERSION}" 1.0.0 && nvm_version_greater 2.0.0 "${NPM_VERSION}"; then
      nvm_echo '* `npm` v1.x needs to first jump to `npm` v1.4.28 to be able to upgrade further'
      $NVM_NPM_CMD install -g npm@1.4.28
    elif nvm_version_greater_than_or_equal_to "${NPM_VERSION}" 2.0.0 && nvm_version_greater 3.0.0 "${NPM_VERSION}"; then
      nvm_echo '* `npm` v2.x needs to first jump to the latest v2 to be able to upgrade further'
      $NVM_NPM_CMD install -g npm@2
    fi
  fi

  if [ $NVM_IS_0_9 -eq 1 ] || [ $NVM_IS_0_6 -eq 1 ]; then
    nvm_echo '* node v0.6 and v0.9 are unable to upgrade further'
  elif nvm_version_greater 1.0.0 "${NODE_VERSION}"; then
    nvm_echo '* `npm` v4.5.x is the last version that works on `node` versions below v1.0.0'
    $NVM_NPM_CMD install -g npm@4.5
  elif nvm_version_greater 4.0.0 "${NODE_VERSION}"; then
    nvm_echo '* `npm` v5 and higher do not work on `node` versions below v4.0.0'
    $NVM_NPM_CMD install -g npm@4
  elif [ $NVM_IS_0_9 -eq 0 ] && [ $NVM_IS_0_6 -eq 0 ]; then
    nvm_echo '* Installing latest `npm`; if this does not work on your node version, please report a bug!'
    $NVM_NPM_CMD install -g npm
  fi
  nvm_echo "* npm upgraded to: v$(npm --version 2>/dev/null)"
}

# Make zsh glob matching behave same as bash
# This fixes the "zsh: no matches found" errors
if [ -z "${NVM_CD_FLAGS-}" ]; then
  export NVM_CD_FLAGS=''
fi
if nvm_has "unsetopt"; then
  unsetopt nomatch 2>/dev/null
  NVM_CD_FLAGS="-q"
fi

# Auto detect the NVM_DIR when not set
if [ -z "${NVM_DIR-}" ]; then
  # shellcheck disable=SC2128
  if [ -n "${BASH_SOURCE-}" ]; then
    # shellcheck disable=SC2169
    NVM_SCRIPT_SOURCE="${BASH_SOURCE[0]}"
  fi
  # shellcheck disable=SC1001
  NVM_DIR="$(nvm_cd ${NVM_CD_FLAGS} "$(dirname "${NVM_SCRIPT_SOURCE:-$0}")" > /dev/null && \pwd)"
  export NVM_DIR
fi
unset NVM_SCRIPT_SOURCE 2> /dev/null

#SH.MET.RatioComment = 15.79
#SH.MET.Nesting=2
#SH.MET.LineOfCode=16
#SH.MET.ComplexitySimplified = 4;
nvm_tree_contains_path() {
  local tree
  tree="${1-}"
  local node_path
  node_path="${2-}"

  if [ "@${tree}@" = "@@" ] || [ "@${node_path}@" = "@@" ]; then
    nvm_err "both the tree and the node path are required"
    return 2
  fi

  local pathdir
  pathdir=$(dirname "${node_path}")
  while [ "${pathdir}" != "" ] && [ "${pathdir}" != "." ] && [ "${pathdir}" != "/" ] && [ "${pathdir}" != "${tree}" ]; do
    pathdir=$(dirname "${pathdir}")
  done
  [ "${pathdir}" = "${tree}" ]
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=2
#SH.MET.LineOfCode=8
#SH.MET.ComplexitySimplified = 3;
# Traverse up in directory tree to find containing folder
nvm_find_up() {
  local path
  path="${PWD}"
  while [ "${path}" != "" ] && [ ! -f "${path}/${1-}" ]; do
    path=${path%/*}
  done
  nvm_echo "${path}"
}


#SH.MET.RatioComment = NaN
#SH.MET.Nesting=2
#SH.MET.LineOfCode=7
#SH.MET.ComplexitySimplified = 2;
nvm_find_nvmrc() {
  local dir
  dir="$(nvm_find_up '.nvmrc')"
  if [ -e "${dir}/.nvmrc" ]; then
    nvm_echo "${dir}/.nvmrc"
  fi
}

#SH.MET.RatioComment = 21.05
#SH.MET.Nesting=2
#SH.MET.LineOfCode=15
#SH.MET.ComplexitySimplified = 3;
# Obtain nvm version from rc file
nvm_rc_version() {
  export NVM_RC_VERSION=''
  local NVMRC_PATH
  NVMRC_PATH="$(nvm_find_nvmrc)"
  if [ ! -e "${NVMRC_PATH}" ]; then
    nvm_err "No .nvmrc file found"
    return 1
  fi
  read -r NVM_RC_VERSION < "${NVMRC_PATH}" || printf ''
  if [ ! -n "${NVM_RC_VERSION}" ]; then
    nvm_err "Warning: empty .nvmrc file found at \"${NVMRC_PATH}\""
    return 2
  fi
  nvm_echo "Found '${NVMRC_PATH}' with version <${NVM_RC_VERSION}>"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_clang_version() {
  clang --version | command awk '{ if ($2 == "version") print $3; else if ($3 == "version") print $4 }' | command sed 's/-.*$//g'
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_curl_version() {
  curl -V | command awk '{ if ($1 == "curl") print $2 }' | command sed 's/-.*$//g'
}

#SH.MET.RatioComment = 12.5
#SH.MET.LineOfCode=14
#SH.MET.ComplexitySimplified = 1
nvm_version_greater() {
  command awk 'BEGIN {
    if (ARGV[1] == "" || ARGV[2] == "") exit(1)
    split(ARGV[1], a, /\./);
    split(ARGV[2], b, /\./);
    for (i=1; i<=3; i++) {
      if (a[i] && a[i] !~ /^[0-9]+$/) exit(2);
      if (b[i] && b[i] !~ /^[0-9]+$/) { exit(0); }
      if (a[i] < b[i]) exit(3);
      else if (a[i] > b[i]) exit(0);
    }
    exit(4)
  }' "${1#v}" "${2#v}";
}

#SH.MET.RatioComment = 18.75
#SH.MET.Nesting=1
#SH.MET.LineOfCode=13
#SH.MET.ComplexitySimplified = 1
nvm_version_greater_than_or_equal_to() {
  command awk 'BEGIN {
    if (ARGV[1] == "" || ARGV[2] == "") exit(1)
    split(ARGV[1], a, /\./);
    split(ARGV[2], b, /\./);
    for (i=1; i<=3; i++) {
      if (a[i] && a[i] !~ /^[0-9]+$/) exit(2);
      if (a[i] < b[i]) exit(3);
      else if (a[i] > b[i]) exit(0);
    }
    exit(0)
  }' "${1#v}" "${2#v}";
}

#SH.MET.RatioComment = 17.65
#SH.MET.Nesting=2
#SH.MET.LineOfCode=14
#SH.MET.ComplexitySimplified = 5
nvm_version_dir() {
  local NVM_WHICH_DIR
  NVM_WHICH_DIR="${1-}"
  if [ -z "${NVM_WHICH_DIR}" ] || [ "${NVM_WHICH_DIR}" = "new" ]; then
    nvm_echo "${NVM_DIR}/versions/node"
  elif [ "_${NVM_WHICH_DIR}" = "_iojs" ]; then
    nvm_echo "${NVM_DIR}/versions/io.js"
  elif [ "_${NVM_WHICH_DIR}" = "_old" ]; then
    nvm_echo "${NVM_DIR}"
  else
    nvm_err 'unknown version dir'
    return 3
  fi
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_alias_path() {
  nvm_echo "$(nvm_version_dir old)/alias"
}

#SH.MET.RatioComment = 17.65
#SH.MET.Nesting=2
#SH.MET.LineOfCode=14
#SH.MET.ComplexitySimplified = 5;
nvm_version_path() {
  local VERSION
  VERSION="${1-}"
  if [ -z "${VERSION}" ]; then
    nvm_err 'version is required'
    return 3
  elif nvm_is_iojs_version "${VERSION}"; then
    nvm_echo "$(nvm_version_dir iojs)/$(nvm_strip_iojs_prefix "${VERSION}")"
  elif nvm_version_greater 0.12.0 "${VERSION}"; then
    nvm_echo "$(nvm_version_dir old)/${VERSION}"
  else
    nvm_echo "$(nvm_version_dir new)/${VERSION}"
  fi
}

#SH.MET.RatioComment = 9.68
#SH.MET.Nesting=3
#SH.MET.LineOfCode=28
#SH.MET.ComplexitySimplified = 6;
nvm_ensure_version_installed() {
  local PROVIDED_VERSION
  PROVIDED_VERSION="${1-}"
  if [ "${PROVIDED_VERSION}" = 'system' ]; then
    if nvm_has_system_iojs || nvm_has_system_node; then
      return 0
    fi
    nvm_err "N/A: no system version of node/io.js is installed."
    return 1
  fi
  local LOCAL_VERSION
  local EXIT_CODE
  LOCAL_VERSION="$(nvm_version "${PROVIDED_VERSION}")"
  EXIT_CODE="$?"
  local NVM_VERSION_DIR
  if [ "${EXIT_CODE}" != "0" ] || ! nvm_is_version_installed "${LOCAL_VERSION}"; then
    if VERSION="$(nvm_resolve_alias "${PROVIDED_VERSION}")"; then
      nvm_err "N/A: version \"${PROVIDED_VERSION} -> ${VERSION}\" is not yet installed."
    else
      local PREFIXED_VERSION
      PREFIXED_VERSION="$(nvm_ensure_version_prefix "${PROVIDED_VERSION}")"
      nvm_err "N/A: version \"${PREFIXED_VERSION:-$PROVIDED_VERSION}\" is not yet installed."
    fi
    nvm_err ""
    nvm_err "You need to run \"nvm install ${PROVIDED_VERSION}\" to install it before using it."
    return 1
  fi
}
#SH.MET.RatioComment = 16.67
#SH.MET.Nesting=2
#SH.MET.LineOfCode=25
#SH.MET.ComplexitySimplified = 5
# Expand a version using the version cache
nvm_version() {
  local PATTERN
  PATTERN="${1-}"
  local VERSION
  # The default version is the current one
  if [ -z "${PATTERN}" ]; then
    PATTERN='current'
  fi

  if [ "${PATTERN}" = "current" ]; then
    nvm_ls_current
    return $?
  fi

  local NVM_NODE_PREFIX
  NVM_NODE_PREFIX="$(nvm_node_prefix)"
  case "_${PATTERN}" in
    "_${NVM_NODE_PREFIX}" | "_${NVM_NODE_PREFIX}-")
      PATTERN="stable"
    ;;
  esac
  VERSION="$(nvm_ls "${PATTERN}" | command tail -1)"
  if [ -z "${VERSION}" ] || [ "_${VERSION}" = "_N/A" ]; then
    nvm_echo "N/A"
    return 3;
  fi
  nvm_echo "${VERSION}"
}

#SH.MET.RatioComment = 9.67
#SH.MET.Nesting=3
#SH.MET.LineOfCode=28
#SH.MET.ComplexitySimplified = 10;
nvm_remote_version() {
  local PATTERN
  PATTERN="${1-}"
  local VERSION
  if nvm_validate_implicit_alias "${PATTERN}" 2> /dev/null ; then
    case "${PATTERN}" in
      "$(nvm_iojs_prefix)")
        VERSION="$(NVM_LTS="${NVM_LTS-}" nvm_ls_remote_iojs | command tail -1)" &&:
      ;;
      *)
        VERSION="$(NVM_LTS="${NVM_LTS-}" nvm_ls_remote "${PATTERN}")" &&:
      ;;
    esac
  else
    VERSION="$(NVM_LTS="${NVM_LTS-}" nvm_remote_versions "${PATTERN}" | command tail -1)"
  fi
  if [ -n "${NVM_VERSION_ONLY-}" ]; then
    command awk 'BEGIN {
      n = split(ARGV[1], a);
      print a[1]
    }' "${VERSION}"
  else
    nvm_echo "${VERSION}"
  fi
  if [ "${VERSION}" = 'N/A' ]; then
    return 3
  fi
}

#SH.MET.RatioComment = 5
#SH.MET.Nesting=2
#SH.MET.LineOfCode=57
nvm_remote_versions() {
  local NVM_IOJS_PREFIX
  NVM_IOJS_PREFIX="$(nvm_iojs_prefix)"
  local NVM_NODE_PREFIX
  NVM_NODE_PREFIX="$(nvm_node_prefix)"

  local PATTERN
  PATTERN="${1-}"

  local NVM_FLAVOR
  if [ -n "${NVM_LTS-}" ]; then
    NVM_FLAVOR="${NVM_NODE_PREFIX}"
  fi

  case "${PATTERN}" in
    "${NVM_IOJS_PREFIX}" | "io.js")
       NVM_FLAVOR="${NVM_IOJS_PREFIX}"
       unset PATTERN
    ;;
    "${NVM_NODE_PREFIX}")
       NVM_FLAVOR="${NVM_NODE_PREFIX}"
       unset PATTERN
    ;;
  esac

  if nvm_validate_implicit_alias "${PATTERN-}" 2> /dev/null ; then
    nvm_err 'Implicit aliases are not supported in nvm_remote_versions.'
    return 1
  fi

  local NVM_LS_REMOTE_EXIT_CODE
  NVM_LS_REMOTE_EXIT_CODE=0
  local NVM_LS_REMOTE_PRE_MERGED_OUTPUT
  NVM_LS_REMOTE_PRE_MERGED_OUTPUT=''
  local NVM_LS_REMOTE_POST_MERGED_OUTPUT
  NVM_LS_REMOTE_POST_MERGED_OUTPUT=''
  if [ -z "${NVM_FLAVOR-}" ] || [ "${NVM_FLAVOR-}" = "${NVM_NODE_PREFIX}" ]; then
    local NVM_LS_REMOTE_OUTPUT
    NVM_LS_REMOTE_OUTPUT=$(NVM_LTS="${NVM_LTS-}" nvm_ls_remote "${PATTERN-}") &&:
    NVM_LS_REMOTE_EXIT_CODE=$?
    # split output into two
    NVM_LS_REMOTE_PRE_MERGED_OUTPUT="${NVM_LS_REMOTE_OUTPUT%%v4\.0\.0*}"
    NVM_LS_REMOTE_POST_MERGED_OUTPUT="${NVM_LS_REMOTE_OUTPUT#$NVM_LS_REMOTE_PRE_MERGED_OUTPUT}"
  fi

  local NVM_LS_REMOTE_IOJS_EXIT_CODE
  NVM_LS_REMOTE_IOJS_EXIT_CODE=0
  local NVM_LS_REMOTE_IOJS_OUTPUT
  if [ -z "${NVM_LTS-}" ] && ( \
    [ -z "${NVM_FLAVOR-}" ] || [ "${NVM_FLAVOR-}" = "${NVM_IOJS_PREFIX}" ] \
  ); then
    NVM_LS_REMOTE_IOJS_OUTPUT=$(nvm_ls_remote_iojs "${PATTERN-}") &&:
    NVM_LS_REMOTE_IOJS_EXIT_CODE=$?
  fi

  VERSIONS="$(nvm_echo "${NVM_LS_REMOTE_PRE_MERGED_OUTPUT}
${NVM_LS_REMOTE_IOJS_OUTPUT}
${NVM_LS_REMOTE_POST_MERGED_OUTPUT}" | nvm_grep -v "N/A" | command sed '/^$/d')"

  if [ -z "${VERSIONS}" ]; then
    nvm_echo 'N/A'
    return 3
  fi
  nvm_echo "${VERSIONS}"
  return $NVM_LS_REMOTE_EXIT_CODE || $NVM_LS_REMOTE_IOJS_EXIT_CODE
}

#SH.MET.RatioComment = 15.79
#SH.MET.Nesting=2
#SH.MET.LineOfCode=16
#SH.MET.ComplexitySimplified = 4;
nvm_is_valid_version() {
  if nvm_validate_implicit_alias "${1-}" 2> /dev/null; then
    return 0
  fi
  case "${1-}" in
    "$(nvm_iojs_prefix)" | \
    "$(nvm_node_prefix)")
      return 0
    ;;
    *)
      local VERSION
      VERSION="$(nvm_strip_iojs_prefix "${1-}")"
      nvm_version_greater_than_or_equal_to "${VERSION}" 0
    ;;
  esac
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=7
nvm_normalize_version() {
  command awk 'BEGIN {
    split(ARGV[1], a, /\./);
    printf "%d%06d%06d\n", a[1], a[2], a[3];
    exit;
  }' "${1#v}"
}

#SH.MET.RatioComment = 15.38
#SH.MET.Nesting=2
#SH.MET.LineOfCode=11
nvm_ensure_version_prefix() {
  local NVM_VERSION
  NVM_VERSION="$(nvm_strip_iojs_prefix "${1-}" | command sed -e 's/^\([0-9]\)/v\1/g')"
  if nvm_is_iojs_version "${1-}"; then
    nvm_add_iojs_prefix "${NVM_VERSION}"
  else
    nvm_echo "${NVM_VERSION}"
  fi
}

#SH.MET.Nesting=2
#SH.MET.LineOfCode=11
nvm_format_version() {
  local VERSION
  VERSION="$(nvm_ensure_version_prefix "${1-}")"
  local NUM_GROUPS
  NUM_GROUPS="$(nvm_num_version_groups "${VERSION}")"
  if [ "${NUM_GROUPS}" -lt 3 ]; then
    nvm_format_version "${VERSION%.}.0"
  else
    nvm_echo "${VERSION}" | command cut -f1-3 -d.
  fi
}

#SH.MET.RatioComment = 16.67
#SH.MET.Nesting=2
#SH.MET.LineOfCode=15
nvm_num_version_groups() {
  local VERSION
  VERSION="${1-}"
  VERSION="${VERSION#v}"
  VERSION="${VERSION%.}"
  if [ -z "${VERSION}" ]; then
    nvm_echo "0"
    return
  fi
  local NVM_NUM_DOTS
  NVM_NUM_DOTS=$(nvm_echo "${VERSION}" | command sed -e 's/[^\.]//g')
  local NVM_NUM_GROUPS
  NVM_NUM_GROUPS=".${NVM_NUM_DOTS}" # add extra dot, since it's (n - 1) dots at this point
  nvm_echo "${#NVM_NUM_GROUPS}"
}

#SH.MET.RatioComment = 13.33
#SH.MET.Nesting=2
#SH.MET.LineOfCode=13
nvm_strip_path() {
  if [ -z "${NVM_DIR-}" ]; then
    nvm_err '${NVM_DIR} not set!'
    return 1
  fi
  nvm_echo "${1-}" | command sed \
    -e "s#${NVM_DIR}/[^/]*${2-}[^:]*:##g" \
    -e "s#:${NVM_DIR}/[^/]*${2-}[^:]*##g" \
    -e "s#${NVM_DIR}/[^/]*${2-}[^:]*##g" \
    -e "s#${NVM_DIR}/versions/[^/]*/[^/]*${2-}[^:]*:##g" \
    -e "s#:${NVM_DIR}/versions/[^/]*/[^/]*${2-}[^:]*##g" \
    -e "s#${NVM_DIR}/versions/[^/]*/[^/]*${2-}[^:]*##g"
}

#SH.MET.RatioComment = NaN
#SH.MET.LineOfCode=7
#SH.MET.ComplexitySimplified = 3;
nvm_prepend_path() {
  if [ -z "${1-}" ]; then
    nvm_echo "${2-}"
  else
    nvm_echo "${2-}:${1-}"
  fi
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
nvm_binary_available() {
  # binaries started with node 0.8.6
  nvm_version_greater_than_or_equal_to "$(nvm_strip_iojs_prefix "${1-}")" v0.8.6
}

#SH.MET.RatioComment = 3.4
#SH.MET.Nesting=3
#SH.MET.LineOfCode=56
nvm_print_formatted_alias() {
  local ALIAS
  ALIAS="${1-}"
  local DEST
  DEST="${2-}"
  local VERSION
  VERSION="${3-}"
  if [ -z "${VERSION}" ]; then
    VERSION="$(nvm_version "${DEST}")" ||:
  fi
  local VERSION_FORMAT
  local ALIAS_FORMAT
  local DEST_FORMAT
  ALIAS_FORMAT='%s'
  DEST_FORMAT='%s'
  VERSION_FORMAT='%s'
  local NEWLINE
  NEWLINE="\n"
  if [ "_${DEFAULT}" = '_true' ]; then
    NEWLINE=" (default)\n"
  fi
  local ARROW
  ARROW='->'
  if [ -z "${NVM_NO_COLORS}" ] && nvm_has_colors; then
    ARROW='\033[0;90m->\033[0m'
    if [ "_${DEFAULT}" = '_true' ]; then
      NEWLINE=" \033[0;37m(default)\033[0m\n"
    fi
    if [ "_${VERSION}" = "_${NVM_CURRENT-}" ]; then
      ALIAS_FORMAT='\033[0;32m%s\033[0m'
      DEST_FORMAT='\033[0;32m%s\033[0m'
      VERSION_FORMAT='\033[0;32m%s\033[0m'
    elif nvm_is_version_installed "${VERSION}"; then
      ALIAS_FORMAT='\033[0;34m%s\033[0m'
      DEST_FORMAT='\033[0;34m%s\033[0m'
      VERSION_FORMAT='\033[0;34m%s\033[0m'
    elif [ "${VERSION}" = '∞' ] || [ "${VERSION}" = 'N/A' ]; then
      ALIAS_FORMAT='\033[1;31m%s\033[0m'
      DEST_FORMAT='\033[1;31m%s\033[0m'
      VERSION_FORMAT='\033[1;31m%s\033[0m'
    fi
    if [ "_${NVM_LTS-}" = '_true' ]; then
      ALIAS_FORMAT='\033[1;33m%s\033[0m'
    fi
    if [ "_${DEST%/*}" = "_lts" ]; then
      DEST_FORMAT='\033[1;33m%s\033[0m'
    fi
  elif [ "_$VERSION" != '_∞' ] && [ "_$VERSION" != '_N/A' ]; then
    VERSION_FORMAT='%s *'
  fi
  if [ "${DEST}" = "${VERSION}" ]; then
    command printf -- "${ALIAS_FORMAT} ${ARROW} ${VERSION_FORMAT}${NEWLINE}" "${ALIAS}" "${DEST}"
  else
    command printf -- "${ALIAS_FORMAT} ${ARROW} ${DEST_FORMAT} (${ARROW} ${VERSION_FORMAT})${NEWLINE}" "${ALIAS}" "${DEST}" "${VERSION}"
  fi
}

#SH.MET.RatioComment = 12.5
#SH.MET.Nesting=2
#SH.MET.LineOfCode=21
nvm_print_alias_path() {
  local NVM_ALIAS_DIR
  NVM_ALIAS_DIR="${1-}"
  if [ -z "${NVM_ALIAS_DIR}" ]; then
    nvm_err 'An alias dir is required.'
    return 1
  fi
  local ALIAS_PATH
  ALIAS_PATH="${2-}"
  if [ -z "${ALIAS_PATH}" ]; then
    nvm_err 'An alias path is required.'
    return 2
  fi
  local ALIAS
  ALIAS="${ALIAS_PATH##${NVM_ALIAS_DIR}\/}"
  local DEST
  DEST="$(nvm_alias "${ALIAS}" 2> /dev/null)" ||:
  if [ -n "${DEST}" ]; then
    NVM_NO_COLORS="${NVM_NO_COLORS-}" NVM_LTS="${NVM_LTS-}" DEFAULT=false nvm_print_formatted_alias "${ALIAS}" "${DEST}"
  fi
}


#SH.MET.RatioComment = 16.67
#SH.MET.Nesting=2
#SH.MET.LineOfCode=15
#SH.MET.ComplexitySimplified = 3;
nvm_make_alias() {
  local ALIAS
  ALIAS="${1-}"
  if [ -z "${ALIAS}" ]; then
    nvm_err "an alias name is required"
    return 1
  fi
  local VERSION
  VERSION="${2-}"
  if [ -z "${VERSION}" ]; then
    nvm_err "an alias target version is required"
    return 2
  fi
  nvm_echo "${VERSION}" | tee "$(nvm_alias_path)/${ALIAS}" >/dev/null
}

#SH.MET.RatioComment = 16.67
#SH.MET.Nesting=2
#SH.MET.LineOfCode=15
#SH.MET.ComplexitySimplified = 3;
nvm_alias() {
  local ALIAS
  ALIAS="${1-}"
  if [ -z "${ALIAS}" ]; then
    nvm_err 'An alias is required.'
    return 1
  fi

  local NVM_ALIAS_PATH
  NVM_ALIAS_PATH="$(nvm_alias_path)/${ALIAS}"
  if [ ! -f "${NVM_ALIAS_PATH}" ]; then
    nvm_err 'Alias does not exist.'
    return 2
  fi

  command cat "${NVM_ALIAS_PATH}"
}

#SH.MET.RatioComment = 14.29
#SH.MET.Nesting=3
#SH.MET.LineOfCode=18
#SH.MET.ComplexitySimplified = 7;
nvm_ls_current() {
  local NVM_LS_CURRENT_NODE_PATH
  if ! NVM_LS_CURRENT_NODE_PATH="$(command which node 2> /dev/null)"; then
    nvm_echo 'none'
  elif nvm_tree_contains_path "$(nvm_version_dir iojs)" "${NVM_LS_CURRENT_NODE_PATH}"; then
    nvm_add_iojs_prefix "$(iojs --version 2>/dev/null)"
  elif nvm_tree_contains_path "${NVM_DIR}" "${NVM_LS_CURRENT_NODE_PATH}"; then
    local VERSION
    VERSION="$(node --version 2>/dev/null)"
    if [ "${VERSION}" = "v0.6.21-pre" ]; then
      nvm_echo 'v0.6.21'
    else
      nvm_echo "${VERSION}"
    fi
  else
    nvm_echo 'system'
  fi
}

#SH.MET.RatioComment = 5.77
#SH.MET.Nesting=3
#SH.MET.LineOfCode=49
#SH.MET.ComplexitySimplified = 11;
nvm_resolve_alias() {
  if [ -z "${1-}" ]; then
    return 1
  fi

  local PATTERN
  PATTERN="${1-}"

  local ALIAS
  ALIAS="${PATTERN}"
  local ALIAS_TEMP

  local SEEN_ALIASES
  SEEN_ALIASES="${ALIAS}"
  while true; do
    ALIAS_TEMP="$(nvm_alias "${ALIAS}" 2> /dev/null || echo)"

    if [ -z "${ALIAS_TEMP}" ]; then
      break
    fi

    if command printf "${SEEN_ALIASES}" | nvm_grep -e "^${ALIAS_TEMP}$" > /dev/null; then
      ALIAS="∞"
      break
    fi

    SEEN_ALIASES="${SEEN_ALIASES}\n${ALIAS_TEMP}"
    ALIAS="${ALIAS_TEMP}"
  done

  if [ -n "${ALIAS}" ] && [ "_${ALIAS}" != "_${PATTERN}" ]; then
    local NVM_IOJS_PREFIX
    NVM_IOJS_PREFIX="$(nvm_iojs_prefix)"
    local NVM_NODE_PREFIX
    NVM_NODE_PREFIX="$(nvm_node_prefix)"
    case "${ALIAS}" in
      '∞' | \
      "${NVM_IOJS_PREFIX}" | "${NVM_IOJS_PREFIX}-" | \
      "${NVM_NODE_PREFIX}" )
        nvm_echo "${ALIAS}"
      ;;
      *)
        nvm_ensure_version_prefix "${ALIAS}"
      ;;
    esac
    return 0
  fi

  if nvm_validate_implicit_alias "${PATTERN}" 2> /dev/null ; then
    local IMPLICIT
    IMPLICIT="$(nvm_print_implicit_alias local "${PATTERN}" 2> /dev/null)"
    if [ -n "${IMPLICIT}" ]; then
      nvm_ensure_version_prefix "${IMPLICIT}"
    fi
  fi

  return 2
}

#SH.MET.RatioComment = 19.05
#SH.MET.Nesting=2
#SH.MET.LineOfCode=17
#SH.MET.ComplexitySimplified = 5;
nvm_resolve_local_alias() {
  if [ -z "${1-}" ]; then
    return 1
  fi

  local VERSION
  local EXIT_CODE
  VERSION="$(nvm_resolve_alias "${1-}")"
  EXIT_CODE=$?
  if [ -z "${VERSION}" ]; then
    return $EXIT_CODE
  fi
  if [ "_${VERSION}" != '_∞' ]; then
    nvm_version "${VERSION}"
  else
    nvm_echo "${VERSION}"
  fi
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_iojs_prefix() {
  nvm_echo 'iojs'
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_node_prefix() {
  nvm_echo 'node'
}

#SH.MET.Nesting=2
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 2;
nvm_is_iojs_version() {
  case "${1-}" in iojs-*) return 0 ;; esac
  return 1
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1 
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_add_iojs_prefix() {
  nvm_echo "$(nvm_iojs_prefix)-$(nvm_ensure_version_prefix "$(nvm_strip_iojs_prefix "${1-}")")"
}

#SH.MET.RatioComment = 29.411
#SH.MET.Nesting=2
#SH.MET.LineOfCode=9
#SH.MET.ComplexitySimplified = 3;
nvm_strip_iojs_prefix() {
  local NVM_IOJS_PREFIX
  NVM_IOJS_PREFIX="$(nvm_iojs_prefix)"
  if [ "${1-}" = "${NVM_IOJS_PREFIX}" ]; then
    nvm_echo
  else
    nvm_echo "${1#${NVM_IOJS_PREFIX}-}"
  fi
}


#SH.MET.RatioComment = 29.41
#SH.MET.Nesting=2
#SH.MET.LineOfCode=12
#SH.MET.ComplexitySimplified = 4;
vm_ls_remote() {
  local PATTERN
  PATTERN="${1-}"
  if nvm_validate_implicit_alias "${PATTERN}" 2> /dev/null ; then
    PATTERN="$(NVM_LTS="${NVM_LTS-}" nvm_ls_remote "$(nvm_print_implicit_alias remote "${PATTERN}")" | command tail -1 | command awk '{ print $1 }')"
  elif [ -n "${PATTERN}" ]; then
    PATTERN="$(nvm_ensure_version_prefix "${PATTERN}")"
  else
    PATTERN=".*"
  fi
  NVM_LTS="${NVM_LTS-}" nvm_ls_remote_index_tab node std "${PATTERN}"
}

#SH.MET.RatioComment = NaN
#SH.MET.Nesting=1
#SH.MET.LineOfCode=3
#SH.MET.ComplexitySimplified = 1;
nvm_ls_remote_iojs() {
  NVM_LTS="${NVM_LTS-}" nvm_ls_remote_index_tab iojs std "${1-}"
}

#SH.MET.RatioComment = 3.7
#SH.MET.Nesting=3
#SH.MET.LineOfCode=102
#SH.MET.ComplexitySimplified = 17;
# args flavor, type, version
nvm_ls_remote_index_tab() {
  local LTS
  LTS="${NVM_LTS-}"
  if [ "$#" -lt 3 ]; then
    nvm_err 'not enough arguments'
    return 5
  fi

  local FLAVOR
  FLAVOR="${1-}"

  local TYPE
  TYPE="${2-}"

  local MIRROR
  MIRROR="$(nvm_get_mirror "${FLAVOR}" "${TYPE}")"
  if [ -z "${MIRROR}" ]; then
    return 3
  fi

  local PREFIX
  PREFIX=''
  case "${FLAVOR}-${TYPE}" in
    iojs-std) PREFIX="$(nvm_iojs_prefix)-" ;;
    node-std) PREFIX='' ;;
    iojs-*)
      nvm_err 'unknown type of io.js release'
      return 4
    ;;
    *)
      nvm_err 'unknown type of node.js release'
      return 4
    ;;
  esac
  local SORT_COMMAND
  SORT_COMMAND='command sort'
  case "${FLAVOR}" in
    node) SORT_COMMAND='command sort -t. -u -k 1.2,1n -k 2,2n -k 3,3n' ;;
  esac

  local PATTERN
  PATTERN="${3-}"

  local VERSIONS
  if [ -n "${PATTERN}" ]; then
    if [ "${FLAVOR}" = 'iojs' ]; then
      PATTERN="$(nvm_ensure_version_prefix "$(nvm_strip_iojs_prefix "${PATTERN}")")"
    else
      PATTERN="$(nvm_ensure_version_prefix "${PATTERN}")"
    fi
  else
    unset PATTERN
  fi

  ZSH_HAS_SHWORDSPLIT_UNSET=1
  if nvm_has "setopt"; then
    ZSH_HAS_SHWORDSPLIT_UNSET="$(set +e ; setopt | nvm_grep shwordsplit > /dev/null ; nvm_echo $?)"
    setopt shwordsplit
  fi
  local VERSION_LIST
  VERSION_LIST="$(nvm_download -L -s "${MIRROR}/index.tab" -o - \
    | command sed "
        1d;
        s/^/${PREFIX}/;
      " \
  )"
  local LTS_ALIAS
  local LTS_VERSION
  command mkdir -p "$(nvm_alias_path)/lts"
  nvm_echo "${VERSION_LIST}" \
    | command awk '{
        if ($10 ~ /^\-?$/) { next }
        if ($10 && !a[tolower($10)]++) {
          if (alias) { print alias, version }
          alias_name = "lts/" tolower($10)
          if (!alias) { print "lts/*", alias_name }
          alias = alias_name
          version = $1
        }
      }
      END {
        if (alias) {
          print alias, version
        }
      }' \
    | while read -r LTS_ALIAS_LINE; do
      LTS_ALIAS="${LTS_ALIAS_LINE%% *}"
      LTS_VERSION="${LTS_ALIAS_LINE#* }"
      nvm_make_alias "$LTS_ALIAS" "$LTS_VERSION" >/dev/null 2>&1
    done

  VERSIONS="$(nvm_echo "${VERSION_LIST}" \
    | command awk -v pattern="${PATTERN-}" -v lts="${LTS-}" '{
        if (!$1) { next }
        if (pattern && tolower($1) !~ tolower(pattern)) { next }
        if (lts == "*" && $10 ~ /^\-?$/) { next }
        if (lts && lts != "*" && tolower($10) !~ tolower(lts)) { next }
        if ($10 !~ /^\-?$/) print $1, $10; else print $1
      }' \
    | nvm_grep -w "${PATTERN:-.*}" \
    | $SORT_COMMAND)"
  if [ "$ZSH_HAS_SHWORDSPLIT_UNSET" -eq 1 ] && nvm_has "unsetopt"; then
    unsetopt shwordsplit
  fi
  if [ -z "${VERSIONS}" ]; then
    nvm_echo 'N/A'
    return 3
  fi
  nvm_echo "${VERSIONS}"
}

