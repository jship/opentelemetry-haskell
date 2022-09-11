#!/bin/bash

set -o errexit
set -o pipefail
[[ "${DEBUG}" == 'true' ]] && set -o xtrace

if ! command -v 'proto-lens-protoc' &>/dev/null; then
  echo "bindings.sh: 'proto-lens-protoc' binary not found on runtime path" >&2
  echo "  It can be installed via:" >&2
  echo "    stack install --resolver='nightly-2022-08-04' proto-lens-protoc" >&2
  exit 1
fi

declare -g tempDir=''
tempDir="$(mktemp -d)"
function removeTempDir() {
  if [ -d "${tempDir}" ]; then
    rm -rf "${tempDir}"
  fi
}
trap removeTempDir EXIT

declare -g packageName='otel-sdk-otlp-bindings'
declare -g packageDir=''
packageDir="$(stack path --project-root)/${packageName}"
declare -g otlpProtoTag='v0.19.0'
declare -g protocVersion='21.5'
declare -g protocZip="protoc-${protocVersion}-linux-x86_64.zip"
declare -g protocURL="https://github.com/protocolbuffers/protobuf/releases/download/v${protocVersion}/${protocZip}"

cd "${tempDir}"

curl -OL "${protocURL}"
unzip "${protocZip}"
rm -f "${protocZip}"

mkdir opentelemetry-proto
cd opentelemetry-proto
git init
git remote add origin 'https://github.com/open-telemetry/opentelemetry-proto.git'
git fetch origin "${otlpProtoTag}"
git reset --hard FETCH_HEAD

cd "${tempDir}"

find opentelemetry-proto -name '*.proto' -type f -print | sort > 'proto-file-list.txt'
bin/protoc \
  --plugin="protoc-gen-haskell=$(command -v 'proto-lens-protoc')" \
  --haskell_out='.' \
  --proto_path='opentelemetry-proto' \
  @'proto-file-list.txt'

find Proto -type f -name '*.hs' -print0 | xargs -0 sed -i -e "s/Proto\\.Opentelemetry\\.Proto/OTel.SDK.OTLP.Bindings/g"

rm -rf "${packageDir}/library"
mkdir -p "${packageDir}/library/OTel/SDK/OTLP/Bindings/"
cp -R Proto/Opentelemetry/Proto/* "${packageDir}/library/OTel/SDK/OTLP/Bindings/"

# Ensure that we regenerate the .cabal file from package.yaml
rm "${packageDir}/${packageName}.cabal"

# Ensure build is all good
cd "${packageDir}"
stack clean "${packageName}"
stack build --test --no-run-tests --bench --no-run-benchmarks --pedantic "${packageName}"
