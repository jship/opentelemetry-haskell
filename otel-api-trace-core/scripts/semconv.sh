#!/usr/bin/env bash

set -o errexit
set -o pipefail
[[ "${DEBUG}" == 'true' ]] && set -o xtrace

declare -g tempDir=''
tempDir="$(mktemp -d)"
function removeTempDir() {
  if [ -d "${tempDir}" ]; then
    rm -rf "${tempDir}"
  fi
}
trap removeTempDir EXIT

declare -g packageDir="$(stack path --project-root)/otel-api-trace-core"
declare -g semconvVersion=1.12.0
declare -g specVersion=v${semconvVersion}
declare -g schemaURL=https://opentelemetry.io/schemas/${semconvVersion}
declare -g generatorVersion=0.8.0

cd "${tempDir}"
cp -R "${packageDir}/scripts/templates" ./templates
rm -rf opentelemetry-specification || true
mkdir opentelemetry-specification
cd opentelemetry-specification
git init
git remote add origin https://github.com/open-telemetry/opentelemetry-specification.git
git fetch origin "${specVersion}"
git reset --hard FETCH_HEAD
cd "${tempDir}"
find .

docker run --rm \
  -v "${tempDir}/opentelemetry-specification/semantic_conventions/trace:/source" \
  -v "${tempDir}/templates:/templates" \
  -v "${packageDir}/library/OTel/API/Trace/Core/:/output" \
  otel/semconvgen:${generatorVersion} \
  -f /source code \
  --template /templates/SemConvAttrs.hs.j2 \
  --output /output/Attributes.hs \
  -Dwhat=SemConvAttrs \
  -DschemaUrl=${schemaURL} \
  -Dmodule=OTel.API.Trace.Core.Attributes
