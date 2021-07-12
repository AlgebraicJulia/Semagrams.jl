using Pkg.TOML

const SEMAGRAMS_VERSION = let
  project_file = normpath(joinpath(@__DIR__, "..", "Project.toml"))
  project = TOML.parsefile(project_file)
  VersionNumber(project["version"])
end

const BUNDLE_URL = "https://unpkg.com/semagrams@$SEMAGRAMS_VERSION/dist/app.bundle.js"
const BUNDLE_DIR = normpath(joinpath(@__DIR__, "bundles"))
const BUNDLE_PATH = normpath(joinpath(BUNDLE_DIR, "app.bundle.js"))

function download_js_bundle()
  mkpath(BUNDLE_DIR)

  @info "Downloading Semagrams bundle from unpkg"
  download(BUNDLE_URL, BUNDLE_PATH)
end

function build_js_bundle()
  mkpath(BUNDLE_DIR)

  cd(joinpath(@__DIR__, "..", "javascript"))

  @info "Building Semagrams bundle"
  run(`npm run devbuild`)

  run(`cp dist/app.bundle.js ../deps/bundles/`)

  run(`rm -rf dist`)
end

if get(ENV, "SEMAGRAMS_DEV", "false") == "true"
  build_js_bundle()
else
  download_js_bundle()
end
