using Pkg.TOML

const SEMAGRAMS_VERSION = let
  project_file = normpath(joinpath(@__DIR__, "..", "Project.toml"))
  project = TOML.parsefile(project_file)
  VersionNumber(project["version"])
end

const BUNDLE_URL = "https://unpkg.com/semagrams@$SEMAGRAMS_VERSION/dist/app.bundle.js"
const BUNDLE_DIR = normpath(joinpath(@__DIR__, "bundles"))
const BUNDLE_PATH = normpath(joinpath(BUNDLE_DIR, "app.bundle.js"))

mkpath(BUNDLE_DIR)

@info "Downloading Semagrams bundle from unpkg"
download(BUNDLE_URL, BUNDLE_PATH)
