import argparse
import pathlib
import re

def get_curr_version(cabal_contents, file_name):
    match = re.search(r"version:\s*([\d.]+)", cabal_contents)
    if match is None:
        raise ValueError(f"No version found in {file_name}")
    return match.group(1)

def increment_version(version, type):
    zero, major, minor, *patch = version.split(".")

    if zero != "0":
        raise ValueError(f"Version should start with 0, but got {version}")
    if len(patch) > 1:
        raise ValueError(f"Expected at most one patch version, got {patch}")

    if type == 'major':
        major = str(int(major) + 1)
    elif type == 'minor':
        minor = str(int(minor) + 1)
    elif type == 'patch':
        if not patch:
            patch = ["1"]
        else:
            patch = [str(int(patch[0]) + 1)]

    return ".".join(["0", major, minor] + patch)

def prepend_changelog(library, new_version):
    changelog = library / "CHANGELOG.md"
    if not changelog.is_file():
        raise ValueError(f"No changelog found at {changelog}")

    with changelog.open() as f:
        contents = f.read()

    new_contents = "\n".join([f"# {new_version}", "", "* TODO", "", contents])

    with changelog.open("w") as f:
        f.write(new_contents)

def bump_version(library, type):
    cabal_file = library / (library.name + ".cabal")
    if not cabal_file.is_file():
        raise ValueError(f"No cabal file found at {cabal_file}")

    with cabal_file.open() as f:
        contents = f.read()

        curr_version = get_curr_version(contents, cabal_file)
        new_version = increment_version(curr_version, type)
        new_contents = re.sub(curr_version, new_version, contents)

    with cabal_file.open("w") as f:
        f.write(new_contents)

    prepend_changelog(library, new_version)
    print(f"{library.name:<19} {curr_version:<9} ->  {new_version:<9}")

def dependencies_to_update(library, type):
    # https://github.com/stevenfontanella/microlens/blob/master/CONTRIBUTING.md#releasing-a-new-version-of-any-package
    to_update = [library.name]
    if type == 'major':
        if library.name == 'microlens':
            to_update += ["microlens-ghc", "microlens-th", "microlens-mtl", "microlens-platform", "microlens-contra"]
        elif library.name in ['microlens-ghc', 'microlens-th', 'microlens-mtl']:
            to_update += ["microlens-platform"]
    elif type == 'minor':
        if library.name == 'microlens':
            to_update += ["microlens-ghc", "microlens-platform"]
        elif library.name in ['microlens-ghc', 'microlens-th', 'microlens-mtl']:
            to_update += ["microlens-platform"]
    return to_update

def main(library, type):
    for lib in map(pathlib.Path, dependencies_to_update(library, type)):
        bump_version(lib, type)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("library", type=pathlib.Path)
    parser.add_argument("type", choices=["major", "minor", "patch"])
    args = parser.parse_args()

    main(args.library, args.type)
