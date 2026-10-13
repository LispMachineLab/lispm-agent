#!/bin/sh
# Prepare a scratch HOME holding a fresh copy of the sample project, for
# demo/record.sh: the agent edits that copy, and LispM keeps its sessions
# under $HOME/.emacs.d/lispm/, so recording leaves the checkout alone.
set -eu
demo=$(cd "$(dirname "$0")" && pwd)
home=${LISPM_DEMO_HOME:-/tmp/lispm-demo}

# Run git on the scratch repository, whoever called us.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_CONFIG_PARAMETERS GIT_CONFIG_COUNT
export HOME="$home" GIT_CONFIG_NOSYSTEM=1

rm -rf "$home"
mkdir -p "$home"
cp -R "$demo/slug" "$home/slug"
cat > "$home/.gitconfig" <<EOF
[user]
	name = LispM Demo
	email = demo@example.invalid
[commit]
	gpgsign = false
[init]
	defaultBranch = main
EOF
cd "$home/slug"
git init -q
git add -A
git commit -qm "Make slugs from titles"
