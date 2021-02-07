#!/usr/bin/env bash

set -eu

hook="$(basename "$0")"

tmp_dir="$(mktemp -d)"

trap 'printf "rm -rf %s\n" "$tmp_dir"; rm -rf "$tmp_dir"' EXIT

git checkout-index --prefix="$tmp_dir/" --all

ps_dir="$tmp_dir"
ln -s "$(realpath node_modules)" "$ps_dir"
ln -s "$(realpath output)" "$ps_dir"
ln -s "$(realpath .spago)" "$ps_dir"

cd "$ps_dir"

git init
git add .
git commit -m 'Test commit'

case $hook in
    pre-commit)
        npm run lint
        npm run bundle
        cabal build
        ;;

    pre-push)
        make test
        ;;

    *)
        echo Unknown hook "$hook"
        exit 1
        ;;
esac
