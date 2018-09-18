#!/bin/sh

set -e

elm make Main.elm --output index.html --optimize
