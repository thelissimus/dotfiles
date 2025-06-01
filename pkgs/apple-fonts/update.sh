#!/usr/bin/env sh

printf '{' >sources.nix

for url in \
  https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg \
  https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg \
  https://devimages-cdn.apple.com/design/resources/download/NY.dmg; do

  font=$(basename $url | tr '[:upper:]' '[:lower:]' | sed 's/\.dmg//')
  hash=$(nix hash to-sri --type sha256 "$(nix-prefetch-url $url)")
  printf '%s = { url = "%s"; hash = "%s"; };' "$font" "$url" "$hash" >>sources.nix
done

printf '}' >>sources.nix
