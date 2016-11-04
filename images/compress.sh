#!/bin/sh

# Compresses the resized images to the format that will be served. Because the
# effect of jpeg compression depends a lot on the image, the compression
# settings have been tweaked for every image individually.

# Mozjpeg insists on conflicting with libjpeg-turbo, but many packages depend on
# libjpeg-turbo and I don't want to replace it system-wide. Fortunately there
# is an aur package that installs mozjpeg to /opt.
mozjpeg='/opt/mozjpeg/bin/cjpeg -quality'

mkdir -p compressed

$mozjpeg 90.0 original/is-it-worth-it.jpg > compressed/is-it-worth-it.jpg
