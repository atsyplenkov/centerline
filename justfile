_default:
    just --list

# Format R code
fmt:
    air format .

# Run jarl linter
lint:
    jarl check . --fix --allow-dirty || true

# Update R documentation
document:
    Rscript -e "devtools::document()"

# Build and test the R package
test:
    #!/usr/bin/env bash
    R CMD build .
    R CMD check --as-cran --no-manual centerline_*.tar.gz
    rm -rf centerline_*.tar.gz centerline.Rcheck

install:
    #!/usr/bin/env bash
    rm centerline_*.tar.gz
    R CMD build .
    R CMD INSTALL centerline_*.tar.gz
    rm centerline_*.tar.gz
