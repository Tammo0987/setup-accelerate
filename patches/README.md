# Submodule patches

## accelerate

Base: `origin/new-pipeline` (`1fcf9b5d`)
Branch the patches were exported from: `ilp-constraint-language`

Apply on a fresh checkout:

```sh
cd accelerate
git checkout -b ilp-constraint-language origin/new-pipeline
git am ../patches/accelerate/*.patch
```
