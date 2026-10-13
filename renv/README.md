# renv for emma_report

`renv.lock` is **mirrored from** [`eco-emma/emma_docker`](https://github.com/eco-emma/emma_docker)
(the canonical package set for `adamwilsonlab/emma` images).

- Update packages in **emma_docker** (DESCRIPTION + regenerate lockfile), rebuild the image, then copy the new `renv.lock` here.
- CI restores with `renv::restore()` and uses the emma container library when versions match.
