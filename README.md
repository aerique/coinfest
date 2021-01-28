# Coinfest

A cryptocurrency tickers app written in Common Lisp.

Thanks to [Renaud Casenave-Péré](https://openrepos.net/user/856/programs)
for packaging both ECL and EQL5.

The canonical home page of this project is https://git.sr.ht/~aerique/coinfest

(This project is also pushed to GitLab and GitHub but those sites are
not monitored for support.)

## Known Issues

- I think I forgot to put in the ability to get updated Kraken tickers.
- Kraken is the only supported exchange at the moment (I don't need
  anything else).
- Having two remorse actions running at the same time will result in
  only one being executed (and an error on STDOUT when running from the
  CLI).
- When running from the CLI you'll see `[W] unknown:-1 - <Unknown File>:
  Syntax error` when downloading messages or logging in.  It seems to be
  connected to spawning threads in ECL and even happens when the
  `download-messages-thread` or `login-and-register-thread` functions
  are empty.  When these functions are not called the error does not
  appear.  However, it does not seems to affect the running of those
  functions.  I have not investigated further.

## Build

### Dependencies

- [SailfishOS Builds in Docker](https://git.sr.ht/~aerique/sfosbid)

### Instructions

The project was build using
[SailfishOS Builds in Docker](https://git.sr.ht/~aerique/sfosbid).

Refer to that project's README on how to get it running.

The actual build steps for Pusfofefe are:

- run `sfosbid` Docker container
- `sb2 -t SailfishOS-latest-armv7hl -m sdk-install -R`
    - `rpm --import https://sailfish.openrepos.net/openrepos.key`
    - `zypper ar -f https://sailfish.openrepos.net/razcampagne/personal-main.repo`
    - `zypper in eql5`
    - `exit`
- `git clone https://git.sr.ht/~aerique/coinfest`
- `cd coinfest`
- `sb2 -t SailfishOS-latest-armv7hl -m sdk-build -R`
    - `qmake`
    - `make`
- `mb2 -t SailfishOS-latest-armv7hl build`
    - or whichever target you require ofcourse
- you now have an RPM in the `RPMS` directory which you can copy to your
  phone and install there

## To Do

### High Priority

- [ ] describe current downloading of tickers wrt to threading, reasons,
      all used functions and vars, etc.

### Normal Priority

*nothing*

### Low Priority

- [ ] figure out why ECL/EQL won't build from `mb2` step below
- [ ] try again to make deploy workflow one command
    - `sb2 -t SailfishOS-latest-armv7hl -m sdk-build -R`
        - `qmake`
        - `make`
    - `mb2 -t SailfishOS-latest-armv7hl build`
    - `scp projects/pusfofefe/RPMS/coinfest-0.2-1.armv7hl.rpm nemo@192.168.2.15:`
    - `pkcon -y install-local coinfest-0.2-1.armv7hl.rpm`

## Attributions

App icon by [dmitri13](https://www.flaticon.com/authors/dmitri13) from
[www.flaticon.com](https://www.flaticon.com/).
