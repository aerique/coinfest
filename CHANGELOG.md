# Changelog

## 1.0 (2021-03-03)

### Changed

- Use `harbour-coinfest` instead of `coinfest` as name on the system
  side of things.

### Fixed

- Use requested Kraken asset pairs instead of requesting them again.

### Removed

- Do not compile in Kraken asset pairs.  It leads to more troubles than
  it is worth.

## 0.11 (2021-03-02)

### Added

- Reset poll time on manual refresh.
- Naive migration from old to new poll time format.

### Changed

- Make poll times similar to Jolla apps.
    - But no 5 minute poll time! (too short)
- Replace BusyLabel with custom widget.

### Fixed

- Use BackgroundJob instead of Timer.

## 0.10 (2021-02-02)

### Changed

- Increase delay between API calls to 0.1s.

### Fixed

- Better handling of network errors.
- Show default setting when no config file exists.

## 0.09 (2021-01-29)

First released version.

## Resources

- https://keepachangelog.com/
