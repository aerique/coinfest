# NOTICE:
#
# Application name defined in TARGET has a corresponding QML filename.
# If name defined in TARGET is changed, the following needs to be done
# to match new name:
#   - corresponding QML filename must be changed
#   - desktop icon filename must be changed
#   - desktop filename must be changed
#   - icon definition filename in desktop file must be changed
#   - translation filenames have to be changed

LISP_FILES = make.lisp \
    lisp/dependencies.lisp \
    lisp/coinfest-common.lisp \
    lisp/coinfest-kraken-api.lisp \
    lisp/coinfest-meta-api.lisp \
    lisp/coinfest.lisp \
    lisp/qml.lisp \
    lisp/app.lisp \
    lisp/app.asd

lisp.output = libapp.a
lisp.commands = eql5 -platform minimal $$PWD/make.lisp
lisp.input = LISP_FILES

QMAKE_EXTRA_COMPILERS += lisp

# The name of your application
TARGET = harbour-coinfest
PRE_TARGETDEPS += libapp.a

CONFIG += sailfishapp
LIBS += -lecl -leql5 -L. -lapp

SOURCES += src/coinfest.cc

DISTFILES += qml/harbour-coinfest.qml \
    qml/cover/CoverPage.qml \
    qml/pages/TickersPage.qml \
    qml/pages/TickerDetailsPage.qml \
    qml/pages/AddTickerDialog.qml \
    qml/pages/SettingsPage.qml \
    qml/pages/AboutPage.qml \
    rpm/harbour-coinfest.changes.in \
    rpm/harbour-coinfest.changes.run.in \
    rpm/harbour-coinfest.spec \
    rpm/harbour-coinfest.yaml \
#    translations/*.ts \
    harbour-coinfest.desktop

SAILFISHAPP_ICONS = 86x86 108x108 128x128 172x172

# to disable building translations every time, comment out the
# following CONFIG line
# CONFIG += sailfishapp_i18n

# German translation is enabled as an example. If you aren't
# planning to localize your app, remember to comment out the
# following TRANSLATIONS line. And also do not forget to
# modify the localized app name in the the .desktop file.
# TRANSLATIONS += translations/harbour-coinfest-de.ts
