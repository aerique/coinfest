import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    SilicaListView {
        id: slv
        anchors.fill: parent
        model: overviewModel

        header: PageHeader { title: "Tickers" }

        delegate: ListItem {
            id: listEntry

            Label {
                id: cpLabel
                anchors {
                    left: parent.left
                    leftMargin: Theme.horizontalPageMargin
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.30
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                text: Lisp.call("cf:ticker-display-name", modelData)
            }

            Label {
                anchors {
                    top: cpLabel.bottom
                    left: parent.left
                    leftMargin: Theme.horizontalPageMargin
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.30
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeExtraSmall
                text: Lisp.call("cf:ticker-exchange", modelData)
            }

            Label {
                id: priceLabel
                anchors {
                    left: cpLabel.right
                    leftMargin: Theme.paddingSmall
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.35
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                //font.pixelSize: Theme.fontSizeLarge
                text: Lisp.call("cf:ticker-price", modelData)
            }

            Label {
                anchors {
                    top: priceLabel.bottom
                    left: cpLabel.right
                    leftMargin: Theme.paddingSmall
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.35
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeExtraSmall
                text: Lisp.call("cf:ticker-timestamp", modelData)
            }

            Label {
                id: prevPriceLabel
                anchors {
                    left: priceLabel.right
                    leftMargin: Theme.paddingSmall
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.35
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                color: Theme.highlightColor
                text: Lisp.call("cf:ticker-previous-price", modelData)
            }

            Label {
                anchors {
                    top: prevPriceLabel.bottom
                    left: priceLabel.right
                    leftMargin: Theme.paddingSmall
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.35
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeExtraSmall
                color: Theme.highlightColor
                text: Lisp.call("cf:ticker-previous-timestamp", modelData)
            }

            onClicked: pageStack.push(Qt.resolvedUrl("TickerDetailsPage.qml"),
                                      { tickerUUID: modelData,
                                        tickerCount: slv.count })

            menu: ContextMenu {
                //MenuItem {
                //    text: "Refresh"
                //    onClicked: console.log("Refreshing currency pair!")
                //}

                MenuItem {
                    text: "Delete"
                    onClicked: listEntry.remorseAction("Deleting message",
                        function() {
                            Lisp.call("cf:delete-ticker", modelData)
                        })
                }
            }
        }

        PullDownMenu {
            MenuItem {
                text: "Settings"
                onClicked: pageStack.push(Qt.resolvedUrl("SettingsPage.qml"))
            }

            MenuItem {
                text: "Add Ticker"
                onClicked: pageStack.push(Qt.resolvedUrl("AddTickerDialog.qml"))
            }

            MenuItem {
                text: "Refresh"
                onClicked: function() {
                    Lisp.call("cf:refresh-tickers")
                    setOverviewModelTimer.running = true
                }
            }
        }

        VerticalScrollDecorator {}

        // This didn't work when directly positioned below `delegate:` ?!
        ViewPlaceholder {
            enabled: slv.count == 0
            text: "No tickers"
            hintText: "Pull down to add a ticker"
        }
    }

    onActiveFocusChanged: {
        //console.log("Something something cover!")
    }
}
