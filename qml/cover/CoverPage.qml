import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

CoverBackground {
    id: cover

    Label {
        id: appNameLabel
        x: Theme.paddingLarge
        y: Theme.paddingLarge
        color: Theme.highlightColor
        //font.pixelSize: Theme.fontSizeExtraSmall
        font.pixelSize: Theme.fontSizeSmall
        text: "Coinfest"
    }

    SilicaListView {
        anchors {
            top: appNameLabel.bottom
            topMargin: Theme.paddingLarge
            // The anchor for the CoverAction(List) is too high.
            bottom: parent.bottom
            bottomMargin: Theme.paddingLarge * 4
        }
        width: parent.width
        model: overviewModel

        delegate: ListItem {
            id: listEntry
            contentHeight: Theme.fontSizeTiny * 2

            Label {
                id: cpLabel
                anchors {
                    left: parent.left
                    leftMargin: Theme.horizontalPageMargin
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.5
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeTiny
                text: Lisp.call("cf:ticker-display-name", modelData)
            }

            Label {
                id: priceLabel
                anchors {
                    left: cpLabel.right
                    leftMargin: Theme.paddingSmall
                }
                width: (parent.width - (2 * Theme.horizontalPageMargin)) * 0.5
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeTiny
                text: Lisp.call("cf:ticker-price", modelData)
            }
        }
    }

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: function() {
                Lisp.call("cf:refresh-tickers")
                setOverviewModelTimer.running = true
            }
        }
    }
}
