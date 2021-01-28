import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        PullDownMenu {
            MenuItem {
                text: "About"
                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
            }

            MenuItem {
                text: "Reset Settings"
                onClicked: remorse.execute(
                    "Resetting all settings to default",
                    function() { console.log("Reset all settings!") })
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader { title: "Settings" }

            //SectionHeader { text: "General" }

            ComboBox {
                id: tickerRefresh
                objectName: "tickerRefresh"
                width: parent.width
                description: "between refreshing all tickers"
                currentIndex: Lisp.call("cf:get-ticker-refresh-for-combobox")
                menu: ContextMenu {
                    MenuItem { text: "30 minutes" }
                    MenuItem { text: "2 hours" }
                    MenuItem { text: "6 hours" }
                    MenuItem { text: "12 hours" }
                    MenuItem { text: "1 day" }
                    onClicked: function() {
                        Lisp.call("cf:set-ticker-refresh", tickerRefresh.value)
                        tickerRefreshTimer.interval = 1000 *
                            Lisp.call("cf:get-ticker-refresh")
                    }
                }
            }
        }
    }
}
