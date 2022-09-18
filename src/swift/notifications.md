---
eleventyNavigation:
  key: Notifications
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS supports two kinds of user notifications.
Local notifications are sent from an app running on the device.
Remote (or push) notifications are sent from a server.

## Local Notifications

Stewart Lynch created an excellent 4-part series of YouTube videos
{% aTargetBlank "https://www.youtube.com/watch?v=tNaSlfLeCB0",
"Local Notifications" %}.
The code below is my version of his code.

### LocalNotification struct

```swift
import Foundation

// This is based on the YouTube videos by Stewart Lynch
// at https://www.youtube.com/watch?v=tNaSlfLeCB0.
struct LocalNotification {
    enum ScheduleType {
        case calendar, timeInterval
    }

    var identifier: String
    var title: String
    var subtitle: String?
    var body: String
    var scheduleType: ScheduleType
    var dateComponents: DateComponents?
    var timeInterval: Double? // in seconds
    var repeats: Bool

    // This provides a way to pass data from a tapped notification
    // to the app that created the notification.
    var userInfo: [AnyHashable: Any]?

    // This describes buttons that should appear in a notification
    // when it is long-pressed.
    var categoryIdentifier: String?

    init(
        identifier: String,
        title: String,
        body: String,
        dateComponents: DateComponents,
        repeats: Bool
    ) {
        scheduleType = .calendar
        self.identifier = identifier
        self.title = title
        self.body = body
        timeInterval = nil
        self.dateComponents = dateComponents
        self.repeats = repeats
    }

    init(
        identifier: String,
        title: String,
        body: String,
        timeInterval: Double,
        repeats: Bool
    ) {
        scheduleType = .timeInterval
        self.identifier = identifier
        self.title = title
        self.body = body
        self.timeInterval = timeInterval
        dateComponents = nil
        self.repeats = repeats
    }

    static func sendNow(title: String, body: String) {
        let identifier = "r.mark.volkmann.gmail.com.NotificationsDemo"
        let notification = LocalNotification(
            identifier: identifier,
            title: title,
            body: body,
            timeInterval: 1, // seconds from now; zero doesn't work
            repeats: false
        )
        Task {
            await LocalNotificationManager.shared.schedule(
                notification: notification
            )
        }
    }
}
```

### LocalNotificationManager class

```swift
import NotificationCenter

@MainActor // so changes to @Published properties occur on the main thread
class LocalNotificationManager: NSObject, ObservableObject {
    @Published var granted: Bool?
    // @Published var nextView: NextView?
    @Published var pendingRequests: [UNNotificationRequest] = []

    static let shared = LocalNotificationManager()

    let notificationCenter = UNUserNotificationCenter.current()

    private override init() {
        super.init()

        // This allows presenting notifications scheduled by this app
        // while it is in the foreground.
        // Without this, local notifications sent while
        // the app is in the foreground will not be presented.
        notificationCenter.delegate = self
    }

    func authorize() async throws {
        try await notificationCenter.requestAuthorization(
            options: [.alert, .badge, .sound]
        )

        // This defines a category of actions.
        // When a notification that uses the category is long-pressed,
        // a button for each action is displayed.
        // See the timeIntervalButton computed property
        // in NotificationListView.swift which uses the category this defines.
        registerActions()

        await updateGranted()
    }

    func openSettings() {
        if let url = URL(string: UIApplication.openSettingsURLString) {
            if UIApplication.shared.canOpenURL(url) {
                Task {
                    await UIApplication.shared.open(url)
                }
            }
        }
    }

    func removeAllRequests() {
        notificationCenter.removeAllPendingNotificationRequests()
        pendingRequests.removeAll()
    }

    func removeRequest(withIdentifier identifier: String) {
        notificationCenter.removePendingNotificationRequests(
            withIdentifiers: [identifier]
        )
        if let index = pendingRequests.firstIndex(
            where: { $0.identifier == identifier }
        ) {
            pendingRequests.remove(at: index)
        }
    }

    func schedule(notification: LocalNotification) async {
        let content = UNMutableNotificationContent()
        content.title = notification.title
        if let subtitle = notification.subtitle {
            content.subtitle = subtitle
        }
        content.body = notification.body
        content.sound = .default
        if let userInfo = notification.userInfo {
            content.userInfo = userInfo
        }
        if let categoryIdentifier = notification.categoryIdentifier {
            content.categoryIdentifier = categoryIdentifier
        }

        let trigger: UNNotificationTrigger!

        switch notification.scheduleType {
        case .calendar:
            guard let dateComponents = notification.dateComponents else { return }
            trigger = UNCalendarNotificationTrigger(
                dateMatching: dateComponents,
                repeats: notification.repeats
            )

        case .timeInterval:
            guard let timeInterval = notification.timeInterval else { return }
            trigger = UNTimeIntervalNotificationTrigger(
                timeInterval: timeInterval,
                repeats: notification.repeats
            )
        }

        let request = UNNotificationRequest(
            identifier: notification.identifier,
            content: content,
            trigger: trigger
        )
        try? await notificationCenter.add(request)

        await updatePendingRequests()
    }

    func updateGranted() async {
        let settings = await notificationCenter.notificationSettings()
        granted = settings.authorizationStatus == .authorized
    }

    func updatePendingRequests() async {
        pendingRequests = await notificationCenter.pendingNotificationRequests()
    }
}

extension LocalNotificationManager: UNUserNotificationCenterDelegate {
    // The actions described here will appear as buttons
    // in a notification if:
    // 1) The notification has a categoryIdentifier of "snooze".
    //    See the timeIntervalButton computed property
    //    in NotificationListView.swift.
    // 2) The user long-presses on the notification.
    // These buttons will be difficult for users to discover, so include
    // text in the notification that encourages users to long-press for options.
    func registerActions() {
        // These are example actions that we are not currently used.
        // Later when we send notifications related to resident questions,
        // we will change this to contain all the possible question answers.
        let snooze10Action = UNNotificationAction(
            identifier: "snooze10",
            title: "Snooze for 10 seconds"
        )
        let snooze60Action = UNNotificationAction(
            identifier: "snooze60",
            title: "Snooze for one minute"
        )
        let snoozeCategory = UNNotificationCategory(
            identifier: "snooze",
            actions: [snooze10Action, snooze60Action],
            intentIdentifiers: []
        )
        // The identifier values above become the value of the
        // actionIdentifier property on the response object
        // when the corresponding button is tapped.
        notificationCenter.setNotificationCategories([snoozeCategory])
    }

    // This is called *before* each notification is displayed.
    // It indicates *how* the user should be alerted.
    func userNotificationCenter(
        _: UNUserNotificationCenter,
        willPresent _: UNNotification
    ) async -> UNNotificationPresentationOptions {
        await updatePendingRequests()
        return [.banner, .sound]
    }

    // This is called *after* each notification is displayed.
    func userNotificationCenter(
        _: UNUserNotificationCenter,
        didReceive _: UNNotificationResponse
    ) async {
        /*
         // This is example code that shows a way to handle
         // snoozing a notification so it appears again later.
         // We way want to place other code here in the future
         // that handles clicking buttons in a notification in a different
         // way such as treating them as answers to a resident question.

         let content = response.notification.request.content
         if let value = content.userInfo["nextView"] as? String {
             // nextView is a published property that is used
             // in the "sheet" view modifier in NotificationsListView
             // to display a specified view in a sheet.
             // Alternatively, it could be used to navigate to a given view.
             nextView = NextView(rawValue: value)
         }

         // If the user long-presses the notification and
         // then tapped one of the snooze buttons ...
         let snoozeInterval: Double =
             response.actionIdentifier == "snooze10" ? 10 :
             response.actionIdentifier == "snooze60" ? 60 :
             0
         if snoozeInterval != 0 {
             // Create and schedule a new notification request
             // that will notify the user again later.
             let content = response.notification.request.content
             let newContent = content.mutableCopy() as! UNMutableNotificationContent
             let newTrigger = UNTimeIntervalNotificationTrigger(
                 timeInterval: snoozeInterval,
                 repeats: false
             )
             let request = UNNotificationRequest(
                 identifier: UUID().uuidString,
                 content: newContent,
                 trigger: newTrigger
             )

             do {
                 try await notificationCenter.add(request)
                 await updatePendingRequests()
             } catch {
                 EMRLogger.shared.logError(
                     error
                 )
             }
         }
         */
    }
}
```

### Example App

For an example of sending local notifications
using the utility type defined above, see the {% aTargetBlank
"https://github.com/mvolkmann/NotificationsDemo", "NotificationsDemo" %}
project in GitHub.

## Remote Notifications

TODO: Add this content.
